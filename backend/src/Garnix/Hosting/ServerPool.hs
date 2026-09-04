-- | Acquiring and releasing hosted servers, with a warm pool in front of
-- the provisioner and a resource budget behind it.
module Garnix.Hosting.ServerPool
  ( acquireServer,
    releaseServer,
    warmPool,
    committedResources,
    fitsBudget,
    leavesReserveFree,
    reserveFor,
    HostingBudget (..),
    sshArgsFor,
    sshArgsForAddress,
  )
where

import Control.Exception.Safe qualified as Safe
import Garnix.DB.Hosting qualified as DB
import Garnix.Hosting.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types (BuildId, Error (..), GhPullRequestId)

-- | vCPU and MiB already spoken for: every live server plus every instance
-- in the pool, warm or still being built.
committedResources :: M (Int, Int)
committedResources = do
  servers <- DB.getLiveServers
  pool <- DB.getPoolServers
  let tiers =
        map _serverInfoTier servers
          <> map _preprovisionedServerTier pool
      resources = map tierResources tiers
  pure (sum (map fst resources), sum (map snd resources))

-- | Whether one more instance of @tier@ fits within the budget.
fitsBudget :: HostingBudget -> (Int, Int) -> ServerTier -> Bool
fitsBudget budget used tier = leavesReserveFree budget used (tierResources tier) (0, 0)

-- | Whether committing @extra@ on top of @used@ stays under the caps while
-- still leaving @reserve@ free.
leavesReserveFree ::
  HostingBudget ->
  -- | Already committed.
  (Int, Int) ->
  -- | About to be committed.
  (Int, Int) ->
  -- | Has to stay free afterwards.
  (Int, Int) ->
  Bool
leavesReserveFree budget (usedVcpus, usedMiB) (extraVcpus, extraMiB) (freeVcpus, freeMiB) =
  within (_hostingBudgetVcpus budget) (usedVcpus + extraVcpus + freeVcpus)
    && within (_hostingBudgetMemoryMiB budget) (usedMiB + extraMiB + freeMiB)
  where
    within Nothing _ = True
    within (Just cap) wanted = wanted <= cap

-- | What this acquisition has to leave behind. Only pull requests owe the
-- reserve; a branch deploy is what it is being kept for.
reserveFor :: HostingBudget -> Maybe GhPullRequestId -> (Int, Int)
reserveFor budget = \case
  Nothing -> (0, 0)
  Just _ -> branchReserveResources budget

-- | Create one warm instance of @tier@ and leave it in the pool.
warmPool :: ServerTier -> M PreprovisionedServerId
warmPool tier = do
  provider <- provisionerProvider
  poolId <- DB.createPoolServer provider tier
  server <-
    provisionServer poolId tier `cleaningUpOnFailure` DB.deletePoolServer poolId
  DB.markPoolServerReady server
  pure poolId

-- | Run @cleanup@ if @action@ fails, then re-raise.
--
-- Both channels are caught. 'M' carries a refusal through 'MonadError', which
-- an exception handler alone never sees, and a driver can still raise a
-- genuine IO exception.
cleaningUpOnFailure :: M a -> M () -> M a
cleaningUpOnFailure action cleanup =
  Safe.try (try action) >>= \case
    Right (Right value) -> pure value
    Right (Left problem) -> cleanup >> rethrow problem
    Left (problem :: SomeException) -> cleanup >> throwM problem

-- | Get a server for a deployment: claim a warm instance if there is one,
-- otherwise create one within budget.
acquireServer ::
  HostingBudget ->
  ServerTier ->
  BuildId ->
  Maybe GhPullRequestId ->
  Bool ->
  M ServerId
acquireServer budget tier buildId pullRequest isPrimary = do
  provider <- provisionerProvider
  let claim = DB.claimPoolServer provider tier buildId pullRequest isPrimary
      reserve = reserveFor budget pullRequest
      refuse why =
        throw
          $ OtherError
          $ "no capacity for a "
          <> getServerTier tier
          <> " server: "
          <> why
  -- Checked before the claim, not only on the create path: a pooled guest is
  -- already counted in 'committedResources', so claiming one never moves the
  -- totals -- but it can still hand a pull request the last warm guest the
  -- reserve was keeping for branch deploys. Hence no extra resources here;
  -- the create path below adds the tier's own.
  unless (reserve == (0, 0)) $ do
    used <- committedResources
    unless (leavesReserveFree budget used (0, 0) reserve)
      $ refuse "the hosting budget is committed down to the branch reserve"
  claim >>= \case
    Just serverId -> pure serverId
    Nothing -> do
      used <- committedResources
      unless (leavesReserveFree budget used (tierResources tier) reserve)
        $ refuse
        $ if reserve == (0, 0)
          then "the hosting budget is fully committed"
          else "the hosting budget is committed down to the branch reserve"
      void $ warmPool tier
      claim >>= \case
        Just serverId -> pure serverId
        Nothing ->
          throw
            $ OtherError
            $ "a concurrent deployment claimed the "
            <> getServerTier tier
            <> " server that was just provisioned; retry"

-- | Tear a deployed server down and stop counting it against the budget.
releaseServer :: ServerId -> M ()
releaseServer serverId = do
  server <- DB.getServer serverId
  case server of
    Nothing -> throw $ OtherError "releaseServer: no such server"
    Just info -> do
      let teardown = case _serverInfoInstanceId info of
            Nothing -> pure ()
            Just instanceId -> deleteServer instanceId
      teardown `cleaningUpOnFailure` DB.endServer serverId
      DB.endServer serverId

-- * Reaching a guest

-- | The address and ssh options to reach a deployed guest with, using the
-- hosting key the provisioner authorized on it.
sshArgsFor :: ServerInfo -> M (Text, [Text])
sshArgsFor server = case serverAddressText (_serverInfoAddress server) of
  Nothing ->
    throw
      $ ProvisioningError
      $ "server "
      <> showPretty (_serverInfoId server)
      <> " has no address to ssh to"
  Just address -> sshArgsForAddress address

sshArgsForAddress :: Text -> M (Text, [Text])
sshArgsForAddress address = do
  keyFiles <- view #hostingSshKeys
  pure
    ( address,
      concatMap (\keyFile -> ["-i", cs keyFile]) keyFiles
        <> [ "-o",
             "BatchMode=yes",
             -- Guests are created fresh with a new host key on every claim, and
             -- are reached over a private bridge we control, so pinning host
             -- keys would only produce spurious mismatches.
             "-o",
             "StrictHostKeyChecking=no",
             "-o",
             "UserKnownHostsFile=/dev/null",
             "-o",
             "ConnectTimeout=15"
           ]
    )
