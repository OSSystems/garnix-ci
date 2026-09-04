-- | Acquiring and releasing hosted servers, with a warm pool in front of
-- the provisioner and a resource budget behind it.
module Garnix.Hosting.ServerPool
  ( acquireServer,
    releaseServer,
    warmPool,
    committedResources,
    fitsBudget,
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
fitsBudget budget (usedVcpus, usedMiB) tier =
  let (vcpus, miB) = tierResources tier
      within Nothing _ = True
      within (Just cap) wanted = wanted <= cap
   in within (_hostingBudgetVcpus budget) (usedVcpus + vcpus)
        && within (_hostingBudgetMemoryMiB budget) (usedMiB + miB)

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
  claim >>= \case
    Just serverId -> pure serverId
    Nothing -> do
      used <- committedResources
      unless (fitsBudget budget used tier)
        $ throw
        $ OtherError
        $ "no capacity for a "
        <> getServerTier tier
        <> " server: the hosting budget is fully committed"
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
      $ "server " <> showPretty (_serverInfoId server) <> " has no address to ssh to"
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
