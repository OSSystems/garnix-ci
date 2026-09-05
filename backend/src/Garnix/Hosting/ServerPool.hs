-- | Acquiring and releasing hosted servers, with a warm pool in front of
-- the provisioner and a resource budget behind it.
module Garnix.Hosting.ServerPool
  ( acquireServer,
    releaseServer,
    warmPool,
    reconcilePool,
    PoolProblem (..),
    poolServerUnusable,
    poolShortfall,
    poolCreationGrace,
    poolEntryMaxAge,
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
import Data.Map qualified as Map
import Garnix.DB.Hosting qualified as DB
import Garnix.Duration
import Garnix.Hosting.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types (BuildId, Error (..), GhPullRequestId, showDebug)

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

-- * Keeping the pool at its target

-- | How long a pool row may sit without an address before it is taken to be a
-- remnant. Above the provisioner's own 35 minute create timeout, so it can
-- never destroy a guest that is still being created.
poolCreationGrace :: Duration
poolCreationGrace = fromMinutes @Int 40

-- | How long a warm instance may wait to be claimed before it is recycled. A
-- guest boots the image the provisioner had when it was created.
poolEntryMaxAge :: Duration
poolEntryMaxAge = fromHours @Int 24

-- | Why a warm instance is no longer usable.
data PoolProblem = PoolDead | PoolExpired | PoolStuckCreating
  deriving stock (Eq, Show)

instance Pretty PoolProblem where
  pretty = \case
    PoolDead -> "the provisioner does not report it as running"
    PoolExpired -> "it has been warm for longer than " <> pretty (show poolEntryMaxAge)
    PoolStuckCreating -> "it never finished being created"

-- | Whether a pool instance has to go, given the provisioner's reading of it.
--
-- A provisioner that has gone quiet must not be read as every guest being
-- dead, so 'Nothing' for the status leaves the instance alone.
poolServerUnusable ::
  -- | Now.
  UTCTime ->
  -- | What the provisioner says about it, when it says anything.
  Maybe Text ->
  PreprovisionedServer ->
  Maybe PoolProblem
poolServerUnusable now status server
  | isNothing (_preprovisionedServerReadyAt server) =
      if age > poolCreationGrace then Just PoolStuckCreating else Nothing
  | Just reported <- status, reported /= runningStatus = Just PoolDead
  | age > poolEntryMaxAge = Just PoolExpired
  | otherwise = Nothing
  where
    age = diffTime now (_preprovisionedServerCreatedAt server)

-- | The status a live guest is reported under.
runningStatus :: Text
runningStatus = "running"

-- | The tiers to create to reach the target, one entry per missing instance.
-- A surplus contributes nothing: it ages out rather than being destroyed
-- while it could still serve a deployment.
poolShortfall :: WarmPoolTargets -> [PreprovisionedServer] -> [ServerTier]
poolShortfall targets pool =
  concat
    [ replicate (target - warm) tier
      | (tier, target) <- Map.toList targets,
        let warm = length (filter ((== tier) . _preprovisionedServerTier) pool),
        target > warm
    ]

-- | Drop what cannot serve a deployment, then create what is missing, as far
-- as the budget allows.
--
-- Best-effort: the pool is paid for out of the same budget as live servers, so
-- a host whose deployments have taken everything keeps an emptier pool until
-- one of them ends.
reconcilePool :: M ()
reconcilePool = do
  targets <- view #warmPoolTargets
  unless (Map.null targets) $ do
    dropUnusablePoolServers
    budget <- view #hostingBudget
    pool <- DB.getPoolServers
    fillPool budget (poolShortfall targets pool)

-- | Destroy every warm instance that cannot serve a deployment any more. One
-- bad instance must not end the sweep.
dropUnusablePoolServers :: M ()
dropUnusablePoolServers = do
  now <- liftIO getCurrentTime
  pool <- DB.getPoolServers
  forM_ pool $ \server -> do
    status <- poolServerStatus server
    forM_ (poolServerUnusable now status server) $ \problem ->
      catchEither (discardPoolServer server problem) $ \problem' ->
        log Error
          $ "reconcilePool: could not discard pool instance "
          <> showPretty (_preprovisionedServerId server)
          <> ": "
          <> either show showDebug problem'

-- | What the provisioner says about an instance, or 'Nothing' when it cannot
-- be asked.
poolServerStatus :: PreprovisionedServer -> M (Maybe Text)
poolServerStatus server = case _preprovisionedServerInstanceId server of
  Nothing -> pure Nothing
  Just instanceId ->
    catchEither (Just <$> getServerStatus instanceId) $ \problem -> do
      log Warning
        $ "reconcilePool: could not read the status of pool instance "
        <> showPretty (_preprovisionedServerId server)
        <> ", leaving it in the pool: "
        <> either show showDebug problem
      pure Nothing

-- | Tear an instance down and stop counting it against the budget. The row
-- goes either way, like in 'releaseServer'.
discardPoolServer :: PreprovisionedServer -> PoolProblem -> M ()
discardPoolServer server problem = do
  let poolId = _preprovisionedServerId server
  log Informational
    $ "reconcilePool: discarding pool instance "
    <> showPretty poolId
    <> " because "
    <> showPretty problem
  let teardown = case _preprovisionedServerInstanceId server of
        Nothing -> pure ()
        Just instanceId -> deleteServer instanceId
  teardown `cleaningUpOnFailure` DB.deletePoolServer poolId
  DB.deletePoolServer poolId

-- | Create the missing warm instances while the budget has room for the next
-- one. Sequential: 'committedResources' is read before each creation, so
-- creating concurrently would race the budget check against itself.
fillPool :: HostingBudget -> [ServerTier] -> M ()
fillPool _ [] = pure ()
fillPool budget (tier : rest) = do
  used <- committedResources
  if not (fitsBudget budget used tier)
    then
      log Informational
        $ "reconcilePool: leaving "
        <> show (length (tier : rest))
        <> " warm instance(s) uncreated; the hosting budget has no room for a "
        <> getServerTier tier
    else do
      void $ warmPool tier
      fillPool budget rest

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
