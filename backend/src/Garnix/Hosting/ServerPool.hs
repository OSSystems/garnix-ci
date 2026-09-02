-- | Acquiring and releasing hosted servers, with a warm pool in front of
-- the provisioner and a resource budget behind it.
module Garnix.Hosting.ServerPool
  ( acquireServer,
    releaseServer,
    warmPool,
    committedResources,
    fitsBudget,
    HostingBudget (..),
  )
where

import Garnix.DB.Hosting qualified as DB
import Garnix.Hosting.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types (BuildId, Error (..), GhPullRequestId)

-- | Resolved absolute caps on what all guests together may hold.
data HostingBudget = HostingBudget
  { _hostingBudgetVcpus :: Maybe Int,
    _hostingBudgetMemoryMiB :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

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
    provisionServer poolId tier `onException'` DB.deletePoolServer poolId
  DB.markPoolServerReady server
  pure poolId
  where
    onException' :: M a -> M () -> M a
    onException' action cleanup = action `catchAny` \problem -> do
      cleanup
      throwM problem

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
      result <- case _serverInfoInstanceId info of
        Nothing -> pure (Right ())
        Just instanceId -> (Right <$> deleteServer instanceId) `catchAny` (pure . Left)
      DB.endServer serverId
      either throwM pure result
