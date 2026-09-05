module Garnix.Hosting.ServerPoolSpec (spec) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map qualified as Map
import Data.Time (UTCTime (..), fromGregorian)
import Garnix.DB.Hosting qualified as Hosting
import Garnix.Duration
import Garnix.Hosting.ServerPool
import Garnix.Hosting.Types
import Garnix.Monad (M, Provisioner (..), throw)
import Garnix.Prelude
import Garnix.TestHelpers (testBuild, truncateDBM)
import Garnix.TestHelpers.Monad (beforeM_, inM, shouldBeM, shouldReturnM, shouldThrowM)
import Garnix.Types (Build (..), BuildId, Error (..), GhPullRequestId (..))
import Test.Hspec

small, large :: ServerTier
small = ServerTier "small"
large = ServerTier "large"

-- | A pull request deploy, as 'acquireServer' sees one.
aPullRequest :: Maybe GhPullRequestId
aPullRequest = Just (GhPullRequestId 7)

unbounded :: HostingBudget
unbounded = HostingBudget Nothing Nothing Nothing Nothing

aBuild :: M BuildId
aBuild = _buildId <$> testBuild identity

warm :: ServerTier -> M PreprovisionedServerId
warm = warmAs (InstanceId "guest-1")

-- | A warm instance the provisioner knows by @instanceId@.
warmAs :: InstanceId -> ServerTier -> M PreprovisionedServerId
warmAs instanceId tier = do
  poolId <- Hosting.createPoolServer MicroVM tier
  Hosting.markPoolServerReady
    PreprovisionedServer
      { _preprovisionedServerId = poolId,
        _preprovisionedServerProvider = MicroVM,
        _preprovisionedServerInstanceId = Just instanceId,
        _preprovisionedServerAddress = ServerAddress (Just "10.111.0.17") Nothing,
        _preprovisionedServerTier = tier,
        _preprovisionedServerCreatedAt = undefined,
        _preprovisionedServerReadyAt = Nothing
      }
  pure poolId

-- | A pool instance as 'poolServerUnusable' reads one.
poolEntry :: UTCTime -> Maybe UTCTime -> PreprovisionedServer
poolEntry createdAt readyAt =
  PreprovisionedServer
    { _preprovisionedServerId = PreprovisionedServerId 1,
      _preprovisionedServerProvider = MicroVM,
      _preprovisionedServerInstanceId = Just (InstanceId "guest-1"),
      _preprovisionedServerAddress = ServerAddress (Just "10.111.0.17") Nothing,
      _preprovisionedServerTier = small,
      _preprovisionedServerCreatedAt = createdAt,
      _preprovisionedServerReadyAt = readyAt
    }

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) 0

-- | Run @action@ against a provisioner that only records what it was asked to
-- do, handing it the tiers created and the instances destroyed, in order.
withFakeProvisioner ::
  -- | What to report for an instance.
  (InstanceId -> M Text) ->
  -- | Whether destroying an instance succeeds.
  Bool ->
  (IORef [ServerTier] -> IORef [InstanceId] -> M a) ->
  M a
withFakeProvisioner status deleteWorks action = do
  created <- liftIO $ newIORef []
  destroyed <- liftIO $ newIORef []
  let provisioner =
        Provisioner
          { _provisionerProvider = MicroVM,
            _provisionerProvisionServer = \poolId tier -> do
              liftIO $ modifyIORef' created (<> [tier])
              now <- liftIO getCurrentTime
              pure
                PreprovisionedServer
                  { _preprovisionedServerId = poolId,
                    _preprovisionedServerProvider = MicroVM,
                    _preprovisionedServerInstanceId =
                      Just (InstanceId (showT (getPreprovisionedServerId poolId))),
                    _preprovisionedServerAddress =
                      ServerAddress (Just "10.111.0.42") Nothing,
                    _preprovisionedServerTier = tier,
                    _preprovisionedServerCreatedAt = now,
                    _preprovisionedServerReadyAt = Nothing
                  },
            _provisionerUpdateMetadata = \_ _ _ _ -> pure (),
            _provisionerDeleteServer = \instanceId -> do
              liftIO $ modifyIORef' destroyed (<> [instanceId])
              unless deleteWorks
                $ throw
                $ OtherError "the fake provisioner cannot destroy guests",
            _provisionerGetServerStatus = status
          }
  local (#provisioner .~ provisioner) (action created destroyed)
  where
    showT :: Int64 -> Text
    showT = show

-- | Every guest is alive.
allRunning :: InstanceId -> M Text
allRunning _ = pure "running"

withTargets :: [(ServerTier, Int)] -> M a -> M a
withTargets targets = local (#warmPoolTargets .~ Map.fromList targets)

spec :: Spec
spec = do
  describe "fitsBudget" $ do
    it "always fits when neither dimension is capped" $ do
      fitsBudget unbounded (1000, 1000000) large `shouldBe` True

    it "counts the instance being asked for, not just what is already used" $ do
      let budget = HostingBudget (Just 4) Nothing Nothing Nothing
      fitsBudget budget (3, 0) small `shouldBe` True
      fitsBudget budget (4, 0) small `shouldBe` False

    it "refuses when either dimension alone is exceeded" $ do
      fitsBudget (HostingBudget (Just 1) Nothing Nothing Nothing) (0, 0) large `shouldBe` False
      fitsBudget (HostingBudget Nothing (Just 2048) Nothing Nothing) (0, 0) large `shouldBe` False

    it "lets a cap be met exactly" $ do
      fitsBudget (HostingBudget (Just 4) (Just 8192) Nothing Nothing) (0, 0) large `shouldBe` True

  describe "tierWithinCap" $ do
    it "lets anything through when no cap is set" $ do
      tierWithinCap Nothing large `shouldBe` True

    it "refuses a tier that is over the cap on either dimension alone" $ do
      tierWithinCap (Just (ServerTier "i4x8")) (ServerTier "i8x8") `shouldBe` False
      tierWithinCap (Just (ServerTier "i4x8")) (ServerTier "i1x16") `shouldBe` False

    it "lets the cap itself, and anything under it, through" $ do
      tierWithinCap (Just large) large `shouldBe` True
      tierWithinCap (Just large) small `shouldBe` True

  describe "reserveFor" $ do
    it "is owed by pull request deploys only" $ do
      let budget = HostingBudget (Just 4) (Just 8192) Nothing (Just small)
      reserveFor budget aPullRequest `shouldBe` tierResources small
      reserveFor budget Nothing `shouldBe` (0, 0)

    it "is nothing when no reserve is configured" $ do
      reserveFor unbounded aPullRequest `shouldBe` (0, 0)

  describe "leavesReserveFree" $ do
    it "refuses what would fit, when fitting would eat the reserve" $ do
      let budget = HostingBudget (Just 4) Nothing Nothing Nothing
      leavesReserveFree budget (2, 0) (1, 0) (0, 0) `shouldBe` True
      leavesReserveFree budget (2, 0) (1, 0) (2, 0) `shouldBe` False

    it "ignores the reserve on a dimension that is not capped" $ do
      leavesReserveFree unbounded (1000, 0) (1000, 0) (1000, 0) `shouldBe` True

  describe "committedResources" $ inM $ beforeM_ truncateDBM $ do
    it "is nothing on an idle host" $ do
      committedResources `shouldReturnM` (0, 0)

    it "counts a pool instance that is not ready yet" $ do
      void $ Hosting.createPoolServer MicroVM large
      committedResources `shouldReturnM` tierResources large

    it "counts a claimed server and the pool instance behind it separately" $ do
      build <- aBuild
      void $ warm small
      void $ warm large
      void $ Hosting.claimPoolServer MicroVM small build Nothing False
      let (smallVcpus, smallMiB) = tierResources small
          (largeVcpus, largeMiB) = tierResources large
      committedResources `shouldReturnM` (smallVcpus + largeVcpus, smallMiB + largeMiB)

    it "frees the budget even when the provisioner cannot delete the instance" $ do
      build <- aBuild
      void $ warm small
      Just serverId <- Hosting.claimPoolServer MicroVM small build Nothing False
      committedResources `shouldReturnM` tierResources small
      releaseServer serverId
        `shouldThrowM` OtherError
          "no hosting provisioner is configured on this server (set GARNIX_PROVISIONER_SOCKET)"
      committedResources `shouldReturnM` (0, 0)

  describe "acquireServer" $ inM $ beforeM_ truncateDBM $ do
    it "claims a warm instance without provisioning a new one" $ do
      build <- aBuild
      void $ warm small
      serverId <- acquireServer unbounded small build Nothing False
      live <- Hosting.getLiveServers
      map _serverInfoId live `shouldBeM` [serverId]
      Hosting.getPoolServers `shouldReturnM` []

    it "refuses rather than queueing when the budget is fully committed" $ do
      build <- aBuild
      void $ Hosting.createPoolServer MicroVM large
      let budget = HostingBudget (Just 4) (Just 8192) Nothing Nothing
      acquireServer budget small build Nothing False
        `shouldThrowM` OtherError
          "no capacity for a small server: the hosting budget is fully committed"

    it "keeps the branch reserve out of a pull request's reach" $ do
      build <- aBuild
      -- 4 of the 4 vCPUs are committed to a large guest, so nothing at all is
      -- free -- let alone the small guest's worth held back for branches.
      void $ Hosting.createPoolServer MicroVM large
      let budget = HostingBudget (Just 4) (Just 8192) Nothing (Just small)
      acquireServer budget small build aPullRequest False
        `shouldThrowM` OtherError
          "no capacity for a small server: the hosting budget is committed down to the branch reserve"

    it "refuses a pull request the warm guest that the reserve is holding" $ do
      build <- aBuild
      -- The guest is already warm, so claiming it would not move the totals
      -- and the plain budget check cannot see the problem. It is still the
      -- last one, and the reserve says a branch deploy gets it.
      void $ warm large
      let budget = HostingBudget (Just 4) (Just 8192) Nothing (Just small)
      acquireServer budget large build aPullRequest False
        `shouldThrowM` OtherError
          "no capacity for a large server: the hosting budget is committed down to the branch reserve"

    it "still gives that warm guest to a branch deploy" $ do
      build <- aBuild
      void $ warm large
      let budget = HostingBudget (Just 4) (Just 8192) Nothing (Just small)
      serverId <- acquireServer budget large build Nothing False
      live <- Hosting.getLiveServers
      map _serverInfoId live `shouldBeM` [serverId]

    it "lets a pull request take what is left above the reserve" $ do
      build <- aBuild
      -- 4 vCPUs, a small guest reserved for branches, and a small guest warm:
      -- claiming it leaves 2 vCPUs free, which covers the reserve.
      void $ warm small
      let budget = HostingBudget (Just 4) (Just 8192) Nothing (Just small)
      serverId <- acquireServer budget small build aPullRequest False
      live <- Hosting.getLiveServers
      map _serverInfoId live `shouldBeM` [serverId]

  describe "poolShortfall" $ do
    it "is nothing when no pool is configured" $ do
      poolShortfall mempty [] `shouldBe` []

    it "asks for the whole target when the pool is empty" $ do
      poolShortfall (Map.fromList [(small, 2), (large, 1)]) []
        `shouldBe` [large, small, small]

    it "asks for nothing when the target is already met" $ do
      let pool = [poolEntry epoch (Just epoch)]
      poolShortfall (Map.fromList [(small, 1)]) pool `shouldBe` []

    it "counts an instance that is still being created" $ do
      -- It is already holding budget, so asking for another would overshoot.
      let pool = [poolEntry epoch Nothing]
      poolShortfall (Map.fromList [(small, 1)]) pool `shouldBe` []

    it "asks for nothing for a tier that has more than the target" $ do
      let pool = replicate 3 (poolEntry epoch (Just epoch))
      poolShortfall (Map.fromList [(small, 1)]) pool `shouldBe` []

    it "ignores a warm instance of a tier nobody asked for" $ do
      let pool = [(poolEntry epoch (Just epoch)) {_preprovisionedServerTier = large}]
      poolShortfall (Map.fromList [(small, 1)]) pool `shouldBe` [small]

  describe "poolServerUnusable" $ do
    let ready = poolEntry epoch (Just epoch)
        creating = poolEntry epoch Nothing
        after duration = addTime duration epoch

    it "keeps a warm instance the provisioner reports as running" $ do
      poolServerUnusable (after (fromMinutes @Int 1)) (Just "running") ready
        `shouldBe` Nothing

    it "discards one the provisioner does not report as running" $ do
      poolServerUnusable (after (fromMinutes @Int 1)) (Just "off") ready
        `shouldBe` Just PoolDead

    it "keeps one the provisioner could not be asked about" $ do
      -- A provisioner that has gone quiet is not every guest being dead.
      poolServerUnusable (after (fromMinutes @Int 1)) Nothing ready
        `shouldBe` Nothing

    it "recycles one that has been warm for too long" $ do
      poolServerUnusable (after (addDuration poolEntryMaxAge (fromMinutes @Int 1))) (Just "running") ready
        `shouldBe` Just PoolExpired

    it "leaves an instance that is still being created alone" $ do
      poolServerUnusable (after (fromMinutes @Int 1)) Nothing creating `shouldBe` Nothing

    it "discards one that never finished being created" $ do
      poolServerUnusable (after (addDuration poolCreationGrace (fromMinutes @Int 1))) Nothing creating
        `shouldBe` Just PoolStuckCreating

  describe "reconcilePool" $ inM $ beforeM_ truncateDBM $ do
    it "does nothing at all when no pool is configured" $ do
      withFakeProvisioner allRunning True $ \created _ -> do
        reconcilePool
        liftIO $ readIORef created `shouldReturn` []

    it "creates warm instances up to the target" $ do
      withFakeProvisioner allRunning True $ \created _ -> do
        withTargets [(small, 2)] reconcilePool
        liftIO $ readIORef created `shouldReturn` [small, small]
        pool <- Hosting.getPoolServers
        map _preprovisionedServerTier pool `shouldBeM` [small, small]
        -- Warm means claimable: every one of them has an address.
        all (isJust . _preprovisionedServerReadyAt) pool `shouldBeM` True

    it "creates nothing when the pool already meets the target" $ do
      void $ warm small
      withFakeProvisioner allRunning True $ \created _ -> do
        withTargets [(small, 1)] reconcilePool
        liftIO $ readIORef created `shouldReturn` []

    it "stops at the budget, keeping what it managed to create" $ do
      withFakeProvisioner allRunning True $ \created _ -> do
        -- Three small guests wanted, two vCPUs to pay for them.
        local (#hostingBudget .~ HostingBudget (Just 2) Nothing Nothing Nothing)
          $ withTargets [(small, 3)] reconcilePool
        liftIO $ readIORef created `shouldReturn` [small, small]
        committedResources `shouldReturnM` (2, 2 * snd (tierResources small))

    it "replaces a guest the provisioner no longer reports as running" $ do
      dead <- warmAs (InstanceId "dead") small
      let status instanceId =
            pure $ if instanceId == InstanceId "dead" then "off" else "running"
      withFakeProvisioner status True $ \created destroyed -> do
        withTargets [(small, 1)] reconcilePool
        liftIO $ readIORef destroyed `shouldReturn` [InstanceId "dead"]
        liftIO $ readIORef created `shouldReturn` [small]
        pool <- Hosting.getPoolServers
        (dead `elem` map _preprovisionedServerId pool) `shouldBeM` False

    it "stops holding budget for a guest the provisioner cannot destroy" $ do
      void $ warmAs (InstanceId "dead") small
      withFakeProvisioner (const (pure "off")) False $ \_ destroyed -> do
        withTargets [(small, 1)] reconcilePool
        liftIO $ readIORef destroyed `shouldReturn` [InstanceId "dead"]
        -- The dead row is gone even though the teardown failed.
        committedResources `shouldReturnM` tierResources small
        Hosting.getPoolServers >>= \pool -> length pool `shouldBeM` 1

    it "leaves a guest alone when the provisioner cannot be asked about it" $ do
      void $ warm small
      let unreachable _ = throw $ OtherError "the provisioner is not answering"
      withFakeProvisioner unreachable True $ \created destroyed -> do
        withTargets [(small, 1)] reconcilePool
        liftIO $ readIORef destroyed `shouldReturn` []
        liftIO $ readIORef created `shouldReturn` []

  describe "warmPool" $ inM $ beforeM_ truncateDBM $ do
    it "leaves no row holding budget when provisioning fails" $ do
      warmPool small
        `shouldThrowM` OtherError
          "no hosting provisioner is configured on this server (set GARNIX_PROVISIONER_SOCKET)"
      Hosting.getPoolServers `shouldReturnM` []
      committedResources `shouldReturnM` (0, 0)
