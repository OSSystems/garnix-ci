module Garnix.Hosting.ServerPoolSpec (spec) where

import Garnix.DB.Hosting qualified as Hosting
import Garnix.Hosting.ServerPool
import Garnix.Hosting.Types
import Garnix.Monad (M)
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
warm tier = do
  poolId <- Hosting.createPoolServer MicroVM tier
  Hosting.markPoolServerReady
    PreprovisionedServer
      { _preprovisionedServerId = poolId,
        _preprovisionedServerProvider = MicroVM,
        _preprovisionedServerInstanceId = Just (InstanceId "guest-1"),
        _preprovisionedServerAddress = ServerAddress (Just "10.111.0.17") Nothing,
        _preprovisionedServerTier = tier,
        _preprovisionedServerCreatedAt = undefined,
        _preprovisionedServerReadyAt = Nothing
      }
  pure poolId

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

  describe "warmPool" $ inM $ beforeM_ truncateDBM $ do
    it "leaves no row holding budget when provisioning fails" $ do
      warmPool small
        `shouldThrowM` OtherError
          "no hosting provisioner is configured on this server (set GARNIX_PROVISIONER_SOCKET)"
      Hosting.getPoolServers `shouldReturnM` []
      committedResources `shouldReturnM` (0, 0)
