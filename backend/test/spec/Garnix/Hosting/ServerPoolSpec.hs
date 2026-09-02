module Garnix.Hosting.ServerPoolSpec (spec) where

import Garnix.DB.Hosting qualified as Hosting
import Garnix.Hosting.ServerPool
import Garnix.Hosting.Types
import Garnix.Monad (M)
import Garnix.Prelude
import Garnix.TestHelpers (testBuild, truncateDBM)
import Garnix.TestHelpers.Monad (beforeM_, inM, shouldBeM, shouldReturnM, shouldThrowM)
import Garnix.Types (Build (..), BuildId, Error (..))
import Test.Hspec

small, large :: ServerTier
small = ServerTier "small"
large = ServerTier "large"

unbounded :: HostingBudget
unbounded = HostingBudget Nothing Nothing

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
      let budget = HostingBudget (Just 4) Nothing
      fitsBudget budget (3, 0) small `shouldBe` True
      fitsBudget budget (4, 0) small `shouldBe` False

    it "refuses when either dimension alone is exceeded" $ do
      fitsBudget (HostingBudget (Just 1) Nothing) (0, 0) large `shouldBe` False
      fitsBudget (HostingBudget Nothing (Just 2048)) (0, 0) large `shouldBe` False

    it "lets a cap be met exactly" $ do
      fitsBudget (HostingBudget (Just 4) (Just 8192)) (0, 0) large `shouldBe` True

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
      let budget = HostingBudget (Just 4) (Just 8192)
      acquireServer budget small build Nothing False
        `shouldThrowM` OtherError
          "no capacity for a small server: the hosting budget is fully committed"

  describe "warmPool" $ inM $ beforeM_ truncateDBM $ do
    it "leaves no row holding budget when provisioning fails" $ do
      warmPool small
        `shouldThrowM` OtherError
          "no hosting provisioner is configured on this server (set GARNIX_PROVISIONER_SOCKET)"
      Hosting.getPoolServers `shouldReturnM` []
      committedResources `shouldReturnM` (0, 0)
