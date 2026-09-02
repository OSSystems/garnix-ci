module Garnix.DB.HostingSpec (spec) where

import Control.Concurrent.Async.Lifted (replicateConcurrently)
import Data.Maybe (maybeToList)
import Garnix.DB.Hosting qualified as Hosting
import Garnix.Hosting.Types
import Garnix.Monad (M)
import Garnix.Prelude
import Garnix.TestHelpers (testBuild, truncateDBM)
import Garnix.TestHelpers.Monad (beforeM_, inM, shouldBeM, shouldReturnM)
import Garnix.Types (Build (..), BuildId)
import Test.Hspec

small :: ServerTier
small = ServerTier "small"

aBuild :: M BuildId
aBuild = _buildId <$> testBuild identity

warm :: ServerTier -> M PreprovisionedServerId
warm tier = do
  poolId <- Hosting.createPoolServer MicroVM tier
  ready poolId tier
  pure poolId

ready :: PreprovisionedServerId -> ServerTier -> M ()
ready poolId tier =
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

spec :: Spec
spec = inM $ beforeM_ truncateDBM $ do
  describe "createPoolServer" $ do
    it "registers an instance that is not yet claimable" $ do
      void $ Hosting.createPoolServer MicroVM small
      pool <- Hosting.getPoolServers
      map _preprovisionedServerReadyAt pool `shouldBeM` [Nothing]
      map _preprovisionedServerInstanceId pool `shouldBeM` [Nothing]

  describe "markPoolServerReady" $ do
    it "records the address the provisioner handed back" $ do
      poolId <- Hosting.createPoolServer MicroVM small
      ready poolId small
      pool <- Hosting.getPoolServers
      map _preprovisionedServerInstanceId pool `shouldBeM` [Just (InstanceId "guest-1")]
      map (serverAddressText . _preprovisionedServerAddress) pool
        `shouldBeM` [Just "10.111.0.17"]
      map (isJust . _preprovisionedServerReadyAt) pool `shouldBeM` [True]

  describe "claimPoolServer" $ do
    it "finds nothing in an empty pool" $ do
      build <- aBuild
      Hosting.claimPoolServer MicroVM small build Nothing False `shouldReturnM` Nothing

    it "refuses an instance that is not ready yet" $ do
      build <- aBuild
      void $ Hosting.createPoolServer MicroVM small
      Hosting.claimPoolServer MicroVM small build Nothing False `shouldReturnM` Nothing

    it "refuses an instance of another tier" $ do
      build <- aBuild
      void $ warm (ServerTier "large")
      Hosting.claimPoolServer MicroVM small build Nothing False `shouldReturnM` Nothing

    it "refuses an instance belonging to another provider" $ do
      build <- aBuild
      void $ warm small
      Hosting.claimPoolServer Hetzner small build Nothing False `shouldReturnM` Nothing

    it "moves the instance out of the pool and into servers" $ do
      build <- aBuild
      void $ warm small
      claimed <- Hosting.claimPoolServer MicroVM small build Nothing True
      Hosting.getPoolServers `shouldReturnM` []
      live <- Hosting.getLiveServers
      map _serverInfoId live `shouldBeM` maybeToList claimed
      map _serverInfoInstanceId live `shouldBeM` [Just (InstanceId "guest-1")]
      map (serverAddressText . _serverInfoAddress) live `shouldBeM` [Just "10.111.0.17"]
      map _serverInfoIsPrimary live `shouldBeM` [True]
      map (isJust . _serverInfoReadyAt) live `shouldBeM` [True]

    it "hands one warm instance to exactly one of many concurrent deployments" $ do
      build <- aBuild
      void $ warm small
      claims <-
        replicateConcurrently 8
          $ Hosting.claimPoolServer MicroVM small build Nothing False
      length (catMaybes claims) `shouldBeM` 1
      Hosting.getPoolServers `shouldReturnM` []
      live <- Hosting.getLiveServers
      length live `shouldBeM` 1

  describe "endServer" $ do
    it "takes the server out of the live set" $ do
      build <- aBuild
      void $ warm small
      Just serverId <- Hosting.claimPoolServer MicroVM small build Nothing False
      Hosting.endServer serverId
      Hosting.getLiveServers `shouldReturnM` []

    it "keeps the original end time when a teardown is retried" $ do
      build <- aBuild
      void $ warm small
      Just serverId <- Hosting.claimPoolServer MicroVM small build Nothing False
      Hosting.endServer serverId
      first <- fmap _serverInfoEndedAt <$> Hosting.getServer serverId
      Hosting.endServer serverId
      second <- fmap _serverInfoEndedAt <$> Hosting.getServer serverId
      fmap isJust first `shouldBeM` Just True
      second `shouldBeM` first
