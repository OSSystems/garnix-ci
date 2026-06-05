module Garnix.EntitlementsSpec where

import Database.PostgreSQL.Typed (pgSQL)
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Entitlements (Hosting (..), baseCiTime, queryCiTimeEntitlements)
import Garnix.Entitlements qualified as Entitlements
import Garnix.Monad
import Garnix.MonetaryCost
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.Monad (beforeM_, inM, shouldBeM)
import Garnix.Types (includedBranchDeploymentHosts, maximumPrDeploymentTime)
import Test.Hspec

spec :: Spec
spec =
  describe "Entitlements"
    $ do
      let noHostingEntitlement =
            Hosting
              { planIncludedBranchDeploymentHosts = 0,
                extraBranchHostingSpend = usd 0,
                maxPrDeploymentTime = emptyDuration,
                largerServers = False
              }

      inM . beforeM_ truncateDBM . describe "canHost" $ do
        it "returns no entitlement by default" $ do
          test (Entitlements.getHosting "test-account") noHostingEntitlement

        it "correctly returns entitlements" $ withTestEntitlement "test" ((includedBranchDeploymentHosts .~ 2) . (maximumPrDeploymentTime .~ fromMinutes @Int 100)) "test-account" $ do
          test (Entitlements.getHosting "test-account") $ Hosting 2 (usd 0) (fromMinutes @Int 100) False
          test (Entitlements.getHosting "other-account") noHostingEntitlement

        it "returns the maximum entitlements when there are multiple products active" $ do
          withTestEntitlement "a" ((includedBranchDeploymentHosts .~ 4) . (maximumPrDeploymentTime .~ fromMinutes @Int 300) . (baseCiTime .~ fromMinutes @Int 800)) "test-account" $ do
            withTestEntitlement "b" ((includedBranchDeploymentHosts .~ 3) . (maximumPrDeploymentTime .~ fromMinutes @Int 400) . (baseCiTime .~ fromMinutes @Int 700)) "test-account" $ do
              test (Entitlements.getHosting "test-account") $ Hosting 4 (usd 0) (fromMinutes @Int 400) False
              ciMinutes <- queryCiTimeEntitlements "test-account"
              ciMinutes `shouldBeM` fromMinutes @Int 800

        it "returns the maximum even when one product has no priceId" $ do
          withTestEntitlement "without-price-id" ((includedBranchDeploymentHosts .~ 4) . (maximumPrDeploymentTime .~ fromMinutes @Int 300) . (baseCiTime .~ fromMinutes @Int 800)) "test-account" $ do
            withTestEntitlement "with-price-id" ((includedBranchDeploymentHosts .~ 3) . (maximumPrDeploymentTime .~ fromMinutes @Int 400) . (baseCiTime .~ fromMinutes @Int 700)) "test-account" $ do
              void
                $ DB.pgExec
                  [pgSQL|
                    UPDATE products
                      SET price_id = NULL
                      WHERE name = 'without-price-id'
                  |]
              test (Entitlements.getHosting "test-account") $ Hosting 4 (usd 0) (fromMinutes @Int 400) False
              ciMinutes <- queryCiTimeEntitlements "test-account"
              ciMinutes `shouldBeM` fromMinutes @Int 800

test :: (HasCallStack, Show a, Eq a) => M a -> a -> M ()
test action expected = do
  result <- action
  liftIO $ result `shouldBe` expected
