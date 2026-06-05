module Garnix.EntitlementsSpec where

import Garnix.Duration
import Garnix.Entitlements (baseCiTime, queryCiTimeEntitlements)
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.Monad (beforeM_, inM, shouldBeM)
import Test.Hspec

spec :: Spec
spec =
  describe "Entitlements"
    $ inM
    . beforeM_ truncateDBM
    . describe "queryCiTimeEntitlements"
    $ do
      it "returns the maximum baseCiTime across active products" $ do
        withTestEntitlement "a" (baseCiTime .~ fromMinutes @Int 800) "test-account" $ do
          withTestEntitlement "b" (baseCiTime .~ fromMinutes @Int 700) "test-account" $ do
            ciMinutes <- queryCiTimeEntitlements "test-account"
            ciMinutes `shouldBeM` fromMinutes @Int 800

test :: (HasCallStack, Show a, Eq a) => M a -> a -> M ()
test action expected = do
  result <- action
  liftIO $ result `shouldBe` expected
