module Garnix.RateLimitSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), fromGregorian)
import Garnix.Prelude
import Garnix.RateLimit (Windows (..), countRequest)
import Test.Hspec

spec :: Spec
spec = describe "countRequest" $ do
  let start = UTCTime (fromGregorian 2026 1 1) 0
      at seconds = addUTCTime seconds start
      -- Two requests per minute, each request at the given second.
      run = go (Windows start mempty)
        where
          go _ [] = []
          go windows ((second, client) : rest) =
            let (windows', allowed) = countRequest 2 60 (at second) client windows
             in allowed : go windows' rest

  it "allows so many requests per client in a window" $ do
    run [(0, "a"), (1, "a"), (2, "a"), (3, "b")] `shouldBe` [True, True, False, True]

  it "starts a new window once the last one is over" $ do
    run [(0, "a"), (1, "a"), (2, "a"), (60, "a"), (61, "a"), (62, "a")] `shouldBe` [True, True, False, True, True, False]

  it "drops expired windows once per window" $ do
    let (windows, _) = countRequest 2 60 (at 0) "a" (Windows start mempty)
        (windows', _) = countRequest 2 60 (at 61) "b" windows
    windowsPruned windows' `shouldBe` at 61
    Map.keys (windowsByClient windows') `shouldBe` ["b"]
