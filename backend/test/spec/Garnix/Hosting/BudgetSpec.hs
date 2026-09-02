module Garnix.Hosting.BudgetSpec (spec) where

import Garnix.Hosting.Budget
import Garnix.Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "parseBudget" $ do
    it "round-trips through renderBudget" $ do
      let budgets = [Absolute 0, Absolute 65536, Reserve 1, Reserve 4096]
      map (parseBudget . renderBudget) budgets `shouldBe` map Just budgets

    it "ignores surrounding whitespace" $ do
      parseBudget "  total:2048\n" `shouldBe` Just (Absolute 2048)

    it "rejects anything that is not one of the two encodings" $ do
      let bad = ["", "total", "total:", "total:x", "2048", "absolute:5", "total:5:5"]
      map parseBudget bad `shouldBe` map (const Nothing) bad

  describe "resolveBudget" $ do
    it "leaves an unconfigured budget unbounded" $ do
      resolveBudget 16384 Nothing `shouldBe` Nothing

    it "passes an absolute cap through, ignoring the host total" $ do
      resolveBudget 16384 (Just (Absolute 2048)) `shouldBe` Just 2048

    it "subtracts a reserve from the host total" $ do
      resolveBudget 16384 (Just (Reserve 4096)) `shouldBe` Just 12288

    it "clamps a reserve larger than the host to zero rather than going negative" $ do
      resolveBudget 2048 (Just (Reserve 4096)) `shouldBe` Just 0
