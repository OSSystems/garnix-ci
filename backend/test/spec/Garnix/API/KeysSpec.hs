module Garnix.API.KeysSpec (spec) where

import Control.Concurrent.Async.Lifted
import Data.Char
import Data.Coerce (Coercible)
import Data.Text qualified as T
import Garnix.API.Keys
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.Deprecated qualified as Deprecated
import Garnix.TestHelpers.Monad
import Garnix.Types hiding (getPublicKey)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "getPublicKey" $ around_ Deprecated.addTestSecrets $ do
    let runTest test = runTestM $ suppressLogsWhenPassing $ do
          truncateDBM
          test
    it "returns a valid age public key"
      $ property
      $ \(PrintableString org, PrintableString name) -> do
        PublicKey key <- runTest $ do
          getRepoPublicKey (RepoId githubForge (coerceT org) (coerceT name))
        cs key `shouldStartWith` "age1"
        all isAlphaNum (cs key :: String) `shouldBe` True

    it "returns a different age key for each repository"
      $ property
      $ \( PrintableString org1,
           PrintableString org2,
           PrintableString name1,
           PrintableString name2
           ) ->
          ( org1
              /= org2
              && name1
              /= name2
          )
            ==> do
              (key1, key2) <-
                runTest $ do
                  (,)
                    <$> getRepoPublicKey (RepoId githubForge (coerceT org1) (coerceT name1))
                    <*> getRepoPublicKey (RepoId githubForge (coerceT org2) (coerceT name2))
              key1 `shouldNotBe` key2

    it "returns the same age key for the same repository even under concurrency" $ do
      keys <- runTest $ do
        replicateConcurrently 100 $ getRepoPublicKey (RepoId githubForge "owner" "repo")
      length (nub keys) `shouldBe` 1

coerceT :: (Coercible Text a) => String -> a
coerceT = coerce . T.pack
