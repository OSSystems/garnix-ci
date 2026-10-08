{-# LANGUAGE OverloadedRecordDot #-}

module Garnix.API.KeysSpec (spec) where

import Control.Concurrent.Async.Lifted
import Data.Char
import Data.Coerce (Coercible)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Garnix.API.Keys
import Garnix.Access (routeRepoId)
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.Deprecated qualified as Deprecated
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.WithServer
import Garnix.Types hiding (getPublicKey)
import Network.Wreq (responseBody)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "/api/keys" $ around_ Deprecated.addTestSecrets $ do
    let runTest test = runTestM $ suppressLogsWhenPassing $ do
          truncateDBM
          withServer test
    it "serves the forge-less repo key route as the github one" $ runTest $ \testServer -> do
      old <- assert200 $ testServer.get "/api/keys/owner/repo/repo-key.public"
      new <- assert200 $ testServer.get "/api/keys/github/owner/repo/repo-key.public"
      (new ^. responseBody) `shouldBeM` (old ^. responseBody)

    it "serves the forge-less action key route as the github one" $ runTest $ \testServer -> do
      old <- assert200 $ testServer.get "/api/keys/owner/repo/actions/deploy/key.public"
      new <- assert200 $ testServer.get "/api/keys/github/owner/repo/actions/deploy/key.public"
      (new ^. responseBody) `shouldBeM` (old ^. responseBody)

    it "returns 404 for a forge that is not configured" $ runTest $ \testServer -> do
      repoKey <- testServer.get "/api/keys/nowhere/owner/repo/repo-key.public"
      repoKey `shouldHaveStatusCode` 404
      actionKey <- testServer.get "/api/keys/nowhere/owner/repo/actions/deploy/key.public"
      actionKey `shouldHaveStatusCode` 404

    it "keeps a repository on another forge apart from the github one with the same owner/name" $ do
      let gitea = testForgeInstance "git.example" GiteaForgeKind
      runTestM $ suppressLogsWhenPassing $ local (#forges %~ Map.insert "git.example" gitea) $ withServer $ \testServer -> do
        let body path = (^. responseBody) <$> assert200 (testServer.get path)
        githubRepoKey <- body "/api/keys/github/owner/repo/repo-key.public"
        otherRepoKey <- body "/api/keys/git.example/owner/repo/repo-key.public"
        liftIO $ otherRepoKey `shouldNotBe` githubRepoKey
        -- Asked again, each forge keeps its own key.
        body "/api/keys/git.example/owner/repo/repo-key.public" `shouldReturnM` otherRepoKey
        body "/api/keys/github/owner/repo/repo-key.public" `shouldReturnM` githubRepoKey
        githubActionKey <- body "/api/keys/github/owner/repo/actions/deploy/key.public"
        otherActionKey <- body "/api/keys/git.example/owner/repo/actions/deploy/key.public"
        liftIO $ otherActionKey `shouldNotBe` githubActionKey

  describe "routeRepoId" $ do
    let configured = Map.fromList [("github", ()), ("git.example", ())]
    it "names the repository on any configured forge" $ do
      routeRepoId configured "git.example" "owner" "repo" `shouldBe` Just (RepoId "git.example" "owner" "repo")
      routeRepoId configured "github" "owner" "repo" `shouldBe` Just (RepoId githubForge "owner" "repo")
    it "names nothing on a forge that is not configured" $ do
      routeRepoId configured "nowhere" "owner" "repo" `shouldBe` Nothing

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
