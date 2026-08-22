{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}

module Garnix.API.Cache.PermissionsSpec where

import Garnix.API.Cache.Permissions
import Garnix.ExpiringCache (clearCache)
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.Types
import Test.Hspec

spec :: Spec
spec = inM
  $ beforeM_ truncateDBM
  $ do
    describe "getRepoPermissions" $ do
      describe "with unauthenticated request" $ do
        it "returns true for public repos" $ do
          GH.withFakeGithubInterface $ \st -> do
            GH.mkRepo st "owner" "repo"
              $ (#publicity .~ RepoIsPublic True)
            result <- getRepoPermissions Nothing (RepoId githubForge "owner" "repo")
            result `shouldBeM` Allowed

        it "returns false for private repos" $ do
          GH.withFakeGithubInterface $ \st -> do
            GH.mkRepo st "owner" "repo"
              $ (#publicity .~ RepoIsPublic False)
              . (#collaborators .~ [])
            result <- getRepoPermissions Nothing (RepoId githubForge "owner" "repo")
            result `shouldBeM` Disallowed

        it "returns false for non-existing repos" $ do
          GH.withFakeGithubInterface $ \_ghFake -> do
            result <- getRepoPermissions Nothing (RepoId githubForge "owner" "repo")
            result `shouldBeM` Disallowed

        it "caches responses" $ do
          GH.withFakeGithubInterface $ \st -> do
            GH.mkRepo st "owner" "repo"
              $ (#publicity .~ RepoIsPublic True)
            result <- getRepoPermissions Nothing (RepoId githubForge "owner" "repo")
            result `shouldBeM` Allowed
            GH.mkRepo st "owner" "repo"
              $ (#publicity .~ RepoIsPublic False)
            result <- getRepoPermissions Nothing (RepoId githubForge "owner" "repo")
            result `shouldBeM` Allowed
            clearCache __getRepoPermissionsCache
            result <- getRepoPermissions Nothing (RepoId githubForge "owner" "repo")
            result `shouldBeM` Disallowed

      describe "with authenticated request" $ do
        it "returns true for public repos" $ do
          GH.withFakeGithubInterface $ \st -> do
            GH.mkRepo st "owner" "repo"
              $ (#publicity .~ RepoIsPublic True)
            result <- getRepoPermissions (Just "someone") (RepoId githubForge "owner" "repo")
            result `shouldBeM` Allowed

        it "returns true for private repos where the requesting user is a collaborator" $ do
          GH.withFakeGithubInterface $ \st -> do
            GH.mkRepo st "owner" "repo"
              $ (#publicity .~ RepoIsPublic False)
              . (#collaborators .~ ["test-user"])
            result <- getRepoPermissions (Just "test-user") (RepoId githubForge "owner" "repo")
            result `shouldBeM` Allowed

        it "returns false for private repos" $ do
          GH.withFakeGithubInterface $ \st -> do
            GH.mkRepo st "owner" "repo"
              $ (#publicity .~ RepoIsPublic False)
              . (#collaborators .~ [])
            result <- getRepoPermissions (Just "test-user") (RepoId githubForge "owner" "repo")
            result `shouldBeM` Disallowed

        it "returns false for non-existing repos" $ do
          GH.withFakeGithubInterface $ \_ghFake -> do
            result <- getRepoPermissions (Just "test-user") (RepoId githubForge "owner" "repo")
            result `shouldBeM` Disallowed

        it "caches responses" $ do
          GH.withFakeGithubInterface $ \st -> do
            GH.mkRepo st "owner" "repo"
              $ (#publicity .~ RepoIsPublic True)
            result <- getRepoPermissions (Just "someone") (RepoId githubForge "owner" "repo")
            result `shouldBeM` Allowed
            GH.mkRepo st "owner" "repo"
              $ (#publicity .~ RepoIsPublic False)
            result <- getRepoPermissions (Just "someone") (RepoId githubForge "owner" "repo")
            result `shouldBeM` Allowed
            clearCache __getRepoPermissionsCache
            result <- getRepoPermissions (Just "someone") (RepoId githubForge "owner" "repo")
            result `shouldBeM` Disallowed
