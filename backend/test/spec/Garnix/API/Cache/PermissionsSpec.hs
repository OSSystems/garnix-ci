{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}

module Garnix.API.Cache.PermissionsSpec where

import Garnix.API.Cache.Permissions
import Garnix.ExpiringCache (clearCache)
import Garnix.Monad (GhCollaborators (..))
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.Types
import Test.Hspec

spec :: Spec
spec = decisionSpec >> getRepoPermissionsSpec

getRepoPermissionsSpec :: Spec
getRepoPermissionsSpec = inM
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
            result <- getRepoPermissions (Just (ForgeLogin githubForge "someone")) (RepoId githubForge "owner" "repo")
            result `shouldBeM` Allowed

        it "returns true for private repos where the requesting user is a collaborator" $ do
          GH.withFakeGithubInterface $ \st -> do
            GH.mkRepo st "owner" "repo"
              $ (#publicity .~ RepoIsPublic False)
              . (#collaborators .~ ["test-user"])
            result <- getRepoPermissions (Just (ForgeLogin githubForge "test-user")) (RepoId githubForge "owner" "repo")
            result `shouldBeM` Allowed

        it "returns false for private repos" $ do
          GH.withFakeGithubInterface $ \st -> do
            GH.mkRepo st "owner" "repo"
              $ (#publicity .~ RepoIsPublic False)
              . (#collaborators .~ [])
            result <- getRepoPermissions (Just (ForgeLogin githubForge "test-user")) (RepoId githubForge "owner" "repo")
            result `shouldBeM` Disallowed

        it "returns false for a collaborator's login on another forge" $ do
          GH.withFakeGithubInterface $ \st -> do
            GH.mkRepo st "owner" "repo"
              $ (#publicity .~ RepoIsPublic False)
              . (#collaborators .~ ["test-user"])
            result <- getRepoPermissions (Just (ForgeLogin githubForge "test-user")) (RepoId githubForge "owner" "repo")
            result `shouldBeM` Allowed
            result <- getRepoPermissions (Just (ForgeLogin (ForgeSlug "git.example") "test-user")) (RepoId githubForge "owner" "repo")
            result `shouldBeM` Disallowed

        it "returns false for non-existing repos" $ do
          GH.withFakeGithubInterface $ \_ghFake -> do
            result <- getRepoPermissions (Just (ForgeLogin githubForge "test-user")) (RepoId githubForge "owner" "repo")
            result `shouldBeM` Disallowed

        it "caches responses" $ do
          GH.withFakeGithubInterface $ \st -> do
            GH.mkRepo st "owner" "repo"
              $ (#publicity .~ RepoIsPublic True)
            result <- getRepoPermissions (Just (ForgeLogin githubForge "someone")) (RepoId githubForge "owner" "repo")
            result `shouldBeM` Allowed
            GH.mkRepo st "owner" "repo"
              $ (#publicity .~ RepoIsPublic False)
            result <- getRepoPermissions (Just (ForgeLogin githubForge "someone")) (RepoId githubForge "owner" "repo")
            result `shouldBeM` Allowed
            clearCache __getRepoPermissionsCache
            result <- getRepoPermissions (Just (ForgeLogin githubForge "someone")) (RepoId githubForge "owner" "repo")
            result `shouldBeM` Disallowed

decisionSpec :: Spec
decisionSpec = do
  let repo = RepoId githubForge "owner" "repo"
      decided (Decided permission _ _) = Just permission
      decided (CheckCollaborator _) = Nothing

  describe "decidePermission" $ do
    it "allows anybody on a public repository" $ do
      decided (decidePermission repo Nothing (RepoIsPublic True)) `shouldBe` Just Allowed
      decided (decidePermission repo (Just (ForgeLogin (ForgeSlug "git.example") "someone")) (RepoIsPublic True))
        `shouldBe` Just Allowed

    it "refuses an anonymous request for a private repository" $ do
      decided (decidePermission repo Nothing (RepoIsPublic False)) `shouldBe` Just Disallowed

    it "refuses an account on another forge without asking for collaborators" $ do
      decided (decidePermission repo (Just (ForgeLogin (ForgeSlug "git.example") "someone")) (RepoIsPublic False))
        `shouldBe` Just Disallowed

    it "asks for the collaborators of a private repository on the account's forge" $ do
      decidePermission repo (Just (ForgeLogin githubForge "someone")) (RepoIsPublic False)
        `shouldBe` CheckCollaborator "someone"

  describe "collaboratorPermission" $ do
    let permission user collaborators = view _1 (collaboratorPermission user collaborators)
    it "allows a collaborator" $ do
      permission "someone" (GhCollaborators ["other", "someone"]) `shouldBe` Allowed

    it "refuses anybody else, and a repository the forge does not know" $ do
      permission "someone" (GhCollaborators ["other"]) `shouldBe` Disallowed
      permission "someone" RepoNotFound `shouldBe` Disallowed
