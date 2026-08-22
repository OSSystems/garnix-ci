{-# LANGUAGE OverloadedRecordDot #-}

module Garnix.API.CommitSpec where

import Control.Lens
import Data.Aeson.Lens
import Garnix.API.Commits
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers hiding (testUser)
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.WithServer
import Garnix.Types hiding (context)
import Network.Wreq
import Test.Hspec hiding (shouldReturn, shouldThrow)

spec :: Spec
spec = inM $ aroundM_ suppressLogsWhenPassing $ beforeM_ truncateDBM $ describe "CommitSpec" $ do
  describe "/api/commits/{commit}" $ around_ (addNixExperimentalFeatures ["nix-command", "flakes"]) $ do
    it "returns a commit summary and builds" $ do
      user <- testUser "test-user" "foo@example.com"
      foo <- testBuild $ (gitCommit .~ "aaaaaa") . (package .~ "foo") . (startTime .~ parseTimestamp "2010-03-04T01:00:00Z")
      void $ testBuild $ (gitCommit .~ "aaaaaa") . (package .~ "foo") . (startTime .~ parseTimestamp "2010-03-04T00:00:00Z")
      bar <- testBuild $ (gitCommit .~ "aaaaaa") . (package .~ "bar") . (startTime .~ parseTimestamp "2010-03-04T02:00:00Z")
      void $ testCommit $ hash .~ "aaaaaa"
      void $ testBuild $ gitCommit .~ "bbbbbb"
      void $ testCommit $ hash .~ "bbbbbb"
      (toJSON <$> getSingleCommit (Just user) "aaaaaa")
        `shouldReturnM` [aesonQQ|
          {
            "summary": {
              "repo_owner": "test-owner",
              "repo_name": "test-repo",
              "repo_is_public": true,
              "branch": "test-branch",
              "git_commit": "aaaaaa",
              "branch": "test-branch",
              "start_time": "2010-03-04T01:00:00Z",
              "req_user": "test-user",
              "succeeded": 2,
              "failed": 0,
              "pending": 0,
              "cancelled": 0
            },
            "runs": [],
            "builds": [{
              "id": #{bar ^. id},
              "package_type": "package",
              "system": "x86_64-linux",
              "package": "bar",
              "branch": "test-branch",
              "git_commit": "aaaaaa",
              "req_user": "test-user",
              "repo_user": "test-owner",
              "repo_name": "test-repo",
              "start_time": "2010-03-04T02:00:00Z",
              "status": "Success",
              "repo_is_public": true,
              "wants_incrementalism": false,
              "eval_host": "garnix-server-test",
              "uploaded_to_cache": false,
              "already_built": false
            }, {
              "id": #{foo ^. id},
              "package_type": "package",
              "system": "x86_64-linux",
              "package": "foo",
              "branch": "test-branch",
              "git_commit": "aaaaaa",
              "req_user": "test-user",
              "repo_user": "test-owner",
              "repo_name": "test-repo",
              "start_time": "2010-03-04T01:00:00Z",
              "status": "Success",
              "repo_is_public": true,
              "wants_incrementalism": false,
              "eval_host": "garnix-server-test",
              "uploaded_to_cache": false,
              "already_built": false
            }]
          }
        |]

    context "access rights" $ around_ (addNixExperimentalFeatures ["nix-command", "flakes"]) $ do
      it "allows users to see the commit if the repo is public" $ do
        user <- testUser "some-user" "foo@example.com"
        build <-
          testBuild
            ( \x ->
                x
                  & gitCommit
                    .~ "aaaaaa"
                  & package
                    .~ "foo"
                  & startTime
                    .~ parseTimestamp "2010-03-04T01:00:00Z"
            )
        void $ testCommit (hash .~ "aaaaaa")
        loggedIn <- getSingleCommit (Just user) "aaaaaa"
        notLoggedIn <- getSingleCommit Nothing "aaaaaa"
        toJSON loggedIn
          `shouldBeM` [aesonQQ|
            {
              "summary": {
                "repo_owner": "test-owner",
                "repo_name": "test-repo",
                "repo_is_public": true,
                "branch": "test-branch",
                "git_commit": "aaaaaa",
                "branch": "test-branch",
                "start_time": "2010-03-04T01:00:00Z",
                "req_user": "test-user",
                "succeeded": 1,
                "failed": 0,
                "pending": 0,
                "cancelled": 0
              },
              "runs": [],
              "builds": [{
                "id": #{build ^. id},
                "package_type": "package",
                "system": "x86_64-linux",
                "package": "foo",
                "branch": "test-branch",
                "git_commit": "aaaaaa",
                "req_user": "test-user",
                "repo_user": "test-owner",
                "repo_name": "test-repo",
                "start_time": "2010-03-04T01:00:00Z",
                "status": "Success",
                "repo_is_public": true,
                "wants_incrementalism": false,
                "eval_host": "garnix-server-test",
                "uploaded_to_cache": false,
                "already_built": false
              }]
            }
          |]
        toJSON loggedIn `shouldBeM` toJSON notLoggedIn

      it "does not allow outside users to see the commit if the repo is not public" $ do
        user <- testUser "some-user" "foo@example.com"
        void $ testBuild $ (gitCommit .~ "aaaaaa") . (package .~ "foo") . (startTime .~ parseTimestamp "2010-03-04T01:00:00Z") . (repoIsPublic .~ RepoIsPublic False)
        result <- try $ getSingleCommit (Just user) "aaaaaa"
        first err result `shouldBeM` Left (NoSuchCommit $ CommitHash "aaaaaa")

      it "allows collaborators to see the commit" $ do
        user <- testUser "dev-user" "foo@example.com"
        withGithubMock repoCollaboratorsLens (\_ _ -> pure $ GhCollaborators ["dev-user"]) $ do
          foo <- testBuild $ (gitCommit .~ "aaaaaa") . (package .~ "foo") . (startTime .~ parseTimestamp "2010-03-04T01:00:00Z") . (repoIsPublic .~ RepoIsPublic False)
          testCommit $ hash .~ "aaaaaa"
          result <- try $ getSingleCommit (Just user) "aaaaaa"
          (toJSON <$> result)
            `shouldBeM` Right
              [aesonQQ|
              {
                "summary": {
                  "repo_owner": "test-owner",
                  "repo_name": "test-repo",
                  "repo_is_public": false,
                  "branch": "test-branch",
                  "git_commit": "aaaaaa",
                  "branch": "test-branch",
                  "start_time": "2010-03-04T01:00:00Z",
                  "req_user": "test-user",
                  "succeeded": 1,
                  "failed": 0,
                  "pending": 0,
                  "cancelled": 0
                },
                "runs": [],
                "builds": [{
                  "id": #{foo ^. id},
                  "package_type": "package",
                  "system": "x86_64-linux",
                  "package": "foo",
                  "branch": "test-branch",
                  "git_commit": "aaaaaa",
                  "req_user": "test-user",
                  "repo_user": "test-owner",
                  "repo_name": "test-repo",
                  "start_time": "2010-03-04T01:00:00Z",
                  "status": "Success",
                  "repo_is_public": false,
                  "wants_incrementalism": false,
                  "eval_host": "garnix-server-test",
                  "uploaded_to_cache": false,
                  "already_built": false
                }]
              }
          |]

      describe "/api/commits" $ around_ (addNixExperimentalFeatures ["nix-command", "flakes"]) $ do
        it "returns commits" $ do
          user <- testUser "test-user" "foo@example.com"
          _ <- testBuild $ (gitCommit .~ "aaaaaa") . (startTime .~ parseTimestamp "2010-03-04T01:00:00Z")
          _ <- testBuild $ (gitCommit .~ "bbbbbb") . (startTime .~ parseTimestamp "2010-03-04T02:00:00Z")
          (toJSON <$> getCommitsForUser user)
            `shouldReturnM` [aesonQQ|
                {
                  "commits": [
                    {
                      "repo_owner": "test-owner",
                      "repo_name": "test-repo",
                      "repo_is_public": true,
                      "branch": "test-branch",
                      "git_commit": "bbbbbb",
                      "start_time": "2010-03-04T02:00:00Z",
                      "req_user": "test-user",
                      "succeeded": 1,
                      "failed": 0,
                      "pending": 0,
                      "cancelled": 0
                    },
                    {
                      "repo_owner": "test-owner",
                      "repo_name": "test-repo",
                      "repo_is_public": true,
                      "branch": "test-branch",
                      "git_commit": "aaaaaa",
                      "start_time": "2010-03-04T01:00:00Z",
                      "req_user": "test-user",
                      "succeeded": 1,
                      "failed": 0,
                      "pending": 0,
                      "cancelled": 0
                    }
                  ]
                }
              |]

        it "returns builds grouped by commit for the requesting user" $ do
          userA <- testUser "user-a" "foo@example.com"
          _userB <- testUser "user-b" "bar@example.com"
          _ <- testBuild $ (reqUser .~ "user-a") . (gitCommit .~ "aaaaaa") . (package .~ "a") . (status ?~ Success)
          _ <- testBuild $ (reqUser .~ "user-a") . (gitCommit .~ "aaaaaa") . (package .~ "b") . (status ?~ Success)
          _ <- testBuild $ (reqUser .~ "user-a") . (gitCommit .~ "aaaaaa") . (package .~ "c") . (status ?~ Success)
          _ <- testBuild $ (reqUser .~ "user-a") . (gitCommit .~ "aaaaaa") . (package .~ "d") . (status ?~ Failure)
          _ <- testBuild $ (reqUser .~ "user-a") . (gitCommit .~ "aaaaaa") . (package .~ "e") . (status ?~ Timeout)
          _ <- testBuild $ (reqUser .~ "user-a") . (gitCommit .~ "aaaaaa") . (package .~ "f") . (status .~ Nothing)
          _ <- testBuild $ (reqUser .~ "user-a") . (gitCommit .~ "aaaaaa") . (package .~ "g") . (status ?~ Cancelled)
          -- Unrelated user:
          _ <- testBuild $ (reqUser .~ "user-b") . (gitCommit .~ "aaaaaa")
          _ <- testBuild $ (reqUser .~ "user-b") . (gitCommit .~ "bbbbbb")
          (toJSON <$> getCommitsForUser userA)
            `shouldReturnM` [aesonQQ|
              {
                "commits": [
                  {
                    "repo_owner": "test-owner",
                    "repo_name": "test-repo",
                    "repo_is_public": true,
                    "branch": "test-branch",
                    "git_commit": "aaaaaa",
                    "start_time": "2010-03-04T00:00:00Z",
                    "req_user": "user-a",
                    "succeeded": 3,
                    "failed": 2,
                    "pending": 1,
                    "cancelled": 1
                  }
                ]
              }
            |]

        it "is distinct on packageType, system and packageName" $ do
          user <- testUser "test-user" "foo@example.com"
          let cases = do
                typ <- [TypePackage, TypeCheck]
                system <- [X8664Linux, X8664Darwin]
                name <- map pure ['a' .. 'f']
                pure (typ, system, name :: String)
          forM_ cases $ \(typ, sys, name) -> do
            testBuild $ (packageType .~ typ) . (system .~ IsSystem sys) . (package .~ fromString name)
          (toJSON <$> getCommitsForUser user)
            `shouldReturnM` [aesonQQ|
              {
                "commits": [
                  {
                    "repo_owner": "test-owner",
                    "repo_name": "test-repo",
                    "repo_is_public": true,
                    "branch": "test-branch",
                    "git_commit": "aaaaaa",
                    "start_time": "2010-03-04T00:00:00Z",
                    "req_user": "test-user",
                    "succeeded": #{length cases},
                    "failed": 0,
                    "pending": 0,
                    "cancelled": 0
                  }
                ]
              }
            |]

        it "only shows the last build per (commit, attribute path)" $ do
          user <- testUser "test-user" "foo@example.com"
          _ <- testBuild $ (status ?~ Failure) . (startTime .~ parseTimestamp "2010-03-04T01:00:00Z")
          _ <- testBuild $ (status ?~ Success) . (startTime .~ parseTimestamp "2010-03-04T03:00:00Z")
          _ <- testBuild $ (status ?~ Failure) . (startTime .~ parseTimestamp "2010-03-04T02:00:00Z")
          (toJSON <$> getCommitsForUser user)
            `shouldReturnM` [aesonQQ|
              {
                "commits": [
                  {
                    "repo_owner": "test-owner",
                    "repo_name": "test-repo",
                    "repo_is_public": true,
                    "branch": "test-branch",
                    "git_commit": "aaaaaa",
                    "start_time": "2010-03-04T03:00:00Z",
                    "req_user": "test-user",
                    "succeeded": 1,
                    "failed": 0,
                    "pending": 0,
                    "cancelled": 0
                  }
                ]
              }
            |]

  describe "/api/commits/repo/<owner>/<name>" $ do
    let mkTestCommits :: GhRepoOwner -> GhRepoName -> M (Build, Build)
        mkTestCommits targetRepoOwner targetRepoName = do
          repoPublicity <- getRepoPublicity undefined (RepoId githubForge targetRepoOwner targetRepoName)
          [commitA : _, commitB : _] <- forM ["aaaaaa", "bbbbbb"] $ \commit -> do
            now <- liftIO getCurrentTime
            forM ["pkg-a", "pkg-b"] $ \pkg -> do
              testBuild
                $ (repoUser .~ targetRepoOwner)
                . (repoName .~ targetRepoName)
                . (repoIsPublic .~ repoPublicity)
                . (startTime .~ now)
                . (gitCommit .~ commit)
                . (package .~ pkg)
          _ <-
            testBuild
              $ (repoUser .~ targetRepoOwner)
              . (repoName .~ "unrelated-repo")
          _ <-
            testBuild
              $ (repoUser .~ "unrelated-user")
              . (repoName .~ targetRepoName)
          pure (commitA, commitB)

    it "returns commits for the specified repo" $ GH.withFakeGithubInterface $ \st -> withServer $ \testServer -> do
      GH.mkRepo st "target-user" "target-repo" identity
      (commitA, commitB) <- mkTestCommits "target-user" "target-repo"
      result <- assert200 $ testServer.get "/api/commits/repo/target-user/target-repo"
      liftIO $ result ^?! responseBody . _Value
        `shouldBe` [aesonQQ|
        {
          commits: [
            {
              repo_owner: "target-user",
              repo_name: "target-repo",
              req_user: "test-user",
              start_time: #{commitB ^. startTime},
              branch: "test-branch",
              git_commit: "bbbbbb",
              repo_is_public: true,
              succeeded: 2,
              cancelled: 0,
              failed: 0,
              pending: 0
            },
            {
              repo_owner: "target-user",
              repo_name: "target-repo",
              req_user: "test-user",
              start_time: #{commitA ^. startTime},
              branch: "test-branch",
              git_commit: "aaaaaa",
              repo_is_public: true,
              succeeded: 2,
              cancelled: 0,
              failed: 0,
              pending: 0
            }
          ]
        }
      |]

    it "returns commits for a private repo where the user is the owner" $ GH.withFakeGithubInterface $ \st -> withServer $ \testServer -> do
      user <- testServer.login
      let owner = GhRepoOwner $ user ^. githubLogin
      GH.mkRepo st owner "target-repo" $ #publicity .~ RepoIsPublic False
      void $ mkTestCommits owner "target-repo"
      result <- assert200 $ testServer.get $ "/api/commits/repo/" <> cs (getGhLogin $ user ^. githubLogin) <> "/target-repo"
      liftIO $ length (result ^?! responseBody . key "commits" . _Array) `shouldBe` 2

    it "returns empty list for a repo that has no commits" $ GH.withFakeGithubInterface $ \st -> withServer $ \testServer -> do
      GH.mkRepo st "target-user" "target-repo" identity
      result <- assert200 $ testServer.get "/api/commits/repo/target-user/target-repo"
      liftIO $ result ^?! responseBody . _Value `shouldBe` [aesonQQ| { commits: [] } |]

    it "returns 404 for repos that don't exist" $ GH.withFakeGithubInterface $ const $ withServer $ \testServer -> do
      result <- testServer.get "/api/commits/repo/target-user/target-repo"
      result `shouldHaveStatusCode` 404

    it "returns 404 for private repos when not logged in" $ GH.withFakeGithubInterface $ \st -> withServer $ \testServer -> do
      GH.mkRepo st "target-user" "target-repo" (#publicity .~ RepoIsPublic False)
      void $ mkTestCommits "target-user" "target-repo"
      result <- testServer.get "/api/commits/repo/target-user/target-repo"
      result `shouldHaveStatusCode` 404

    it "returns 404 for private repos that you don't have access to" $ GH.withFakeGithubInterface $ \st -> withServer $ \testServer -> do
      void testServer.login
      GH.mkRepo st "target-user" "target-repo" $ (#publicity .~ RepoIsPublic False) . (#collaborators .~ [])
      void $ mkTestCommits "target-user" "target-repo"
      result <- testServer.get "/api/commits/repo/target-user/target-repo"
      result `shouldHaveStatusCode` 404

    it "returns commits for private repos" $ GH.withFakeGithubInterface $ \st -> withServer $ \testServer -> do
      user <- testServer.login
      GH.mkRepo st "target-user" "target-repo" $ (#publicity .~ RepoIsPublic False) . (#collaborators .~ [user ^. githubLogin])
      (commitA, commitB) <- mkTestCommits "target-user" "target-repo"
      result <- assert200 $ testServer.get "/api/commits/repo/target-user/target-repo"
      liftIO $ result ^?! responseBody . _Value
        `shouldBe` [aesonQQ|
        {
          commits: [
            {
              repo_owner: "target-user",
              repo_name: "target-repo",
              req_user: "test-user",
              start_time: #{commitB ^. startTime},
              branch: "test-branch",
              git_commit: "bbbbbb",
              repo_is_public: false,
              succeeded: 2,
              cancelled: 0,
              failed: 0,
              pending: 0
            },
            {
              repo_owner: "target-user",
              repo_name: "target-repo",
              req_user: "test-user",
              start_time: #{commitA ^. startTime},
              branch: "test-branch",
              git_commit: "aaaaaa",
              repo_is_public: false,
              succeeded: 2,
              cancelled: 0,
              failed: 0,
              pending: 0
            }
          ]
        }
      |]

testUser :: GhLogin -> Email -> M User
testUser ghLogin email =
  DB.newUser
    ghLogin
    email
    FreeSubscription
    True
