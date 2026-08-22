{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Garnix.API.BuildSpec where

import Control.Lens
import Data.Aeson.Lens
import Data.String.Interpolate
import Data.Text qualified as T
import Garnix.API.Builds (updateBuild)
import Garnix.Build (buildFlake)
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Monad
import Garnix.Monad.Async (resolve)
import Garnix.Prelude
import Garnix.Reporters.OpenSearchReporter (openSearchReporter)
import Garnix.TestHelpers
  ( addNixExperimentalFeatures,
    fromSingleton,
    parseTimestamp,
    repoCollaboratorsLens,
    shouldMatchRegexpLines,
    testBuild,
    truncateDBM,
    withGithubMock,
  )
import Garnix.TestHelpers.Common
import Garnix.TestHelpers.Deprecated qualified as Deprecated
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.WithServer
import Garnix.Types hiding (context)
import Network.Wreq.Lens
import System.Random
import Test.Hspec hiding (shouldThrow)

spec :: Spec
spec = inM $ aroundM_ suppressLogsWhenPassing $ beforeM_ truncateDBM $ do
  describe "BuildSpec"
    $ around_ Deprecated.quietWhenPassing
    $ do
      describe "/api/build/{id}" $ do
        describe "collaborators" $ do
          it "shows builds to collaborators even if they didn't start the build"
            $ withGithubMock repoCollaboratorsLens (\_ _ -> pure $ GhCollaborators ["dev-user"])
            $ do
              withServer $ \testServer -> do
                now <- liftIO getCurrentTime
                _user <- testServer.login
                let mkBuild :: (Build -> Build) -> M Build
                    mkBuild f =
                      testBuild $ \b ->
                        b
                          & drvPath ?~ "/nix/store/target-drv.drv"
                          & reqUser .~ "some random user"
                          & repoIsPublic .~ RepoIsPublic False
                          & f
                build <- mkBuild (endTime ?~ now)
                void
                  $ assert200
                  $ testServer.get
                  $ "/api/build/"
                  <> cs (getHashId $ getBuildId $ build ^. id)

        it "responds with 404 if the build id is not found" $ withServer $ \testServer -> do
          result <- testServer.get "/api/build/GgbmXOW9"
          result `shouldHaveStatusCode` 404

        it "provides the most recent other build the user has access to with the same drv path" $ withServer $ \testServer -> do
          now <- liftIO getCurrentTime
          user <- testServer.login
          let mkBuild :: (Build -> Build) -> M Build
              mkBuild f =
                testBuild $ \b ->
                  b
                    & drvPath ?~ "/nix/store/target-drv.drv"
                    & reqUser .~ (user ^. githubLogin)
                    & repoIsPublic .~ RepoIsPublic False
                    & alreadyBuilt ?~ False
                    & f
          _differentDrvPath <- mkBuild $ drvPath ?~ "/nix/store/unrelated-drv.drv"
          _wrongUser <- mkBuild $ reqUser .~ "unrelated"
          _older <- mkBuild (endTime ?~ subTime (fromMinutes @Int 1) now)
          previousBuild <- mkBuild (endTime ?~ now)
          targetBuild <- mkBuild (alreadyBuilt ?~ True)
          res <- assert200 $ testServer.get $ "/api/build/" <> cs (getHashId $ getBuildId $ targetBuild ^. id)
          let [relatedBuilds] = res ^.. responseBody . key "original_build" . _Value
          liftIO
            $ relatedBuilds
            `shouldBe` [aesonQQ|
                         { id: #{previousBuild ^. id}, git_commit: "aaaaaa", status: "Success" }
                       |]

      describe "put /api/build/{buildid}" $ do
        let cancelBuild = [aesonQQ|{ "status": "Cancelled" }|]

        it "responds with 401 if the build id is not found" $ withServer $ \testServer -> do
          result <- testServer.putWithHeaders ("/api/build/GgbmXOW9" :: String) [] cancelBuild
          result `shouldHaveStatusCode` 401

        it "cancel the build for a given build id" $ withServer $ \testServer -> do
          _user <- testServer.login
          build <- testBuild $ (gitCommit .~ "aaaaaa") . (startTime .~ parseTimestamp "2010-03-04T01:00:00Z") . (status .~ Nothing) . (reqUser .~ (_user ^. githubLogin))
          result <- testServer.putWithHeaders ("/api/build/" <> cs (getHashId $ getBuildId $ build ^. id)) [] (toJSON cancelBuild)
          result `shouldHaveStatusCode` 200

        it "responds with 401 if the user don't have write access to the public repository" $ withServer $ \testServer -> do
          build <- testBuild $ (gitCommit .~ "aaaaaa") . (startTime .~ parseTimestamp "2010-03-04T01:00:00Z") . (status .~ Nothing) . (repoIsPublic .~ RepoIsPublic True) . (reqUser .~ "some random user")
          result <- testServer.putWithHeaders ("/api/build/" <> cs (getHashId $ getBuildId $ build ^. id)) [] (toJSON cancelBuild)
          result `shouldHaveStatusCode` 401

      describe "/api/build/{buildid}/logs" $ do
        let flake =
              [i|
                {
                  outputs = {self}: {
                    packages.x86_64-linux.test-pkg = derivation {
                      name = "test-pkg";
                      builder = "/bin/sh";
                      args = ["-c" "echo some-build-output"];
                      system = "x86_64-linux";
                    };
                  };
                }
              |]
            commitInfo =
              CommitInfo
                { _commitInfoReqUser = undefined,
                  _commitInfoRepoPublicity = RepoIsPublic True,
                  _commitInfoRepoInfo =
                    RepoInfo
                      { _repoInfoInstallationAuth = undefined,
                        _repoInfoGhToken = undefined,
                        _repoInfoRepoId = RepoId githubForge "owner" "repo"
                      },
                  _commitInfoBranch = Just $ Branch "test-branch",
                  _commitInfoPrFromFork = Nothing,
                  _commitInfoCommit = undefined
                }

        it "responds with 404 if the build id is not found" $ withServer $ \testServer -> do
          result <- testServer.get "/api/build/GgbmXOW9/logs"
          result `shouldHaveStatusCode` 404

        it "responds with 400 if the build id is not valid" $ withServer $ \testServer -> do
          result <- testServer.get "/api/build/abc123/logs"
          result `shouldHaveStatusCode` 400

        it "returns the build logs for a given build" $ withServer $ \testServer -> do
          user <- testUser "owner" "user@example.com"
          _ <- Deprecated.writeMockRemote "test-branch" def
          mockRemote <- view #workingDir
          liftIO $ writeFile (mockRemote </> "flake.nix") flake
          commit' <- commitAll mockRemote
          resolve
            =<< buildFlake
              openSearchReporter
              ( commitInfo
                  & reqUser .~ ForgeLogin githubForge (user ^. githubLogin)
                  & commit .~ commit'
              )
          build' <- fromSingleton . filter (\x -> x ^. packageType == TypePackage) <$> DB.getBuilds user
          res <- assert200 $ testServer.get $ "/api/build/" <> cs (getHashId $ getBuildId $ build' ^. id) <> "/logs"
          let [log] = res ^.. responseBody . key "logs" . _Array . traverse . filtered (\l -> l ^. key "package" . _String == "test-pkg")
          liftIO $ log ^. key "log_message" . _String `shouldBe` "some-build-output"

        it "returns private builds if you are logged in as a user who has access to it" $ withServer $ \testServer -> do
          user <- testServer.login
          _ <- Deprecated.writeMockRemote "test-branch" def
          mockRemote <- view #workingDir
          liftIO $ writeFile (mockRemote </> "flake.nix") flake
          commit' <- commitAll mockRemote
          resolve
            =<< buildFlake
              openSearchReporter
              ( commitInfo
                  & reqUser .~ ForgeLogin githubForge (user ^. githubLogin)
                  & commit .~ commit'
                  & repoPublicity .~ RepoIsPublic False
              )
          build' <- fromSingleton . filter (\x -> x ^. packageType == TypePackage) <$> DB.getBuilds user
          res <- assert200 $ testServer.get $ "/api/build/" <> cs (getHashId $ getBuildId $ build' ^. id) <> "/logs"
          let [log] = res ^.. responseBody . key "logs" . _Array . traverse . filtered (\l -> l ^. key "package" . _String == "test-pkg")
          liftIO $ log ^. key "log_message" . _String `shouldBe` "some-build-output"

        it "returns 404 if you are logged in as a user who does not have access to a build" $ withServer $ \testServer -> do
          buildUser <- testUser "build-user" "build-user@example.com"
          _loggedInUser <- testServer.login
          _ <- Deprecated.writeMockRemote "test-branch" def
          mockRemote <- view #workingDir
          liftIO $ writeFile (mockRemote </> "flake.nix") flake
          commit' <- commitAll mockRemote
          resolve
            =<< buildFlake
              openSearchReporter
              ( commitInfo
                  & reqUser .~ ForgeLogin githubForge (buildUser ^. githubLogin)
                  & commit .~ commit'
                  & repoPublicity .~ RepoIsPublic False
              )
          build' <- fromSingleton . filter (\x -> x ^. packageType == TypePackage) <$> DB.getBuilds buildUser
          result <- testServer.get $ "/api/build/" <> cs (getHashId $ getBuildId $ build' ^. id) <> "/logs"
          result `shouldHaveStatusCode` 404

        it "supports passing `after` query string to allow streaming of logs" $ withServer $ \testServer -> do
          let flake =
                [i|
                  {
                    outputs = {self}: {
                      packages.x86_64-linux.test-pkg = derivation {
                        name = "test-pkg";
                        builder = "/bin/sh";
                        args = ["-c" "echo line1 && echo line2 && echo line3 && echo line4"];
                        system = "x86_64-linux";
                      };
                    };
                  }
                |]
          user <- testUser "owner" "user@example.com"
          _ <- Deprecated.writeMockRemote "test-branch" def
          mockRemote <- view #workingDir
          liftIO $ writeFile (mockRemote </> "flake.nix") flake
          commit' <- commitAll mockRemote
          resolve
            =<< buildFlake
              openSearchReporter
              ( commitInfo
                  & reqUser .~ ForgeLogin githubForge (user ^. githubLogin)
                  & commit .~ commit'
              )
          build' <- fromSingleton . filter (\x -> x ^. packageType == TypePackage) <$> DB.getBuilds user
          let getLogsForQuery query = do
                logs <- assert200 $ testServer.get . cs $ "/api/build/" <> getHashId (getBuildId $ build' ^. id) <> "/logs?" <> query
                pure $ logs ^.. responseBody . key "logs" . _Array . traverse . filtered (\l -> l ^. key "package" . _String == "test-pkg")

          allLogs <- getLogsForQuery ""
          let [timestamp1, timestamp2, timestamp3, timestamp4] = map (^. (key "timestamp" . _String)) allLogs
          afterLine1 <- getLogsForQuery $ "after=" <> timestamp1
          afterLine2 <- getLogsForQuery $ "after=" <> timestamp2
          afterLine3 <- getLogsForQuery $ "after=" <> timestamp3
          afterLine4 <- getLogsForQuery $ "after=" <> timestamp4
          liftIO $ map (^. (key "log_message" . _String)) afterLine1 `shouldBe` ["line2", "line3", "line4"]
          liftIO $ map (^. (key "log_message" . _String)) afterLine2 `shouldBe` ["line3", "line4"]
          liftIO $ map (^. (key "log_message" . _String)) afterLine3 `shouldBe` ["line4"]
          liftIO $ map (^. (key "log_message" . _String)) afterLine4 `shouldBe` []

        it "supports unicode" $ withServer $ \testServer -> do
          let flake =
                [i|
                  {
                    outputs = {self}: {
                      packages.x86_64-linux.test-pkg = derivation {
                        name = "test-pkg";
                        builder = "/bin/sh";
                        args = ["-c" "echo 🇳🇱"];
                        system = "x86_64-linux";
                      };
                    };
                  }
                |]
          user <- testServer.login
          _ <- Deprecated.writeMockRemote "test-branch" def
          mockRemote <- view #workingDir
          liftIO $ writeFile (mockRemote </> "flake.nix") flake
          commit' <- commitAll mockRemote
          resolve
            =<< buildFlake
              openSearchReporter
              ( commitInfo
                  & reqUser .~ ForgeLogin githubForge (user ^. githubLogin)
                  & commit .~ commit'
              )
          build' <- fromSingleton . filter (\x -> x ^. packageType == TypePackage) <$> DB.getBuilds user
          res <- assert200 $ testServer.get $ "/api/build/" <> cs (getHashId $ getBuildId $ build' ^. id) <> "/logs"
          let [log] = res ^.. responseBody . key "logs" . _Array . traverse . filtered (\l -> l ^. key "package" . _String == "test-pkg")
          liftIO $ log ^. key "log_message" . _String `shouldBe` "🇳🇱"

      describe "/api/build/{buildid}/logs/raw" $ do
        let flake :: String -> IO String
            flake script = do
              random :: Int <- randomIO
              pure
                [i|
                  {
                    outputs = {self}: {
                      packages.x86_64-linux.test-pkg = derivation {
                        name = "test-pkg";
                        builder = "/bin/sh";
                        args = ["-c" ''#{script} && echo #{random} > $out''];
                        system = "x86_64-linux";
                      };
                    };
                  }
                |]
            commitInfo =
              CommitInfo
                { _commitInfoReqUser = undefined,
                  _commitInfoRepoPublicity = RepoIsPublic True,
                  _commitInfoRepoInfo =
                    RepoInfo
                      { _repoInfoInstallationAuth = undefined,
                        _repoInfoGhToken = undefined,
                        _repoInfoRepoId = RepoId githubForge "owner" "repo"
                      },
                  _commitInfoBranch = Just $ Branch "test-branch",
                  _commitInfoPrFromFork = Nothing,
                  _commitInfoCommit = undefined
                }
        it "returns the build logs for a given build" $ withServer $ \testServer -> do
          user <- testUser "owner" "user@example.com"
          _ <- Deprecated.writeMockRemote "test-branch" def
          mockRemote <- view #workingDir
          liftIO $ writeFile (mockRemote </> "flake.nix") =<< flake "echo line1 && echo line2 && echo line3 && echo line4"
          commit' <- commitAll mockRemote
          resolve
            =<< buildFlake
              openSearchReporter
              ( commitInfo
                  & reqUser .~ ForgeLogin githubForge (user ^. githubLogin)
                  & commit .~ commit'
              )
          build' <- fromSingleton . filter (\x -> x ^. packageType == TypePackage) <$> DB.getBuilds user

          res <- assert200 $ testServer.get $ "/api/build/" <> cs (getHashId $ getBuildId $ build' ^. id) <> "/logs/raw"
          let lines =
                T.lines (T.strip $ cs $ res ^. responseBody)
                  & dropWhile (\line -> not $ "this derivation will be built:" `T.isSuffixOf` line)
          liftIO
            $ lines
            `shouldMatchRegexpLines` [ "^\\d{4}-\\d{2}-\\d{2} [\\d:.]+ UTC> this derivation will be built:$",
                                       "^\\d{4}-\\d{2}-\\d{2} [\\d:.]+ UTC>   /nix/store/[a-zA-Z0-9]{32}-test-pkg.drv$",
                                       "^\\d{4}-\\d{2}-\\d{2} [\\d:.]+ UTC test-pkg> line1$",
                                       "^\\d{4}-\\d{2}-\\d{2} [\\d:.]+ UTC test-pkg> line2$",
                                       "^\\d{4}-\\d{2}-\\d{2} [\\d:.]+ UTC test-pkg> line3$",
                                       "^\\d{4}-\\d{2}-\\d{2} [\\d:.]+ UTC test-pkg> line4$"
                                     ]

        it "streams multiple pages of log lines from opensearch" $ withServer $ \testServer -> do
          user <- testUser "owner" "user@example.com"
          _ <- Deprecated.writeMockRemote "test-branch" def
          mockRemote <- view #workingDir
          liftIO $ writeFile (mockRemote </> "flake.nix") =<< flake "function seq { if [[ $1 -gt 0 ]]; then echo \"seq $1\" && seq $(($1 - 1)); fi ; } && seq 5000"
          commit' <- commitAll mockRemote
          resolve
            =<< buildFlake
              openSearchReporter
              ( commitInfo
                  & reqUser .~ ForgeLogin githubForge (user ^. githubLogin)
                  & commit .~ commit'
              )
          build' <- fromSingleton . filter (\x -> x ^. packageType == TypePackage) <$> DB.getBuilds user
          res <- assert200 $ testServer.get $ "/api/build/" <> cs (getHashId $ getBuildId $ build' ^. id) <> "/logs/raw"
          let lines =
                T.lines (cs $ res ^. responseBody)
                  & dropWhile (\line -> not $ "seq 5000" `T.isSuffixOf` line)
                  & dropWhileEnd (\line -> not $ "seq 1" `T.isSuffixOf` line)
          liftIO $ length lines `shouldBe` 5000

      describe "cancel build" $ around_ (addNixExperimentalFeatures ["nix-command", "flakes"]) $ do
        let cancelBuild = BuildUpdate {_buildUpdateStatus = Just Cancelled}
            timeoutBuild = BuildUpdate {_buildUpdateStatus = Just Timeout}

        it "succeeds when build has no status" $ do
          user <- testUser "test-user" "foo@example.com"
          b <- testBuild $ (gitCommit .~ "aaaaaa") . (startTime .~ parseTimestamp "2010-03-04T01:00:00Z") . (status .~ Nothing)
          updateBuild user (b ^. id) cancelBuild

        it "fails when cancels a build already stopped" $ do
          user <- testUser "test-user" "foo@example.com"
          b <- testBuild $ (gitCommit .~ "aaaaaa") . (startTime .~ parseTimestamp "2010-03-04T01:00:00Z")
          updateBuild user (b ^. id) cancelBuild `shouldThrowM` BuildAlreadyStopped (b ^. id)

        it "fails when using an unknown type of status" $ do
          user <- testUser "test-user" "foo@example.com"
          b <- testBuild $ (gitCommit .~ "aaaaaa") . (startTime .~ parseTimestamp "2010-03-04T01:00:00Z") . (status .~ Nothing)
          updateBuild user (b ^. id) timeoutBuild `shouldThrowM` InvalidBuildUpdate timeoutBuild

        it "fails when cancelling the build for another user" $ do
          user <- testUser "test-user" "foo@example.com"
          b <- testBuild $ (gitCommit .~ "aaaaaa") . (startTime .~ parseTimestamp "2010-03-04T01:00:00Z") . (reqUser .~ "some random user")
          updateBuild user (b ^. id) cancelBuild `shouldThrowM` NoSuchBuild (b ^. id)

testUser :: GhLogin -> Email -> M User
testUser ghLogin email =
  DB.newUser ghLogin email FreeSubscription True
