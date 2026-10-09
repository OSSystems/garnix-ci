{-# LANGUAGE OverloadedRecordDot #-}

module Garnix.API.RunsSpec where

import Control.Lens
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens
import Data.Yaml (decodeThrow)
import Data.Yaml.TH (yamlQQ)
import Garnix.BuildLogs.Types
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Monad.Async (resolve)
import Garnix.Orchestrator
import Garnix.Prelude
import Garnix.Reporters.OpenSearchReporter (openSearchReporter)
import Garnix.Reporters.Utils (withRunReporter)
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.WithServer
import Garnix.Types
import Garnix.UserLogs (storeRunLogLine)
import Network.Wreq.Lens
import Test.Hspec

spec :: Spec
spec = inM
  $ aroundM_ suppressLogsWhenPassing
  $ beforeM_ truncateDBM
  $ do
    describe "get /api/run/{id}" $ do
      it "shows runs for public repos when not logged in" $ do
        withFakeGithubInterface $ \ghState -> do
          withServer $ \testServer -> do
            mkRepo ghState "owner" "repo" (#publicity .~ RepoIsPublic True)
            run <- DB.newRun "some-run" defaultCommitInfo
            let runId = run ^. id . to getRunId . re hashIdText
            result <- testServer.get $ cs $ "/api/run/" <> runId
            result `shouldHaveStatusCode` 200
            (result ^?! responseBody . key "id" . _String) `shouldBeM` runId
            (result ^?! responseBody . key "name" . _String) `shouldBeM` "some-run"
            (result ^?! responseBody . key "repo_user" . _String) `shouldBeM` "owner"
            (result ^?! responseBody . key "repo_name" . _String) `shouldBeM` "repo"
            (result ^?! responseBody . key "git_commit" . _String) `shouldBeM` "aaaaaaaa"

      it "shows runs for public repos when logged in" $ do
        withFakeGithubInterface $ \ghState -> do
          withServer $ \testServer -> do
            void testServer.login
            mkRepo ghState "owner" "repo" (#publicity .~ RepoIsPublic True)
            run <- DB.newRun "some-run" defaultCommitInfo
            let runId = run ^. id . to getRunId . re hashIdText
            result <- testServer.get $ cs $ "/api/run/" <> runId
            result `shouldHaveStatusCode` 200
            (result ^?! responseBody . key "id" . _String) `shouldBeM` runId

      it "shows runs for private repos a user has access to" $ do
        withFakeGithubInterface $ \ghState -> do
          withServer $ \testServer -> do
            user <- testServer.login
            mkRepo ghState "owner" "repo" $ (#publicity .~ RepoIsPublic False) . (#collaborators .~ [user ^. soleLogin])
            run <- DB.newRun "some-run" defaultCommitInfo
            let runId = run ^. id . to getRunId . re hashIdText
            result <- testServer.get $ cs $ "/api/run/" <> runId
            result `shouldHaveStatusCode` 200
            (result ^?! responseBody . key "id" . _String) `shouldBeM` runId

      it "responds with 404 if the run id is not found" $ do
        withServer $ \testServer -> do
          result <- testServer.get "/api/run/GgbmXOW9"
          result `shouldHaveStatusCode` 404

      it "responds with 404 for private repos when not logged in" $ do
        withFakeGithubInterface $ \ghState -> do
          withServer $ \testServer -> do
            mkRepo ghState "owner" "repo" (#publicity .~ RepoIsPublic False)
            run <- DB.newRun "some-run" defaultCommitInfo
            result <- testServer.get $ cs $ "/api/run/" <> run ^. id . to getRunId . re hashIdText
            result `shouldHaveStatusCode` 404

      it "responds with 404 for private repos a logged in user does not have access to" $ do
        withFakeGithubInterface $ \ghState -> do
          withServer $ \testServer -> do
            void testServer.login
            mkRepo ghState "owner" "repo" (#publicity .~ RepoIsPublic False)
            run <- DB.newRun "some-run" defaultCommitInfo
            result <- testServer.get $ cs $ "/api/run/" <> run ^. id . to getRunId . re hashIdText
            result `shouldHaveStatusCode` 404

    describe "get /api/run/{id}/logs" $ do
      it "shows logs for runs for public repos when not logged in" $ do
        withFakeGithubInterface $ \ghState -> do
          withServer $ \testServer -> do
            mkRepo ghState "owner" "repo" (#publicity .~ RepoIsPublic True)
            run <- DB.newRun "some-run" defaultCommitInfo
            storeRunLogLine run (mkLogLine "some log line")
            let runId = run ^. id . to getRunId . re hashIdText
            result <- testServer.get $ cs $ "/api/run/" <> runId <> "/logs"
            result `shouldHaveStatusCode` 200
            (result ^.. responseBody . key "logs" . _Array . traverse . key "log_message" . _String) `shouldBeM` ["some log line"]

      it "handles pagination of log lines" $ do
        withFakeGithubInterface $ \ghState -> do
          withServer $ \testServer -> do
            mkRepo ghState "owner" "repo" (#publicity .~ RepoIsPublic True)
            run <- DB.newRun "some-run" defaultCommitInfo
            forM_ ["a", "b", "c", "d", "e", "f"] $ \logLine -> do
              storeRunLogLine run (mkLogLine logLine)
            let runId = run ^. id . to getRunId . re hashIdText
            result <- testServer.get $ cs $ "/api/run/" <> runId <> "/logs"
            (result ^.. responseBody . key "logs" . _Array . traverse . key "log_message" . _String) `shouldBeM` ["a", "b", "c", "d", "e", "f"]
            let logLineDTimestamp = (result ^.. responseBody . key "logs" . _Array . traverse . key "timestamp" . _String) !! 3
            result <- testServer.get $ cs $ "/api/run/" <> runId <> "/logs?after=" <> logLineDTimestamp
            (result ^.. responseBody . key "logs" . _Array . traverse . key "log_message" . _String) `shouldBeM` ["e", "f"]

      it "shows logs for runs for public repos when logged in" $ do
        withFakeGithubInterface $ \ghState -> do
          withServer $ \testServer -> do
            void testServer.login
            mkRepo ghState "owner" "repo" (#publicity .~ RepoIsPublic True)
            run <- DB.newRun "some-run" defaultCommitInfo
            storeRunLogLine run (mkLogLine "some log line")
            let runId = run ^. id . to getRunId . re hashIdText
            result <- testServer.get $ cs $ "/api/run/" <> runId <> "/logs"
            result `shouldHaveStatusCode` 200
            (result ^.. responseBody . key "logs" . _Array . traverse . key "log_message" . _String) `shouldBeM` ["some log line"]

      it "shows logs for runs for private repos a user has access to" $ do
        withFakeGithubInterface $ \ghState -> do
          withServer $ \testServer -> do
            user <- testServer.login
            mkRepo ghState "owner" "repo" $ (#publicity .~ RepoIsPublic False) . (#collaborators .~ [user ^. soleLogin])
            run <- DB.newRun "some-run" defaultCommitInfo
            storeRunLogLine run (mkLogLine "some log line")
            let runId = run ^. id . to getRunId . re hashIdText
            result <- testServer.get $ cs $ "/api/run/" <> runId <> "/logs"
            result `shouldHaveStatusCode` 200
            (result ^.. responseBody . key "logs" . _Array . traverse . key "log_message" . _String) `shouldBeM` ["some log line"]

      it "responds with 404 if the run id is not found" $ do
        withServer $ \testServer -> do
          result <- testServer.get "/api/run/GgbmXOW9/logs"
          result `shouldHaveStatusCode` 404

      it "responds with 404 for private repos when not logged in" $ do
        withFakeGithubInterface $ \ghState -> do
          withServer $ \testServer -> do
            mkRepo ghState "owner" "repo" (#publicity .~ RepoIsPublic False)
            run <- DB.newRun "some-run" defaultCommitInfo
            result <- testServer.get $ cs $ "/api/run/" <> run ^. id . to getRunId . re hashIdText <> "/logs"
            result `shouldHaveStatusCode` 404

      it "responds with 404 for private repos a logged in user does not have access to" $ do
        withFakeGithubInterface $ \ghState -> do
          withServer $ \testServer -> do
            void testServer.login
            mkRepo ghState "owner" "repo" (#publicity .~ RepoIsPublic False)
            run <- DB.newRun "some-run" defaultCommitInfo
            result <- testServer.get $ cs $ "/api/run/" <> run ^. id . to getRunId . re hashIdText <> "/logs"
            result `shouldHaveStatusCode` 404

      describe "openSearchReporter" $ do
        let setup test = do
              withFakeGithubInterface $ \ghState -> do
                withServer $ \testServer -> do
                  let commitInfo = defaultCommitInfo & repoPublicity .~ RepoIsPublic True
                  withLocalRepo ghState "owner" "repo" (#publicity .~ RepoIsPublic True) commitInfo (simpleSetup "{outputs = inputs:{ };}") $ \commitInfo -> do
                    handleCommit openSearchReporter True commitInfo >>= resolve
                    test testServer commitInfo openSearchReporter
        let getLogs :: (HasCallStack) => TestServer -> CommitInfo -> M Aeson.Value
            getLogs testServer commitInfo = do
              getCommit <- assert200 $ testServer.get $ cs $ "/api/commits/" <> (commitInfo ^. commit . to getCommitHash)
              let nonOverallRuns =
                    getCommit ^. responseBody . key "runs" . _Array
                      & toList
                      & filter (\x -> x ^. key "name" . _String == "test run")
              let runId = fromSingleton nonOverallRuns ^?! key "id" . _String
              result <- assert200 $ testServer.get $ cs $ "/api/run/" <> runId <> "/logs"
              body <- decodeThrow $ cs $ result ^. responseBody
              pure $ body & key "logs" . _Array . traverse . atKey "timestamp" .~ Nothing

        it "allows creating runs" $ do
          setup $ \testServer commitInfo reporter -> do
            run <- DB.newRun "test run" commitInfo
            withRunReporter reporter (ReportRun run) $ \runReporter -> do
              reportLogs runReporter (mkLogLine "test message")
              reportComplete runReporter RunReportStatusSuccess
            getLogs testServer commitInfo
              `shouldReturnM` [yamlQQ|
                    finished: true
                    max_page_size: 4096
                    logs:
                      - log_message: test message
                  |]

        it "allows adding logs incrementally" $ do
          setup $ \testServer commitInfo reporter -> do
            run <- DB.newRun "test run" commitInfo
            withRunReporter reporter (ReportRun run) $ \runReporter -> do
              reportLogs runReporter (mkLogLine "first message")
              reportLogs runReporter (mkLogLine "final message")
              reportComplete runReporter RunReportStatusSuccess
            getLogs testServer commitInfo
              `shouldReturnM` [yamlQQ|
                    finished: true
                    max_page_size: 4096
                    logs:
                      - log_message: first message
                      - log_message: final message
                  |]

        it "returns runs as not finished when they're still streaming logs" $ do
          setup $ \testServer commitInfo reporter -> do
            run <- DB.newRun "test run" commitInfo
            withRunReporter reporter (ReportRun run) $ \runReporter -> do
              reportLogs runReporter (mkLogLine "first message")
              getLogs testServer commitInfo
                `shouldReturnM` [yamlQQ|
                      finished: false
                      max_page_size: 4096
                      logs:
                        - log_message: first message
                    |]
              reportLogs runReporter (mkLogLine "final message")
              reportComplete runReporter RunReportStatusSuccess
              getLogs testServer commitInfo
                `shouldReturnM` [yamlQQ|
                      finished: true
                      max_page_size: 4096
                      logs:
                        - log_message: first message
                        - log_message: final message
                    |]
