{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Garnix.Reporters.OpenSearchReporterSpec where

import Control.Lens
import Data.Aeson (Value, throwDecode')
import Data.Aeson.Lens
import Garnix.BuildLogs.Types (mkLogLine)
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Monad.Async
import Garnix.Orchestrator qualified as Orchestrator
import Garnix.Prelude
import Garnix.Reporters.OpenSearchReporter
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.WithServer
import Garnix.Types
import Network.Wreq
import Test.Hspec

spec :: Spec
spec = do
  inM $ aroundM_ suppressLogsWhenPassing $ describe "openSearchReporter" $ do
    it "allows creating in-progress runs on a commit" $ withServer $ \server -> do
      setupFakeCommit $ \commitInfo -> do
        beforeReportStart <- liftIO getCurrentTime
        run <- DB.newRun "test run name" commitInfo
        void $ createNewRun openSearchReporter (ReportRun run)
        afterReportStart <- liftIO getCurrentTime
        runId <- getRunIdsByCommitAndPackage server (commitInfo ^. commit) "test run name"
        res <- assert200 $ server.get (cs $ "/api/run/" <> runId)
        let startTime = res ^?! responseBody . key "start_time" . _String . to cs . to parseTimestamp
        startTime `shouldSatisfyM` (> beforeReportStart)
        startTime `shouldSatisfyM` (< afterReportStart)
        (res ^? responseBody . key "end_time" . _String) `shouldBeM` Nothing
        withoutTimes :: Value <-
          throwDecode' (res ^. responseBody)
            <&> atKey "start_time" .~ Nothing
        withoutTimes
          `shouldBeM` [aesonQQ|
            {
              id: #{runId},
              name: "test run name",
              forge: "github",
              repo_user: "owner",
              repo_name: "repo",
              git_commit: #{commitInfo ^. commit},
              branch: "branch"
            }
          |]

    let testCases =
          [ (flip reportComplete RunReportStatusSuccess, "Success"),
            (flip reportComplete RunReportStatusFailure, "Failure"),
            (flip reportComplete RunReportStatusCancelled, "Cancelled")
          ]
    forM_ testCases $ \(reportCompleteFn, status) -> do
      it ("allows marking runs with status '" <> cs status <> "'") $ withServer $ \server -> do
        setupFakeCommit $ \commitInfo -> do
          run <- DB.newRun "name" commitInfo
          runReporter <- createNewRun openSearchReporter (ReportRun run)
          beforeReportStatus <- liftIO getCurrentTime
          reportCompleteFn runReporter
          afterReportStatus <- liftIO getCurrentTime
          runId <- getRunIdsByCommitAndPackage server (commitInfo ^. commit) "name"

          res <- assert200 $ server.get (cs $ "/api/run/" <> runId)
          (res ^? responseBody . key "id" . _String) `shouldBeM` Just runId
          (res ^? responseBody . key "status" . _String) `shouldBeM` Just status
          let endTime = res ^?! responseBody . key "end_time" . _String . to cs . to parseTimestamp
          endTime `shouldSatisfyM` (> beforeReportStatus)
          endTime `shouldSatisfyM` (< afterReportStatus)

    it "allows sending log messages with a report" $ withServer $ \server -> do
      setupFakeCommit $ \commitInfo -> do
        run <- DB.newRun "name" commitInfo
        runReporter <- createNewRun openSearchReporter (ReportRun run)
        reportLogs runReporter (mkLogLine "some log line")
        reportComplete runReporter RunReportStatusSuccess
        runId <- getRunIdsByCommitAndPackage server (commitInfo ^. commit) "name"
        res <- assert200 $ server.get (cs $ "/api/run/" <> runId <> "/logs")
        let logMessages = res ^.. responseBody . key "logs" . _Array . traverse . key "log_message" . _String
        logMessages `shouldBeM` ["some log line"]

    describe "reportLogs" $ do
      it "allows sending log messages without a final status" $ withServer $ \server -> do
        setupFakeCommit $ \commitInfo -> do
          run <- DB.newRun "name" commitInfo
          runReporter <- createNewRun openSearchReporter (ReportRun run)
          reportLogs runReporter (mkLogLine "some log line")
          runId <- getRunIdsByCommitAndPackage server (commitInfo ^. commit) "name"
          res <- assert200 $ server.get (cs $ "/api/run/" <> runId <> "/logs")
          let logMessages = res ^.. responseBody . key "logs" . _Array . traverse . key "log_message" . _String
          logMessages `shouldBeM` ["some log line"]

      it "does not modify the status" $ withServer $ \server -> do
        setupFakeCommit $ \commitInfo -> do
          run <- DB.newRun "name" commitInfo
          runReporter <- createNewRun openSearchReporter (ReportRun run)
          runId <- getRunIdsByCommitAndPackage server (commitInfo ^. commit) "name"
          reportLogs runReporter (mkLogLine "foo")
          getStatus server runId `shouldReturnM` Nothing
          reportComplete runReporter RunReportStatusSuccess
          getStatus server runId `shouldReturnM` Just "Success"
          reportLogs runReporter (mkLogLine "bar")
          getStatus server runId `shouldReturnM` Just "Success"
          res <- assert200 $ server.get (cs $ "/api/run/" <> runId <> "/logs")
          let logMessages = res ^.. responseBody . key "logs" . _Array . traverse . key "log_message" . _String
          logMessages `shouldBeM` ["foo", "bar"]

getStatus :: TestServer -> Text -> M (Maybe Text)
getStatus server runId = do
  res <- assert200 $ server.get (cs $ "/api/run/" <> runId)
  pure $ res ^? responseBody . key "status" . _String

setupFakeCommit :: (CommitInfo -> M ()) -> M ()
setupFakeCommit action = do
  let flake = "{ outputs = _: {}; }"
  let commitInfo = defaultCommitInfo & repoPublicity .~ RepoIsPublic True
  GH.withFakeGithubInterface $ \ghState -> do
    GH.withLocalRepo ghState "owner" "repo" identity commitInfo (GH.simpleSetup flake) $ \commitInfo -> do
      resolve =<< Orchestrator.handleCommit mempty True commitInfo
      action commitInfo

getRunIdsByCommitAndPackage :: (HasCallStack) => TestServer -> CommitHash -> PackageName -> M Text
getRunIdsByCommitAndPackage server commitHash packageName = do
  res <- assert200 $ server.get (cs $ "/api/build/commit/" <> getCommitHash commitHash)
  let runs =
        res ^. responseBody . key "runs" . _Array . to toList
          & filter (\x -> x ^. key "name" . _String == getPackageName packageName)
  pure $ fromSingleton $ map (^. key "id" . _String) runs
