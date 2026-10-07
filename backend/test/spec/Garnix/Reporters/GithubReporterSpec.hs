module Garnix.Reporters.GithubReporterSpec where

import Control.Lens
import Data.Text qualified as T
import Garnix.BuildLogs.Types (mkLogLine)
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Monad
import Garnix.Prelude
import Garnix.Reporters.GithubReporter
import Garnix.TestHelpers (defaultCommitInfo, waitFor)
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.Types
import Test.Hspec

spec :: Spec
spec = do
  let testDebounceDuration = fromMilliSeconds @Int 5
  inM $ aroundM_ (local (#githubLogDebounceDuration .~ testDebounceDuration)) $ describe "mkGithubReporter" $ do
    it "reports successful runs to github" $ GH.withFakeGithubInterface $ \ghState -> do
      GH.mkRepo ghState "owner" "repo" identity
      let reporter = mkGithubReporter (RepoInfo undefined undefined (RepoId githubForge "owner" "repo")) "abc"
      run <- DB.newRun "name" defaultCommitInfo
      runReporter <- createNewRun reporter (ReportRun run)
      reportLogs runReporter (mkLogLine "some log line")
      reportComplete runReporter RunReportStatusSuccess
      reports <- fromReports <$> GH.getReports ghState
      reports `shouldBeM` [("name", RunReportStatusInProgress, ""), ("name", RunReportStatusSuccess, "some log line\n")]

    it "reports failing runs to github" $ GH.withFakeGithubInterface $ \ghState -> do
      GH.mkRepo ghState "owner" "repo" identity
      let reporter = mkGithubReporter (RepoInfo undefined undefined (RepoId githubForge "owner" "repo")) "abc"
      run <- DB.newRun "name" defaultCommitInfo
      runReporter <- createNewRun reporter (ReportRun run)
      reportLogs runReporter (mkLogLine "some log line")
      reportComplete runReporter RunReportStatusFailure
      reports <- fromReports <$> GH.getReports ghState
      reports `shouldBeM` [("name", RunReportStatusInProgress, ""), ("name", RunReportStatusFailure, "some log line\n")]

    describe "reportLogs" $ do
      it "reports log lines without final status to github" $ GH.withFakeGithubInterface $ \ghState -> do
        GH.mkRepo ghState "owner" "repo" identity
        let reporter = mkGithubReporter (RepoInfo undefined undefined (RepoId githubForge "owner" "repo")) "abc"
        run <- DB.newRun "name" defaultCommitInfo
        runReporter <- createNewRun reporter (ReportRun run)
        reportLogs runReporter (mkLogLine "some log line")
        threadDelay (testDebounceDuration `multiplyDuration` (2 :: Int))
        reports <- fromReports <$> GH.getReports ghState
        reports `shouldBeM` [("name", RunReportStatusInProgress, ""), ("name", RunReportStatusInProgress, "some log line\n")]

      it "doesn't modify the current status" $ GH.withFakeGithubInterface $ \ghState -> do
        GH.mkRepo ghState "owner" "repo" identity
        let reporter = mkGithubReporter (RepoInfo undefined undefined (RepoId githubForge "owner" "repo")) "abc"
        run <- DB.newRun "name" defaultCommitInfo
        runReporter <- createNewRun reporter (ReportRun run)
        reportLogs runReporter (mkLogLine "foo")
        threadDelay (testDebounceDuration `multiplyDuration` (2 :: Int))
        reportComplete runReporter RunReportStatusSuccess
        reportLogs runReporter (mkLogLine "bar")
        threadDelay (testDebounceDuration `multiplyDuration` (2 :: Int))
        reports <- fromReports <$> GH.getReports ghState
        reports
          `shouldBeM` [ ("name", RunReportStatusInProgress, ""),
                        ("name", RunReportStatusInProgress, "foo\n"),
                        ("name", RunReportStatusSuccess, "foo\n"),
                        ("name", RunReportStatusSuccess, "foo\nbar\n")
                      ]

      it "debounces requests to github" $ GH.withFakeGithubInterface $ \ghState -> do
        local (#githubLogDebounceDuration .~ fromMilliSeconds @Int 100) $ do
          GH.mkRepo ghState "owner" "repo" identity
          let reporter = mkGithubReporter (RepoInfo undefined undefined (RepoId githubForge "owner" "repo")) "abc"
          run <- DB.newRun "name" defaultCommitInfo
          runReporter <- createNewRun reporter (ReportRun run)
          replicateM_ 10 $ do
            reportLogs runReporter (mkLogLine "x")
          reports <- waitFor (fromSeconds @Int 5) $ do
            reports <- fromReports <$> GH.getReports ghState
            reports
              ^? _last
                `shouldBeM` Just ("name", RunReportStatusInProgress, T.unlines $ replicate 10 "x")
            pure reports
          length reports `shouldSatisfyM` (< 11)

      it "sends the last update immediately" $ GH.withFakeGithubInterface $ \ghState -> do
        local (#githubLogDebounceDuration .~ fromSeconds @Int 5) $ do
          GH.mkRepo ghState "owner" "repo" identity
          let reporter = mkGithubReporter (RepoInfo undefined undefined (RepoId githubForge "owner" "repo")) "abc"
          run <- DB.newRun "name" defaultCommitInfo
          runReporter <- createNewRun reporter (ReportRun run)
          replicateM_ 10 $ do
            reportLogs runReporter (mkLogLine "x")
          reportLogs runReporter (mkLogLine "final")
          reportComplete runReporter RunReportStatusSuccess
          reports <- fromReports <$> GH.getReports ghState
          reports
            `shouldBeM` [ ("name", RunReportStatusInProgress, ""),
                          ("name", RunReportStatusSuccess, T.unlines (replicate 10 "x" <> ["final"]))
                        ]

    describe "when check logs are final" $ do
      it "only creates the check run and completes it, with the logs" $ GH.withFakeGithubInterface $ \ghState -> do
        local (#githubCheckLogs .~ GithubCheckLogsFinal) $ do
          GH.mkRepo ghState "owner" "repo" identity
          let reporter = mkGithubReporter (RepoInfo undefined undefined (RepoId githubForge "owner" "repo")) "abc"
          run <- DB.newRun "name" defaultCommitInfo
          runReporter <- createNewRun reporter (ReportRun run)
          replicateM_ 10 $ do
            reportLogs runReporter (mkLogLine "x")
            threadDelay (testDebounceDuration `multiplyDuration` (2 :: Int))
          reportComplete runReporter RunReportStatusSuccess
          threadDelay (testDebounceDuration `multiplyDuration` (2 :: Int))
          reports <- fromReports <$> GH.getReports ghState
          reports
            `shouldBeM` [ ("name", RunReportStatusInProgress, ""),
                          ("name", RunReportStatusSuccess, T.unlines (replicate 10 "x"))
                        ]

      it "doesn't send lines logged after completion" $ GH.withFakeGithubInterface $ \ghState -> do
        local (#githubCheckLogs .~ GithubCheckLogsFinal) $ do
          GH.mkRepo ghState "owner" "repo" identity
          let reporter = mkGithubReporter (RepoInfo undefined undefined (RepoId githubForge "owner" "repo")) "abc"
          run <- DB.newRun "name" defaultCommitInfo
          runReporter <- createNewRun reporter (ReportRun run)
          reportLogs runReporter (mkLogLine "foo")
          reportComplete runReporter RunReportStatusSuccess
          reportLogs runReporter (mkLogLine "uploaded to cache")
          threadDelay (testDebounceDuration `multiplyDuration` (2 :: Int))
          reports <- fromReports <$> GH.getReports ghState
          reports
            `shouldBeM` [ ("name", RunReportStatusInProgress, ""),
                          ("name", RunReportStatusSuccess, "foo\n")
                        ]

  describe "parseGithubCheckLogs" $ do
    it "accepts the values the NixOS option allows" $ do
      parseGithubCheckLogs "live" `shouldBe` Right GithubCheckLogsLive
      parseGithubCheckLogs "final" `shouldBe` Right GithubCheckLogsFinal

    it "rejects anything else" $ do
      parseGithubCheckLogs "Final" `shouldSatisfy` isLeft
      parseGithubCheckLogs "off" `shouldSatisfy` isLeft
      parseGithubCheckLogs "" `shouldSatisfy` isLeft

fromReports :: [[(RepoInfo, GhRunReport)]] -> [(Text, RunReportStatus, Text)]
fromReports reports = map go $ concat reports
  where
    go :: (RepoInfo, GhRunReport) -> (Text, RunReportStatus, Text)
    go (_, report) = (report ^. name, report ^. status, getRawLogs (report ^. logs))
