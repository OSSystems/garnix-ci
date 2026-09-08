module Garnix.SweepSpec where

import Control.Lens
import Data.Map.Strict qualified as Map
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.Build.Reporting (reportNameForBuild)
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.Reporters.GithubReporter (mkGithubReporter)
import Garnix.Sweep
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface
import Garnix.TestHelpers.Monad
import Garnix.Types
import Test.HUnit (assertFailure)
import Test.Hspec
import Prelude qualified

deadInstance :: Text
deadInstance = "some-other-host#gone"

spec :: Spec
spec = inM
  $ aroundM_ suppressLogsWhenPassing
  $ beforeM_ truncateDBM
  $ describe "sweepOrphans"
  $ do
    it "cancels a build whose process is gone" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        build <- abandonedBuild
        heartbeat
        sweepOrphans

        swept <- onlyBuild
        (swept ^. status) `shouldBeM` Just Cancelled
        liftIO $ (swept ^. endTime) `shouldSatisfy` isJust

        statusOf ghState (reportNameForBuild build)
          >>= (`shouldBeM` Just RunReportStatusCancelled)

    it "fails the meta check once the builds under it are closed" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        void abandonedBuild
        heartbeat
        sweepOrphans

        statusOf ghState "All Garnix checks"
          >>= (`shouldBeM` Just RunReportStatusFailure)

    it "reports a meta check left without its verdict" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        build <- abandonedBuild
        now <- liftIO getCurrentTime
        DB.reportBuildResultDB (build & status ?~ Success & endTime ?~ now)
        heartbeat
        sweepOrphans

        statusOf ghState "All Garnix checks"
          >>= (`shouldBeM` Just RunReportStatusSuccess)

    it "leaves a build alone while the process that took it is alive" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        instance_ <- view #evalInstance
        void $ buildOwnedBy instance_
        heartbeat
        sweepOrphans

        untouched <- onlyBuild
        (untouched ^. status) `shouldBeM` Nothing
        liftIO $ (untouched ^. endTime) `shouldSatisfy` isNothing

    it "does nothing at all when no process is recorded as alive" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        void abandonedBuild
        sweepOrphans

        untouched <- onlyBuild
        (untouched ^. status) `shouldBeM` Nothing

    it "cancels a run whose process is gone" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        run <- local (#evalInstance .~ deadInstance) $ do
          run <- DB.newRun "deployment web" defaultCommitInfo
          runReporter <- createNewRun (reporter defaultCommit) (ReportRun run)
          forM_ (Garnix.Monad.ghRunId runReporter) $ DB.setRunGithubId (run ^. id)
          pure run
        heartbeat
        sweepOrphans

        runs <- DB.getRuns "owner" "repo" defaultCommit
        ((^. status) <$> runs) `shouldBeM` [Just Cancelled]
        statusOf ghState (run ^. name) >>= (`shouldBeM` Just RunReportStatusCancelled)

    it "fails the meta check of an evaluation that never finished" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        local (#evalInstance .~ deadInstance) $ DB.newCommit "owner" "repo" defaultCommit
        heartbeat
        sweepOrphans

        statusOf ghState "All Garnix checks"
          >>= (`shouldBeM` Just RunReportStatusFailure)
        commit' <- DB.getCommit "owner" "repo" defaultCommit
        ((^. status) <$> commit') `shouldBeM` Just Evaluated

    it "leaves an evaluation alone while the process running it is alive" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        DB.newCommit "owner" "repo" defaultCommit
        heartbeat
        sweepOrphans

        commit' <- DB.getCommit "owner" "repo" defaultCommit
        ((^. status) <$> commit') `shouldBeM` Just Evaluating

    it "stops counting a process once it misses the whole window" $ do
      heartbeat
      instance_ <- view #evalInstance
      DB.getLiveEvalInstances `shouldReturnM` [instance_]

      void
        $ DB.pgExec
          [pgSQL|
            UPDATE eval_heartbeat
            SET last_beat = NOW() - interval '1 hour'
          |]
      DB.getLiveEvalInstances `shouldReturnM` []

    it "keeps one row per machine, holding whichever process runs there now" $ do
      local (#evalInstance .~ deadInstance) heartbeat
      instance_ <- view #evalInstance
      heartbeat
      DB.getLiveEvalInstances `shouldReturnM` [instance_]
  where
    defaultCommit :: CommitHash
    defaultCommit = defaultCommitInfo ^. commit

    reporter :: CommitHash -> Reporter
    reporter = mkGithubReporter (defaultCommitInfo ^. repoInfo)

    abandonedBuild :: M Build
    abandonedBuild = buildOwnedBy deadInstance

    buildOwnedBy :: Text -> M Build
    buildOwnedBy owner = local (#evalInstance .~ owner) $ do
      DB.newCommit "owner" "repo" defaultCommit
      DB.setCommitStatus "owner" "repo" defaultCommit Evaluated
      build <-
        DB.newBuildDB
          defaultCommitInfo
          (PackageInfo TypePackage (IsSystem X8664Linux) (PackageName "web"))
          "some-other-host"
          False
      runReporter <- createNewRun (reporter defaultCommit) (ReportBuild (reportNameForBuild build) build)
      let reported = build & githubRunId .~ Garnix.Monad.ghRunId runReporter
      DB.reportBuildResultDB reported
      pure reported

    onlyBuild :: M Build
    onlyBuild =
      DB.getBuildsByCommit "owner" "repo" defaultCommit >>= \case
        [build] -> pure build
        builds -> liftIO $ assertFailure $ "expected exactly one build, got " <> Prelude.show (length builds)

    statusOf :: GithubFakeState -> Text -> M (Maybe RunReportStatus)
    statusOf ghState name = do
      reports <- getSimpleReports ghState
      pure $ fst <$> (Map.lookup defaultCommit reports >>= Map.lookup name)
