module Garnix.SweepSpec where

import Control.Lens
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.Build.MetaCheck qualified as MetaCheck
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

    it "leaves a rerun build alone while the process that took it is alive" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        original <- abandonedBuild
        now <- liftIO getCurrentTime
        DB.reportBuildResultDB (original & status ?~ Failure & endTime ?~ now)
        ghRunId' <- maybe (liftIO $ assertFailure "original build has no github run id") pure (original ^. githubRunId)
        rerun <- DB.makeNewBuildForGithubRunId (GhLogin "alice") ghRunId' "this-host"
        heartbeat
        sweepOrphans

        untouched <- DB.getBuild (rerun ^. id)
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

        runs <- DB.getRuns (RepoId githubForge "owner" "repo") defaultCommit
        ((^. status) <$> runs) `shouldBeM` [Just Cancelled]
        statusOf ghState (run ^. name) >>= (`shouldBeM` Just RunReportStatusCancelled)

    it "fails the meta check of an evaluation that never finished" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        local (#evalInstance .~ deadInstance) $ void $ DB.newCommit (RepoId githubForge "owner" "repo") defaultCommit
        heartbeat
        sweepOrphans

        statusOf ghState "All Garnix checks"
          >>= (`shouldBeM` Just RunReportStatusFailure)
        commit' <- DB.getCommit (RepoId githubForge "owner" "repo") defaultCommit
        ((^. status) <$> commit') `shouldBeM` Just Evaluated

    it "closes a meta check run its process left open after the verdict was written" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        void $ local (#evalInstance .~ deadInstance) $ MetaCheck.newReport (reporter defaultCommit) defaultCommitInfo
        void
          $ DB.setMetaCheck
            (RepoId githubForge "owner" "repo")
            defaultCommit
            (DB.CheckStatusUpdate Nothing CheckPending CheckFail)
        heartbeat
        sweepOrphans

        metaCheckRunStatuses ghState `shouldReturnM` [RunReportStatusFailure]

    it "closes the meta check run of a superseded run whose process is gone" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        void $ local (#evalInstance .~ deadInstance) $ MetaCheck.newReport (reporter defaultCommit) defaultCommitInfo
        newer <- MetaCheck.newReport (reporter defaultCommit) defaultCommitInfo
        MetaCheck.updateSuccess defaultCommitInfo newer
        heartbeat
        sweepOrphans

        metaCheckRunStatuses ghState
          `shouldReturnM` [RunReportStatusSuccess, RunReportStatusSuccess]

    it "leaves a meta check run alone while its verdict is pending" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        void $ MetaCheck.newReport (reporter defaultCommit) defaultCommitInfo
        heartbeat
        sweepOrphans

        metaCheckRunStatuses ghState `shouldReturnM` [RunReportStatusInProgress]

    it "leaves an evaluation alone while the process running it is alive" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        void $ DB.newCommit (RepoId githubForge "owner" "repo") defaultCommit
        heartbeat
        sweepOrphans

        commit' <- DB.getCommit (RepoId githubForge "owner" "repo") defaultCommit
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
      void $ DB.newCommit (RepoId githubForge "owner" "repo") defaultCommit
      DB.setCommitStatus (RepoId githubForge "owner" "repo") defaultCommit Evaluated
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
      DB.getBuildsByCommit (RepoId githubForge "owner" "repo") defaultCommit >>= \case
        [build] -> pure build
        builds -> liftIO $ assertFailure $ "expected exactly one build, got " <> Prelude.show (length builds)

    metaCheckRunStatuses :: GithubFakeState -> M [RunReportStatus]
    metaCheckRunStatuses ghState =
      mapMaybe (fmap (^. status) . (^? _last) . filter ((== "All Garnix checks") . (^. name)) . map snd)
        <$> getReports ghState

    statusOf :: GithubFakeState -> Text -> M (Maybe RunReportStatus)
    statusOf ghState name = do
      reports <- getSimpleReports ghState
      pure $ fst <$> (Map.lookup defaultCommit reports >>= Map.lookup name)
