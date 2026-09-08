module Garnix.Sweep
  ( heartbeat,
    sweepOrphans,
  )
where

import Control.Exception.Safe qualified as SafeException
import Data.Containers.ListUtils (nubOrd)
import Garnix.Build.MetaCheck qualified as MetaCheck
import Garnix.Build.Reporting (reportNameForBuild)
import Garnix.BuildLogs.Types (mkLogLine)
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.Reporters.GithubReporter (mkGithubReporter)
import Garnix.Reporters.OpenSearchReporter (openSearchReporter)
import Garnix.Types
import GitHub.Data.Id (Id (Id))

heartbeat :: M ()
heartbeat = DB.upsertEvalHeartbeat

sweepOrphans :: M ()
sweepOrphans = do
  live <- DB.getLiveEvalInstances
  if null live
    then log Warning "sweepOrphans: no live instances recorded, not even this one; skipping"
    else do
      sweepBuilds live
      sweepRuns live
      sweepStuckMetaChecks live
      sweepEvaluations live

sweepBuilds :: [Text] -> M ()
sweepBuilds live = do
  builds <- DB.getOrphanedBuilds live
  unless (null builds) $ do
    log Notice $ "sweepOrphans: " <> show (length builds) <> " abandoned build(s)"
    forM_ (groupOn (\build -> (build ^. repoUser, build ^. repoName)) builds)
      $ \((owner, repoName), repoBuilds) ->
        withRepoInfo owner repoName (length repoBuilds) $ \repoInfo -> do
          forM_ repoBuilds $ \build ->
            closeBuild (reporterFor repoInfo (build ^. gitCommit)) build
          forM_ (groupOn (^. gitCommit) repoBuilds) $ \(commitHash, commitBuilds) ->
            forM_ (take 1 commitBuilds) $ \build ->
              MetaCheck.update
                (reporterFor repoInfo commitHash)
                (commitInfoFor repoInfo commitHash build)

closeBuild :: Reporter -> Build -> M ()
closeBuild reporter build = do
  case build ^. githubRunId of
    Nothing -> pure ()
    Just ghRunId -> do
      runReporter <- resumeRun reporter ghRunId (ReportBuild (reportNameForBuild build) build)
      reportLogs runReporter (mkLogLine abandonedMessage)
      reportComplete runReporter RunReportStatusCancelled
  now <- liftIO getCurrentTime
  DB.reportBuildResultDB (build & status ?~ Cancelled & endTime ?~ now)

sweepRuns :: [Text] -> M ()
sweepRuns live = do
  runs <- DB.getOrphanedRuns live
  unless (null runs) $ do
    log Notice $ "sweepOrphans: " <> show (length runs) <> " abandoned run(s)"
    forM_ (groupOn (\(run, _) -> (run ^. repoUser, run ^. repoName)) runs)
      $ \((owner, repoName), repoRuns) ->
        withRepoInfo owner repoName (length repoRuns) $ \repoInfo ->
          forM_ repoRuns $ \(run, mGhRunId) -> case mGhRunId of
            Nothing -> DB.setRunStatus (run ^. id) (Just Cancelled)
            Just ghRunId -> do
              runReporter <- resumeRun (reporterFor repoInfo (run ^. gitCommit)) ghRunId (ReportRun run)
              reportLogs runReporter (mkLogLine abandonedMessage)
              reportComplete runReporter RunReportStatusCancelled

sweepStuckMetaChecks :: [Text] -> M ()
sweepStuckMetaChecks live = do
  commits <- DB.getStuckMetaChecks live
  unless (null commits) $ do
    log Notice $ "sweepOrphans: " <> show (length commits) <> " unreported meta check(s)"
    forM_ commits $ \(owner, repoName, commitHash) ->
      withRepoInfo owner repoName 1 $ \repoInfo ->
        MetaCheck.update
          (reporterFor repoInfo commitHash)
          (minimalCommitInfo repoInfo commitHash)

sweepEvaluations :: [Text] -> M ()
sweepEvaluations live = do
  evaluations <- DB.getOrphanedEvaluations live
  unless (null evaluations) $ do
    log Notice $ "sweepOrphans: " <> show (length evaluations) <> " abandoned evaluation(s)"
    forM_ evaluations $ \(owner, repoName, commitHash) ->
      withRepoInfo owner repoName 1 $ \repoInfo -> do
        let reporter = reporterFor repoInfo commitHash
        runReporter <- createNewRun reporter MetaCheck
        MetaCheck.updateFail
          MetaCheck.NoComment
          (minimalCommitInfo repoInfo commitHash)
          runReporter
          Nothing
        DB.setCommitStatus owner repoName commitHash Evaluated

withRepoInfo :: GhRepoOwner -> GhRepoName -> Int -> (RepoInfo -> M ()) -> M ()
withRepoInfo owner repoName subjects action = do
  credentials <-
    ( (Right <$> fetchCredentials)
        `catchError` (pure . Left . show . pretty . err)
    )
      `SafeException.catchAny` (pure . Left . show)
  case credentials of
    Right (Just repoInfo) -> action repoInfo
    Right Nothing -> skip "garnix is not installed on it"
    Left problem -> skip $ "could not get credentials for it: " <> problem
  where
    fetchCredentials :: M (Maybe RepoInfo)
    fetchCredentials =
      getGarnixInstallationId owner repoName >>= \case
        Nothing -> pure Nothing
        Just installationId -> do
          installationAuth <- getInstallation (Id (fromInteger installationId))
          token <- getAccessToken installationAuth
          pure $ Just $ RepoInfo installationAuth token owner repoName

    skip reason =
      log Warning
        $ "sweepOrphans: leaving "
        <> show subjects
        <> " abandoned item(s) of "
        <> getGhLogin (getGhRepoOwner owner)
        <> "/"
        <> getGhRepoName repoName
        <> " open: "
        <> reason

reporterFor :: RepoInfo -> CommitHash -> Reporter
reporterFor repoInfo commitHash = mkGithubReporter repoInfo commitHash <> openSearchReporter

commitInfoFor :: RepoInfo -> CommitHash -> Build -> CommitInfo
commitInfoFor repoInfo commitHash build =
  CommitInfo
    { _commitInfoReqUser = build ^. reqUser,
      _commitInfoRepoPublicity = build ^. repoIsPublic,
      _commitInfoRepoInfo = repoInfo,
      _commitInfoBranch = build ^. branch,
      _commitInfoPrFromFork = build ^. prFromFork,
      _commitInfoCommit = commitHash
    }

minimalCommitInfo :: RepoInfo -> CommitHash -> CommitInfo
minimalCommitInfo repoInfo commitHash =
  CommitInfo
    { _commitInfoReqUser = getGhRepoOwner (repoInfo ^. ghRepoOwner),
      _commitInfoRepoPublicity = RepoIsPublic False,
      _commitInfoRepoInfo = repoInfo,
      _commitInfoBranch = Nothing,
      _commitInfoPrFromFork = Nothing,
      _commitInfoCommit = commitHash
    }

groupOn :: (Ord k) => (a -> k) -> [a] -> [(k, [a])]
groupOn key items = [(k, filter ((== k) . key) items) | k <- nubOrd (map key items)]

abandonedMessage :: Text
abandonedMessage =
  "The garnix server running this restarted before it finished. Push again to retry."
