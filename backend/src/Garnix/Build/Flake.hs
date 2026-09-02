module Garnix.Build.Flake
  ( runBuildFlake,
  )
where

import Control.Lens
import Data.IORef (newIORef, readIORef, writeIORef)
import Garnix.Attribute
import Garnix.Build.Action qualified as Action
import Garnix.Build.Checkout (Remote, runWithCheckout, withAuthorization)
import Garnix.Build.FodCheck qualified as FodCheck
import Garnix.Build.Helpers
import Garnix.Build.MetaCheck qualified as MetaCheck
import Garnix.Build.Package (doBuild)
import Garnix.Build.Reporting
import Garnix.DB qualified as DB
import Garnix.GetAttributes
import Garnix.Limits qualified as Limits
import Garnix.Modules qualified as Modules
import Garnix.Monad
import Garnix.Hosting.Deploy (rolloutNewServerVersion)
import Garnix.Hosting.Types (DeploymentType (..), ServerInfo (..))
import Garnix.Monad.Async (joinAll, joinAll_, resolve, spawn)
import Garnix.Prelude
import Garnix.Types as Types
import Garnix.YamlConfig (Action, ExcludeBranches (..), GarnixConfig, IncrementalizeBuildsSection (..), commentOnFailure, flakeDir, incrementalizeBuildsSection)

runBuildFlake :: (HasCallStack) => Reporter -> BuildKind -> CommitInfo -> Remote -> M ()
runBuildFlake reporter buildKind commitInfo withCheckout = do
  (startingBuild, startingBuildRunReporter) <- newBuild reporter commitInfo (PackageInfo TypeOverall NoSystem buildStarting) False
  withInternalCacheToken (commitInfo ^. reqUser) $ do
    metaCheckRun <- MetaCheck.newReport reporter commitInfo
    -- Only known once the checkout hands us the repo's garnix.yaml, but needed
    -- by the handler below, which also covers failures from before that point
    -- (a broken checkout, an unparseable garnix.yaml).
    commentPolicy <- liftIO $ newIORef MetaCheck.NoComment
    let onFail err = do
          policy <- liftIO $ readIORef commentPolicy
          MetaCheck.updateFail policy commitInfo metaCheckRun (Just err)
          rethrowEither err
    flip catchEither onFail $ do
      reportOnError startingBuildRunReporter startingBuild commitInfo $ do
        repoConfig <- DB.getRepoConfig (commitInfo ^. repoInfo . ghRepoOwner) (commitInfo ^. repoInfo . ghRepoName)
        runWithCheckout withCheckout commitInfo $ \config -> do
          let policy =
                if config ^. commentOnFailure
                  then MetaCheck.CommentOnFailure
                  else MetaCheck.NoComment
          liftIO $ writeIORef commentPolicy policy
          withAuthorization (config ^. flakeDir) repoConfig commitInfo $ do
            initialBuilds <- setupBuilds reporter commitInfo config
            initialActions <- setupActions reporter commitInfo config
            updatedBuild <-
              liftIO getCurrentTime <&> \now ->
                startingBuild
                  & status ?~ Success
                  & endTime ?~ now
            DB.setCommitStatus (commitInfo ^. repoInfo . ghRepoOwner) (commitInfo ^. repoInfo . ghRepoName) (commitInfo ^. commit) Evaluated
            reportBuildResult startingBuildRunReporter updatedBuild

            FodCheck.withFodChecker reporter commitInfo $ \fodChecker -> do
              buildPromises <- forM initialBuilds $ \(initialBuild, runReporter) -> do
                spawn $ doBuild fodChecker runReporter buildKind (config ^. flakeDir) repoConfig initialBuild
              actionPromises <- forM initialActions $ \(initialBuild, runReporter, actionConfig) -> do
                spawn
                  $ buildAndRunAction
                    reporter
                    fodChecker
                    runReporter
                    commitInfo
                    buildKind
                    (config ^. flakeDir)
                    repoConfig
                    initialBuild
                    actionConfig
              builds <- joinAll buildPromises >>= resolve
              joinAll_ actionPromises >>= resolve

              let allBuildsSucceeded = all (\build -> build ^. status == Just Success) builds

              when allBuildsSucceeded $ do
                Modules.publish reporter config commitInfo

              deployments <- case (commitInfo ^. prFromFork, commitInfo ^. branch) of
                (Nothing, Just branch') ->
                  if allBuildsSucceeded
                    then rolloutNewServerVersion reporter commitInfo (BranchDeployment branch')
                    else pure []
                (Just _, Nothing) -> do
                  -- A fork PR's code would run on a guest holding this repo's
                  -- deploy key, so it never gets one.
                  log Notice "PR is from a fork; not deploying servers"
                  pure []
                _ -> do
                  log Critical "Commit has neither, or both, of a branch and a fork; not deploying servers"
                  pure []

              let allDeploymentsSucceeded =
                    all
                      ( \server ->
                          isJust (_serverInfoReadyAt server)
                            && isNothing (_serverInfoEndedAt server)
                      )
                      deployments

              if allBuildsSucceeded && allDeploymentsSucceeded
                then MetaCheck.updateSuccess commitInfo metaCheckRun
                else MetaCheck.updateFail policy commitInfo metaCheckRun Nothing

setupBuilds :: Reporter -> CommitInfo -> GarnixConfig -> M [(Build, RunReporter)]
setupBuilds reporter commitInfo config = do
  toBuild <- do
    attributes <- getAttributesToBuild commitInfo config
    maxPackages <- liftIO Limits.maxPackagesPerFlake
    when (length attributes > fromIntegral maxPackages) $ do
      throw
        $ OtherError
        $ "Number of packages too large. Maximum is "
        <> show maxPackages
        <> ", you have "
        <> show (length attributes)
    pure attributes
  log Informational $ "Will build the following attributes: " <> show toBuild
  forM toBuild $ \attr -> do
    setupBuild reporter config commitInfo attr

setupActions :: Reporter -> CommitInfo -> GarnixConfig -> M [(Build, RunReporter, Action)]
setupActions reporter commitInfo config = do
  log Informational $ "Will run the following actions: " <> show (Action.getActionAppAttributes config)
  forM (Action.getActionAppAttributes config) $ \(attr, actionConfig) -> do
    (build, reporter) <- setupBuild reporter config commitInfo attr
    return (build, reporter, actionConfig)

buildAndRunAction ::
  Reporter ->
  Maybe FodChecker ->
  RunReporter ->
  CommitInfo ->
  BuildKind ->
  FlakeDir ->
  RepoConfig ->
  Build ->
  Action ->
  M ()
buildAndRunAction reporter fodChecker runReporter commitInfo buildKind flakeDir repoConfig initialBuild actionConfig = do
  build <- doBuild fodChecker runReporter buildKind flakeDir repoConfig initialBuild
  Action.run flakeDir repoConfig reporter commitInfo (attribute build) actionConfig build

newBuild :: Reporter -> CommitInfo -> PackageInfo -> Bool -> M (Build, RunReporter)
newBuild reporter commitInfo packageInfo wantsIncrementalism = withSpan packageInfo $ do
  hostname <- view #hostname
  initialBuild <-
    DB.newBuildDB commitInfo packageInfo hostname wantsIncrementalism
      <?> "Creating a build in the DB"
  withSpan (initialBuild ^. id) $ do
    runReporter <- createNewRun reporter $ ReportBuild (reportNameForBuild initialBuild) initialBuild
    log Informational $ "My GH run id is: " <> show (Garnix.Monad.ghRunId runReporter)
    let build = initialBuild & githubRunId .~ Garnix.Monad.ghRunId runReporter
    DB.reportBuildResultDB build <?> "Adding build github ID to DB"
    pure (build, runReporter)

setupBuild :: Reporter -> GarnixConfig -> CommitInfo -> Attribute -> M (Build, RunReporter)
setupBuild reporter config commitInfo attr = case attr ^. packageName of
  Nothing -> do
    throw $ OtherError "Tried to build, but no package name available"
  Just pkgName -> do
    let wantsIncrementalism' = case config ^. incrementalizeBuildsSection of
          IncrementalizeBuilds True -> True
          IncrementalizeBuilds False -> False
          IncrementalBuildsExcludeBranches (ExcludeBranches brs) -> case commitInfo ^. branch of
            Nothing -> False
            Just br -> br `notElem` brs
    (build, runReporter) <-
      newBuild
        reporter
        commitInfo
        (PackageInfo (attr ^. packageType) (attr ^. system . from maybeSystemIso) pkgName)
        wantsIncrementalism'
    pure (build, runReporter)
