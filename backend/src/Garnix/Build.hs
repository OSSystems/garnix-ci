module Garnix.Build
  ( buildFlake,
    rerunBuild,
    buildModule,
  )
where

import Control.Concurrent.Async.Lifted
import Cradle hiding (ExitCode)
import Garnix.Async
import Garnix.Build.Checkout (withAuthorization)
import Garnix.Build.Checkout qualified as Checkout
import Garnix.Build.Flake
import Garnix.Build.FodCheck qualified as FodCheck
import Garnix.Build.Helpers
import Garnix.Build.MetaCheck qualified as MetaCheck
import Garnix.Build.Module qualified as Module
import Garnix.Build.Package (doBuild)
import Garnix.Build.Reporting
import Garnix.DB qualified as DB
import Garnix.DB.ModuleValues qualified as ModuleValues
import Garnix.Monad
import Garnix.Monad.Async (emptyPromise, spawn)
import Garnix.Monad.Concurrency
import Garnix.Prelude
import Garnix.Reporters.GithubReporter (mkGithubReporter)
import Garnix.Reporters.OpenSearchReporter (openSearchReporter)
import Garnix.Types as Types
import Garnix.YamlConfig (flakeDir)

buildModule :: GhLogin -> ModuleValues.GetRepoAndModuleValues -> M CommitInfo
buildModule reqUser modules = do
  commitInfo <- Module.getCommitInfo reqUser modules
  let reporter = openSearchReporter <> mkGithubReporter (commitInfo ^. repoInfo) (commitInfo ^. commit)
  ensureNoFlakeOnCommit commitInfo
  let defaultBranch = maybe (Branch "main") identity $ commitInfo ^. branch
      remote = Module.remoteWithFlake defaultBranch modules Checkout.remoteWithConfig
  forkM $ runBuildFlake reporter ModulePreview commitInfo remote
  pure commitInfo
  where
    ensureNoFlakeOnCommit :: CommitInfo -> M ()
    ensureNoFlakeOnCommit commitInfo =
      doesRepoFileExist commitInfo "flake.nix" >>= \case
        FileExists -> do
          log Informational "flake.nix already exists - skipping."
          throw ModuleErrorFlakeExists
        _ -> pure ()

-- | Build the flake, sending updates to github and the DB as you go along.
--
-- N.B.: This function should return as quickly as possible so the connection
-- can be closed.
buildFlake :: (HasCallStack) => Reporter -> CommitInfo -> M (Promise ())
buildFlake = curry $ mockable #buildFlakeMock $ \(reporter, commitInfo) -> do
  filesExist <-
    concurrently
      (doesRepoFileExist commitInfo "flake.nix")
      (doesRepoFileExist commitInfo "garnix.yaml")
  case filesExist of
    (FileDoesntExist, FileDoesntExist) -> do
      log Informational "No flake.nix or garnix.yaml - skipping."
      emptyPromise
    _ -> spawn $ runBuildFlake reporter Webhook commitInfo Checkout.remoteWithConfig

rerunBuild :: Reporter -> Build -> CommitInfo -> M ()
rerunBuild reporter build commitInfo = do
  MetaCheck.update reporter commitInfo
  runReporter <- createNewRun reporter $ ReportBuild (reportNameForBuild build) build
  let build' = build & githubRunId .~ Garnix.Monad.ghRunId runReporter
  DB.reportBuildResultDB build' <?> "Adding build github ID to DB"
  reportOnError runReporter build' commitInfo $ do
    repoConfig <- DB.getRepoConfig (commitInfo ^. repoInfo . ghRepoOwner) (commitInfo ^. repoInfo . ghRepoName)
    Checkout.runWithCheckout Checkout.remoteWithConfig commitInfo $ \config -> do
      withAuthorization (config ^. flakeDir) repoConfig commitInfo $ do
        reportBuildResult runReporter build'
        void $ withInternalCacheToken (commitInfo ^. reqUser) $ do
          FodCheck.withFodChecker reporter commitInfo $ \fodChecker -> do
            doBuild fodChecker runReporter Webhook (config ^. flakeDir) repoConfig build'
        MetaCheck.update reporter commitInfo
