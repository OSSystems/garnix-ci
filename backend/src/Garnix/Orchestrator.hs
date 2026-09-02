{-# LANGUAGE DuplicateRecordFields #-}

module Garnix.Orchestrator
  ( handlePullRequest,
    handleCommit,
    handleRerun,
    RerunEvent (..),
  )
where

import Garnix.Async (Promise)
import Garnix.Build (buildFlake, rerunBuild)
import Garnix.Build.Checkout qualified as Build.Checkout
import Garnix.Build.Helpers (withInternalCacheToken)
import Garnix.DB qualified as DB
import Garnix.Hosting.Deploy (rolloutNewServerVersion)
import Garnix.Hosting.Types (DeploymentType (..))
import Garnix.Monad
import Garnix.Monad.Async (emptyPromise, resolve, spawn)
import Garnix.Prelude
import Garnix.Reporters.GithubReporter (mkGithubReporter)
import Garnix.Reporters.OpenSearchReporter (openSearchReporter)
import Garnix.Types as Types hiding (ghRunId)
import GitHub.App.Auth qualified as GH

data RerunEvent = RerunEvent
  { reqUser :: GhLogin,
    ghRunId :: GhRunId,
    installAuth :: GH.InstallationAuth,
    token :: GhToken,
    repoIsPublic :: RepoPublicity
  }
  deriving stock (Generic)

handlePullRequest :: (HasCallStack) => Reporter -> CommitInfo -> GhPullRequestId -> M (Promise ())
handlePullRequest reporter commitInfo prId = do
  assertIsAllowedToBuild (commitInfo ^. repoInfo . ghRepoOwner) (commitInfo ^. repoInfo . ghRepoName)

  withSpan commitInfo $ spawn $ do
    -- A PR from a fork has no branch on the base repo, so nothing has built it
    -- yet. A PR from a branch of this repo was already built by the push that
    -- created it — but that push deployed under its BRANCH, so the PR still
    -- needs its own rollout to get a pull-N deployment.
    if isJust (commitInfo ^. prFromFork)
      then buildFlake reporter commitInfo >>= resolve
      else deployPrServers
  where
    deployPrServers =
      Build.Checkout.withCheckout commitInfo
        $ withSpan prId
        $ withInternalCacheToken (commitInfo ^. Types.reqUser)
        $ void
        $ rolloutNewServerVersion reporter commitInfo (GhPrDeployment prId)

handleCommit :: (HasCallStack) => Reporter -> Bool -> CommitInfo -> M (Promise ())
handleCommit reporter allowDuplicateRun commitInfo = do
  withSpan commitInfo $ do
    assertIsAllowedToBuild (commitInfo ^. repoInfo . ghRepoOwner) (commitInfo ^. repoInfo . ghRepoName)
    pushResult <- case commitInfo ^. branch of
      Nothing -> do
        log Informational "handleCommit: CommitInfo is missing branch. Not registering push"
        pure Nothing
      Just branch -> do
        Just
          <$> DB.registerPush
            (commitInfo ^. repoInfo . ghRepoOwner)
            (commitInfo ^. repoInfo . ghRepoName)
            (commitInfo ^. commit)
            branch
    case (allowDuplicateRun, pushResult) of
      (False, Just DB.AlreadyPushed) -> do
        log Informational "handleCommit: This repoOwner, repoName, commit, branch combination has already been pushed before. Skipping build"
        emptyPromise
      (False, Nothing) -> do
        log Informational "handleCommit: CommitInfo is missing branch, but allowDuplicateRun is set. Skipping build"
        emptyPromise
      (False, Just DB.NewPush) -> do
        buildFlake reporter commitInfo <?> "Build flake"
      (True, _) -> do
        buildFlake reporter commitInfo <?> "Build flake"

handleRerun :: (HasCallStack) => RerunEvent -> M ()
handleRerun ev = do
  hostname <- view #hostname
  build' <- DB.makeNewBuildForGithubRunId (ev ^. #reqUser) (ev ^. #ghRunId) hostname
  withSpan (build' ^. id) $ do
    let commitInfo =
          CommitInfo
            { _commitInfoReqUser = ev ^. #reqUser,
              _commitInfoRepoPublicity = ev ^. #repoIsPublic,
              _commitInfoRepoInfo = RepoInfo (ev ^. #installAuth) (ev ^. #token) (build' ^. repoUser) (build' ^. repoName),
              _commitInfoBranch = build' ^. branch,
              _commitInfoPrFromFork = build' ^. prFromFork,
              _commitInfoCommit = build' ^. gitCommit
            }
    let reporter = openSearchReporter <> mkGithubReporter (commitInfo ^. repoInfo) (commitInfo ^. commit)
    assertIsAllowedToBuild (build' ^. repoUser) (build' ^. repoName)
    withSpan commitInfo $ rerunBuild reporter build' commitInfo

assertIsAllowedToBuild :: GhRepoOwner -> GhRepoName -> M ()
assertIsAllowedToBuild owner repo = do
  isDenied <- DB.isDenylisted owner repo
  when isDenied $ do
    throw IsDeniedAccess
