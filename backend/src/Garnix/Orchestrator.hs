{-# LANGUAGE DuplicateRecordFields #-}

module Garnix.Orchestrator
  ( ForgeEvent (..),
    handleForgeEvent,
    handlePullRequest,
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

-- | What a forge's webhook asks garnix to do, in terms every forge can
-- produce. Each forge's webhook handler verifies and parses its own payloads
-- into one of these and hands it to 'handleForgeEvent'.
data ForgeEvent
  = -- | A commit landed on a branch. The flag is set when the forge asked for
    -- the commit to be built again even if it was built before.
    CommitPushed Bool CommitInfo
  | -- | A pull request was opened or its head moved.
    PullRequestUpdated CommitInfo GhPullRequestId
  | -- | Someone asked the forge to rerun a single build. Only forges whose
    -- reports carry a rerun button (GitHub check runs) send this.
    RunRerequested RerunEvent

handleForgeEvent :: (HasCallStack) => ForgeEvent -> M (Promise ())
handleForgeEvent = \case
  CommitPushed allowDuplicateRun commitInfo ->
    handleCommit (reporterFor commitInfo) allowDuplicateRun commitInfo
  PullRequestUpdated commitInfo prId ->
    handlePullRequest (reporterFor commitInfo) commitInfo prId
  RunRerequested rerunEvent -> do
    handleRerun rerunEvent
    emptyPromise
  where
    reporterFor commitInfo =
      openSearchReporter <> mkGithubReporter (commitInfo ^. repoInfo) (commitInfo ^. commit)

data RerunEvent = RerunEvent
  { reqUser :: GhLogin,
    ghRunId :: GhRunId,
    credentials :: ForgeCredentials,
    token :: GhToken,
    repoIsPublic :: RepoPublicity
  }
  deriving stock (Generic)

handlePullRequest :: (HasCallStack) => Reporter -> CommitInfo -> GhPullRequestId -> M (Promise ())
handlePullRequest reporter commitInfo prId = do
  assertIsAllowedToBuild (commitInfo ^. repoInfo . repoId)

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
        $ withInternalCacheToken (commitInfo ^. Types.reqUser . ghLogin)
        $ void
        $ rolloutNewServerVersion reporter commitInfo (GhPrDeployment prId)

handleCommit :: (HasCallStack) => Reporter -> Bool -> CommitInfo -> M (Promise ())
handleCommit reporter allowDuplicateRun commitInfo = do
  withSpan commitInfo $ do
    assertIsAllowedToBuild (commitInfo ^. repoInfo . repoId)
    pushResult <- case commitInfo ^. branch of
      Nothing -> do
        log Informational "handleCommit: CommitInfo is missing branch. Not registering push"
        pure Nothing
      Just branch -> do
        Just
          <$> DB.registerPush
            (commitInfo ^. repoInfo . repoId)
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
            { _commitInfoReqUser = ForgeLogin githubForge (ev ^. #reqUser),
              _commitInfoRepoPublicity = ev ^. #repoIsPublic,
              _commitInfoRepoInfo = RepoInfo (ev ^. #credentials) (ev ^. #token) (RepoId githubForge (build' ^. repoUser) (build' ^. repoName)),
              _commitInfoBranch = build' ^. branch,
              _commitInfoPrFromFork = build' ^. prFromFork,
              _commitInfoCommit = build' ^. gitCommit
            }
    let reporter = openSearchReporter <> mkGithubReporter (commitInfo ^. repoInfo) (commitInfo ^. commit)
    assertIsAllowedToBuild (commitInfo ^. repoInfo . repoId)
    withSpan commitInfo $ rerunBuild reporter build' commitInfo

assertIsAllowedToBuild :: RepoId -> M ()
assertIsAllowedToBuild repo = do
  isDenied <- DB.isDenylisted repo
  when isDenied $ do
    throw IsDeniedAccess
