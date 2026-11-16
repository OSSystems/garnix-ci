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
import Garnix.Supersede (arrivalOn, onLine)
import Garnix.Supersede.Types (Arrival (..), Line (..))
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
      then onLine (PullRequestLine (commitInfo ^. repoInfo . repoId) prId) (commitInfo ^. commit) NewHead (buildFlake reporter commitInfo) >>= resolve
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
    let repo = commitInfo ^. repoInfo . repoId
        onBranch arrival action =
          case commitInfo ^. branch of
            Just branch' -> do
              arrival' <- arrival branch'
              onLine (BranchLine repo branch') (commitInfo ^. commit) arrival' action
            Nothing -> action
        asPushed branch' =
          arrivalOn (BranchLine repo branch') (commitInfo ^. commit)
            $ (Just <$> getHeadCommit (commitInfo ^. repoInfo . ghToken) repo branch')
            `catchError` (\e -> logError e $> Nothing)
    case (allowDuplicateRun, pushResult) of
      (False, Just DB.AlreadyPushed) -> do
        log Informational "handleCommit: This repoOwner, repoName, commit, branch combination has already been pushed before. Skipping build"
        emptyPromise
      (False, Nothing) -> do
        log Informational "handleCommit: CommitInfo is missing branch, but allowDuplicateRun is set. Skipping build"
        emptyPromise
      -- Only a commit newly pushed to the branch supersedes the branch's
      -- other runs: one asked to be built again may well be an old one.
      (_, Just DB.NewPush) -> do
        onBranch asPushed (buildFlake reporter commitInfo) <?> "Build flake"
      -- The commit was superseded before it was built. Either the branch is
      -- back on it, or this is a late duplicate of the event that first
      -- pushed it, which must not supersede what came after.
      (False, Just DB.SupersededPush) ->
        backOnSupersededCommit >>= \case
          True -> onBranch (const $ pure NewHead) (buildFlake reporter commitInfo) <?> "Build flake"
          False -> do
            log Informational "handleCommit: the commit was superseded and is not the head of the branch. Skipping build"
            emptyPromise
      (True, _) -> do
        onBranch (const $ pure Rebuilt) (buildFlake reporter commitInfo) <?> "Build flake"
  where
    backOnSupersededCommit = case commitInfo ^. branch of
      Nothing -> pure False
      Just branch' -> do
        let repo = commitInfo ^. repoInfo . repoId
        isHead <-
          ((== commitInfo ^. commit) <$> getHeadCommit (commitInfo ^. repoInfo . ghToken) repo branch')
            `catchError` (\e -> logError e $> False)
        if isHead
          then DB.claimSupersededPush repo (commitInfo ^. commit) branch'
          else pure False

handleRerun :: (HasCallStack) => RerunEvent -> M ()
handleRerun ev = do
  hostname <- view #hostname
  build' <- DB.makeNewBuildForGithubRunId (ev ^. #reqUser) (ev ^. #ghRunId) hostname
  withSpan (build' ^. id) $ do
    let commitInfo =
          CommitInfo
            { _commitInfoReqUser = ForgeLogin (build' ^. forge) (ev ^. #reqUser),
              _commitInfoRepoPublicity = ev ^. #repoIsPublic,
              _commitInfoRepoInfo = RepoInfo (ev ^. #credentials) (ev ^. #token) (buildRepoId build'),
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
