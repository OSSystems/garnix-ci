{-# OPTIONS_GHC -fno-warn-orphans #-}

module Garnix.API.GhWebhooks where

import Data.Text qualified as T
import Garnix.Async
import Garnix.Monad
import Garnix.Monad.Async (emptyPromise, logPromiseErrors)
import Garnix.Monad.Concurrency (forkM)
import Garnix.Orchestrator
import Garnix.Prelude
import Garnix.Types as Types
import GitHub (untagId)
import GitHub.App.Auth qualified as GH
import GitHub.Data.Id (Id (..))
import GitHub.Data.Webhooks.Events
import GitHub.Data.Webhooks.Payload
import Servant.GitHub.Webhook

data GhWebhookAPI route = GhWebhookAPI
  { _ghWebhookCheckSuite ::
      route
        :- GitHubEvent '[ 'WebhookCheckSuiteEvent]
        :> GitHubSignedReqBody '[JSON] CheckSuiteEvent
        :> Post '[JSON] (),
    _ghWebhookCheckRun ::
      route
        :- GitHubEvent '[ 'WebhookCheckRunEvent]
        :> GitHubSignedReqBody '[JSON] CheckRunEvent
        :> Post '[JSON] (),
    _ghWebhookPullRequest ::
      route
        :- GitHubEvent '[ 'WebhookPullRequestEvent]
        :> GitHubSignedReqBody '[JSON] PullRequestEvent
        :> Post '[JSON] (),
    _ghWebhookPush ::
      route
        :- GitHubEvent '[ 'WebhookPushEvent]
        :> GitHubSignedReqBody '[JSON] PushEvent
        :> Post '[JSON] ()
  }
  deriving (Generic)

ghWebhookAPI :: GhWebhookAPI (AsServerT M)
ghWebhookAPI =
  GhWebhookAPI
    { _ghWebhookCheckSuite = wrap $ \event -> ghWebhookCheckSuite event >>= logPromiseErrors,
      _ghWebhookCheckRun = wrap ghWebhookCheckRun,
      _ghWebhookPullRequest = wrap $ \event -> ghWebhookPullRequest event >>= logPromiseErrors,
      _ghWebhookPush = wrap $ \event -> ghWebhookPush event >>= logPromiseErrors
    }
  where
    wrap :: (Show event) => (event -> M a) -> Servant.GitHub.Webhook.RepoWebhookEvent -> ((), event) -> M a
    wrap handler _repoWebhookEvent ((), event) = do
      uniqueId <- randomBase64 64
      withTextSpan ("event_id", uniqueId) $ do
        withTextSpan ("tag", "github webhook event") $ do
          log Informational $ show event
        handler event

-- | Triggers a normal build and server deployment.
ghWebhookCheckSuite :: (HasCallStack) => CheckSuiteEvent -> M (Promise ())
ghWebhookCheckSuite ev
  | evCheckSuiteAction ev
      == CheckSuiteEventActionRequested
      || evCheckSuiteAction ev
      == CheckSuiteEventActionRerequested = do
      repoId' <- parseRepoFullname (whRepoFullName $ repoForEvent ev)
      (iAuth, tok) <- getAuthAndToken (whChecksInstallationId <$> evCheckSuiteInstallation ev)
      let commitInfo =
            CommitInfo
              { _commitInfoReqUser = ForgeLogin githubForge . GhLogin . whUserLogin $ senderOfEvent ev,
                _commitInfoRepoPublicity = RepoIsPublic . not . whRepoIsPrivate $ repoForEvent ev,
                _commitInfoRepoInfo = RepoInfo (GithubInstallationCredentials iAuth) tok repoId',
                _commitInfoBranch = branch',
                _commitInfoPrFromFork = Nothing,
                _commitInfoCommit = commit'
              }
      clientId <- view #githubAppId
      isGarnixApp <- case checkSuite' ^. app of
        Nothing ->
          throw $ OtherError "Check suite without app. Don't know how to proceed"
        Just app -> do
          pure (app ^. id == untagId clientId)
      if isGarnixApp
        then handleForgeEvent $ CommitPushed (evCheckSuiteAction ev == CheckSuiteEventActionRerequested) commitInfo
        else do
          log Informational "Ignoring check suite event from non-Garnix app"
          emptyPromise
  where
    branch' = Branch <$> whCheckSuiteHeadBranch checkSuite'
    checkSuite' = evCheckSuiteCheckSuite ev
    commit' = CommitHash $ whCheckSuiteHeadSha checkSuite'
ghWebhookCheckSuite _ = emptyPromise

-- | If the event is a request for rerunning an individual build, triggers a build. Otherwise does nothing.
ghWebhookCheckRun :: (HasCallStack) => CheckRunEvent -> M ()
ghWebhookCheckRun ev
  | evCheckRunAction ev == CheckRunEventActionRerequested = forkM $ do
      let ghRunId = GhRunId . fromIntegral . whCheckRunId $ evCheckRunCheckRun ev
          reqUser = GhLogin . whUserLogin $ senderOfEvent ev
      -- The trick to getting GitHub to properly display the run as re-running is
      -- to create a *new* build with the same name as the old one.
      -- See https://github.com/orgs/community/discussions/38288
      (iAuth, token) <- getAuthAndToken (whChecksInstallationId <$> evCheckRunInstallation ev)
      let rerunEvent =
            RerunEvent
              { reqUser,
                ghRunId,
                credentials = GithubInstallationCredentials iAuth,
                token,
                repoIsPublic = RepoIsPublic . not . whRepoIsPrivate $ repoForEvent ev
              }
      void $ handleForgeEvent $ RunRerequested rerunEvent
  | otherwise = pure ()

-- | Triggers two things:
--
--   - Builds for PRs from *forked* repos,
--   - PR-deployments (if the PR is *not* coming from a fork).
ghWebhookPullRequest :: (HasCallStack) => PullRequestEvent -> M (Promise ())
ghWebhookPullRequest ev = do
  case evPullReqAction ev of
    -- We explicitly pattern-match rather than providing a wildcard so that we
    -- get warnings in case there's a change of constructors (in particular,
    -- in case 'synchronize' becomes a constructor)
    PullRequestOpenedAction -> handlePrEvent
    PullRequestActionOther "synchronize" -> handlePrEvent
    PullRequestActionOther _ -> emptyPromise
    PullRequestAssignedAction -> emptyPromise
    PullRequestUnassignedAction -> emptyPromise
    PullRequestReviewRequestedAction -> emptyPromise
    PullRequestReviewRequestRemovedAction -> emptyPromise
    PullRequestLabeledAction -> emptyPromise
    PullRequestUnlabeledAction -> emptyPromise
    PullRequestEditedAction -> emptyPromise
    PullRequestClosedAction -> emptyPromise
    PullRequestReopenedAction -> emptyPromise
  where
    handlePrEvent :: M (Promise ())
    handlePrEvent = do
      let commit' = CommitHash $ ev ^. payload . Types.head . sha
      repoId' <- parseRepoFullname (repoForEvent ev ^. fullName)
      (iAuth, tok) <- getAuthAndToken (evPullReqInstallationId ev)
      (fromRepo, toRepo) <- getFromTo ev
      let prFromFork =
            if whRepoFullName toRepo /= whRepoFullName fromRepo
              then Just $ PrFromFork (whRepoFullName fromRepo)
              else Nothing
      let commitInfo =
            CommitInfo
              { _commitInfoReqUser = ForgeLogin githubForge . GhLogin . whUserLogin $ senderOfEvent ev,
                _commitInfoRepoPublicity = RepoIsPublic . not . whRepoIsPrivate $ repoForEvent ev,
                _commitInfoRepoInfo = RepoInfo (GithubInstallationCredentials iAuth) tok repoId',
                _commitInfoBranch = Nothing,
                _commitInfoPrFromFork = prFromFork,
                _commitInfoCommit = commit'
              }
      handleForgeEvent $ PullRequestUpdated commitInfo (GhPullRequestId $ fromIntegral $ ev ^. number)

    getFromTo :: PullRequestEvent -> M (HookRepository, HookRepository)
    getFromTo ev = do
      from <- case ev ^. (payload . Types.head . repo) of
        Nothing -> do
          -- I don't know why this would happen, so am logging it as an
          -- Error for now.
          throw $ OtherError "Pull request target without repo. Don't know how to proceed"
        Just repo -> pure repo
      pure (from, ev ^. repo)

-- | Triggers a new run iff it's not the first time this commit
-- has been pushed. (If it is the first time, a check suite event has been
-- already created.)
ghWebhookPush :: (HasCallStack) => PushEvent -> M (Promise ())
ghWebhookPush ev
  | evPushDeleted ev = emptyPromise
  | otherwise = do
      repoId' <- parseRepoFullname (whRepoFullName $ repoForEvent ev)
      (iAuth, tok) <- getAuthAndToken (whChecksInstallationId <$> evPushInstallation ev)
      commit' <- case evPushHeadSha ev of
        Nothing -> throw $ OtherError "Push without head sha"
        Just c -> pure $ CommitHash c
      reqUser <- case evPushSender ev of
        Nothing -> throw $ OtherError "Push without a sender"
        Just s -> pure . GhLogin . whUserLogin $ s
      let commitInfo =
            CommitInfo
              { _commitInfoReqUser = ForgeLogin githubForge reqUser,
                _commitInfoRepoPublicity = RepoIsPublic . not . whRepoIsPrivate $ repoForEvent ev,
                _commitInfoRepoInfo = RepoInfo (GithubInstallationCredentials iAuth) tok repoId',
                _commitInfoBranch = branch',
                _commitInfoPrFromFork = Nothing,
                _commitInfoCommit = commit'
              }
      handleForgeEvent $ CommitPushed False commitInfo
  where
    branch' = case T.splitOn "/" (evPushRef ev) of
      "refs" : "heads" : rest -> Just . Branch $ T.intercalate "/" rest
      _ -> Nothing

parseRepoFullname :: Text -> M RepoId
parseRepoFullname name =
  case T.splitOn "/" name of
    [o, r] -> pure (RepoId githubForge (GhRepoOwner (GhLogin o)) (GhRepoName r))
    x ->
      throw
        . OtherError
        $ "Expected full name to split in two. Got: "
        <> show x

getAuthAndToken :: Maybe Int -> M (GH.InstallationAuth, GhToken)
getAuthAndToken =
  \case
    Nothing -> throw $ OtherError "Installation in check suite was Nothing"
    Just i -> do
      iAuth <- getInstallation (Id i)
      tok <- getAccessToken iAuth
      pure (iAuth, tok)
