-- | Posting a comment on the pull request when a commit's garnix checks fail,
-- and when its servers are deployed or fail to deploy.
--
-- Check runs from a third-party app create no notification thread in the Github
-- inbox, so a failure is invisible unless the user goes looking. Commenting is
-- the only way an app can notify -- and nothing at all tells a user the address
-- their deploy landed on.
module Garnix.Build.PrComment
  ( CommentPolicy (..),
    commentOnFailure,
    commentDeployedUrls,
    commentDeployFailed,
  )
where

import Control.Lens
import Data.Text qualified as T
import Garnix.Build.Reporting (reportNameForBuild)
import Garnix.DB qualified as DB
import Garnix.DB.Hosting qualified as DBHosting
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types as Types

-- | Whether the repo's @garnix.yaml@ asked us to comment on failure.
data CommentPolicy
  = CommentOnFailure
  | NoComment
  deriving stock (Eq, Show)

-- | Comment on every pull request this commit is the head of, listing the
-- checks that failed. Never fails the build: a missing @pull_requests: write@
-- permission shows up as a 403 here and is only logged.
commentOnFailure :: CommitInfo -> M ()
commentOnFailure commitInfo = ignoringAllErrors $ do
  prIds <- getPullRequestsForCommit (commitInfo ^. repoInfo) (commitInfo ^. commit)
  case prIds of
    [] -> log Informational "commentOnFailure: commit is not the head of any pull request - not commenting"
    _ -> do
      -- Claim before commenting, so that a re-run of the same commit - or the
      -- check_suite and pull_request webhooks both firing for it - doesn't
      -- comment twice.
      claimed <-
        DB.claimFailureComment
          (commitInfo ^. repoInfo . ghRepoOwner)
          (commitInfo ^. repoInfo . ghRepoName)
          (commitInfo ^. commit)
      if not claimed
        then log Informational "commentOnFailure: already commented on this commit - not commenting again"
        else do
          body <- mkBody commitInfo
          forM_ prIds $ \prId -> do
            log Informational $ "commentOnFailure: commenting on " <> show prId
            commentOnPullRequest (commitInfo ^. repoInfo) prId body

mkBody :: CommitInfo -> M Text
mkBody commitInfo = do
  fromRelativeUrl <- relativeUrlConverter
  failed <-
    DB.getBuildsAndRunsByCommit
      (commitInfo ^. repoInfo . ghRepoOwner)
      (commitInfo ^. repoInfo . ghRepoName)
      (commitInfo ^. commit)
      <&> \case
        CommitEvaluating -> []
        CommitEvaluated _ builds runs ->
          map (buildLink fromRelativeUrl) (filter hasFailed builds)
            <> map (runLink fromRelativeUrl) (filter runHasFailed runs)
  let commitUrl = fromRelativeUrl $ "/commit/" <> getCommitHash (commitInfo ^. commit)
  pure
    $ T.unlines
    $ [ "## :x: garnix checks failed",
        "",
        "The garnix checks for ["
          <> T.take 7 (getCommitHash (commitInfo ^. commit))
          <> "]("
          <> commitUrl
          <> ") did not pass."
      ]
    <> if null failed
      then ["", "Evaluation failed - see the `All Garnix checks` check run for details."]
      else "" : "Failed checks:" : "" : failed

hasFailed :: Build -> Bool
hasFailed build = maybe False (/= Success) (build ^. status)

runHasFailed :: Run -> Bool
runHasFailed run = maybe False (/= Success) (run ^. status)

buildLink :: (Text -> Text) -> Build -> Text
buildLink fromRelativeUrl build =
  bullet
    (reportNameForBuild build)
    (fromRelativeUrl $ "/build/" <> build ^. id . to getBuildId . re hashIdText)

runLink :: (Text -> Text) -> Run -> Text
runLink fromRelativeUrl run =
  bullet
    (run ^. name)
    (fromRelativeUrl $ "/run/" <> run ^. id . to getRunId . re hashIdText)

bullet :: Text -> Text -> Text
bullet label url = "- [" <> label <> "](" <> url <> ")"

-- * Deploy comments

-- | Tell the pull request where its servers can be reached.
--
-- Once per pull request: the address does not change as the branch moves, so
-- repeating it on every push would be noise.
--
-- Not gated on a @garnix.yaml@ flag, unlike 'commentOnFailure' -- declaring an
-- @on-pull-request@ server is itself the opt-in. A repo that has not granted
-- @pull_requests: write@ produces a 403 that is swallowed here.
commentDeployedUrls ::
  CommitInfo ->
  GhPullRequestId ->
  -- | One (configuration, public hostname) per deployed server.
  [(Text, Text)] ->
  M ()
commentDeployedUrls commitInfo prId deployed = ignoringAllErrors $ case deployed of
  [] -> pure ()
  _ -> do
    claimed <-
      DBHosting.claimDeployUrlComment
        (commitInfo ^. repoInfo . ghRepoOwner)
        (commitInfo ^. repoInfo . ghRepoName)
        prId
    if not claimed
      then log Informational "commentDeployedUrls: already commented on this pull request"
      else do
        log Informational $ "commentDeployedUrls: commenting on " <> show prId
        commentOnPullRequest (commitInfo ^. repoInfo) prId (deployedBody deployed)

-- | Tell the pull request that its deploy did not go through.
--
-- Claimed per commit, not per pull request: a deploy that used to work and
-- broke on a later push is exactly the case worth reporting.
commentDeployFailed :: CommitInfo -> GhPullRequestId -> Text -> M ()
commentDeployFailed commitInfo prId reason = ignoringAllErrors $ do
  claimed <-
    DBHosting.claimDeployFailureComment
      (commitInfo ^. repoInfo . ghRepoOwner)
      (commitInfo ^. repoInfo . ghRepoName)
      prId
      (commitInfo ^. commit)
  if not claimed
    then log Informational "commentDeployFailed: already commented on this commit"
    else do
      log Informational $ "commentDeployFailed: commenting on " <> show prId
      body <- failedBody commitInfo reason
      commentOnPullRequest (commitInfo ^. repoInfo) prId body

deployedBody :: [(Text, Text)] -> Text
deployedBody deployed =
  T.unlines
    $ [ "## :rocket: garnix deployed this pull request",
        ""
      ]
    <> [bullet configuration ("https://" <> host) | (configuration, host) <- deployed]
    <> [ "",
         "These servers are torn down once the pull request stops being reached."
       ]

failedBody :: CommitInfo -> Text -> M Text
failedBody commitInfo reason = do
  fromRelativeUrl <- relativeUrlConverter
  let commitUrl = fromRelativeUrl $ "/commit/" <> getCommitHash (commitInfo ^. commit)
  pure
    $ T.unlines
      [ "## :x: garnix could not deploy this pull request",
        "",
        "The deployment for ["
          <> T.take 7 (getCommitHash (commitInfo ^. commit))
          <> "]("
          <> commitUrl
          <> ") failed:",
        "",
        "```",
        reason,
        "```"
      ]
