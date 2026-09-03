-- | Posting a comment on the pull request when a commit's garnix checks fail.
--
-- Check runs from a third-party app create no notification thread in the Github
-- inbox, so a failure is invisible unless the user goes looking. Commenting is
-- the only way an app can notify.
module Garnix.Build.PrComment
  ( CommentPolicy (..),
    commentOnFailure,
  )
where

import Control.Lens
import Data.Text qualified as T
import Garnix.Build.Reporting (reportNameForBuild)
import Garnix.DB qualified as DB
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
