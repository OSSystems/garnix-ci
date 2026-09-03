module Garnix.Build.MetaCheck
  ( CommentPolicy (..),
    update,
    newReport,
    updateFail,
    updateSuccess,
  )
where

import Garnix.Build.PrComment (CommentPolicy (..))
import Garnix.Build.PrComment qualified as PrComment
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types as Types

update :: Reporter -> CommitInfo -> M ()
update reporter commitInfo = do
  DB.getBuildsAndRunsByCommit (commitInfo ^. repoInfo . ghRepoOwner) (commitInfo ^. repoInfo . ghRepoName) (commitInfo ^. commit)
    >>= \case
      CommitEvaluating -> pure ()
      CommitEvaluated commitState builds _ ->
        setCheckTo
          commitState
          (foldl' mergeMetaCheckStatus CheckSuccess (map (^. status . to mapToMetaCheckStatus) builds))
  where
    mapToMetaCheckStatus :: Maybe Status -> CheckStatus
    mapToMetaCheckStatus = \case
      Nothing -> CheckPending
      Just Success -> CheckSuccess
      Just Failure -> CheckFail
      Just Timeout -> CheckFail
      Just Cancelled -> CheckFail

    mergeMetaCheckStatus :: CheckStatus -> CheckStatus -> CheckStatus
    mergeMetaCheckStatus a b = case (a, b) of
      (CheckFail, _) -> CheckFail
      (_, CheckFail) -> CheckFail
      (CheckPending, _) -> CheckPending
      (_, CheckPending) -> CheckPending
      (CheckSuccess, CheckSuccess) -> CheckSuccess

    setCheckTo :: Commit -> CheckStatus -> M ()
    setCheckTo commit' newStatus = do
      updatedCheck <-
        DB.setMetaCheck
          (commitInfo ^. repoInfo . ghRepoOwner)
          (commitInfo ^. repoInfo . ghRepoName)
          (commitInfo ^. commit)
          ( DB.CheckStatusUpdate
              { _checkStatusUpdateFrom = commit' ^. metaCheck,
                _checkStatusUpdateTo = newStatus
              }
          )
      when updatedCheck $ do
        let st = case newStatus of
              CheckPending -> Nothing
              CheckFail -> Just RunReportStatusFailure
              CheckSuccess -> Just RunReportStatusSuccess
        case st of
          Just status -> do
            runReporter <- createNewRun reporter MetaCheck
            reportComplete runReporter status
          Nothing -> pure ()

newReport :: Reporter -> CommitInfo -> M RunReporter
newReport reporter commitInfo = do
  void $ DB.newCommit (commitInfo ^. repoInfo . ghRepoOwner) (commitInfo ^. repoInfo . ghRepoName) (commitInfo ^. commit)
  createNewRun reporter MetaCheck

updateFail :: CommentPolicy -> CommitInfo -> RunReporter -> Maybe (Either SomeException ErrorWithContext) -> M ()
updateFail commentPolicy commitInfo runReporter e = do
  case e of
    Just err ->
      log Warning $ "failMetaCheck: uncaught " <> either (const "IO exception: ") (const "monadic error: ") err <> either show show err
    Nothing -> pure ()
  updated <-
    DB.setMetaCheck
      (commitInfo ^. repoInfo . ghRepoOwner)
      (commitInfo ^. repoInfo . ghRepoName)
      (commitInfo ^. commit)
      ( DB.CheckStatusUpdate
          { _checkStatusUpdateFrom = CheckPending,
            _checkStatusUpdateTo = CheckFail
          }
      )
  when updated $ do
    reportComplete runReporter RunReportStatusFailure
    case commentPolicy of
      CommentOnFailure -> PrComment.commentOnFailure commitInfo
      NoComment -> pure ()

updateSuccess :: CommitInfo -> RunReporter -> M ()
updateSuccess commitInfo runReporter = do
  updated <-
    DB.setMetaCheck
      (commitInfo ^. repoInfo . ghRepoOwner)
      (commitInfo ^. repoInfo . ghRepoName)
      (commitInfo ^. commit)
      ( DB.CheckStatusUpdate
          { _checkStatusUpdateFrom = CheckPending,
            _checkStatusUpdateTo = CheckSuccess
          }
      )
  when updated $ reportComplete runReporter RunReportStatusSuccess
