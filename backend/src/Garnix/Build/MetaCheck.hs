module Garnix.Build.MetaCheck
  ( CommentPolicy (..),
    MetaCheckRun,
    update,
    newReport,
    resumeReport,
    settle,
    failAbandoned,
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
  DB.getBuildsAndRunsByCommit (commitInfo ^. repoInfo . repoId) (commitInfo ^. commit)
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
          (commitInfo ^. repoInfo . repoId)
          (commitInfo ^. commit)
          ( DB.CheckStatusUpdate
              { _checkStatusUpdateGeneration = Nothing,
                _checkStatusUpdateFrom = commit' ^. metaCheck,
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

data MetaCheckRun = MetaCheckRun DB.MetaCheckGeneration RunReporter

newReport :: Reporter -> CommitInfo -> M MetaCheckRun
newReport reporter commitInfo = do
  generation <- DB.newCommit (commitInfo ^. repoInfo . repoId) (commitInfo ^. commit)
  runReporter <- createNewRun reporter MetaCheck
  forM_ (Garnix.Monad.ghRunId runReporter) $ DB.recordMetaCheckRun (commitInfo ^. repoInfo . repoId) (commitInfo ^. commit) generation
  pure $ MetaCheckRun generation runReporter

resumeReport :: Reporter -> DB.MetaCheckGeneration -> GhRunId -> M MetaCheckRun
resumeReport reporter generation ghRunId' =
  MetaCheckRun generation <$> resumeRun reporter ghRunId' MetaCheck

updateFail :: CommentPolicy -> CommitInfo -> MetaCheckRun -> Maybe (Either SomeException ErrorWithContext) -> M ()
updateFail commentPolicy commitInfo metaCheckRun e = do
  case e of
    Just err ->
      log Warning $ "failMetaCheck: uncaught " <> either (const "IO exception: ") (const "monadic error: ") err <> either show show err
    Nothing -> pure ()
  updated <- setOwnVerdict commitInfo metaCheckRun CheckFail
  if updated
    then do
      complete commitInfo metaCheckRun RunReportStatusFailure
      case commentPolicy of
        CommentOnFailure -> PrComment.commentOnFailure commitInfo
        NoComment -> pure ()
    else settle commitInfo metaCheckRun

updateSuccess :: CommitInfo -> MetaCheckRun -> M ()
updateSuccess commitInfo metaCheckRun = do
  updated <- setOwnVerdict commitInfo metaCheckRun CheckSuccess
  if updated
    then complete commitInfo metaCheckRun RunReportStatusSuccess
    else settle commitInfo metaCheckRun

settle :: CommitInfo -> MetaCheckRun -> M ()
settle commitInfo metaCheckRun =
  DB.getCommit (commitInfo ^. repoInfo . repoId) (commitInfo ^. commit) >>= \case
    Nothing -> pure ()
    Just commit' ->
      complete commitInfo metaCheckRun $ case commit' ^. metaCheck of
        CheckSuccess -> RunReportStatusSuccess
        CheckFail -> RunReportStatusFailure
        CheckPending -> RunReportStatusCancelled

failAbandoned :: Reporter -> CommitInfo -> M ()
failAbandoned reporter commitInfo = do
  updated <-
    DB.setMetaCheck
      (commitInfo ^. repoInfo . repoId)
      (commitInfo ^. commit)
      ( DB.CheckStatusUpdate
          { _checkStatusUpdateGeneration = Nothing,
            _checkStatusUpdateFrom = CheckPending,
            _checkStatusUpdateTo = CheckFail
          }
      )
  when updated $ do
    runReporter <- createNewRun reporter MetaCheck
    reportComplete runReporter RunReportStatusFailure

setOwnVerdict :: CommitInfo -> MetaCheckRun -> CheckStatus -> M Bool
setOwnVerdict commitInfo (MetaCheckRun generation _) verdict =
  DB.setMetaCheck
    (commitInfo ^. repoInfo . repoId)
    (commitInfo ^. commit)
    ( DB.CheckStatusUpdate
        { _checkStatusUpdateGeneration = Just generation,
          _checkStatusUpdateFrom = CheckPending,
          _checkStatusUpdateTo = verdict
        }
    )

complete :: CommitInfo -> MetaCheckRun -> RunReportStatus -> M ()
complete commitInfo (MetaCheckRun generation runReporter) status = do
  reportComplete runReporter status
  DB.completeMetaCheckRun (commitInfo ^. repoInfo . repoId) (commitInfo ^. commit) generation
