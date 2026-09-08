module Garnix.TestHelpers.Reporter where

import Control.Concurrent (modifyMVar_, newMVar, readMVar)
import Data.Map.Strict
import Garnix.Monad hiding (log)
import Garnix.Prelude hiding (insert)

data TestReport = TestReport
  { logs :: Text,
    success :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

type TestReporterResult = Map Text TestReport

withTestReporter :: (Reporter -> M a) -> M (TestReporterResult, a)
withTestReporter action = do
  logsMVar <- liftIO $ newMVar mempty
  let mkReporter :: ReportType -> M RunReporter
      mkReporter reportType = do
        let name = reportName reportType
        let addToReport :: Maybe Bool -> Maybe Text -> M ()
            addToReport success toAppend =
              liftIO
                $ modifyMVar_ logsMVar
                $ pure
                . alter
                  ( Just . \case
                      Just (TestReport logs _success) ->
                        TestReport (maybe logs ((logs <> "\n") <>) toAppend) success
                      Nothing -> TestReport (fromMaybe "" toAppend) success
                  )
                  name
        pure
          $ RunReporter
            { reportLogs = \logs -> do
                addToReport Nothing $ Just (logs ^. #log),
              reportComplete = \status -> do
                let success = case status of
                      RunReportStatusInProgress -> error "reportComplete should never be called with RunReportStatusInProgress"
                      RunReportStatusSuccess -> True
                      RunReportStatusFailure -> False
                      RunReportStatusTimeout -> False
                      RunReportStatusCancelled -> False
                addToReport (Just success) Nothing,
              ghRunId = Nothing
            }
  result <-
    action
      $ Reporter
        { createNewRun = mkReporter,
          resumeRun = const mkReporter
        }
  logs <- liftIO $ readMVar logsMVar
  pure (logs, result)

withTestReporter_ :: (Reporter -> M ()) -> M TestReporterResult
withTestReporter_ = fmap fst . withTestReporter
