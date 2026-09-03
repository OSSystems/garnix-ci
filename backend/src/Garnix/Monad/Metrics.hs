module Garnix.Monad.Metrics where

import Data.Generics.Product (HasField')
import Garnix.Prelude
import Network.Wai.Handler.Warp (Port, run)
import System.Metrics.Prometheus.Concurrent.Registry
import System.Metrics.Prometheus.Http.Scrape qualified as Prom
import System.Metrics.Prometheus.Metric.Counter (Counter, add, inc)
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import System.Metrics.Prometheus.Metric.Histogram (Histogram, observe)
import System.Metrics.Prometheus.MetricId (fromList)

-- For consistency, all time-related Doubles should be seconds
data Metrics = Metrics
  { evalQueueLen :: Gauge,
    evalQueueWaitTime :: Histogram,
    -- | Including queue wait time
    evalDrvPathTime :: Histogram,
    -- | Including queue wait time
    getAttrsToBuildTime :: Histogram,
    s3QueueLen :: Gauge,
    s3QueueWaitTime :: Histogram,
    gitCloneTime :: Histogram,
    cachePushTime :: Histogram,
    cachePushSuccess :: Counter,
    cachePushFailure :: Counter,
    packageBuildsAttempted :: Counter,
    dbQueryTime :: Histogram,
    dbQueries :: Counter,
    logsCritical :: Counter,
    logsError :: Counter,
    logsWarning :: Counter,
    s3CacheFallbacksToOldCache :: Counter,
    s3CacheUploads :: Counter,
    s3CacheNarfilesServed :: Counter,
    s3CacheUploadsSkippedDeleting :: Counter,
    fodCheckTime :: Histogram,
    fodCheckBatchSize :: Histogram,
    fodCheckQueueLen :: Gauge,
    fodCheckQueueWaitTime :: Histogram,
    registry :: Registry
  }
  deriving (Generic)

timingAs :: (MonadIO m, MonadReader e m, HasField' "metrics" e Metrics) => Lens' Metrics Histogram -> m x -> m x
timingAs l action = do
  h <- view (#metrics . l)
  start <- liftIO getCurrentTime
  result <- action
  end <- liftIO getCurrentTime
  -- We fork to not hold up the thread (or crash it)
  void
    . liftIO
    $ fork
    $ observe
      (fromRational . toRational $ nominalDiffTimeToSeconds $ diffUTCTime end start)
      h
  pure result

incrementEvent :: (MonadIO m, MonadReader e m, HasField' "metrics" e Metrics) => Lens' Metrics Counter -> m ()
incrementEvent l = do
  h <- view (#metrics . l)
  void . liftIO $ fork $ inc h

addEvent :: (MonadIO m, MonadReader e m, HasField' "metrics" e Metrics) => Lens' Metrics Counter -> Int -> m ()
addEvent l n = do
  h <- view (#metrics . l)
  void . liftIO $ fork $ add n h

registerMetrics :: IO Metrics
registerMetrics = do
  registry <- new
  evalQueueLen <-
    registerGauge
      "garnix_server_eval_queue_len"
      mempty
      registry
  evalQueueWaitTime <-
    registerHistogram
      "garnix_server_eval_queue_wait_time"
      mempty
      [0.01, 0.1, 0.5, 1, 2, 4, 6, 10, 30, 60, 120, 600]
      registry
  evalDrvPathTime <-
    registerHistogram
      "garnix_server_eval_drv_path_time"
      mempty
      [0.2, 0.5, 1, 5, 10, 30, 60, 120, 360, 600, 900]
      registry
  getAttrsToBuildTime <-
    registerHistogram
      "garnix_server_get_attrs_to_build_time"
      mempty
      [0.01, 0.1, 0.5, 1, 2, 4, 6, 10, 30, 60, 120, 600]
      registry
  s3QueueLen <-
    registerGauge
      "garnix_server_s3_queue_len"
      mempty
      registry
  s3QueueWaitTime <-
    registerHistogram
      "garnix_server_s3_queue_wait_time"
      mempty
      [0.2, 0.5, 1, 5, 10, 30, 60, 120, 360, 600, 900, 1500, 3000, 10000]
      registry
  gitCloneTime <-
    registerHistogram
      "garnix_server_git_clone_time"
      mempty
      [0.01, 0.1, 0.5, 1, 2, 4, 6, 10, 30, 60, 120, 300]
      registry
  cachePushTime <-
    registerHistogram
      "garnix_server_cache_push_time"
      mempty
      [0.2, 0.5, 1, 2, 5, 10, 30, 60, 120, 300, 600]
      registry
  cachePushSuccess <-
    registerCounter
      "garnix_server_cache_push_success"
      mempty
      registry
  cachePushFailure <-
    registerCounter
      "garnix_server_cache_push_failure"
      mempty
      registry
  packageBuildsAttempted <-
    registerCounter
      "garnix_server_package_builds_attempted"
      mempty
      registry
  dbQueries <-
    registerCounter
      "garnix_server_db_queries"
      mempty
      registry
  dbQueryTime <-
    registerHistogram
      "garnix_server_db_query_time"
      mempty
      [0.05, 0.1, 0.5, 1, 2, 4, 6, 10, 30, 60, 120, 300]
      registry
  let errorCountName = "garnix_server_log_errors_total"
      severity = "severity"
  logsCritical <-
    registerCounter
      errorCountName
      (fromList [(severity, "critical")])
      registry
  logsError <-
    registerCounter
      errorCountName
      (fromList [(severity, "error")])
      registry
  logsWarning <-
    registerCounter
      errorCountName
      (fromList [(severity, "warning")])
      registry
  s3CacheFallbacksToOldCache <-
    registerCounter
      "garnix_s3_cache_fallback_to_old_cache"
      mempty
      registry
  s3CacheUploads <-
    registerCounter
      "garnix_s3_cache_uploads"
      mempty
      registry
  s3CacheNarfilesServed <-
    registerCounter
      "garnix_s3_cache_narfile_served"
      mempty
      registry
  s3CacheUploadsSkippedDeleting <-
    registerCounter
      "garnix_s3_cache_uploads_skipped_deleting"
      mempty
      registry
  fodCheckTime <-
    registerHistogram
      "garnix_server_fod_check_time"
      mempty
      [0.2, 0.5, 1, 5, 10, 30, 60, 120, 360, 600, 900]
      registry
  fodCheckBatchSize <-
    registerHistogram
      "garnix_server_fod_check_batch_size"
      mempty
      [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024]
      registry
  fodCheckQueueLen <-
    registerGauge
      "garnix_server_fod_check_queue_len"
      mempty
      registry
  fodCheckQueueWaitTime <-
    registerHistogram
      "garnix_server_fod_check_queue_wait_time"
      mempty
      [0.2, 0.5, 1, 5, 10, 30, 60, 120, 360, 600, 900, 1500, 3000, 10000]
      registry
  pure $ Metrics {..}

serveMetrics :: Port -> Metrics -> IO ()
serveMetrics port m = do
  void
    $ fork
    $ run port
    $ Prom.prometheusApp []
    $ sample (registry m)
