{-# OPTIONS_GHC -Wno-type-defaults #-}

module Garnix.Monad.PoolSpec where

import Control.Concurrent (MVar, modifyMVar_, newChan, newEmptyMVar, newMVar, putMVar, readChan, readMVar, takeMVar, writeChan)
import Control.Concurrent.Async.Lifted (async)
import Garnix.Duration
import Garnix.Monad
import Garnix.Monad.Async (joinAll_, resolve, spawn)
import Garnix.Monad.Pool
import Garnix.Prelude
import Garnix.TestHelpers (runTestM, waitFor)
import Garnix.TestHelpers.Monad
import System.Metrics.Prometheus.Concurrent.Registry (sample)
import System.Metrics.Prometheus.Metric (MetricSample (GaugeMetricSample, HistogramMetricSample))
import System.Metrics.Prometheus.Metric.Gauge (GaugeSample (..))
import System.Metrics.Prometheus.Metric.Histogram qualified as Hist
import System.Metrics.Prometheus.MetricId (MetricId (..))
import System.Metrics.Prometheus.Registry (unRegistrySample)
import Test.Hspec

spec :: Spec
spec = do
  describe "parsePoolSize" $ do
    it "uses the default when the variable is unset"
      $ parsePoolSize 50 Nothing
      `shouldBe` 50

    it "reads a positive integer"
      $ parsePoolSize 50 (Just "8")
      `shouldBe` 8

    -- newQSem 0 is a semaphore no acquire can ever pass, so honouring a zero
    -- here would hang every build with nothing in the log to explain it.
    it "falls back to the default rather than building a pool nothing can enter" $ do
      parsePoolSize 50 (Just "0") `shouldBe` 50
      parsePoolSize 50 (Just "-1") `shouldBe` 50

    it "falls back to the default on anything unparseable" $ do
      parsePoolSize 50 (Just "") `shouldBe` 50
      parsePoolSize 50 (Just "eight") `shouldBe` 50
      parsePoolSize 50 (Just "8x") `shouldBe` 50

  describe "newPool" $ do
    it "runs given actions" $ runTestM $ do
      metrics <- view #metrics
      pool <- newPool 4 metrics #evalQueueWaitTime #evalQueueLen
      result <- withPool pool () $ pure 42
      liftIO $ result `shouldBe` 42

    it "restricts the number of concurrent commands to the given number" $ runTestM $ do
      concurrencyGauge <- newConcurrencyGauge
      let concurrencyLimit = 4
      metrics <- view #metrics
      pool <- newPool concurrencyLimit metrics #evalQueueWaitTime #evalQueueLen
      promises <- forM [0 .. 9] $ \_ -> spawn
        $ withPool pool ()
        $ withConcurrencyGauge concurrencyGauge
        $ do
          liftIO $ threadDelay $ fromMilliSeconds 20
      resolve =<< joinAll_ promises
      maxMeasuredConcurrency <- readMaxConcurrency concurrencyGauge
      liftIO $ maxMeasuredConcurrency `shouldBe` concurrencyLimit

    it "implements round robin based on given keys" $ runTestM $ do
      started <- liftIO newChan
      metrics <- view #metrics
      pool <- newPool 1 metrics #evalQueueWaitTime #evalQueueLen
      let enqueueAction key = do
            oneshot <- liftIO newEmptyMVar
            _ <- async $ withPool pool key $ liftIO $ do
              writeChan started key
              readMVar oneshot
            threadDelay $ fromMilliSeconds 80
            return $ liftIO $ putMVar oneshot ()

      terminateA1 <- enqueueAction 'a'
      terminateA2 <- enqueueAction 'a'
      terminateA3 <- enqueueAction 'a'
      terminateB1 <- enqueueAction 'b'
      liftIO $ readChan started `shouldReturn` 'a'
      terminateA1
      liftIO $ readChan started `shouldReturn` 'a'
      terminateA2
      liftIO $ readChan started `shouldReturn` 'b'
      terminateB1
      liftIO $ readChan started `shouldReturn` 'a'
      terminateA3

    it "adds wait time to the metrics" $ runTestM $ do
      metrics <- view #metrics
      pool <- newPool 1 metrics #evalQueueWaitTime #evalQueueLen
      promises <-
        replicateM 2
          $ spawn
          $ withPool pool ()
          $ threadDelay
          $ fromMilliSeconds 100
      resolve =<< joinAll_ promises
      r <- view $ #metrics . #registry
      s <- liftIO $ sample r
      case unRegistrySample s ^. at (MetricId "garnix_server_eval_queue_wait_time" mempty) of
        Just (HistogramMetricSample h) -> liftIO $ do
          Hist.histSum h `shouldSatisfy` (< 100)
          Hist.histCount h `shouldBe` 2
        _ -> error "Expectected histogram metric"

    it "adds queue length to the metrics" $ runTestM $ do
      let getCurrentGaugeCount :: M Double
          getCurrentGaugeCount = do
            r <- view $ #metrics . #registry
            s <- liftIO $ sample r
            case unRegistrySample s ^. at (MetricId "garnix_server_eval_queue_len" mempty) of
              Just (GaugeMetricSample (GaugeSample g)) -> pure g
              _ -> error "Expectected gauge metric"
      metrics <- view #metrics
      pool <- newPool 3 metrics #evalQueueWaitTime #evalQueueLen
      waitFor (fromSeconds 10) $ getCurrentGaugeCount `shouldReturnM` (-3)
      mvar <- liftIO newEmptyMVar
      replicateM_ 5
        $ spawn
        $ withPool pool ()
        $ liftIO
        $ takeMVar mvar
      waitFor (fromSeconds 10) $ getCurrentGaugeCount `shouldReturnM` 2
      liftIO $ putMVar mvar ()
      waitFor (fromSeconds 10) $ getCurrentGaugeCount `shouldReturnM` 1
      liftIO $ putMVar mvar ()
      waitFor (fromSeconds 10) $ getCurrentGaugeCount `shouldReturnM` 0
      liftIO $ putMVar mvar ()
      waitFor (fromSeconds 10) $ getCurrentGaugeCount `shouldReturnM` (-1)
      liftIO $ putMVar mvar ()
      waitFor (fromSeconds 10) $ getCurrentGaugeCount `shouldReturnM` (-2)
      liftIO $ putMVar mvar ()
      waitFor (fromSeconds 10) $ getCurrentGaugeCount `shouldReturnM` (-3)

type ConcurrencyGauge = MVar (Int, Int)

newConcurrencyGauge :: M ConcurrencyGauge
newConcurrencyGauge = liftIO $ newMVar (0, 0)

withConcurrencyGauge :: ConcurrencyGauge -> M a -> M a
withConcurrencyGauge mvar action = do
  liftIO
    $ modifyMVar_ mvar
    $ \((+ 1) -> current, maximum) -> pure (current, max current maximum)
  r <- action
  liftIO
    $ modifyMVar_ mvar
    $ \(subtract 1 -> current, maximum) -> pure (current, max current maximum)
  pure r

readMaxConcurrency :: ConcurrencyGauge -> M Int
readMaxConcurrency mvar = snd <$> liftIO (readMVar mvar)
