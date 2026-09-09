module Garnix.Monad.Pool (Pool, newPool, poolSizeFromEnv, parsePoolSize, withPoolM, withPool) where

import Control.Concurrent.Lifted (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Data.Generics.Product (HasField')
import Data.Sequence (Seq ((:<|)), (|>))
import Data.Sequence qualified as Seq
import Garnix.Monad.Metrics
import Garnix.Prelude
import System.Metrics.Prometheus.Metric.Gauge (Gauge, set)
import System.Metrics.Prometheus.Metric.Histogram (Histogram)
import Text.Read (readMaybe)

newtype FairQSem a
  = FairQSem (MVar (Queue a))

data Queue a
  = SlotsAvailable
      { count :: Int
      }
  | BackedUp
      { scheduled :: Seq (PerKeyQueue a)
      }

data PerKeyQueue a = PerKeyQueue
  { key :: a,
    queue :: Seq (MVar ())
  }

newQSem :: Gauge -> Int -> IO (FairQSem a)
newQSem gauge initial = do
  let q = SlotsAvailable initial
  updateGauge gauge q
  sem <- newMVar q
  return (FairQSem sem)

modifyAndUpdateGauge :: Gauge -> FairQSem a -> (Queue a -> IO (Queue a, output)) -> IO output
modifyAndUpdateGauge gauge (FairQSem q) action = do
  modifyMVar q $ \queue -> do
    (newQueue, output) <- action queue
    updateGauge gauge newQueue
    pure (newQueue, output)

updateGauge :: Gauge -> Queue a -> IO ()
updateGauge g q = do
  flip set g $ realToFrac $ case q of
    SlotsAvailable n -> -n
    BackedUp seq -> foldl' (+) 0 $ fmap (length . queue) seq

-- | Like the traditional 'waitQSem', but takes an extra argument, the key for the
-- fairness property. After a threads with key k is woken up, another one with
-- the same key will only be woken up after all waiting keys have had one thread
-- served.
--
-- In this way, FairQSem is round-robin on the waiting keys.
waitQSem :: (Show a, Eq a) => Gauge -> a -> FairQSem a -> IO ()
waitQSem gauge v q = do
  res <- schedule
  case res of
    Nothing -> pure ()
    Just me -> takeMVar me
  where
    schedule :: IO (Maybe (MVar ()))
    schedule = modifyAndUpdateGauge gauge q $ \queue -> do
      case queue of
        BackedUp {scheduled} -> do
          me <- newEmptyMVar
          pure (BackedUp {scheduled = signMeUp me scheduled}, Just me)
        SlotsAvailable {count = 0} -> do
          me <- newEmptyMVar
          pure (BackedUp {scheduled = signMeUp me Seq.Empty}, Just me)
        SlotsAvailable {count} -> do
          pure (SlotsAvailable (count - 1), Nothing)
    signMeUp me s = case Seq.spanl (\PerKeyQueue {key} -> v /= key) s of
      (aheadOfMe, PerKeyQueue {queue} :<| behindMe) ->
        aheadOfMe <> (PerKeyQueue v (queue |> me) :<| behindMe)
      (aheadOfMe, Seq.Empty) -> aheadOfMe |> PerKeyQueue v (Seq.singleton me)

signalQSem :: (Show a) => Gauge -> FairQSem a -> IO ()
signalQSem gauge q = do
  modifyAndUpdateGauge gauge q $ \queue -> do
    (,()) <$> case queue of
      SlotsAvailable {count} -> pure $ SlotsAvailable {count = count + 1}
      BackedUp {scheduled} -> do
        case scheduled of
          Seq.Empty -> pure $ SlotsAvailable {count = 1}
          -- We 'do' the first thing in the queue, *and then* shift all of
          -- the items in the key with the same key as that thing to the back.
          (PerKeyQueue {key, queue = upNext :<| rest}) :<| others -> do
            putMVar upNext ()
            let remaining =
                  if Seq.null rest
                    then others
                    else others |> PerKeyQueue key rest
            pure $ BackedUp {scheduled = remaining}
          (PerKeyQueue {queue = Seq.Empty}) :<| _ -> error "impossible"

data Pool a = Pool
  { _qsem :: FairQSem a,
    _timerMetric :: Lens' Metrics Histogram,
    _lenMetric :: Lens' Metrics Gauge
  }

newPool :: (MonadIO m) => Int -> Metrics -> Lens' Metrics Histogram -> Lens' Metrics Gauge -> m (Pool a)
newPool limit metrics timerMetric lenMetric = do
  qsem <- liftIO $ newQSem (metrics ^. lenMetric) limit
  pure $ Pool qsem timerMetric lenMetric

withPoolM :: (MonadMask m, MonadReader s m, HasField' "metrics" s Metrics, Eq a, Show a, MonadIO m) => (s -> Pool a) -> a -> m b -> m b
withPoolM poolLens key action = do
  pool <- asks poolLens
  withPool pool key action

withPool :: (MonadMask m, MonadReader s m, HasField' "metrics" s Metrics, Eq a, Show a, MonadIO m) => Pool a -> a -> m b -> m b
withPool (Pool qsem timerMetricLens lenMetricLens) key action = do
  bracket_ acquire release action
  where
    acquire = do
      lenMetric <- view (#metrics . lenMetricLens)
      timingAs timerMetricLens $ liftIO $ waitQSem lenMetric key qsem
    release = do
      lenMetric <- view (#metrics . lenMetricLens)
      liftIO $ signalQSem lenMetric qsem

-- | Pool size from an environment variable, falling back to @def@.
poolSizeFromEnv :: (MonadIO m) => String -> Int -> m Int
poolSizeFromEnv name def = parsePoolSize def <$> liftIO (lookupEnv name)

-- | Anything that is not a positive integer falls back to @def@. A pool of zero
-- would be a semaphore no acquire can ever pass, so an operator typo would hang
-- every build with nothing in the log to say why.
parsePoolSize :: Int -> Maybe String -> Int
parsePoolSize def raw = fromMaybe def $ do
  s <- raw
  n <- readMaybe s
  guard (n > 0)
  pure n
