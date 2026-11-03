module Garnix.Monad.Pool (Pool, newPool, poolSizeFromEnv, parsePoolSize, withPoolM, withPool, withPoolSlotsM, withPoolSlots) where

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

data Queue a = Queue
  { available :: Int,
    scheduled :: Seq (PerKeyQueue a)
  }

data PerKeyQueue a = PerKeyQueue
  { key :: a,
    -- | Slots credited to this key towards its next waiter.
    deficit :: Int,
    queue :: Seq Waiter
  }

data Waiter = Waiter
  { slots :: Int,
    wakeUp :: MVar ()
  }

newQSem :: Gauge -> Int -> IO (FairQSem a)
newQSem gauge initial = do
  let q = Queue {available = initial, scheduled = Seq.Empty}
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
updateGauge g Queue {available, scheduled} = do
  flip set g $ realToFrac $ case scheduled of
    Seq.Empty -> -available
    _ -> foldl' (+) 0 $ fmap (length . queue) scheduled

-- | Like the traditional 'waitQSem', but takes the number of slots to take,
-- and an extra argument, the key for the fairness property. Waiting keys are
-- served round-robin by slots, not by waiters (deficit round robin): each
-- turn credits a key one slot, and its next waiter is woken up once the
-- key's credit covers the slots it asked for. A waiter whose credit suffices
-- but whose slots are not free yet holds up everyone behind it, so the
-- slots freed meanwhile are kept for it and it does not starve.
--
-- With one slot per waiter, this is plain round-robin on the waiting keys:
-- after a thread with key k is woken up, another one with the same key will
-- only be woken up after all waiting keys have had one thread served.
waitQSem :: (Show a, Eq a) => Gauge -> a -> Int -> FairQSem a -> IO ()
waitQSem gauge v slots q = do
  me <- newEmptyMVar
  modifyAndUpdateGauge gauge q $ fmap (,()) . dispatch . signMeUp (Waiter slots me)
  -- Cancelled while waiting, the waiter must leave the queue, or give back
  -- the slots it was handed meanwhile: otherwise those are lost for good.
  takeMVar me `onException` uninterruptibleMask_ (giveUp me)
  where
    signMeUp waiter queue@Queue {scheduled} =
      queue
        { scheduled = case Seq.spanl (\PerKeyQueue {key} -> v /= key) scheduled of
            (aheadOfMe, perKey@PerKeyQueue {queue = waiters} :<| behindMe) ->
              aheadOfMe <> (perKey {queue = waiters |> waiter} :<| behindMe)
            (aheadOfMe, Seq.Empty) -> aheadOfMe |> PerKeyQueue v 0 (Seq.singleton waiter)
        }
    giveUp me =
      modifyAndUpdateGauge gauge q $ \queue@Queue {available, scheduled} ->
        fmap (,())
          $ dispatch
          $ if isWaiting me scheduled
            then queue {scheduled = withoutWaiter me scheduled}
            else queue {available = available + slots}

isWaiting :: MVar () -> Seq (PerKeyQueue a) -> Bool
isWaiting me = any (\PerKeyQueue {queue} -> any ((== me) . wakeUp) queue)

withoutWaiter :: MVar () -> Seq (PerKeyQueue a) -> Seq (PerKeyQueue a)
withoutWaiter me =
  Seq.filter (\PerKeyQueue {queue} -> not (Seq.null queue))
    . fmap (\perKey@PerKeyQueue {queue} -> perKey {queue = Seq.filter ((/= me) . wakeUp) queue})

signalQSem :: Gauge -> Int -> FairQSem a -> IO ()
signalQSem gauge slots q =
  modifyAndUpdateGauge gauge q $ \queue@Queue {available} ->
    (,()) <$> dispatch queue {available = available + slots}

-- | Wakes up waiters while there are slots for them; see 'waitQSem'.
dispatch :: Queue a -> IO (Queue a)
dispatch queue@Queue {available, scheduled} = case scheduled of
  Seq.Empty -> pure queue
  perKey@PerKeyQueue {deficit, queue = waiter :<| rest} :<| others
    | deficit < slots waiter ->
        dispatch queue {scheduled = others |> perKey {deficit = deficit + 1}}
    | available < slots waiter -> pure queue
    | otherwise -> do
        putMVar (wakeUp waiter) ()
        dispatch
          queue
            { available = available - slots waiter,
              -- A key leaves with its queue, and with its credit.
              scheduled =
                if Seq.null rest
                  then others
                  else others |> perKey {deficit = deficit - slots waiter, queue = rest}
            }
  PerKeyQueue {queue = Seq.Empty} :<| _ -> error "impossible"

data Pool a = Pool
  { _qsem :: FairQSem a,
    _capacity :: Int,
    _timerMetric :: Lens' Metrics Histogram,
    _lenMetric :: Lens' Metrics Gauge
  }

newPool :: (MonadIO m) => Int -> Metrics -> Lens' Metrics Histogram -> Lens' Metrics Gauge -> m (Pool a)
newPool limit metrics timerMetric lenMetric = do
  qsem <- liftIO $ newQSem (metrics ^. lenMetric) limit
  pure $ Pool qsem limit timerMetric lenMetric

withPoolM :: (MonadMask m, MonadReader s m, HasField' "metrics" s Metrics, Eq a, Show a, MonadIO m) => (s -> Pool a) -> a -> m b -> m b
withPoolM poolLens = withPoolSlotsM poolLens 1

withPool :: (MonadMask m, MonadReader s m, HasField' "metrics" s Metrics, Eq a, Show a, MonadIO m) => Pool a -> a -> m b -> m b
withPool pool = withPoolSlots pool 1

-- | Like 'withPoolM', for an action that takes several slots at once, e.g. one
-- process running several evaluators.
withPoolSlotsM :: (MonadMask m, MonadReader s m, HasField' "metrics" s Metrics, Eq a, Show a, MonadIO m) => (s -> Pool a) -> Int -> a -> m b -> m b
withPoolSlotsM poolLens slots key action = do
  pool <- asks poolLens
  withPoolSlots pool slots key action

-- | Takes between one slot and all of them: more could never be free at once.
withPoolSlots :: (MonadMask m, MonadReader s m, HasField' "metrics" s Metrics, Eq a, Show a, MonadIO m) => Pool a -> Int -> a -> m b -> m b
withPoolSlots (Pool qsem capacity timerMetricLens lenMetricLens) requested key action = do
  bracket_ acquire release action
  where
    slots = max 1 (min capacity requested)
    acquire = do
      lenMetric <- view (#metrics . lenMetricLens)
      timingAs timerMetricLens $ liftIO $ waitQSem lenMetric key slots qsem
    release = do
      lenMetric <- view (#metrics . lenMetricLens)
      liftIO $ signalQSem lenMetric slots qsem

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
