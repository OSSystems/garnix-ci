module Garnix.Monad.KeyedMutex
  ( KeyedMutex,
    newKeyedMutex,
    withKeyedMutex,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq ((:<|)), (|>))
import Data.Sequence qualified as Seq
import Garnix.Prelude

-- | A per-key mutual-exclusion lock: at most one 'withKeyedMutex' body runs at
-- a time for a given key, oldest-arrival-first; DIFFERENT keys never block
-- each other. This is a different guarantee than 'Garnix.Monad.Pool', which
-- caps GLOBAL concurrency with round-robin fairness across keys but gives no
-- key mutual exclusion — two callers with the same key can both hold a Pool
-- slot at once whenever slots are free (its "key" only governs who gets
-- served next out of a shared backlog). 'KeyedMutex' has no slot count at
-- all: it is "concurrency exactly 1, PER key", with unlimited keys running
-- concurrently.
--
-- Used to serialize deploy/redeployment execution per (owner, repo); see
-- 'Garnix.Hosting.Deploy.rolloutNewServerVersion'.
--
-- == FIFO guarantee
--
-- Acquisition never depends on GHC's fairness of contended 'takeMVar' wakeups
-- on a /shared/ MVar — that would mean trusting an implementation detail this
-- module would rather not lean on. Instead:
--
--   * Each waiter mints its OWN fresh, never-shared 'MVar' and, while holding
--     the single bookkeeping 'MVar' via 'modifyMVar' (so this step is
--     serialized against every other acquire\/release), appends it to an
--     explicit 'Seq' queue for its key. The append order under that single
--     lock IS the true arrival order — no scheduler fairness required.
--   * The thread that releases the lock (also under the bookkeeping MVar)
--     pops the head of that key's Seq and does exactly one 'putMVar' to that
--     specific waiter's private MVar. That MVar is never touched by anyone
--     else, so there is no "which blocked thread wakes up" question to beg:
--     the wakeup unconditionally targets the longest-waiting thread for that
--     key.
--
-- (This mirrors the per-key fairness technique 'Garnix.Monad.Pool' already
-- uses for its own waiter queues — an explicit 'Seq' of per-waiter MVars,
-- rather than contending threads on one shared MVar.)
--
-- A key with no current holder and no waiters is dropped from the map on
-- release, so the map stays bounded by the number of currently-locked-or-
-- waiting keys, not the number of distinct keys ever seen.
newtype KeyedMutex k = KeyedMutex (MVar (Map k (Seq (MVar ()))))

newKeyedMutex :: (MonadIO m) => m (KeyedMutex k)
newKeyedMutex = liftIO $ KeyedMutex <$> newMVar Map.empty

-- | Run @action@ holding the exclusive lock for @key@, waiting in FIFO order
-- behind any thread that already holds (or is queued for) that same key.
-- Exception-safe: the lock is always released — and the next waiter, if any,
-- woken — even if @action@ throws or the calling thread is killed.
withKeyedMutex :: (MonadMask m, MonadIO m, Ord k) => KeyedMutex k -> k -> m a -> m a
withKeyedMutex (KeyedMutex lockVar) key action = bracket_ (liftIO acquire) (liftIO release) action
  where
    -- A key present in the map at all (even mapped to an empty Seq) means
    -- "currently held"; absent means "free". A free (or never-seen) key is
    -- grabbed immediately, with nothing to wait on.
    acquire :: IO ()
    acquire = do
      mMe <- modifyMVar lockVar $ \locks -> case Map.lookup key locks of
        Nothing -> pure (Map.insert key Seq.empty locks, Nothing)
        Just waiters -> do
          me <- newEmptyMVar
          pure (Map.insert key (waiters |> me) locks, Just me)
      forM_ mMe takeMVar

    release :: IO ()
    release = modifyMVar_ lockVar $ \locks -> case Map.lookup key locks of
      Just (next :<| rest) -> do
        putMVar next ()
        pure (Map.insert key rest locks)
      Just Seq.Empty -> pure (Map.delete key locks)
      -- Releasing a key nobody holds can't happen through this module's API
      -- (every 'release' is paired with a prior 'acquire' via 'bracket_');
      -- no-op defensively rather than crash a deploy on a bug here.
      Nothing -> pure locks
