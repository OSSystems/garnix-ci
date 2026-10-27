{-# LANGUAGE UndecidableInstances #-}

module Garnix.Monad.ForkT
  ( ForkT,
    HasForkT,
    runForkT,
    safeFork,
    safeAcquire,
    safeSystemTempDirectory,
    safeSystemTempFile,
    removeLeftoverTemporaries,
  )
where

import Control.Concurrent.Lifted (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Exception.Safe qualified as Safe
import Control.Monad.Base
import Control.Monad.Trans.Control
import Garnix.Prelude
import System.Directory (listDirectory, removeDirectoryRecursive, removeFile, removePathForcibly)
import System.IO (hClose, openTempFile)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

data ForkState = ForkState
  { threads :: [MVar ()],
    releasers :: [IO ()]
  }
  deriving stock (Generic)

instance Default ForkState where
  def = ForkState [] []

newtype ForkT m a = ForkT (ReaderT (MVar ForkState) m a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadTrans,
      MonadTransControl,
      MonadBaseControl base,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

runForkT :: (MonadBase IO m, MonadMask m) => ForkT m a -> m a
runForkT (ForkT inner) = do
  mvar <- newMVar def
  runReaderT inner mvar `finally` releaseAll mvar
  where
    releaseAll mvar = do
      ForkState {threads, releasers} <- readMVar mvar
      forM_ threads $ \thread -> do
        takeMVar thread
      results :: [Either SomeException ()] <- forM releasers $ \releaser -> do
        liftBase $ Safe.try releaser
      forM_ results $ \result -> do
        case result of
          Left exception -> liftBase $ Safe.throwIO exception
          Right () -> pure ()

class (MonadBaseControl IO m, MonadMask m) => HasForkT m where
  forkTAsk :: m (MVar ForkState)

instance {-# OVERLAPS #-} (MonadMask m, MonadBaseControl IO m) => HasForkT (ForkT m) where
  forkTAsk :: ForkT m (MVar ForkState)
  forkTAsk = ForkT ask

instance
  {-# OVERLAPS #-}
  (MonadBaseControl IO (t m), MonadMask (t m), MonadTrans t, Monad m, HasForkT m) =>
  HasForkT (t m)
  where
  forkTAsk = lift forkTAsk

safeFork :: (HasForkT m) => m () -> m ThreadId
safeFork action = do
  threadEnded <- newEmptyMVar
  threadId <- fork (action `finally` putMVar threadEnded ())
  forkState <- forkTAsk
  modifyMVar_ forkState (pure . (#threads %~ (threadEnded :)))
  pure threadId

instance (MonadBase base m) => MonadBase base (ForkT m) where
  liftBase = ForkT . liftBase

safeAcquire :: (HasForkT m) => m resource -> (resource -> IO ()) -> m resource
safeAcquire acquire release = do
  resource <- acquire
  forkState <- forkTAsk
  modifyMVar_ forkState (pure . (#releasers %~ (release resource :)))
  pure resource

-- * convenience helpers

safeSystemTempDirectory :: (HasForkT m) => Text -> m FilePath
safeSystemTempDirectory snippet = do
  systemTempDir <- liftBase getCanonicalTemporaryDirectory
  safeAcquire
    (liftBase $ createTempDirectory systemTempDir (cs snippet))
    removeDirectoryRecursive

safeSystemTempFile :: (HasForkT m) => String -> m (FilePath, Handle)
safeSystemTempFile snippet = do
  systemTempDir <- liftBase getCanonicalTemporaryDirectory
  safeAcquire
    (liftBase $ openTempFile systemTempDir snippet)
    (\(path, handle) -> liftBase (hClose handle >> removeFile path))

-- | Removes paths that 'safeSystemTempDirectory' and 'safeSystemTempFile'
-- created under these templates. Releasers do not run when the process is
-- killed, so a new process calls this before it acquires any of its own.
removeLeftoverTemporaries :: (MonadIO m) => [String] -> m [FilePath]
removeLeftoverTemporaries templates = liftIO $ do
  systemTempDir <- getCanonicalTemporaryDirectory
  leftovers <-
    map (systemTempDir </>)
      . filter (\name -> any (`isPrefixOf` name) templates)
      <$> listDirectory systemTempDir
  forM_ leftovers removePathForcibly
  pure leftovers
