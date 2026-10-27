module Garnix.Monad.ForkTSpec where

import Control.Concurrent.Lifted
  ( MVar,
    modifyMVar,
    modifyMVar_,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
  )
import Control.Concurrent.Lifted qualified as Lifted
import Control.Exception (ErrorCall (..))
import Control.Exception.Safe (throwIO)
import Control.Exception.Safe qualified as Safe
import Control.Monad.Trans.Control (liftBaseOp_)
import Data.Set qualified as Set
import Garnix.Duration
import Garnix.Monad
import Garnix.Monad.ForkT
import Garnix.Prelude
import Garnix.TestHelpers hiding (shouldReturn)
import Garnix.TestHelpers.Monad
import Garnix.Types (Error (OtherError))
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.IO.Silently (capture, hSilence)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Hspec

spec :: Spec
spec = before (newMVar def :: IO (MVar TestResourcesManager)) $ do
  describe "ForkT" $ do
    it "allows forking" $ \_man -> do
      runForkT $ do
        mvar <- newEmptyMVar
        _ <- safeFork $ do
          takeMVar mvar
        putMVar mvar ()
      pure () :: IO ()

    it "allows to acquire resources" $ \man -> do
      runForkT $ do
        resource <- safeAcquire (acquire man) (release man)
        liftIO $ resource `shouldBe` 0
      pure () :: IO ()

    it "releases resources at the end" $ \man -> do
      resource <- runForkT $ do
        safeAcquire (acquire man) (release man)
      readMVar man `shouldReturn` TRM 1 mempty [resource]

    it "releases resources in LIFO order" $ \man -> do
      (first, second) <- runForkT $ do
        first <- safeAcquire (acquire man) (release man)
        second <- safeAcquire (acquire man) (release man)
        pure (first, second)
      readMVar man `shouldReturn` TRM 2 mempty [second, first]

    it "keeps resources around when any threads are still running" $ \man -> do
      result <- newEmptyMVar
      resource <- runForkT $ do
        waiter <- newEmptyMVar
        resource <- safeAcquire (acquire man) (release man)
        _ <- safeFork $ do
          takeMVar waiter
          Lifted.threadDelay 100_000
          readMVar man >>= putMVar result
        putMVar waiter ()
        pure resource
      readMVar result `shouldReturn` TRM 1 (Set.singleton resource) mempty

    it "releases resources when the action throws" $ \man -> do
      result <- Safe.try $ runForkT $ do
        _resource <- safeAcquire (acquire man) (release man)
        throwIO $ ErrorCall "test error"
      result `shouldBe` (Left (ErrorCall "test error") :: Either ErrorCall ())
      readMVar man `shouldReturn` TRM 1 mempty [0]

    it "releases subsequent resources when release handlers crash" $ \man -> do
      result <- Safe.try $ runForkT $ do
        _resource0 <- safeAcquire (acquire man) (release man)
        _resource1 <- safeAcquire (acquire man) (\_ -> throwIO $ ErrorCall "test error")
        _resource2 <- safeAcquire (acquire man) (release man)
        pure ()
      result `shouldBe` Left (ErrorCall "test error")
      readMVar man `shouldReturn` TRM 3 (Set.singleton 1) [2, 0]

    it "removes leftovers of a previous process by template" $ \_man -> do
      tmp <- getCanonicalTemporaryDirectory
      leftover <- createTempDirectory tmp "garnix-leftover-spec"
      kept <- createTempDirectory tmp "garnix-kept-spec"
      removed <- removeLeftoverTemporaries ["garnix-leftover-spec"]
      removed `shouldBe` [leftover]
      doesDirectoryExist leftover `shouldReturn` False
      doesDirectoryExist kept `shouldReturn` True
      removeDirectoryRecursive kept

    describe "using in M" $ do
      it "releases resources when threads throw runtime exceptions" $ \man -> do
        resource <- hSilence [stderr] $ runTestM $ do
          resource <- safeAcquire (acquire man) (release man)
          _ <- safeFork $ do
            throwIO $ ErrorCall "test error"
          pure resource
        readMVar man `shouldReturn` TRM 1 mempty [resource]

      it "releases resources when threads get terminated from the outside" $ \man -> do
        resource <- hSilence [stderr] $ runTestM $ do
          resource <- safeAcquire (acquire man) (release man)
          threadId <- safeFork $ do
            threadDelay (fromSeconds @Int 1_000)
          threadDelay (fromMilliSeconds @Int 100)
          killThread threadId
          pure resource
        readMVar man `shouldReturn` TRM 1 mempty [resource]

      it "releases resources when threads throw monadic errors" $ \man -> do
        resource <- runTestM $ do
          resource <- safeAcquire (acquire man) (release man)
          _ <- safeFork $ do
            throw $ OtherError "test error"
          pure resource
        readMVar man `shouldReturn` TRM 1 mempty [resource]

      it "can be used in M to create temporary directories" $ \_man -> do
        tmpDir <- runTestM $ do
          tempDir <- safeSystemTempDirectory "garnix-test"
          liftIO $ do
            tempDir `shouldStartWith` "/tmp"
            doesDirectoryExist tempDir `shouldReturn` True
            pure tempDir
        doesDirectoryExist tmpDir `shouldReturn` False

      it "can be used in M to fork threads" $ \_man -> do
        runTestM $ do
          mvar <- newEmptyMVar
          _ <- safeFork $ do
            takeMVar mvar
          putMVar mvar ()

      it "works correctly for liftBaseOp_" $ \man -> do
        runTestM $ do
          resource <- liftBaseOp_ identity $ do
            safeAcquire (acquire man) (release man)
          readMVar man `shouldReturnM` TRM 1 (Set.singleton resource) mempty
        pure ()

      it "works when acquiring resources in `beforeM_`" $ \man -> do
        (stdout, exception) :: (String, Either SomeException ()) <- capture $ Safe.try $ hspec $ inM $ do
          beforeM_ (void $ safeAcquire (acquire man) (release man)) $ do
            it "works" $ do
              pure () :: M ()
        case exception of
          Right () -> pure ()
          Left e -> putStrLn $ "inner test suite threw: " <> cs (show e) <> "\n" <> stdout
        readMVar man `shouldReturn` TRM 1 mempty [0]

data TestResourcesManager = TRM
  { counter :: Int,
    acquired :: Set Int,
    released :: [Int]
  }
  deriving stock (Eq, Show)

instance Default TestResourcesManager where
  def = TRM 0 mempty mempty

acquire :: (MonadBaseControl IO m) => MVar TestResourcesManager -> m Int
acquire man = modifyMVar man $ \TRM {counter, acquired, released} ->
  pure (TRM (succ counter) (Set.insert counter acquired) released, counter)

release :: (MonadBaseControl IO m) => MVar TestResourcesManager -> Int -> m ()
release man id = modifyMVar_ man $ \TRM {counter, acquired, released} ->
  if id `Set.member` acquired
    then
      pure
        $ TRM
          { counter,
            acquired = Set.delete id acquired,
            released = released <> [id]
          }
    else error $ "double releasing of " <> show id
