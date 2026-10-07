{-# OPTIONS_GHC -Wno-type-defaults #-}

module Garnix.Build.NixBuildPoolSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async.Lifted (async, wait)
import Garnix.Async (timeout)
import Garnix.Duration (fromMilliSeconds, threadDelay)
import Garnix.Monad.Pool
import Garnix.Prelude
import Garnix.TestHelpers (runTestM)
import Test.Hspec

-- | Regression guard for the fix in "Garnix.Build.Package".@runNixBuild@: the
-- build slot is acquired *outside* the build timeout, so a build that is merely
-- waiting in the queue must not burn its timeout budget. Only once it holds a
-- slot does the clock start. If the pool were acquired *inside* the timeout (the
-- old, implicit behaviour where every attribute fired @nix build@ at once and
-- backlogged ones timed out while queued), this test would fail.
spec :: Spec
spec = describe "nix build pool" $ do
  it "does not spend the build-timeout budget while queued for a slot" $ runTestM $ do
    metrics <- view #metrics
    pool <- newPool 1 metrics #nixBuildQueueWaitTime #nixBuildQueueLen
    gate <- liftIO newEmptyMVar
    done <- liftIO newEmptyMVar

    -- Occupy the only slot until we release `gate`.
    occupier <- async $ withPool pool () $ liftIO $ takeMVar gate
    -- Let the occupier actually acquire the slot before the waiter queues.
    threadDelay $ fromMilliSeconds 50

    -- The waiter has a short (100ms) timeout, but the pool wraps it, so it
    -- blocks *untimed* on the slot and only starts timing once it runs.
    waiter <- async $ withPool pool () $ do
      r <- timeout (fromMilliSeconds 100) $ pure ()
      liftIO $ putMVar done (isJust r)

    -- Hold the slot far longer than the waiter's own timeout budget.
    threadDelay $ fromMilliSeconds 300
    liftIO $ putMVar gate ()

    completed <- liftIO $ takeMVar done
    liftIO $ completed `shouldBe` True
    wait occupier
    wait waiter
