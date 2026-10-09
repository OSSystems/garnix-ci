-- | A fixed-window request counter per client, held in memory: enough to keep
-- one client from hammering an unauthenticated endpoint of one process.
module Garnix.RateLimit
  ( RateLimiter,
    newRateLimiter,
    allowRequest,
    Windows (..),
    countRequest,
  )
where

import Control.Concurrent (MVar, modifyMVar, newMVar)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Garnix.Prelude

data RateLimiter = RateLimiter
  { maxRequests :: Int,
    window :: NominalDiffTime,
    windows :: MVar Windows
  }

-- | When expired windows were last dropped, and when each client's current
-- window started, with its requests in it.
data Windows = Windows
  { windowsPruned :: UTCTime,
    windowsByClient :: Map Text (UTCTime, Int)
  }
  deriving stock (Eq, Show)

-- | At most the given number of requests per client in each window.
newRateLimiter :: Int -> NominalDiffTime -> IO RateLimiter
newRateLimiter maxRequests window = do
  now <- getCurrentTime
  RateLimiter maxRequests window <$> newMVar (Windows now mempty)

-- | Counts a request of the client, and says whether it is within the limit.
allowRequest :: (MonadIO m) => RateLimiter -> Text -> m Bool
allowRequest RateLimiter {maxRequests, window, windows} client = liftIO $ do
  now <- getCurrentTime
  modifyMVar windows $ pure . countRequest maxRequests window now client

-- | A request of the client at the given time, counted: the windows after
-- it, and whether it is within the limit of so many requests per window.
countRequest :: Int -> NominalDiffTime -> UTCTime -> Text -> Windows -> (Windows, Bool)
countRequest maxRequests window now client (Windows pruned current) =
  (Windows pruned' (Map.insert client (start, count + 1) kept), count < maxRequests)
  where
    expired (start', _) = diffUTCTime now start' >= window
    -- Dropping every expired window walks the whole map, so it happens once
    -- per window, not on every request.
    (pruned', kept)
      | diffUTCTime now pruned >= window = (now, Map.filter (not . expired) current)
      | otherwise = (pruned, current)
    (start, count) = case Map.lookup client kept of
      Just entry | not (expired entry) -> entry
      _ -> (now, 0)
