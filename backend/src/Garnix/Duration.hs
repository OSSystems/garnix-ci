module Garnix.Duration
  ( Duration,
    addDuration,
    addTime,
    diffTime,
    multiplyDuration,
    divideDuration,
    emptyDuration,
    fromDays,
    fromHours,
    fromMilliSeconds,
    fromMinutes,
    fromSeconds,
    maxDuration,
    parseDuration,
    subTime,
    subtractDuration,
    threadDelay,
    toMicroseconds,
    toMilliseconds,
    toMinutes,
    toSeconds,
  )
where

import Control.Concurrent qualified as M (threadDelay)
import Data.Text qualified as T
import Garnix.Prelude
import Text.Read (readMaybe)
import Prelude qualified

newtype Duration = Seconds {inSeconds :: Double}
  deriving stock (Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

instance Show Duration where
  show =
    cs
      . fmt
        [ ("d", fromDays @Int 1),
          ("h", fromHours @Int 1),
          ("m", fromMinutes @Int 1),
          ("s", fromSeconds @Int 1),
          ("ms", fromMilliSeconds @Int 1)
        ]
    where
      fmt :: [(Text, Duration)] -> Duration -> Text
      fmt [] (Seconds {inSeconds}) = "Duration " <> show inSeconds
      fmt ((unitName, unitDuration) : rest) duration =
        let durationInUnit :: Int = floor $ duration `divideDuration` unitDuration
         in (if durationInUnit >= 1 then show durationInUnit <> unitName <> " " else "")
              <> fmt rest (duration `subtractDuration` Seconds (inSeconds unitDuration * realToFrac durationInUnit))

emptyDuration :: Duration
emptyDuration = Seconds 0

addDuration :: Duration -> Duration -> Duration
addDuration (Seconds a) (Seconds b) = Seconds $ a + b

subtractDuration :: Duration -> Duration -> Duration
subtractDuration (Seconds a) (Seconds b) = Seconds $ a - b

multiplyDuration :: (Real a) => Duration -> a -> Duration
multiplyDuration (Seconds s) r = Seconds (s * realToFrac r)

divideDuration :: Duration -> Duration -> Double
divideDuration (Seconds a) (Seconds b) = a / b

maxDuration :: Duration -> Duration -> Duration
maxDuration (Seconds a) (Seconds b) = Seconds $ max a b

fromMilliSeconds :: (Real a) => a -> Duration
fromMilliSeconds ms = Seconds (realToFrac ms / 1000)

fromSeconds :: (Real a) => a -> Duration
fromSeconds = Seconds . realToFrac

fromMinutes :: (Real a) => a -> Duration
fromMinutes m = Seconds (60 * realToFrac m)

fromHours :: (Real a) => a -> Duration
fromHours h = Seconds (60 * 60 * realToFrac h)

fromDays :: (Real a) => a -> Duration
fromDays d = Seconds (60 * 60 * 24 * realToFrac d)

toMilliseconds :: Duration -> Int
toMilliseconds secs = round $ 1000 * inSeconds secs

toMicroseconds :: Duration -> Int
toMicroseconds secs = 1000 * toMilliseconds secs

toSeconds :: Duration -> Double
toSeconds = inSeconds

toMinutes :: Duration -> Double
toMinutes d = inSeconds d / 60

diffTime :: UTCTime -> UTCTime -> Duration
diffTime a b = Seconds $ realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime a b

addTime :: Duration -> UTCTime -> UTCTime
addTime duration = addUTCTime (secondsToNominalDiffTime $ realToFrac $ inSeconds duration)

subTime :: Duration -> UTCTime -> UTCTime
subTime duration = addTime $ Seconds (inSeconds duration * (-1))

threadDelay :: (MonadIO m) => Duration -> m ()
threadDelay = liftIO . M.threadDelay . toMicroseconds

parseDuration :: Text -> Maybe Duration
parseDuration raw =
  case T.unsnoc (T.strip raw) of
    Nothing -> Nothing
    Just (init', last') -> case last' of
      'd' -> fromDays <$> number init'
      'h' -> fromHours <$> number init'
      'm' -> fromMinutes <$> number init'
      's' -> fromSeconds <$> number init'
      _ -> fromSeconds <$> number (T.strip raw)
  where
    number :: Text -> Maybe Double
    number text = do
      value <- readMaybe (cs (T.strip text))
      guard (value >= 0)
      pure value
