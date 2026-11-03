-- | Static, env-overridable limits for the self-host build pipeline.
--
-- Replaces the per-org 'ProductPlan' lookup. Defaults are deliberately
-- generous — last-resort safety, not policy. Operators tighten by setting
-- the env vars; Phase 7's NixOS module surfaces typed options that emit them.
module Garnix.Limits
  ( buildTimeout,
    evalTimeout,
    maxPackagesPerFlake,
    fodCheckEnabled,
    evalJobsWorkers,
    serviceMemoryBudget,
    -- exported for testing
    cgroupPath,
    parseCgroupLimit,
    parseMemTotal,
  )
where

import Control.Exception.Safe qualified as Safe
import Data.Text qualified as T
import Data.Maybe (listToMaybe)
import Data.Text.IO qualified as T
import Garnix.Duration
import Garnix.Prelude
import Garnix.Types (Memory (..))
import Text.Read (readMaybe)

buildTimeout :: IO Duration
buildTimeout = fromMinutes <$> envIntDefault "GARNIX_BUILD_TIMEOUT_MINUTES" (60 :: Int)

evalTimeout :: IO Duration
evalTimeout = fromMinutes <$> envIntDefault "GARNIX_EVAL_TIMEOUT_MINUTES" (10 :: Int)

maxPackagesPerFlake :: IO Int32
maxPackagesPerFlake = envIntDefault "GARNIX_MAX_PACKAGES_PER_FLAKE" 1000

fodCheckEnabled :: IO Bool
fodCheckEnabled = envBoolDefault "GARNIX_FOD_CHECK" True

-- | Most workers one nix-eval-jobs run may use.
evalJobsWorkers :: Int
evalJobsWorkers = 8

-- | Memory the server may use before the kernel throttles it: the lower of its
-- cgroup's @memory.high@ and @memory.max@ (systemd's @MemoryHigh@ and
-- @MemoryMax@), or the machine's RAM if it has neither. Evaluations run as the
-- server's children, so they all share it.
serviceMemoryBudget :: IO Memory
serviceMemoryBudget = do
  cgroupLimits <- orNothing $ do
    path <- cgroupPath <$> T.readFile "/proc/self/cgroup"
    forM path $ \dir ->
      forM ["memory.high", "memory.max"] $ \file ->
        orNothing $ parseCgroupLimit <$> T.readFile (dir </> file)
  memTotal <- orNothing $ parseMemTotal <$> T.readFile "/proc/meminfo"
  pure $ case catMaybes (fromMaybe [] cgroupLimits) of
    [] -> Memory $ fromMaybe maxBound memTotal
    limits -> Memory $ minimum limits
  where
    orNothing :: IO (Maybe a) -> IO (Maybe a)
    orNothing = Safe.handleIO (const $ pure Nothing)

-- | The cgroup v2 directory of a @\/proc\/\<pid\>\/cgroup@ file. 'Nothing' under
-- cgroup v1.
cgroupPath :: Text -> Maybe FilePath
cgroupPath contents =
  listToMaybe [cs ("/sys/fs/cgroup" <> path) | line <- T.lines contents, Just path <- [T.stripPrefix "0::" line]]

-- | Bytes in a cgroup memory file; 'Nothing' for @max@, i.e. no limit.
parseCgroupLimit :: Text -> Maybe Int64
parseCgroupLimit = readMaybe . cs . T.strip

-- | Bytes of RAM in @\/proc\/meminfo@.
parseMemTotal :: Text -> Maybe Int64
parseMemTotal contents =
  listToMaybe
    [ kilobytes * 1024
    | line <- T.lines contents,
      Just rest <- [T.stripPrefix "MemTotal:" line],
      [amount, "kB"] <- [T.words rest],
      Just kilobytes <- [readMaybe (cs amount)]
    ]

envIntDefault :: (Read a) => String -> a -> IO a
envIntDefault name fallback = do
  raw <- lookupEnv name
  pure $ fromMaybe fallback (raw >>= readMaybe)

envBoolDefault :: String -> Bool -> IO Bool
envBoolDefault name fallback = do
  raw <- lookupEnv name
  pure $ case T.toLower . T.strip . cs <$> raw of
    Just "0" -> False
    Just "false" -> False
    Just "off" -> False
    Just "no" -> False
    Just "" -> fallback
    Just "1" -> True
    Just "true" -> True
    Just "on" -> True
    Just "yes" -> True
    _ -> fallback
