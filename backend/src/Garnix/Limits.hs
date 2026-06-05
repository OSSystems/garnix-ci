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
  )
where

import Data.Text qualified as T
import Garnix.Duration
import Garnix.Prelude
import Text.Read (readMaybe)

buildTimeout :: IO Duration
buildTimeout = fromMinutes <$> envIntDefault "GARNIX_BUILD_TIMEOUT_MINUTES" (60 :: Int)

evalTimeout :: IO Duration
evalTimeout = fromMinutes <$> envIntDefault "GARNIX_EVAL_TIMEOUT_MINUTES" (10 :: Int)

maxPackagesPerFlake :: IO Int32
maxPackagesPerFlake = envIntDefault "GARNIX_MAX_PACKAGES_PER_FLAKE" 1000

fodCheckEnabled :: IO Bool
fodCheckEnabled = envBoolDefault "GARNIX_FOD_CHECK" True

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
