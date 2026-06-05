module Garnix.TestHelpers.Deprecated where

import Control.Exception qualified as E
import Control.Exception.Safe (throwIO, tryAny)
import Cradle
import Data.IORef.Lifted (newIORef)
import Data.Text.IO qualified as T
import Data.Yaml (encode)
import GHC.IO.Handle (hFlush)
import Garnix (withEnv)
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers.Common
import Garnix.TestHelpers.GithubInterface.Deprecated qualified as Deprecated
import Garnix.TestHelpers.HetznerMock
import Garnix.TestHelpers.Monad (cleanDbConn, githubAppPk)
import Garnix.TestInstances ()
import Garnix.Types hiding (head)
import Garnix.YamlConfig (GarnixConfig)
import System.Directory (copyFile, getCurrentDirectory, makeAbsolute)
import System.IO.Silently
import System.IO.Temp (withSystemTempDirectory)
import System.Random (randomIO)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Mockery.Environment

-- | Consider using `Garnix.TestHelpers.Monad.suppressLogsWhenPassing` instead.
quietWhenPassing :: IO a -> IO a
quietWhenPassing action = do
  (logs, result) <- capture . tryAny $ E.try (action `finally` hFlush stdout)
  let appendedLogs = "\f===== logs =====\n" <> logs <> "=== end logs ===\n"
  case result of
    Right (Right a) -> pure a
    Right (Left e@(HUnitFailure loc reason)) ->
      throwIO
        $ if null logs
          then e
          else HUnitFailure loc
            $ case reason of
              Reason str -> Reason $ str <> appendedLogs
              ExpectedButGot pref expected got ->
                ExpectedButGot
                  (Just $ fromMaybe "" pref <> appendedLogs)
                  expected
                  got
    Left e -> do
      unless (null logs) (putStrLn appendedLogs)
      throwIO e

-- | Consider using `Garnix.TestHelpers.Monad.withDevSecrets`
addTestSecrets :: IO a -> IO a
addTestSecrets test =
  withSystemTempDirectory "garnix-test" $ \tempDir -> do
    backendDir <- getCurrentDirectory >>= makeAbsolute
    let jwtKey = backendDir <> "/dev-key.jwt"
        hetznerToken = tempDir <> "/hetznerToken"
        s3CacheKeyFile = tempDir </> "cache-priv-key-file"
    writeFile hetznerToken "foo"
    writeFile s3CacheKeyFile "key-name:key"
    sshKey <- makeAbsolute "ssh-key-for-tests"
    repoSecretsPath <- makeAbsolute "test/spec/data/repo-secrets.key"
    withModifiedEnvironment
      [ ("GITHUB_WEBHOOK_SECRET", "foo"),
        ("GITHUB_CLIENT_SECRET", "foo"),
        ("GITHUB_CLIENT_ID", "foo"),
        ("GITHUB_APP_ID", "42"),
        ("GITHUB_APP_PK", githubAppPk),
        ("GITHUB_APP_NAME", "foo"),
        ("GARNIX_SERVER_SSH_KEYS", sshKey),
        ("JWT_KEY", jwtKey),
        ("HETZNER_TOKEN", hetznerToken),
        ("OPENSEARCH_API", "foo"),
        ("REPO_SECRETS_KEY_PATH", repoSecretsPath),
        ("REPO_SECRETS_PUB_KEY", "age107r0e6nxchkrqdxg42tzdxeauez2ce7cpsajcggjwmpjgrlrnqfqy6tnlf"),
        ("S3_CACHE_ACCESS_KEY_ID", "foo"),
        ("S3_CACHE_SECRET_ACCESS_KEY", "foo"),
        ("S3_CACHE_REGION", "foo"),
        ("S3_CACHE_HOST", "foo"),
        ("S3_CACHE_PUBLIC_BUCKET", "foo"),
        ("S3_CACHE_PUBLIC_BASE_URL", "foo"),
        ("S3_CACHE_PRIVATE_BUCKET", "foo"),
        ("CACHE_PRIV_KEY_FILE", s3CacheKeyFile)
      ]
      test

-- This function does too many things at once and seems not very
-- composable. Consider using `inM` or `runTestM` instead.
withMockRepo :: Text -> Maybe Text -> Branch -> (FilePath -> CommitHash -> M a) -> IO (Either ErrorWithContext a)
withMockRepo flake yaml branch action = do
  buildRef <- newIORef mempty
  withSystemTempDirectory "garnix-test" $ \mockGithubRepo -> do
    withEnv
      localDevelopment
      mockGithubRepo
      Nothing
      $ \env' -> do
        ghInterface <- Deprecated.testGithubInterface mockGithubRepo buildRef
        let env =
              env'
                & #hetznerInterface
                .~ testHetznerInterface
                & #githubInterface
                .~ ghInterface
                & #s3CacheEnv
                .~ error "withMockRepo: mock s3CacheEnv"
        result <- runM env $ do
          commit <- writeMockRemoteWithFlake mockGithubRepo branch flake yaml
          action mockGithubRepo commit
        cleanDbConn env
        pure result

-- | Consider using `withFakeGithubInterface`.
--
-- This function uses `Env`s `workingDir` for the mock remote, which is really confusing.
writeMockRemote :: Branch -> GarnixConfig -> M CommitHash
writeMockRemote branch config = do
  let defaultFlake = "{ outputs = { self }: {}; }"
  dir <- view #workingDir
  writeMockRemoteWithFlake dir branch defaultFlake (Just $ cs $ encode config)

-- | Consider using `withFakeGithubInterface`.
writeMockRemoteWithFlake :: FilePath -> Branch -> Text -> Maybe Text -> M CommitHash
writeMockRemoteWithFlake mockGithubRepo branch flake config = do
  liftIO $ T.writeFile (mockGithubRepo </> "flake.nix") flake
  -- Reusing the top-level flake file can substantially speed up tests.
  liftIO $ copyFile "../flake.lock" (mockGithubRepo </> "flake.lock")
  traverse_ (liftIO . T.writeFile (mockGithubRepo </> "garnix.yaml")) config
  -- To ensure unique git commit hashes
  randomness :: Int <- randomIO
  liftIO $ T.writeFile (mockGithubRepo </> "some-file") (show randomness)
  run_
    $ cmd "git"
    & setWorkingDir mockGithubRepo
    & addArgs ["init", "." :: String]
    & silenceStdout
    & silenceStderr
  run_
    $ cmd "git"
    & setWorkingDir mockGithubRepo
    & addArgs ["checkout", "-B", getBranch branch]
    & silenceStderr
  commitAll mockGithubRepo
