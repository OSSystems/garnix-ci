{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Garnix.TestHelpers.Monad
  ( inM,
    inMWith,
    cleanDbConn,
    aroundM_,
    beforeM_,
    pendingM,
    shouldBeM,
    shouldReturnM,
    shouldContainM,
    shouldNotContainM,
    shouldSatisfyM,
    shouldThrowM,
    shouldThrowIO,
    shouldTerminate,
    withTestEnvironment,
    withDevSecrets,
    githubAppPk,
    suppressLogsWhenPassing,
    suppressLogs,
    withLogCapturing,
    captureLogs,
    captureLogs_,
    withLogLevel,
  )
where

import Control.Concurrent.Lifted (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception.Lifted (throwIO)
import Control.Exception.Safe qualified as Safe
import Control.Lens
import Control.Monad.Trans.Control (liftBaseDiscard)
import Cradle
import Crypto.PubKey.RSA.Read (readRsaPem)
import Data.Aeson (Key)
import Data.Aeson.Lens (key, _String)
import Data.Either (fromRight)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Pool qualified as Pool
import Data.String.Conversions (SBS)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO (hPutStr, hPutStrLn)
import Data.Yaml (Value (..), decodeEither')
import Database.PostgreSQL.Typed (PGDatabase (..), pgConnect, pgDisconnect)
import Database.PostgreSQL.Typed.TH (getTPGDatabase)
import Garnix (envMocks, lookupOptionalSecret)
import Garnix.Async qualified
import Garnix.DB.FeatureFlags.Types (getFeatureFlagConfig)
import Garnix.Duration
import Garnix.Monad
import Garnix.Monad.Metrics (registerMetrics)
import Garnix.Monad.Pool qualified
import Garnix.NixConfig (defaultNixConfig, githubAccessTokenNixConfig)
import Garnix.Prelude
import Garnix.TestHelpers.GithubInterface.Deprecated qualified as Deprecated
import Garnix.Types hiding (pending)
import GitHub.App.Auth (AppAuth (..))
import GitHub.Data.Id (Id (..))
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Auth.Server (CookieSettings (..), defaultCookieSettings, defaultJWTSettings, fromSecret)
import System.Directory (createDirectory, doesFileExist, makeAbsolute)
import System.Environment (getEnv)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe qualified
import Test.HUnit (assertFailure)
import Test.Hspec
import Test.Hspec.Core.Spec qualified as Hspec
import Test.Hspec.Golden (Golden)
import Prelude qualified (Show (..))
import Garnix.Monad.KeyedMutex (newKeyedMutex)
import Garnix.Hosting.Types (HostingBudget (..))

cleanDbConn :: Env -> IO ()
cleanDbConn env =
  case env ^. #dbConn of
    ConnectionPool pool -> Pool.destroyAllResources pool
    Transaction _ -> pure ()

inM :: SpecWith Env -> SpecWith ()
inM spec = aroundAll (\spec -> spec identity) $ do
  inMWith spec

-- An `aroundAllM_` doesn't work, since the `Env` would leak from the first test to following tests.
-- We can combine `inMWith` with `aroundAll` to achieve something similar to `aroundAllM_` though.
inMWith :: SpecWith Env -> SpecWith (Env -> Env)
inMWith = aroundWith $ \test modEnv -> do
  withSystemTempDirectory "garnix-test" $ \tmp -> do
    withTestEnvironment tmp $ \env -> do
      test (modEnv env)

instance Example (M ()) where
  type Arg (M ()) = Env
  evaluateExample example _params hook _ = do
    hook $ \env -> do
      result <- runM env $ do
        example
      case result of
        Right () -> pure ()
        Left e -> Safe.throwIO $ MonadicError e
    pure $ Hspec.Result "" Hspec.Success

instance (Eq string) => Example (M (Golden string)) where
  type Arg (M (Golden string)) = Env
  evaluateExample example params hook callback = do
    ref <- newIORef (error "should be set inside `hook`")
    hook $ \env -> do
      result <- runM env example
      case result of
        Right golden -> do
          result <- Hspec.safeEvaluateExample golden params (\action -> action ()) callback
          writeIORef ref result
        Left e -> Safe.throwIO $ MonadicError e
    readIORef ref

newtype MonadicError = MonadicError ErrorWithContext

instance Show MonadicError where
  show (MonadicError error) = "MonadicError\n" <> cs (showDebug error)

instance Exception MonadicError

aroundM_ :: (M () -> M ()) -> SpecWith Env -> SpecWith Env
aroundM_ wrapper = do
  aroundWith $ \(test :: Env -> IO ()) env -> do
    result <- runM env $ do
      wrapper $ do
        env <- ask
        result :: Either MonadicError () <- liftIO $ Safe.try $ test env
        case result of
          Right () -> pure ()
          Left (MonadicError e) -> rethrow e
    case result of
      Right () -> pure ()
      Left e -> Safe.throwIO $ MonadicError e

beforeM_ :: M () -> SpecWith Env -> SpecWith Env
beforeM_ setup = aroundM_ (setup >>)

pendingM :: (HasCallStack) => M ()
pendingM = liftIO pending

shouldBeM :: (HasCallStack, Show a, Eq a) => a -> a -> M ()
shouldBeM a b = liftIO $ a `shouldBe` b

infixl 1 `shouldBeM`

shouldSatisfyM :: (HasCallStack, Show a) => a -> (a -> Bool) -> M ()
shouldSatisfyM a pred = liftIO $ a `shouldSatisfy` pred

shouldReturnM :: (HasCallStack, Show a, Eq a) => M a -> a -> M ()
shouldReturnM action expected = do
  a <- action
  a `shouldBeM` expected

shouldContainM :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> M ()
shouldContainM whole sublist = liftIO $ whole `shouldContain` sublist

shouldNotContainM :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> M ()
shouldNotContainM whole sublist = liftIO $ whole `shouldNotContain` sublist

shouldThrowIO :: (HasCallStack, Show a, Eq a, Exception e) => M a -> Selector e -> M ()
shouldThrowIO sh selector = liftBaseDiscard (`shouldThrow` selector) (void sh)

shouldThrowM :: (HasCallStack) => M a -> Error -> M ()
shouldThrowM sh expected =
  try sh >>= \case
    Left actualError -> liftIO $ err actualError `shouldBe` expected
    Right _ -> liftIO $ assertFailure "Expected monadic error but got success."

shouldTerminate :: (HasCallStack, MonadIO m, MonadBaseControl IO m) => Duration -> m a -> m a
shouldTerminate duration action = do
  result <- Garnix.Async.timeout duration action
  case result of
    Just a -> pure a
    Nothing -> liftIO $ assertFailure $ cs $ "shouldTerminate: didn't terminate in " <> show duration

suppressLogs :: M a -> M a
suppressLogs = local (#logger .~ const (pure ()))

suppressLogsWhenPassing :: M a -> M a
suppressLogsWhenPassing action = do
  logsRef <- liftIO $ newMVar []
  let log logItem = liftIO $ do
        modifyMVar_ logsRef $ \logs -> pure (logItem : logs)
      outputLogs :: (MonadIO m) => m ()
      outputLogs = liftIO $ do
        logs <- readMVar logsRef
        hPutStr stderr $ T.unlines $ map msg $ reverse logs
  local (#logger .~ log) $ do
    result :: Either SomeException (Either ErrorWithContext a) <- Safe.try $ try action
    case result of
      Left runtimeException -> do
        outputLogs
        throwIO runtimeException
      Right (Left error) -> do
        outputLogs
        rethrow error
      Right (Right a) -> pure a

withLogCapturing :: (M [LogItem] -> M a) -> M a
withLogCapturing action = do
  mvar <- liftIO $ newMVar []
  let getLogs = liftIO (readMVar mvar) <&> reverse
  let captureLogItem logItem = modifyMVar_ mvar $ \acc -> pure (logItem : acc)
  local
    (#logger %~ (\existingLogger logItem -> existingLogger logItem >> captureLogItem logItem))
    (action getLogs)

captureLogs :: M a -> M ([LogItem], a)
captureLogs action = withLogCapturing $ \getLogs -> do
  result <- action
  logs <- getLogs
  pure (logs, result)

captureLogs_ :: M a -> M [LogItem]
captureLogs_ action = fst <$> captureLogs action

withLogLevel :: Severity -> M a -> M a
withLogLevel logLevel = do
  local
    ( #logger
        %~ ( \existingLogger logItem ->
               when (logItem ^. #severity <= logLevel) $ existingLogger logItem
           )
    )

-- * test `Env`s

withTestEnvironment :: FilePath -> (Env -> IO a) -> IO a
withTestEnvironment tempDir action = do
  Safe.bracket
    mkPool
    Pool.destroyAllResources
    $ \pgConn -> do
      checkThatInBackendDir
      let testFeatures = localDevelopment
      let githubAppAuth = AppAuth (Id 42) (fromRight (error "cannot read githubAppPk") $ readRsaPem $ cs githubAppPk)
      buildRef <- newIORef mempty
      jwtKey <- makeAbsolute "dev-key.jwt"
      repoSecretsEncryptionKeyPath <- RepoSecretsEncryptionKeyPath <$> makeAbsolute "test/spec/data/repo-secrets.key"
      mgr <- newTlsManager
      buildLogsDir <- do
        dir <- makeAbsolute (tempDir </> "build-logs")
        createDirectory dir
        pure dir
      metrics <- registerMetrics
      nixEvalPool <- Garnix.Monad.Pool.newPool 120 metrics #evalQueueWaitTime #evalQueueLen
      s3UploadPool <- Garnix.Monad.Pool.newPool 80 metrics #s3QueueWaitTime #s3QueueLen
      mocks <- envMocks testFeatures
      Just emptyDir' <- lookupEnv "EMPTY_DIR"
      featureFlagConfig <- getFeatureFlagConfig
      fodCheckPool <- Garnix.Monad.Pool.newPool 40 metrics #fodCheckQueueWaitTime #fodCheckQueueLen
      compressionBudget <- newCompressionBudget
      nixConfig <- do
        secretsDir <- fromMaybe "/run/secrets" <$> lookupEnv "GARNIX_SECRETS_DIR"
        lookupOptionalSecret "GITHUB_ACCESS_TOKEN" (secretsDir <> "/github_access_token")
          <&> maybe defaultNixConfig (\token -> githubAccessTokenNixConfig (GhToken token) <> defaultNixConfig)
      deployMutex <- newKeyedMutex
      withDefaultLogger $ \defaultLogger -> do
        ghInterface <- Deprecated.testGithubInterface tempDir buildRef
        let env =
              Env
                { testFeatures = testFeatures,
                  githubAppAuth = githubAppAuth,
                  githubAppName = "github-app-name",
                  githubAppId = Id 12345,
                  githubClientSecret = "github-client-secret",
                  githubClientId = "github-client-id",
                  adminGithubLogin = Nothing,
                  buildLogsReportingPort = Nothing,
                  workingDir = tempDir,
                  nixXdgCacheDir = Nothing,
                  userNixConfig = nixConfig,
                  evalMemoryConfig =
                    EvalMemoryConfig
                      { defaultEvalMemory = defaultRepoConfig ^. maxEvalMemory,
                        perRepositoryEvalMemory = mempty
                      },
                  githubWebhookSecret = "github-webhook-secret",
                  githubInterface = ghInterface,
                  cookieSettings = defaultCookieSettings {cookieXsrfSetting = Nothing},
                  jwtSettings = defaultJWTSettings $ fromSecret $ cs jwtKey,
                  sessionLifetime = defaultSessionLifetime,
                  repoSecretsEncryptionKeyPath = repoSecretsEncryptionKeyPath,
                  repoSecretsEncryptionPubKey = RepoSecretsEncryptionPubKey "age107r0e6nxchkrqdxg42tzdxeauez2ce7cpsajcggjwmpjgrlrnqfqy6tnlf",
                  dbConn = ConnectionPool pgConn,
                  manager = mgr,
                  baseUrl = "https://garnix.io",
                  logger = defaultLogger,
                  buildLogsDir = buildLogsDir,
                  opensearchQueryUrl = "http://example.com/_msearch",
                  opensearchPassword = "opensearch-api",
                  s3CacheEnabled = True,
                  s3CacheEnv = error "s3CacheEnv: cache uploading should be mocked",
                  action =
                    ActionEnv
                      { runnerHost = error "misconfigured-host",
                        runnerSshKey = error "unset-ssh-key",
                        timeoutDuration = fromMinutes 5,
                        sharedResourcesUsers = ["garnix-io"]
                      },
                  nixEvalPool = nixEvalPool,
                  s3UploadPool = s3UploadPool,
                  compressionBudget,
                  mocks,
                  spanCtx = [],
                  metrics = metrics,
                  emptyDir = emptyDir',
                  hostname = "garnix-server-test",
                  githubLogDebounceDuration = fromSeconds 0,
                  featureFlagConfig,
                  fodCheckPool,
                  provisioner = unconfiguredProvisioner,
                  provisionerSocket = Nothing,
                  hostingDomain = "hosting.garnix.test",
                  statsReportUrl = Just "https://garnix.io/api/hosts/stats",
                  deployMutex,
                  hostingBudget = HostingBudget Nothing Nothing,
                  hostingSshKeys = []
                }
        action env
  where
    mkPool :: IO (Pool.Pool PGConnection)
    mkPool = do
      db <- getTPGDatabase
      dbPass <- cs <$> getEnv "PGPASSWORD"
      Pool.newPool
        ( Pool.setNumStripes (Just 2)
            $ Pool.defaultPoolConfig
              (pgConnect $ db {pgDBPass = dbPass})
              pgDisconnect
              60
              10
        )

githubAppPk :: String
githubAppPk =
  [i|
-----BEGIN PRIVATE KEY-----
MIICdgIBADANBgkqhkiG9w0BAQEFAASCAmAwggJcAgEAAoGBAJUge4agVoqHlw6F
fShKtu2U/wQqc5ZVASSMikqLJEs6D3cx6gNgqco8xFE5hfaE6hw+tIUH+Gxyenj6
FLMYvlpa+xNBoknzyOHFZfzD2MTsHIGqaDk+c3tquVZ/czBFA7Ia6R/jwPgxXpXC
NmOUR4YnePwDByaWmZNRhuUkc16vAgMBAAECgYA3fB55uK56XHiXrpMiqqnlO8qm
giT/iiEiuCe8FIksdC3M64VmPFDwLivdDMoLLujsGWiRFqYXs4BeAq4w9MPdyRQv
pGEjwQw1IhdMPlfbbplWDnqpsgx+w5xV9/QgOHHt/qI3gbnPdOlkZBy/rgmkES1A
bDTnlm1jnFKB5HdoAQJBAMWtgTkNm2EbTypDOJORQnem3UDY8zPT22X4+2bMwIOQ
eSlv5wGmCzNmkU6OY+KA3824gh7Pl4/sC0qW7Gphpp8CQQDBH/TiGvrP/aI+8+ru
uXb27d3Hc86kmGS4ilWCCPRTvLrI/QVwtJ03KCEzz/Agi1TEZ5k0SX4b9X0sqx5y
6p3xAkAx8YpKjeOJ/0pbFSzAK90tOd2Aus+Hcqll9Cggau7gzqmuDHXC9t6xl+Jy
hIs7+O+SnGFTw4M5e5vGtqb4ob9lAkAHXfS1e1n9/SrnQ96+ZIzJNqGzLhO/66BL
+drxLu5DE3v8lspSVlF4/SrnExOR6j69j0Yk3HjXEDJKNezfbVvBAkEAikOdWH37
XGVeR4U1W8n8Y+MJgSZVkRu8sQc8XSTWNJdp7YTCCh96sx6KRkQIEiKME6Q03rwz
lI0pssmOmu3Ssw==
-----END PRIVATE KEY-----
  |]

withDevSecrets :: M a -> M a
withDevSecrets action = do
  baseEnv <- ask
  newEnv <- liftIO $ addDevSecrets baseEnv
  local (const newEnv) action

addDevSecrets :: Env -> IO Env
addDevSecrets baseEnv = do
  DevSecrets {..} <- getDevSecrets
  let appPkPem' = case readRsaPem appPkPem of
        Right a -> a
        Left _ -> error "error reading GitHub App private key"
  pure
    $ baseEnv
      { githubAppAuth = AppAuth (Id $ read appId) appPkPem',
        githubWebhookSecret = githubWebhookSecret
      }

data DevSecrets = DevSecrets
  { appId :: String,
    appPkPem :: SBS,
    githubWebhookSecret :: SBS
  }
  deriving (Show)

getDevSecrets :: IO DevSecrets
getDevSecrets = do
  let getSecret :: Key -> String -> IO Text
      getSecret keyName envVarName = do
        fromEnv <- fmap cs <$> lookupEnv envVarName
        case fromEnv of
          Just secret -> pure secret
          Nothing -> do
            fromFile <- getDevSecretsFromFile
            pure $ fromFile ^. key keyName . _String
  appId <- cs <$> getSecret "github_app_id" "GITHUB_APP_ID"
  appPkPem <- cs <$> getSecret "github_app_pk" "GITHUB_APP_PK"
  githubWebhookSecret <- cs <$> getSecret "github_webhook_secret" "GITHUB_WEBHOOK_SECRET"
  pure $ DevSecrets {appId, appPkPem, githubWebhookSecret}

getDevSecretsFromFile :: IO Value
getDevSecretsFromFile = __memoize $ do
  checkThatInBackendDir
  hPutStrLn stderr "decrypting dev secrets..."
  StdoutRaw secretsYaml <-
    run
      $ cmd "sops"
      & addArgs ["-d", "../secrets/dev.yaml"]
  case decodeEither' secretsYaml of
    Left err -> error $ show err
    Right value -> pure value

{-# NOINLINE __memoizeMVar #-}
__memoizeMVar :: MVar (Maybe Value)
__memoizeMVar = System.IO.Unsafe.unsafePerformIO $ newMVar Nothing

__memoize :: IO Value -> IO Value
__memoize action = do
  modifyMVar __memoizeMVar $ \case
    Just cached -> pure (Just cached, cached)
    Nothing -> do
      result <- action
      pure (Just result, result)

checkThatInBackendDir :: IO ()
checkThatInBackendDir = do
  exists <- doesFileExist "garnix.cabal"
  when (not exists) $ do
    error "garnix.cabal does not exist, are we not in ./backend?"
