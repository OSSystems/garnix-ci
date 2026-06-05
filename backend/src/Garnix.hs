module Garnix where

import Amazonka qualified
import Amazonka.Auth qualified as Amazonka
import Amazonka.S3 qualified as Amazonka
import Control.Concurrent (forkIO, getNumCapabilities, newMVar)
import Control.Exception qualified
import Control.Exception.Safe qualified as Safe
import Cradle qualified
import Crypto.PubKey.RSA.Read (readRsaPem)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified
import Data.ByteString.Char8 qualified as BSC
import Data.Functor ((<&>))
import Data.HashTable.IO qualified as HashTables
import Data.Pool qualified as Pool
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import Data.Text.IO qualified as T
import Database.PostgreSQL.Typed (pgDisconnect)
import GHC.Conc (getNumProcessors)
import Garnix.API
import Garnix.DB qualified as DB
import Garnix.DB.FeatureFlags (withRecachedFeatureFlags)
import Garnix.DB.FeatureFlags.Types (getFeatureFlagConfig)
import Garnix.Duration
import Garnix.GithubInterface
import Garnix.HetznerInterface
import Garnix.Hosting.Deploy (stopUnusedServers)
import Garnix.Hosting.ServerPool qualified as ServerPool
import Garnix.Hosting.ServerPool.Types
import Garnix.Monad
import Garnix.Monad.Metrics (registerMetrics, serveMetrics)
import Garnix.Monad.Pool qualified
import Garnix.NixConfig (defaultNixConfig)
import Garnix.Prelude
import Garnix.Types
import Garnix.UserLogs
import GitHub.App.Auth (AppAuth (..))
import GitHub.Data.Id (Id (..))
import GitHub.Data.Webhooks.Events
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Gzip
import Servant
import Servant.Auth.Server
  ( CookieSettings (..),
    JWTSettings,
    defaultCookieSettings,
    defaultJWTSettings,
    fromSecret,
  )
import Servant.GitHub.Webhook
import System.Directory
import System.Environment (getEnv)
import System.Systemd.Daemon (notifyReady)
import WithCli (HasArguments, withCli)

run :: IO ()
run = withCli runWith

data Options = Options
  { enable :: [String],
    port :: Warp.Port,
    monitoringPort :: Warp.Port,
    metricsPort :: Warp.Port,
    buildLogsDir :: FilePath,
    buildLogsReportingPort :: Maybe Warp.Port,
    provisionServerPool :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasArguments)

envMocks :: Set TestFeature -> IO (Maybe EnvMocks)
envMocks testFeatures = do
  foldM helper Nothing testFeatures
  where
    helper :: Maybe EnvMocks -> TestFeature -> IO (Maybe EnvMocks)
    helper mEnvMocks testFeature = case testFeature of
      DevApi -> pure mEnvMocks
      OpenSearchMocks -> do
        let envMocks = fromMaybe emptyMocks mEnvMocks
        (storeLogLineMock, queryOpenSearchMock) <- Garnix.UserLogs.testImplementation
        pure
          $ Just
          $ envMocks
            { storeLogLineMock = Just storeLogLineMock,
              queryOpenSearchMock = Just queryOpenSearchMock
            }
      CacheUploadMocks -> do
        let envMocks = fromMaybe emptyMocks mEnvMocks
        s3UploadMock <- newMock (\_ -> pure ())
        pure
          $ Just
          $ envMocks
            { s3CacheUploadMock = Just s3UploadMock
            }
      FodCheckMocks -> do
        let envMocks = fromMaybe emptyMocks mEnvMocks
        fodCheckMock <- newMock (\_ -> pure ())
        pure $ Just $ envMocks {fodCheckMock = Just fodCheckMock}

withEnv :: (HasCallStack) => Set TestFeature -> FilePath -> Maybe Warp.Port -> (Env -> IO a) -> IO a
withEnv testFeatures buildLogsDir buildLogsReportingPort action = do
  buildLogsDir' <- makeAbsolute buildLogsDir
  ghK <-
    lookupEnv "GITHUB_WEBHOOK_SECRET"
      >>= maybe (BSC.readFile "/run/secrets/github_webhook_secret") (pure . cs)
  ghClientSecret <-
    lookupEnv "GITHUB_CLIENT_SECRET"
      >>= maybe (cs <$> readFile "/run/secrets/github_client_secret") (pure . cs)
  Just emptyDir' <- lookupEnv "EMPTY_DIR"
  ghClientId <-
    lookupEnv "GITHUB_CLIENT_ID"
      >>= maybe (cs <$> readFile "/run/secrets/github_client_id") (pure . cs)
  appId <-
    fmap (Id . read)
      $ lookupEnv "GITHUB_APP_ID"
      >>= maybe (readFile "/run/secrets/github_app_id") pure
  appPkPem' <-
    lookupEnv "GITHUB_APP_PK"
      >>= maybe (BSC.readFile "/run/secrets/github_app_pk") (pure . cs)
  ghAppName <-
    lookupEnv "GITHUB_APP_NAME"
      >>= maybe (cs <$> readFile "/run/secrets/github_app_name") (pure . cs)
  sshKeys <- do
    envValue <- lookupEnv "GARNIX_SERVER_SSH_KEYS"
    let sshKeyPaths = case envValue of
          Nothing -> ["/run/secrets/garnix_server_ssh_hosting"]
          Just v -> map cs $ T.splitOn "," (cs v)
    forM sshKeyPaths $ \sshKey -> do
      doesFileExist sshKey >>= \exists ->
        unless exists
          $ error
          $ "ssh key not found: "
          <> cs sshKey
      liftIO $ makeAbsolute sshKey
  s3CacheEnv <- do
    amazonkaEnv <- do
      accessKeyId <-
        ( lookupEnv "S3_CACHE_ACCESS_KEY_ID"
            >>= maybe (BSC.readFile "/run/secrets/s3-cache-access-key-id") (pure . cs)
          )
          <&> Amazonka.AccessKey
      secretAccessKey <-
        ( lookupEnv "S3_CACHE_SECRET_ACCESS_KEY"
            >>= maybe (BSC.readFile "/run/secrets/s3-cache-secret-access-key") (pure . cs)
          )
          <&> Amazonka.SecretKey
      region <- cs <$> getEnv "S3_CACHE_REGION"
      host <- cs <$> getEnv "S3_CACHE_HOST"
      Amazonka.newEnv (pure . Amazonka.fromKeys accessKeyId secretAccessKey)
        <&> (#region .~ Amazonka.Region' region)
        <&> Amazonka.overrideService (Amazonka.setEndpoint True host 443)
        <&> Amazonka.overrideService (#s3AddressingStyle .~ Amazonka.S3AddressingStylePath)
    publicBucket <- Amazonka.BucketName . cs <$> getEnv "S3_CACHE_PUBLIC_BUCKET"
    publicBaseUrl <-
      getEnv "S3_CACHE_PUBLIC_BASE_URL"
        <&> cs . (\url -> if "/" `isSuffixOf` url then url else url <> "/")
    privateBucket <- Amazonka.BucketName . cs <$> getEnv "S3_CACHE_PRIVATE_BUCKET"
    cachePrivKeyFile <-
      lookupEnv "CACHE_PRIV_KEY_FILE"
        <&> fromMaybe "/run/secrets/cache-priv-key"
    cachePrivKeyName <- do
      cachePrivKey <- T.readFile cachePrivKeyFile
      case T.split (== ':') (cs cachePrivKey) of
        [name, _key] -> pure name
        _ -> Control.Exception.throwIO $ Control.Exception.ErrorCall "cannot parse cachePrivKey"
    let expiration = fromHours @Int 2
    let maxUploadSize = 4 * 2 ^ (30 :: Integer)
    isInNixosCacheMemoTable <- HashTables.new >>= newMVar
    pure
      $ S3CacheEnv
        { amazonkaEnv,
          publicBucket,
          publicBaseUrl,
          privateBucket,
          cachePrivKeyFile,
          cachePrivKeyName,
          expiration,
          maxUploadSize,
          isInNixosCacheMemoTable
        }
  actionServerUrl <- fromMaybe "action-runner2.garnix.io" <$> lookupEnv "GARNIX_ACTION_HOST"
  actionRunnerSshKey <- lookupEnv "GARNIX_ACTION_RUNNER_SSH_KEY" >>= maybe (pure "/run/secrets/garnix_action_runner_ssh") makeAbsolute
  curDir <- getCurrentDirectory
  let appPkPem = case readRsaPem appPkPem' of
        Right a -> a
        Left _ -> error "error reading GitHub App private key"
  mgr <- newTlsManager
  jwtKey <-
    lookupEnv "JWT_KEY"
      >>= maybe (BSC.readFile "/run/secrets/garnix-jwt-key") BSC.readFile
      <&> fromSecret . B64.decodeLenient
  burl <-
    lookupEnv "GARNIX_URL" >>= \case
      Nothing -> pure "https://app.garnix.io"
      Just u -> pure u
  hetznerTok <-
    lookupEnv "HETZNER_TOKEN"
      >>= maybe (BSC.readFile "/run/secrets/hetzner-token") (pure . cs)
  opensearchQueryUrl <- fromMaybe "https://opensearch.garnix.io/_msearch" <$> lookupEnv "OPENSEARCH_URL"
  opensearchPass <-
    lookupEnv "OPENSEARCH_API"
      >>= maybe (BSC.readFile "/run/secrets/opensearch-garnix") (pure . cs)
  dbPass <- do
    p <-
      lookupEnv "PGPASSWORD"
        >>= maybe (BSC.readFile "/run/secrets/database-password") (pure . cs)
    pure $ Data.ByteString.Char8.words p
  repoSecretsKeyPath <-
    RepoSecretsEncryptionKeyPath
      . fromMaybe "/run/secrets/repo-secrets-key"
      <$> lookupEnv "REPO_SECRETS_KEY_PATH"
  repoSecretsPubKey <-
    fmap RepoSecretsEncryptionPubKey
      $ lookupEnv "REPO_SECRETS_PUB_KEY"
      >>= maybe (T.readFile "/run/secrets/repo-secrets-key-pub") (pure . cs)
  dbConnectionPool <-
    ConnectionPool
      <$> Pool.newPool
        ( Pool.setNumStripes (Just 2)
            $ Pool.defaultPoolConfig
              (DB.getDBConnection dbPass)
              pgDisconnect
              60
              10
        )
  metrics <- registerMetrics
  nixEvalPool <- Garnix.Monad.Pool.newPool 50 metrics #evalQueueWaitTime #evalQueueLen
  s3UploadPool <- Garnix.Monad.Pool.newPool 100 metrics #s3QueueWaitTime #s3QueueLen
  Cradle.StdoutTrimmed hostname <- Cradle.run $ Cradle.cmd "hostname"
  mocks <- envMocks testFeatures
  featureFlagConfig <- getFeatureFlagConfig
  fodCheckPool <- Garnix.Monad.Pool.newPool 20 metrics #fodCheckQueueWaitTime #fodCheckQueueLen
  withDefaultLogger $ \defaultLogger -> do
    let env =
          Env
            { testFeatures = testFeatures,
              githubAppAuth = AppAuth appId appPkPem,
              githubAppId = appId,
              githubAppName = ghAppName,
              githubClientSecret = ghClientSecret,
              githubClientId = ghClientId,
              buildLogsReportingPort = buildLogsReportingPort,
              workingDir = curDir,
              nixXdgCacheDir = Nothing,
              userNixConfig = defaultNixConfig,
              githubWebhookSecret = ghK,
              githubInterface = realGithubInterface,
              hetznerInterface = realHetznerInterface,
              serverPoolConfig =
                [ (I2x4, 10),
                  (I4x8, 2),
                  (I8x16, 1),
                  (I16x32, 1)
                ],
              cookieSettings =
                defaultCookieSettings
                  { cookieXsrfSetting = Nothing,
                    cookieIsSecure = if DevApi `elem` testFeatures then NotSecure else Secure
                  },
              jwtSettings = defaultJWTSettings jwtKey,
              repoSecretsEncryptionKeyPath = repoSecretsKeyPath,
              repoSecretsEncryptionPubKey = repoSecretsPubKey,
              dbConn = dbConnectionPool,
              manager = mgr,
              baseUrl = cs burl,
              logger = defaultLogger,
              buildLogsDir = buildLogsDir',
              hetznerToken = hetznerTok,
              opensearchQueryUrl = opensearchQueryUrl,
              opensearchPassword = opensearchPass,
              sshUserHostingKeys = sshKeys,
              s3CacheEnv,
              action =
                ActionEnv
                  { runnerHost = cs actionServerUrl,
                    runnerSshKey = cs actionRunnerSshKey,
                    timeoutDuration = fromHours @Int 2
                  },
              nixEvalPool = nixEvalPool,
              s3UploadPool = s3UploadPool,
              mocks = mocks,
              spanCtx = [],
              metrics = metrics,
              emptyDir = emptyDir',
              hostname = hostname,
              githubLogDebounceDuration = fromSeconds @Int 15,
              featureFlagConfig,
              fodCheckPool
            }
    action env

runWith :: Options -> IO ()
runWith opts = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  testFeatures <- case mapM parseTestFeature $ enable opts of
    Right testFeatures -> pure $ Set.fromList testFeatures
    Left err -> Control.Exception.throwIO $ Control.Exception.ErrorCall $ cs err
  hPutStrLn stderr $ "Test features: " <> if Set.null testFeatures then "none" else T.intercalate ", " (fmap show (toList testFeatures))
  do
    n <- getNumProcessors
    hPutStrLn stderr $ "number of processors: " <> show n
    n <- getNumCapabilities
    hPutStrLn stderr $ "number of capabilities: " <> show n
  withEnv
    testFeatures
    (Garnix.buildLogsDir opts)
    (Garnix.buildLogsReportingPort opts)
    $ \env -> do
      serveMetrics (Garnix.metricsPort opts) (env ^. #metrics)
      void . forkIO . void . runM env $ forever (stopUnusedServers' *> threadDelay (fromMinutes @Int 5))
      if Garnix.provisionServerPool opts
        then void $ runM env ServerPool.initializeProvisioningPool
        else hPutStrLn stderr "Not provisioning server pool"
      let settings =
            Warp.defaultSettings
              & Warp.setPort (port opts)
              & Warp.setBeforeMainLoop
                ( do
                    hPutStrLn stderr $ "Listening on port " <> show (port opts)
                    void notifyReady
                )
      Warp.runSettings settings $ Garnix.toApplication env
  where
    stopUnusedServers' :: M ()
    stopUnusedServers' = stopUnusedServers `catchError` (\e -> log Error $ "stopUnusedServer error: " <> show e)

type ContextList =
  '[ JWTSettings,
     CookieSettings,
     GitHubKey CheckSuiteEvent,
     GitHubKey CheckRunEvent,
     GitHubKey PullRequestEvent,
     GitHubKey PushEvent
   ]

toApplication :: Env -> Application
toApplication env =
  let ghKey :: GitHubKey a
      ghKey = gitHubKey . pure $ env ^. #githubWebhookSecret
      context :: Context ContextList
      context =
        (env ^. #jwtSettings)
          :. (env ^. #cookieSettings)
          :. ghKey
          :. ghKey
          :. ghKey
          :. ghKey
          :. EmptyContext
      contextProxy :: Proxy ContextList
      contextProxy = Proxy
   in gzip gzipSettings
        $ logRequestsMiddleware
          env
          ( \requestTraceId ->
              serveWithContext api context
                $ hoistServerWithContext api contextProxy (mToHandler env requestTraceId) (toServant wholeAPI)
          )

gzipSettings :: GzipSettings
gzipSettings = defaultGzipSettings {gzipFiles = GzipPreCompressed GzipIgnore}

mToHandler :: Env -> RequestTraceId -> M a -> Servant.Handler a
mToHandler env requestTraceId action = do
  r <- liftIO $ runM env $ do
    withSpan requestTraceId $ do
      logThrownErrors $ do
        turnRuntimeExceptionsIntoMonadicErrors $ do
          withRecachedFeatureFlags $ do
            action
  case r of
    Right v -> pure v
    Left e -> throwError $ servantizeError e
  where
    turnRuntimeExceptionsIntoMonadicErrors :: M a -> M a
    turnRuntimeExceptionsIntoMonadicErrors action =
      action
        `Safe.catch` ( \(e :: SomeException) ->
                         throw $ UncaughtRuntimeException (show e)
                     )
