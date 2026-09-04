module Garnix where

import Amazonka qualified
import Amazonka.Auth qualified as Amazonka
import Amazonka.S3 qualified as Amazonka
import Control.Concurrent (getNumCapabilities, newMVar)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception qualified
import Control.Exception.Safe qualified as Safe
import Cradle qualified
import Crypto.PubKey.RSA.Read (readRsaPem)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified
import Data.ByteString.Char8 qualified as BSC
import Data.Functor ((<&>))
import Data.HashSet qualified as HashSet
import Data.HashTable.IO qualified as HashTables
import Data.Map.Strict qualified as Map
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
import Garnix.LocalProvisioner (localProvisionerInterface)
import Garnix.Monad
import Garnix.Monad.Metrics (registerMetrics, serveMetrics)
import Garnix.Monad.Pool qualified
import Garnix.NixConfig (defaultNixConfig, githubAccessTokenNixConfig)
import Garnix.Prelude
import Garnix.S3Cache (runCacheMaintenance)
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
import Text.Read (readMaybe)
import WithCli (HasArguments, withCli)
import Garnix.Monad.KeyedMutex (newKeyedMutex)
import Garnix.Hosting.Types (HostingBudget (..))
import Garnix.Hosting.Budget (hostTotalMiB, hostVcpus, parseBudget, resolveBudget)

run :: IO ()
run = withCli runWith

data Options = Options
  { enable :: [String],
    port :: Warp.Port,
    monitoringPort :: Warp.Port,
    metricsPort :: Warp.Port,
    buildLogsDir :: FilePath,
    buildLogsReportingPort :: Maybe Warp.Port
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

sessionLifetimeFromEnv :: IO Duration
sessionLifetimeFromEnv = lookupEnv "GARNIX_SESSION_LIFETIME" >>= resolve
  where
    resolve :: Maybe String -> IO Duration
    resolve Nothing = pure defaultSessionLifetime
    resolve (Just raw) = case readMaybe raw of
      Just seconds | seconds > (0 :: Int) -> pure $ fromSeconds seconds
      _ ->
        error
          $ "GARNIX_SESSION_LIFETIME must be a positive whole number of seconds, got: "
          <> cs raw

evalMemoryConfigFromEnv :: IO EvalMemoryConfig
evalMemoryConfigFromEnv = do
  defaultEvalMemory <-
    lookupEnv "GARNIX_DEFAULT_EVAL_MEMORY_GB"
      <&> maybe
        (defaultRepoConfig ^. maxEvalMemory)
        (fromGigabytes . parseGigabytes "GARNIX_DEFAULT_EVAL_MEMORY_GB" . cs)
  perRepositoryEvalMemory <-
    lookupEnv "GARNIX_REPO_EVAL_MEMORY"
      <&> maybe mempty (Map.fromList . map parseEntry . filter (not . T.null) . map T.strip . T.splitOn "," . cs)
  pure EvalMemoryConfig {defaultEvalMemory, perRepositoryEvalMemory}
  where
    parseGigabytes :: Text -> Text -> Int64
    parseGigabytes var raw = case readMaybe (cs raw) of
      Just gigabytes | gigabytes > 0 -> gigabytes
      _ -> error $ var <> " must be a positive whole number of gigabytes, got: " <> raw

    parseEntry :: Text -> ((GhRepoOwner, GhRepoName), Memory)
    parseEntry entry = case map T.strip (T.splitOn "=" entry) of
      [slug, gigabytes]
        | [owner, name] <- T.splitOn "/" slug,
          not (T.null owner),
          not (T.null name) ->
            ( (GhRepoOwner (GhLogin owner), GhRepoName name),
              fromGigabytes (parseGigabytes "GARNIX_REPO_EVAL_MEMORY" gigabytes)
            )
      _ ->
        error
          $ "GARNIX_REPO_EVAL_MEMORY entries must be owner/name=<gigabytes>, got: "
          <> entry

lookupOptionalSecret :: String -> FilePath -> IO (Maybe Text)
lookupOptionalSecret envVar path = do
  fromEnv <- lookupEnv envVar
  case fromEnv >>= trimmedNonEmpty of
    Just value -> pure $ Just value
    Nothing -> do
      exists <- doesFileExist path
      if exists
        then trimmedNonEmpty <$> readFile path
        else pure Nothing
  where
    trimmedNonEmpty raw =
      let trimmed = T.dropWhileEnd (`elem` ['\n', '\r', ' ', '\t']) (cs raw)
       in if T.null trimmed then Nothing else Just trimmed

lookupEnvText :: String -> IO (Maybe Text)
lookupEnvText name =
  lookupEnv name <&> \case
    Nothing -> Nothing
    Just raw -> let trimmed = T.strip (cs raw) in if T.null trimmed then Nothing else Just trimmed

lookupEnvBool :: String -> Bool -> IO Bool
lookupEnvBool name fallback =
  lookupEnvText name <&> \case
    Nothing -> fallback
    Just raw -> T.toLower raw `elem` ["true", "yes", "1", "on"]

lookupEnvDuration :: String -> Duration -> IO Duration
lookupEnvDuration name fallback =
  lookupEnvText name >>= \case
    Nothing -> pure fallback
    Just raw -> case parseDuration raw of
      Just duration -> pure duration
      Nothing ->
        Control.Exception.throwIO
          $ Control.Exception.ErrorCall
          $ name
          <> " is not a valid duration: "
          <> cs raw

lookupEnvInt :: String -> Int -> IO Int
lookupEnvInt name fallback =
  lookupEnvText name >>= \case
    Nothing -> pure fallback
    Just raw -> case readMaybe (cs raw) of
      Just value | value > 0 -> pure value
      _ ->
        Control.Exception.throwIO
          $ Control.Exception.ErrorCall
          $ name
          <> " must be a positive whole number, got: "
          <> cs raw

gcConfigFromEnv :: IO GcConfig
gcConfigFromEnv = do
  gcEnabled <- lookupEnvBool "S3_CACHE_GC_ENABLED" (gcEnabled defaultGcConfig)
  gcInterval <- lookupEnvDuration "S3_CACHE_GC_INTERVAL" (gcInterval defaultGcConfig)
  retentionPeriod <- lookupEnvDuration "S3_CACHE_GC_RETENTION_PERIOD" (retentionPeriod defaultGcConfig)
  warmupPeriod <- lookupEnvDuration "S3_CACHE_GC_WARMUP_PERIOD" (warmupPeriod defaultGcConfig)
  batchSize <- lookupEnvInt "S3_CACHE_GC_BATCH_SIZE" (batchSize defaultGcConfig)
  deleteConcurrency <- lookupEnvInt "S3_CACHE_GC_DELETE_CONCURRENCY" (deleteConcurrency defaultGcConfig)
  dryRun <- lookupEnvBool "S3_CACHE_GC_DRY_RUN" (dryRun defaultGcConfig)
  pure
    GcConfig
      { gcEnabled,
        gcInterval,
        retentionPeriod,
        warmupPeriod,
        batchSize,
        deleteConcurrency,
        dryRun
      }

withEnv :: (HasCallStack) => Set TestFeature -> FilePath -> Maybe Warp.Port -> (Env -> IO a) -> IO a
withEnv testFeatures buildLogsDir buildLogsReportingPort action = do
  buildLogsDir' <- makeAbsolute buildLogsDir
  secretsDir <- fromMaybe "/run/secrets" <$> lookupEnv "GARNIX_SECRETS_DIR"
  let secretFile name = secretsDir <> "/" <> name
  ghK <-
    lookupEnv "GITHUB_WEBHOOK_SECRET"
      >>= maybe (BSC.readFile (secretFile "github_webhook_secret")) (pure . cs)
  ghClientSecret <-
    lookupEnv "GITHUB_CLIENT_SECRET"
      >>= maybe (cs <$> readFile (secretFile "github_client_secret")) (pure . cs)
  Just emptyDir' <- lookupEnv "EMPTY_DIR"
  ghClientId <-
    lookupEnv "GITHUB_CLIENT_ID"
      >>= maybe (cs <$> readFile (secretFile "github_client_id")) (pure . cs)
  appId <-
    fmap (Id . read)
      $ lookupEnv "GITHUB_APP_ID"
      >>= maybe (readFile (secretFile "github_app_id")) pure
  appPkPem' <-
    lookupEnv "GITHUB_APP_PK"
      >>= maybe (BSC.readFile (secretFile "github_app_pk")) (pure . cs)
  ghAppName <-
    lookupEnv "GITHUB_APP_NAME"
      >>= maybe (cs <$> readFile (secretFile "github_app_name")) (pure . cs)
  adminGhLogin <-
    fmap GhLogin
      <$> lookupOptionalSecret "GARNIX_ADMIN_GITHUB_LOGIN" (secretFile "garnix_admin_github_login")
  nixConfig <-
    lookupOptionalSecret "GITHUB_ACCESS_TOKEN" (secretFile "github_access_token")
      <&> maybe defaultNixConfig (\token -> githubAccessTokenNixConfig (GhToken token) <> defaultNixConfig)
  s3CacheEnabled <-
    lookupEnv "S3_CACHE_ENABLED" <&> \case
      Just v | T.toLower (cs v) == "false" -> False
      _ -> True
  s3CacheEnv <-
    if s3CacheEnabled
      then do
        amazonkaEnv <- do
          accessKeyId <-
            ( lookupEnv "S3_CACHE_ACCESS_KEY_ID"
                >>= maybe (BSC.readFile (secretFile "s3-cache-access-key-id")) (pure . cs)
              )
              <&> Amazonka.AccessKey
          secretAccessKey <-
            ( lookupEnv "S3_CACHE_SECRET_ACCESS_KEY"
                >>= maybe (BSC.readFile (secretFile "s3-cache-secret-access-key")) (pure . cs)
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
        publicRepoOwners <-
          lookupEnv "S3_CACHE_PUBLIC_REPO_OWNERS"
            <&> maybe mempty (Set.fromList . filter (not . T.null) . map (T.toLower . T.strip) . T.splitOn "," . cs)
        cachePrivKeyFile <-
          lookupEnv "CACHE_PRIV_KEY_FILE"
            <&> fromMaybe (secretFile "cache-priv-key")
        cachePrivKeyName <- do
          cachePrivKey <- T.readFile cachePrivKeyFile
          case T.split (== ':') (cs cachePrivKey) of
            [name, _key] -> pure name
            _ -> Control.Exception.throwIO $ Control.Exception.ErrorCall "cannot parse cachePrivKey"
        let expiration = fromHours @Int 2
        let maxUploadSize = 4 * 2 ^ (30 :: Integer)
        isInNixosCacheMemoTable <- HashTables.new >>= newMVar
        accessBuffer <- newTVarIO HashSet.empty
        accessFlushEvery <- lookupEnvDuration "S3_CACHE_ACCESS_FLUSH_EVERY" defaultAccessFlushEvery
        accessFlushMax <- lookupEnvInt "S3_CACHE_ACCESS_FLUSH_MAX" defaultAccessFlushMax
        accessBumpMinAge <- lookupEnvDuration "S3_CACHE_ACCESS_BUMP_MIN_AGE" defaultAccessBumpMinAge
        gc <- gcConfigFromEnv
        pure
          $ S3CacheEnv
            { amazonkaEnv,
              publicBucket,
              publicBaseUrl,
              privateBucket,
              publicRepoOwners,
              cachePrivKeyFile,
              cachePrivKeyName,
              expiration,
              maxUploadSize,
              isInNixosCacheMemoTable,
              accessBuffer,
              accessFlushEvery,
              accessFlushMax,
              accessBumpMinAge,
              gc
            }
      else do
        amazonkaEnv <-
          Amazonka.newEnv (pure . Amazonka.fromKeys (Amazonka.AccessKey "") (Amazonka.SecretKey ""))
            <&> (#region .~ Amazonka.Region' "auto")
        isInNixosCacheMemoTable <- HashTables.new >>= newMVar
        accessBuffer <- newTVarIO HashSet.empty
        pure
          $ S3CacheEnv
            { amazonkaEnv,
              publicBucket = Amazonka.BucketName "",
              publicBaseUrl = "",
              privateBucket = Amazonka.BucketName "",
              publicRepoOwners = mempty,
              cachePrivKeyFile = "",
              cachePrivKeyName = "",
              expiration = fromHours @Int 2,
              maxUploadSize = 4 * 2 ^ (30 :: Integer),
              isInNixosCacheMemoTable,
              accessBuffer,
              accessFlushEvery = defaultAccessFlushEvery,
              accessFlushMax = defaultAccessFlushMax,
              accessBumpMinAge = defaultAccessBumpMinAge,
              gc = defaultGcConfig
            }
  actionServerUrl <- fromMaybe "action-runner2.garnix.io" <$> lookupEnv "GARNIX_ACTION_HOST"
  actionRunnerSshKey <- lookupEnv "GARNIX_ACTION_RUNNER_SSH_KEY" >>= maybe (pure (secretFile "garnix_action_runner_ssh")) makeAbsolute
  sharedResourcesUsers <-
    lookupEnv "GARNIX_SHARED_RESOURCES_USERS"
      <&> maybe [] (filter (not . T.null) . map (T.toLower . T.strip) . T.splitOn "," . cs)
  curDir <- getCurrentDirectory
  let appPkPem = case readRsaPem appPkPem' of
        Right a -> a
        Left _ -> error "error reading GitHub App private key"
  mgr <- newTlsManager
  sessionLifetime <- sessionLifetimeFromEnv
  evalMemoryConfig <- evalMemoryConfigFromEnv
  jwtKey <-
    lookupEnv "JWT_KEY"
      >>= maybe (BSC.readFile (secretFile "garnix-jwt-key")) BSC.readFile
      <&> fromSecret . B64.decodeLenient
  burl <-
    lookupEnv "GARNIX_URL" >>= \case
      Nothing -> pure "https://app.garnix.io"
      Just u -> pure u
  opensearchQueryUrl <- fromMaybe "https://opensearch.garnix.io/_msearch" <$> lookupEnv "OPENSEARCH_URL"
  opensearchPass <-
    lookupEnv "OPENSEARCH_API"
      >>= maybe (BSC.readFile (secretFile "opensearch-garnix")) (pure . cs)
  dbPass <- do
    p <-
      lookupEnv "PGPASSWORD"
        >>= maybe (BSC.readFile (secretFile "database-password")) (pure . cs)
    pure $ Data.ByteString.Char8.words p
  repoSecretsKeyPath <-
    RepoSecretsEncryptionKeyPath
      . fromMaybe (secretFile "repo-secrets-key")
      <$> lookupEnv "REPO_SECRETS_KEY_PATH"
  repoSecretsPubKey <-
    fmap RepoSecretsEncryptionPubKey
      $ lookupEnv "REPO_SECRETS_PUB_KEY"
      >>= maybe (T.readFile (secretFile "repo-secrets-key-pub")) (pure . cs)
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
  compressionBudget <- newCompressionBudget
  provisionerSocket <- lookupEnv "GARNIX_PROVISIONER_SOCKET"
  let provisioner =
        maybe unconfiguredProvisioner localProvisionerInterface provisionerSocket
  hostingDomain <- cs . fromMaybe "" <$> lookupEnv "GARNIX_HOSTING_DOMAIN"
  statsReportUrl <- fmap cs <$> lookupEnv "GARNIX_STATS_REPORT_URL"
  deployMutex <- newKeyedMutex
  hostingSshKeys <-
    maybe [] (map cs . filter (not . T.null) . T.splitOn ":" . cs)
      <$> lookupEnv "GARNIX_HOSTING_SSH_KEYS"
  hostingBudget <- do
    vcpuSpec <- (>>= parseBudget) . fmap cs <$> lookupEnv "GARNIX_HOSTING_VCPU_BUDGET"
    memorySpec <- (>>= parseBudget) . fmap cs <$> lookupEnv "GARNIX_HOSTING_MEMORY_BUDGET"
    HostingBudget
      <$> (flip resolveBudget vcpuSpec <$> hostVcpus)
      <*> (flip resolveBudget memorySpec . fromMaybe 0 <$> hostTotalMiB)
  withDefaultLogger $ \defaultLogger -> do
    let env =
          Env
            { testFeatures = testFeatures,
              githubAppAuth = AppAuth appId appPkPem,
              githubAppId = appId,
              githubAppName = ghAppName,
              githubClientSecret = ghClientSecret,
              githubClientId = ghClientId,
              adminGithubLogin = adminGhLogin,
              buildLogsReportingPort = buildLogsReportingPort,
              workingDir = curDir,
              nixXdgCacheDir = Nothing,
              userNixConfig = nixConfig,
              evalMemoryConfig = evalMemoryConfig,
              githubWebhookSecret = ghK,
              githubInterface = realGithubInterface,
              cookieSettings =
                defaultCookieSettings
                  { cookieXsrfSetting = Nothing,
                    cookieIsSecure = if DevApi `elem` testFeatures then NotSecure else Secure
                  },
              jwtSettings = defaultJWTSettings jwtKey,
              sessionLifetime,
              repoSecretsEncryptionKeyPath = repoSecretsKeyPath,
              repoSecretsEncryptionPubKey = repoSecretsPubKey,
              dbConn = dbConnectionPool,
              manager = mgr,
              baseUrl = cs burl,
              logger = defaultLogger,
              buildLogsDir = buildLogsDir',
              opensearchQueryUrl = opensearchQueryUrl,
              opensearchPassword = opensearchPass,
              s3CacheEnabled,
              s3CacheEnv,
              action =
                ActionEnv
                  { runnerHost = cs actionServerUrl,
                    runnerSshKey = cs actionRunnerSshKey,
                    timeoutDuration = fromHours @Int 2,
                    sharedResourcesUsers
                  },
              nixEvalPool = nixEvalPool,
              s3UploadPool = s3UploadPool,
              compressionBudget,
              mocks = mocks,
              spanCtx = [],
              metrics = metrics,
              emptyDir = emptyDir',
              hostname = hostname,
              githubLogDebounceDuration = fromSeconds @Int 15,
              featureFlagConfig,
              fodCheckPool,
              provisioner,
              provisionerSocket,
              hostingDomain,
              statsReportUrl,
              deployMutex,
              hostingBudget,
              hostingSshKeys
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
      runM env runCacheMaintenance >>= \case
        Right () -> pure ()
        Left err -> hPutStrLn stderr $ "could not start s3 cache maintenance: " <> show err
      let settings =
            Warp.defaultSettings
              & Warp.setPort (port opts)
              & Warp.setBeforeMainLoop
                ( do
                    hPutStrLn stderr $ "Listening on port " <> show (port opts)
                    void notifyReady
                )
      Warp.runSettings settings $ Garnix.toApplication env

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
