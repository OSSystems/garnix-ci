{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Garnix.Monad
  ( module Garnix.Monad,
    Severity (..),
  )
where

import Amazonka.Env qualified as Amazonka (Env)
import Amazonka.S3 qualified as Amazonka
import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Exception.Safe qualified as SafeException
import Control.Lens (IndexedTraversal')
import Control.Lens.Regex.Text qualified as RE
import Control.Monad.Base (MonadBase)
import Data.Aeson (Value, eitherDecode')
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Containers.ListUtils (nubOrd)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Format.Numbers (PrettyCfg (PrettyCfg), prettyF)
import Data.Time (Day)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.UUID qualified
import Data.UUID.V4 qualified
import GHC.Conc (getNumProcessors)
import Garnix.Async (Promise)
import Garnix.Build.Types (EvaluationResult)
import Garnix.BuildLogs.Types (LogLine)
import Garnix.DB.FeatureFlags.Types (FeatureFlagConfig)
import Garnix.Duration
import Garnix.GithubInterface.Types
import Garnix.Log
import Garnix.Monad.ForkT
import Garnix.Monad.Memoization (MemoTable)
import Garnix.Monad.Metrics (Metrics, incrementEvent)
import Garnix.Monad.Pool (Pool)
import Garnix.Nix.Types (StoreHash)
import Garnix.Nix.Types qualified as Nix
import Garnix.Prelude
import Garnix.Types hiding (ghRunId, statusCode)
import GitHub qualified as GH
import GitHub.App.Auth (InstallationAuth)
import GitHub.App.Auth qualified as GHA
import GitHub.Data (Id)
import GitHub.Data.Apps (App)
import GitHub.Data.Installations qualified as GHA
import Network.HTTP.Client (Manager)
import Network.HTTP.Types (statusCode)
import Network.Wai qualified as Wai
import Network.Wai.Header (contentLength)
import Network.Wreq qualified as Wreq
import Servant.Auth.Server (CookieSettings, JWTSettings)
import System.Directory (canonicalizePath)
import System.Log.FastLogger as FastLogger
import Text.Read (readMaybe)

data CompressionBudget = CompressionBudget
  { unreservedThreads :: TVar Int,
    perCompressionLimit :: Int
  }

minimumConcurrentCompressions :: Int
minimumConcurrentCompressions = 4

compressionThreadLimit :: Int -> Int
compressionThreadLimit totalThreads =
  max 1 (totalThreads `div` minimumConcurrentCompressions)

newCompressionBudget :: (MonadIO m) => m CompressionBudget
newCompressionBudget = liftIO $ do
  totalThreads <- getNumProcessors
  unreservedThreads <- newTVarIO totalThreads
  pure
    CompressionBudget
      { unreservedThreads,
        perCompressionLimit = compressionThreadLimit totalThreads
      }

data Env = Env
  { testFeatures :: Set TestFeature,
    githubAppAuth :: GHA.AppAuth,
    githubAppName :: Text,
    githubAppId :: Id App,
    githubWebhookSecret :: ByteString,
    manager :: Manager,
    githubClientSecret :: Text,
    githubClientId :: Text,
    adminGithubLogin :: Maybe GhLogin,
    buildLogsReportingPort :: Maybe Int,
    githubInterface :: GithubInterface,
    -- | A thread-safe version of `CWD`
    workingDir :: FilePath,
    nixXdgCacheDir :: Maybe FilePath,
    userNixConfig :: NixConfig,
    cookieSettings :: CookieSettings,
    jwtSettings :: JWTSettings,
    sessionLifetime :: Duration,
    dbConn :: DatabaseConnection,
    baseUrl :: Text,
    s3CacheEnabled :: Bool,
    s3CacheEnv :: S3CacheEnv,
    action :: ActionEnv,
    repoSecretsEncryptionKeyPath :: RepoSecretsEncryptionKeyPath,
    repoSecretsEncryptionPubKey :: RepoSecretsEncryptionPubKey,
    logger :: LogItem -> IO (),
    buildLogsDir :: FilePath,
    opensearchQueryUrl :: String,
    opensearchPassword :: ByteString,
    nixEvalPool :: Garnix.Monad.Pool.Pool GhRepoOwner,
    s3UploadPool :: Garnix.Monad.Pool.Pool GhRepoOwner,
    mocks :: Maybe EnvMocks,
    emptyDir :: FilePath,
    spanCtx :: [(Text, Text)],
    metrics :: Metrics,
    hostname :: Text,
    githubLogDebounceDuration :: Duration,
    featureFlagConfig :: FeatureFlagConfig,
    fodCheckPool :: Garnix.Monad.Pool.Pool (),
    compressionBudget :: CompressionBudget
  }
  deriving stock (Generic)

defaultSessionLifetime :: Duration
defaultSessionLifetime = fromDays @Int 7

data TestFeature
  = DevApi
  | OpenSearchMocks
  | CacheUploadMocks
  | FodCheckMocks
  deriving stock (Eq, Show, Read, Ord, Generic, Enum, Bounded)

localDevelopment :: Set TestFeature
localDevelopment = Set.fromList [minBound .. maxBound]

parseTestFeature :: String -> Either Text TestFeature
parseTestFeature feature = case readMaybe feature of
  Just feature -> Right feature
  Nothing ->
    Left
      $ "unknown test feature passed to --enable: "
      <> cs feature
      <> ". Possible values: "
      <> T.intercalate ", " (map show [minBound .. maxBound :: TestFeature])

data S3CacheEnv = S3CacheEnv
  { amazonkaEnv :: Amazonka.Env,
    publicBucket :: Amazonka.BucketName,
    publicBaseUrl :: Text,
    privateBucket :: Amazonka.BucketName,
    cachePrivKeyFile :: FilePath,
    cachePrivKeyName :: Text,
    expiration :: Duration,
    maxUploadSize :: Integer,
    isInNixosCacheMemoTable :: MVar (MemoTable StoreHash Bool)
  }
  deriving (Generic)

data ActionEnv = ActionEnv
  { runnerHost :: Text,
    runnerSshKey :: Text,
    timeoutDuration :: Duration,
    sharedResourcesUsers :: [Text]
  }
  deriving (Generic)

data EnvMocks = EnvMocks
  { buildFlakeMock ::
      Maybe
        (Mock (Reporter, CommitInfo) (Promise ())),
    storeLogLineMock :: Maybe (Mock (OpenSearchId, LogLine) ()),
    queryOpenSearchMock ::
      Maybe
        (Mock (OpenSearchId, [Day], Maybe UTCTime, Int) [OpenSearchMessage]),
    makeOpenSearchMsearchRequestMock ::
      Maybe
        (Mock (Value, Value) BSL.ByteString),
    getBuildPlanMock :: Maybe (Mock ByteString Nix.Plan),
    buildPkgMock :: Maybe (Mock (Maybe FodChecker, RunReporter, BuildKind, FlakeDir, RepoConfig, Build) Build),
    s3CacheUploadMock :: Maybe (Mock (RunReporter, GhRepoOwner, GhRepoName, EvaluationResult, RepoPublicity) ()),
    fodCheckMock :: Maybe (Mock (Maybe FodChecker, Nix.DrvPath) ()),
    rebuildFodMock :: Maybe (Mock (System, Nix.DrvPath) (Either Text Text))
  }
  deriving (Generic)

emptyMocks :: EnvMocks
emptyMocks =
  EnvMocks
    { buildFlakeMock = Nothing,
      storeLogLineMock = Nothing,
      queryOpenSearchMock = Nothing,
      makeOpenSearchMsearchRequestMock = Nothing,
      getBuildPlanMock = Nothing,
      buildPkgMock = Nothing,
      s3CacheUploadMock = Nothing,
      fodCheckMock = Nothing,
      rebuildFodMock = Nothing
    }

data Mock arg result = Mock
  { mockImplementation :: arg -> M result,
    calls :: MVar [arg]
  }

relativeUrlConverter :: M (Text -> Text)
relativeUrlConverter = do
  burl <- asks baseUrl
  pure $ \end ->
    if
      | "/" `T.isPrefixOf` end && "/" `T.isSuffixOf` burl -> burl <> T.drop 1 end
      | "/" `T.isPrefixOf` end || "/" `T.isSuffixOf` burl -> burl <> end
      | otherwise -> burl <> "/" <> end

newtype M a = M {runM' :: ExceptT ErrorWithContext (ReaderT Env (ForkT IO)) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError ErrorWithContext,
      MonadIO,
      MonadBase IO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader Env,
      MonadBaseControl IO,
      HasForkT
    )

runM :: Env -> M a -> IO (Either ErrorWithContext a)
runM env (M action) = runForkT (runReaderT (runExceptT action) env)

newtype FlakeDir = FlakeDir {__unsafeGetFlakeDir :: FilePath}
  deriving newtype (Eq, Show, Generic)

newtype ReportSummary = ReportSummary {getReportSummary :: Text}
  deriving stock (Show, Eq)

data RunReporter = RunReporter
  { reportLogs :: LogLine -> M (),
    reportComplete :: RunReportStatus -> M (),
    ghRunId :: Maybe GhRunId
  }

instance Semigroup RunReporter where
  a <> b =
    RunReporter
      { reportLogs = \logs -> reportLogs a logs >> reportLogs b logs,
        reportComplete = \status -> reportComplete a status >> reportComplete b status,
        ghRunId = ghRunId a <|> ghRunId b
      }

instance Monoid RunReporter where
  mempty =
    RunReporter
      { reportLogs = \_ -> pure (),
        reportComplete = \_ -> pure (),
        ghRunId = Nothing
      }

data ReportType
  = ReportBuild {_reportTypeName :: Text, _reportTypeBuild :: Build}
  | ReportRun {_reportTypeRun :: Run}
  | MetaCheck
  deriving stock (Show, Eq)

reportName :: ReportType -> Text
reportName = \case
  ReportBuild name _ -> name
  ReportRun run -> run ^. name
  MetaCheck -> "All Garnix checks"

newtype Reporter = Reporter {createNewRun :: ReportType -> M RunReporter}
  deriving stock (Show)

instance Semigroup Reporter where
  a <> b =
    Reporter
      { createNewRun = \reportType ->
          (<>) <$> createNewRun a reportType <*> createNewRun b reportType
      }

instance Monoid Reporter where
  mempty = Reporter {createNewRun = \_ -> pure mempty}

data FodChecker = FodChecker
  { runReporter :: RunReporter,
    totalSkipped :: MVar Int,
    totalVerified :: MVar Int,
    promises :: MVar (Maybe [Promise (Either [(Nix.DrvPath, Text)] ())]),
    startedOrDone :: MVar (Set Nix.DrvPath)
  }
  deriving (Generic)

-- * Github interface

data GhCollaborators
  = GhCollaborators [GhLogin]
  | RepoNotFound
  deriving (Show)

newtype RemoteUrl = RemoteUrl Text

realRemoteUrl :: RemoteUrl -> Text
realRemoteUrl (RemoteUrl url) = url

data GithubInterface = GithubInterface
  { _githubInterfaceGetInstallation :: (HasCallStack) => GH.Id GHA.Installation -> M GHA.InstallationAuth,
    _githubInterfaceGetInstallations :: (HasCallStack) => GhToken -> M [GH.Id GHA.Installation],
    _githubInterfaceGetGarnixInstallationId :: (HasCallStack) => GhRepoOwner -> GhRepoName -> M (Maybe Integer),
    _githubInterfaceGetAccessToken :: (HasCallStack) => GHA.InstallationAuth -> M GhToken,
    _githubInterfaceGetDefaultBranch :: (HasCallStack) => Maybe GHA.InstallationAuth -> GhRepoOwner -> GhRepoName -> M (Maybe Branch),
    _githubInterfaceGetHeadCommit :: (HasCallStack) => GhToken -> GhRepoOwner -> GhRepoName -> Branch -> M CommitHash,
    _githubInterfaceNewBuildReport :: (HasCallStack) => RepoInfo -> GhRunReport -> M GhRunId,
    _githubInterfaceUpdateBuildReport :: (HasCallStack) => GhRunId -> GhRunReport -> RepoInfo -> M (),
    _githubInterfaceDoesRepoFileExist :: (HasCallStack) => CommitInfo -> FilePath -> M DoesFileExist,
    _githubInterfaceGetRemote :: (HasCallStack) => CommitInfo -> M RemoteUrl,
    _githubInterfaceGetRepoCollaborators :: (HasCallStack) => InstallationAuth -> GhRepoOwner -> GhRepoName -> M GhCollaborators,
    _githubInterfaceGetRepoPublicity :: (HasCallStack) => InstallationAuth -> GhRepoOwner -> GhRepoName -> M RepoPublicity,
    _githubInterfaceGetInstalledOrgs :: (HasCallStack) => GhToken -> M [GhUserOrgMembership],
    _githubInterfaceGetReposInInstallationAccessibleTo :: (HasCallStack) => GH.Id GHA.Installation -> GhToken -> M [Text],
    _githubInterfaceOpenGithubPullRequest :: (HasCallStack) => GhRepoOwner -> GhRepoName -> PullRequest -> M PullRequestResult
  }

data RunReportStatus
  = RunReportStatusInProgress
  | RunReportStatusSuccess
  | RunReportStatusFailure
  | RunReportStatusTimeout
  | RunReportStatusCancelled
  deriving stock (Eq, Show)

data GhRunReport = GhRunReport
  { _ghRunReportName :: Text,
    _ghRunReportCommit :: CommitHash,
    _ghRunReportUrl :: Maybe Text,
    _ghRunReportStatus :: RunReportStatus,
    _ghRunReportTitle :: Text,
    _ghRunReportSummary :: Text,
    _ghRunReportLogs :: RawLogs
  }
  deriving stock (Eq, Show)

data DeploymentStatus = DeploymentInProgress | DeploymentSuccess | DeploymentFailure

data PullRequest = PullRequest
  { _pullRequestTitle :: Text,
    _pullRequestBody :: Text,
    _pullRequestHeadBranch :: Branch,
    _pullRequestBaseBranch :: Branch
  }

makeFields ''PullRequest
makeFields ''EnvMocks
makeFields ''GhRunReport

-- Accessors

getInstallation :: GH.Id GHA.Installation -> M GHA.InstallationAuth
getInstallation inst = do
  gh <- view #githubInterface
  _githubInterfaceGetInstallation gh inst

getInstallations :: GhToken -> M [GH.Id GHA.Installation]
getInstallations token = do
  gh <- view #githubInterface
  _githubInterfaceGetInstallations gh token

getGarnixInstallationId :: GhRepoOwner -> GhRepoName -> M (Maybe Integer)
getGarnixInstallationId owner name = do
  gh <- view #githubInterface
  _githubInterfaceGetGarnixInstallationId gh owner name

getAccessToken :: GHA.InstallationAuth -> M GhToken
getAccessToken iAuth = do
  gh <- view #githubInterface
  _githubInterfaceGetAccessToken gh iAuth

getDefaultBranch :: Maybe GHA.InstallationAuth -> GhRepoOwner -> GhRepoName -> M (Maybe Branch)
getDefaultBranch miAuth owner repo = do
  gh <- view #githubInterface
  _githubInterfaceGetDefaultBranch gh miAuth owner repo

getHeadCommit :: GhToken -> GhRepoOwner -> GhRepoName -> Branch -> M CommitHash
getHeadCommit token owner repo branch = do
  gh <- view #githubInterface
  _githubInterfaceGetHeadCommit gh token owner repo branch

newBuildReport :: RepoInfo -> GhRunReport -> M GhRunId
newBuildReport repoInfo build' = do
  gh <- view #githubInterface
  _githubInterfaceNewBuildReport gh repoInfo build'

updateBuildReport :: GhRunId -> GhRunReport -> RepoInfo -> M ()
updateBuildReport runId' runReport repoInfo = do
  gh <- view #githubInterface
  _githubInterfaceUpdateBuildReport gh runId' runReport repoInfo

getRemote :: (HasCallStack) => CommitInfo -> M RemoteUrl
getRemote commitInfo = do
  gh <- view #githubInterface
  _githubInterfaceGetRemote gh commitInfo

getRepoCollaborators :: (HasCallStack) => InstallationAuth -> GhRepoOwner -> GhRepoName -> M GhCollaborators
getRepoCollaborators iAuth owner repo = do
  gh <- view #githubInterface
  _githubInterfaceGetRepoCollaborators gh iAuth owner repo

doesRepoFileExist :: (HasCallStack) => CommitInfo -> FilePath -> M DoesFileExist
doesRepoFileExist commitInfo path = do
  gh <- view #githubInterface
  _githubInterfaceDoesRepoFileExist gh commitInfo path

getRepoPublicity :: (HasCallStack) => InstallationAuth -> GhRepoOwner -> GhRepoName -> M RepoPublicity
getRepoPublicity iAuth owner name = do
  gh <- view #githubInterface
  _githubInterfaceGetRepoPublicity gh iAuth owner name

getInstalledOrgs :: (HasCallStack) => GhToken -> M [GhUserOrgMembership]
getInstalledOrgs tok = do
  gh <- view #githubInterface
  _githubInterfaceGetInstalledOrgs gh tok

getReposInInstallationAccessibleTo :: (HasCallStack) => GH.Id GHA.Installation -> GhToken -> M [Text]
getReposInInstallationAccessibleTo installation token = do
  gh <- view #githubInterface
  _githubInterfaceGetReposInInstallationAccessibleTo gh installation token

openGithubPullRequest :: (HasCallStack) => GhRepoOwner -> GhRepoName -> PullRequest -> M PullRequestResult
openGithubPullRequest owner name pr = do
  gh <- view #githubInterface
  _githubInterfaceOpenGithubPullRequest gh owner name pr

withWreqOptions :: (Wreq.Options -> IO a) -> M a
withWreqOptions action = do
  manager <- view #manager
  let options = Wreq.defaults & Wreq.manager .~ Right manager
  liftIO $ action options

getNixXdgCacheDir :: M String
getNixXdgCacheDir =
  view #nixXdgCacheDir
    >>= maybe (throw $ OtherError "Local cache not properly set up.") pure

newtype RequestTraceId = RequestTraceId {getRequestTraceId :: Data.UUID.UUID}

instance Loggable RequestTraceId where
  asLog (RequestTraceId uuid) = [("request_trace_id", Data.UUID.toText uuid)]

-- * mocking

newMock :: (a -> M b) -> IO (Mock a b)
newMock f = do
  Mock f <$> newMVar []

mockable ::
  Lens' EnvMocks (Maybe (Mock arg result)) ->
  (arg -> M result) ->
  (arg -> M result)
mockable lens prodImplementation arg = do
  mocks <- view #mocks
  case mocks >>= (^. lens) of
    Nothing -> prodImplementation arg
    Just (Mock mock calls) -> do
      liftIO $ modifyMVar_ calls $ \acc -> pure (acc ++ [arg])
      mock arg

withMock ::
  Lens' EnvMocks (Maybe (Mock arg result)) ->
  (arg -> M result) ->
  M a ->
  M a
withMock lens mock action = do
  mocks <- view #mocks
  case mocks of
    Nothing -> log Critical "trying to mock during production"
    Just _ -> pure ()
  calls <- liftIO $ newMVar []
  local (setMock calls) action
  where
    setMock calls env =
      env & #mocks . _Just . lens ?~ Mock mock calls

withUnmock :: Lens' EnvMocks (Maybe (Mock arg result)) -> M a -> M a
withUnmock lens action = do
  local unsetMock action
  where
    unsetMock env = case mocks env of
      Nothing -> env
      Just mocks ->
        env {mocks = Just (mocks & lens .~ Nothing)}

withMockReturning ::
  Lens' EnvMocks (Maybe (Mock arg result)) ->
  result ->
  M a ->
  M a
withMockReturning lens result =
  withMock lens $ const $ pure result

getMockCalls ::
  Lens' EnvMocks (Maybe (Mock arg result)) ->
  M [arg]
getMockCalls lens = do
  mocks <- view #mocks
  case mocks >>= (^. lens) of
    Just mock -> liftIO $ readMVar $ calls mock
    Nothing -> throw $ OtherError "getMockCalls called for unmocked function"

-- * logging

withDefaultLogger :: ((LogItem -> IO ()) -> IO a) -> IO a
withDefaultLogger action =
  bracket
    (FastLogger.newFastLogger1 $ FastLogger.LogStdout FastLogger.defaultBufSize)
    snd
    (\(logger, _) -> action (\x -> logger $ FastLogger.toLogStr x <> "\n"))

log :: Severity -> Text -> M ()
log sev txt = do
  spans <- asks spanCtx
  logger <- asks logger
  liftIO $ logger (LogItem sev (nubOrd spans) (limitMessage txt))
  case sev of
    Critical -> incrementEvent #logsCritical
    Error -> incrementEvent #logsError
    Warning -> incrementEvent #logsWarning
    _ -> pure ()
  where
    limit = 20000
    limitMessage text =
      if T.length text > limit
        then T.take limit text <> "...[snip]"
        else text

logRequestsMiddleware :: Env -> (RequestTraceId -> Wai.Application) -> Wai.Application
logRequestsMiddleware env innerApp request respond = do
  requestTraceId <- RequestTraceId <$> liftIO Data.UUID.V4.nextRandom
  innerApp requestTraceId request $ \response -> do
    result <- respond response
    void $ runM env $ do
      let spans =
            [ ("method", cs $ Wai.requestMethod request),
              ("path", cs $ Wai.rawPathInfo request),
              ("http_version", show $ Wai.httpVersion request),
              ("status", show $ statusCode $ Wai.responseStatus response),
              ("response_content_length", maybe "-" show $ contentLength $ Wai.responseHeaders response),
              ("referrer", maybe "-" cs $ Wai.requestHeaderReferer request),
              ("user_agent", maybe "-" cs $ Wai.requestHeaderUserAgent request)
            ]
      withSpan requestTraceId
        $ withTextSpans spans
        $ do
          log Informational "http request"
    pure result

withSpan :: (Loggable l) => l -> M a -> M a
withSpan toAdd = withTextSpans (asLog toAdd)

withTextSpan :: (Text, Text) -> M a -> M a
withTextSpan toAdd = withTextSpans [toAdd]

withTextSpans :: [(Text, Text)] -> M a -> M a
withTextSpans toAdd = local $ \env -> env {spanCtx = map addSpanPrefix toAdd <> spanCtx env}
  where
    addSpanPrefix :: (Text, Text) -> (Text, Text)
    addSpanPrefix = first ("span_" <>)

logThrownErrors :: M a -> M a
logThrownErrors action = action `catchError` (\e -> logError e >> rethrow e)

logError :: ErrorWithContext -> M ()
logError error = withRawTextSpans (spans error) $ do
  log (error ^. #severity) $ showDebug error
  where
    withRawTextSpans :: [(Text, Text)] -> M a -> M a
    withRawTextSpans rawSpans = local $ \env -> env {spanCtx = rawSpans <> spanCtx env}

logSomeException :: SomeException -> M ()
logSomeException error =
  log Error $ "runtime exception: " <> show error

(<?>) :: M a -> Text -> M a
action <?> msg = do
  log Notice msg
  result <-
    action `whenErrorEither` \e -> do
      log Warning (msg <> " - FAILED: " <> either show showDebug e)
  log Notice (msg <> " - DONE")
  pure result

withMessage :: Text -> M a -> M a
withMessage = flip (<?>)

logDuration :: Text -> M a -> M a
logDuration message action = do
  before <- liftIO getSystemTime
  result <- action
  after <- liftIO getSystemTime
  let seconds = nominalDiffTimeToSeconds $ diffUTCTime (systemToUTCTime after) (systemToUTCTime before)
  log Informational $ "logDuration: " <> message <> ", seconds: " <> prettyF (PrettyCfg 2 Nothing '.') seconds
  pure result

-- * error handling

throwWithSeverity :: (HasCallStack) => Severity -> Error -> M a
throwWithSeverity severity e = do
  spans <- asks spanCtx
  throwError $ ErrorWithContext {callstack = callStack, spans, severity, err = e}

throw :: (HasCallStack) => Error -> M a
throw = throwWithSeverity Error

shortcut :: (HasCallStack) => Error -> M a
shortcut = throwWithSeverity Informational

rethrow :: ErrorWithContext -> M a
rethrow = throwError

catchIfErrorMatches :: M a -> IndexedTraversal' Int T.Text RE.Match -> (NonEmpty [Text] -> M a) -> M a
catchIfErrorMatches action regex fallback = do
  action
    `catchError` ( \e -> case err e of
                     RunProcessError {..} -> case stdErr ^.. regex . RE.groups of
                       [] -> throwError e
                       m : rest -> fallback (m :| rest)
                     _ -> throwError e
                 )

catchEither :: M a -> (Either SomeException ErrorWithContext -> M a) -> M a
catchEither action handler = (action `catchError` (handler . Right)) `catchAny` (handler . Left)

rethrowEither :: Either SomeException ErrorWithContext -> M a
rethrowEither = \case
  Right e -> rethrow e
  Left e -> SafeException.throwIO e

tryEither :: M a -> M (Either (Either SomeException ErrorWithContext) a)
tryEither action = catchEither (Right <$> action) (pure . Left)

-- | Executes a given clean-up function on exceptions and errors.
-- Runs the clean-up functions also for async exceptions.
whenErrorEither :: M a -> (Either SomeException ErrorWithContext -> M ()) -> M a
whenErrorEither action onError =
  ( action
      `SafeException.withException` (onError . Left)
  )
    `catchError` ( \e -> do
                     onError $ Right e
                     throwError e
                 )

-- Catches both IO and MonadError errors, logs them at Critical, but otherwise
-- ignores them
ignoringAllErrors :: M a -> M ()
ignoringAllErrors action =
  (void action `catchError` logIt) `SafeException.catchAny` logIt
  where
    logIt x = log Critical $ "Got an error (ignored): " <> show x

aesonDecode :: (HasCallStack) => Text -> (Value -> Parser a) -> Text -> M a
aesonDecode description parser json = do
  case eitherDecode' (cs json) >>= Data.Aeson.Types.parseEither parser of
    Left e ->
      throw
        $ DecodeError
          { original = json,
            message =
              "Could not decode "
                <> description
                <> ". Error was: "
                <> cs e
          }
    Right parsed -> pure parsed

safeGetAbsoluteFlakeDir :: FlakeDir -> M FilePath
safeGetAbsoluteFlakeDir (FlakeDir flakeDir) = do
  curDir <- view #workingDir
  result <- liftIO $ canonicalizePath (curDir </> flakeDir)
  if result == curDir || (curDir <> "/") `isPrefixOf` result
    then pure result
    else do
      throw $ OtherError $ "'" <> cs flakeDir <> "' is not a path within the repo"
