module Garnix.TestHelpers where

import Control.Exception.Safe (throwIO)
import Control.Exception.Safe qualified
import Cradle
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _String)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import Data.Text.IO qualified as T
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Database.PostgreSQL.Typed (pgSQL)
import GHC.IO.Handle (hClose)
import Garnix.API.GhWebhooks
import Garnix.DB qualified as DB
import Garnix.DB.FeatureFlags (withRecachedFeatureFlags)
import Garnix.DB.FeatureFlags.Types (FeatureFlagConfigDbo)
import Garnix.Duration
import Garnix.Monad
import Garnix.Monad.Async (resolve)
import Garnix.Prelude
import Garnix.TestHelpers.Monad (shouldBeM, withTestEnvironment)
import Garnix.TestInstances ()
import Garnix.Types hiding (head)
import Garnix.Types qualified as G
import GitHub.App.Auth (InstallationAuth)
import GitHub.Data.Webhooks.Events
import GitHub.Data.Webhooks.Payload
import Iso.Deriving (isom)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (ownerModes, setFileMode)
import Test.Hspec
import Test.Mockery.Environment
import Test.QuickCheck (Arbitrary, arbitrary, generate)
import Text.Regex.PCRE.Light

fromSingleton :: (HasCallStack, Show a) => [a] -> a
fromSingleton list = case list of
  [x] -> x
  [] -> error "fromSingleton: Expected one element, got none"
  xs -> error $ "fromSingleton: Expected one element, got " <> show (length xs) <> ": \n" <> pShow xs

fromRight :: forall error a. (HasCallStack, Show error) => Either error a -> a
fromRight either = case either of
  Right a -> a
  Left e -> error $ "fromRight: Expected Right, got " <> show (Left e :: Either error ())

(~>) :: key -> value -> Map key value
key ~> value = Map.singleton key value

wrapError :: (HasCallStack) => Severity -> Error -> ErrorWithContext
wrapError severity err = ErrorWithContext {callstack = callStack, spans = [], severity, err}

addNixExperimentalFeatures :: [String] -> IO a -> IO a
addNixExperimentalFeatures features =
  withModifiedEnvironment
    [("NIX_CONFIG", "experimental-features = " <> unwords features)]

testNixpkgsUrl :: Text
testNixpkgsUrl = unsafePerformIO $ do
  tracked <- lookupEnv "TEST_NIXPKGS_REF"
  pure $ case tracked of
    Just ref | not (null ref) -> "github:NixOS/nixpkgs/" <> cs ref
    _ -> "github:NixOS/nixpkgs/nixos-26.05"
{-# NOINLINE testNixpkgsUrl #-}

defaultEvent :: CheckSuiteEvent
defaultEvent =
  defaultViaArbitrary
    { evCheckSuiteAction = CheckSuiteEventActionRequested,
      evCheckSuiteInstallation = Just $ HookChecksInstallation 1 "blah"
    }
    & (eventBranch ?~ "branch")
    & (eventRepoName .~ ("owner", "repo"))
    & (eventRepoPublicity .~ RepoIsPublic False)
    & ((checkSuite . app . _Just . slug) ?~ "github-app-name")
    & (checkSuite . app . _Just . id .~ 12345)

defaultCommitInfo :: CommitInfo
defaultCommitInfo =
  CommitInfo
    { _commitInfoReqUser = "owner",
      _commitInfoRepoPublicity = RepoIsPublic False,
      _commitInfoRepoInfo = defaultRepoInfo,
      _commitInfoBranch = Just "branch",
      _commitInfoPrFromFork = Nothing,
      _commitInfoCommit = CommitHash "aaaaaaaa"
    }

defaultRepoInfo :: RepoInfo
defaultRepoInfo =
  RepoInfo
    { _repoInfoInstallationAuth = error "defaultEventInfo does not set installation auth",
      _repoInfoGhToken = GhToken "",
      _repoInfoGhRepoOwner = "owner",
      _repoInfoGhRepoName = "repo"
    }

eventRepoName :: Lens' CheckSuiteEvent (GhRepoOwner, GhRepoName)
eventRepoName =
  lens
    (\event -> (GhRepoOwner (GhLogin (event ^. sender . login)), GhRepoName (event ^. repository . G.name)))
    ( \event (GhRepoOwner (GhLogin owner), GhRepoName name) ->
        event
          & (repository . fullName .~ owner <> "/" <> name)
          & sender
          . login
          .~ owner
          & repository
          . G.name
          .~ name
    )

eventRepoPublicity :: Lens' CheckSuiteEvent RepoPublicity
eventRepoPublicity =
  repository
    . isPrivate
    . lens (RepoIsPublic . not) (\_private -> not . isRepoPublic)

eventBranch :: Lens' CheckSuiteEvent (Maybe Branch)
eventBranch = checkSuite . headBranch . isom

eventCommit :: Lens' CheckSuiteEvent CommitHash
eventCommit = checkSuite . headSha . isom

mkPullRequestEvent :: CommitHash -> Branch -> Text -> Text -> Int -> PullRequestEvent
mkPullRequestEvent commit branch fromRepo toRepo installationId' =
  let hookRepository =
        defaultViaArbitrary
          & (fullName .~ fromRepo)
          & (G.name .~ getGhRepoName (snd (parseRepo fromRepo)))
   in defaultViaArbitrary
        & (G.action .~ PullRequestOpenedAction)
        & (G.sender . G.login .~ "owner")
        & (G.repo . fullName .~ toRepo)
        & (installationId ?~ installationId')
        & (payload . G.head . G.repo ?~ hookRepository)
        & (payload . G.head . sha .~ getCommitHash commit)
        & (payload . G.head . ref .~ getBranch branch)

parseRepo :: Text -> (GhRepoOwner, GhRepoName)
parseRepo repo =
  case T.splitOn "/" repo of
    [o, r] -> (GhRepoOwner (GhLogin o), GhRepoName r)
    x -> error $ "Expected full name to split in two. Got: " <> show x

-- | Send a notification to our webhook handler of a commit (as though github
-- had called it).
notifyOfCommit :: CheckSuiteEvent -> M ()
notifyOfCommit event = do
  let GhRepoOwner (GhLogin ghRepoOwner) = event ^. eventRepoName . _1
  _ <- try $ DB.newUser (GhLogin ghRepoOwner) "owner@owner.com" FreeSubscription True
  mFlakePromise <- ghWebhookCheckSuite event
  resolve mFlakePromise

addLogger :: (Text -> IO ()) -> Env -> Env
addLogger newLogger =
  #logger
    %~ ( \oldLogger logItem -> do
           oldLogger logItem
           newLogger $ msg logItem
       )

-- | There's so much stuff inside some events that it's way easier to just
-- create an arbitrary one as a sort of `def`.
defaultViaArbitrary :: (Arbitrary a) => a
defaultViaArbitrary = unsafePerformIO $ generate arbitrary
{-# NOINLINE defaultViaArbitrary #-}

truncateDBMNoInsert :: M ()
truncateDBMNoInsert = do
  void
    $ DB.pgExec
      [pgSQL|
        TRUNCATE
          users,
          builds,
          commits,
          heartbeat,
          access_tokens,
          github_user_credentials,
          cache_store_hashes,
          cache_store_hash_tags,
          cache_store_hash_references,
          repo_config,
          modules,
          module_user_repo,
          module_values,
          feature_flags,
          verified_fods
      |]

truncateDBM :: M ()
truncateDBM = truncateDBMNoInsert

runTestM :: (HasCallStack) => M a -> IO a
runTestM action = do
  res <- withSystemTempDirectory "garnix-test" $ \tmp -> do
    withTestEnvironment tmp $ flip runM action
  case res of
    Left e -> do
      hPutStrLn stderr $ cs $ prettyCallStack $ callstack e
      error $ "should not be Left. Got: " <> pShow (err e)
    Right v -> pure v

-- | Writes the argument into a temporary file with the executable bit set,
-- giving the filepath of the file to the inner function.
withScript :: Text -> (FilePath -> IO a) -> IO a
withScript script action = withSystemTempFile "garnix-test.sh" $ \filepath hdl -> do
  T.hPutStr hdl script
  hClose hdl
  setFileMode filepath ownerModes
  action filepath

shouldReturn :: (HasCallStack, Eq a, Show a) => M a -> a -> Expectation
shouldReturn action expected = do
  result <- runTestM $ try action
  result `shouldBe` Right expected

shouldMatchRegexp :: (HasCallStack, MonadIO m) => Text -> Text -> m ()
shouldMatchRegexp text regexp =
  liftIO $ case match (compile (cs regexp) [utf8]) (cs text) [] of
    Nothing -> expectationFailure $ cs $ "expected \"" <> text <> "\" to match the regexp \"" <> regexp <> "\""
    Just _ -> pure ()

shouldMatchRegexpLines :: (HasCallStack) => [Text] -> [Text] -> Expectation
shouldMatchRegexpLines lines regexps = do
  when (length lines /= length regexps)
    $ expectationFailure
    $ cs
    $ "Expected "
    <> show (length regexps)
    <> " lines:\n"
    <> T.unlines regexps
    <> "\nBut got "
    <> show (length lines)
    <> " lines:\n"
    <> T.unlines lines
  forM_ (zip lines regexps) (uncurry shouldMatchRegexp)

waitFor :: forall m a. (MonadIO m, MonadCatch m) => Duration -> m a -> m a
waitFor duration action = do
  startTime <- liftIO getCurrentTime
  go startTime
  where
    go startTime = do
      result :: Either SomeException a <- Control.Exception.Safe.try action
      case result of
        Right a -> return a
        Left err -> do
          now <- liftIO getCurrentTime
          if diffTime now startTime > duration
            then throwIO err
            else do
              threadDelay $ fromSeconds @Double 0.1
              go startTime

testUser :: M User
testUser =
  DB.newUser
    (GhLogin "user")
    (Email "foo@example.com")
    FreeSubscription
    True

testBuild :: (Build -> Build) -> M Build
testBuild f = do
  let build =
        f
          $ Build
            { _buildId = undefined,
              _buildRepoUser = "test-owner",
              _buildRepoName = "test-repo",
              _buildPrFromFork = undefined,
              _buildBranch = Just "test-branch",
              _buildRepoIsPublic = RepoIsPublic True,
              _buildGitCommit = "aaaaaa",
              _buildPackage = "test-package",
              _buildPackageType = TypePackage,
              _buildSystem = IsSystem X8664Linux,
              _buildReqUser = "test-user",
              _buildStatus = Just Success,
              _buildStartTime = parseTimestamp "2010-03-04T00:00:00Z",
              _buildEndTime = Nothing,
              _buildDrvPath = Nothing,
              _buildOutputPaths = Nothing,
              _buildGithubRunId = undefined,
              _buildPersistenceName = Nothing,
              _buildWantsIncrementalism = False,
              _buildEvalHost = Just "garnix-server-test",
              _buildUploadedToCache = Just False,
              _buildAlreadyBuilt = Just False
            }
  do
    [build] <-
      DB.pgQueryPrism
        _Build
        [pgSQL|
        INSERT INTO builds
            (
              repo_user,
              repo_name,
              branch,
              repo_is_public,
              git_commit,
              package_type,
              system,
              package,
              req_user,
              status,
              start_time,
              end_time,
              drv_path,
              output_paths,
              persistence_name,
              wants_incrementalism,
              eval_host,
              uploaded_to_cache,
              already_built
            )
        VALUES
            (
              ${build ^. repoUser},
              ${build ^. repoName},
              ${build ^. branch},
              ${build ^. G.repoIsPublic},
              ${build ^. gitCommit},
              ${build ^. packageType},
              ${build ^. system},
              ${build ^. package},
              ${build ^. G.reqUser},
              ${build ^. status},
              ${build ^. startTime},
              ${build ^. endTime},
              ${build ^. drvPath},
              ${build ^. outputPaths},
              ${build ^. persistenceName},
              ${build ^. wantsIncrementalism},
              ${build ^. evalHost},
              ${build ^. uploadedToCache},
              ${build ^. alreadyBuilt}
            )
        RETURNING
          id,
          repo_user,
          repo_name,
          pr_from_fork,
          branch,
          repo_is_public,
          git_commit,
          package,
          package_type,
          system,
          req_user,
          status,
          start_time,
          end_time,
          drv_path,
          output_paths,
          github_run_id,
          persistence_name,
          wants_incrementalism,
          eval_host,
          uploaded_to_cache,
          already_built
      |]
    pure build

addTestBuild :: GhRepoOwner -> UTCTime -> Duration -> M Build
addTestBuild owner ended duration = do
  let started = subTime duration ended
  testBuild ((repoUser .~ owner) . (startTime .~ started) . (endTime ?~ ended))

testCommit :: (Commit -> Commit) -> M ()
testCommit f = do
  let commit =
        f
          $ Commit
            { _commitRepoOwner = "test-owner",
              _commitRepoName = "test-repo",
              _commitHash = "aaaaaa",
              _commitStatus = Evaluated,
              _commitMetaCheck = CheckSuccess
            }
  n <-
    DB.pgExec
      [pgSQL|
        INSERT INTO commits
          (repo_user, repo_name, git_commit, status, meta_check)
        VALUES
          (${commit ^. repoOwner}, ${commit ^. repoName}, ${commit ^. hash}, ${commit ^. status}, ${commit ^. metaCheck})
      |]
  when (n /= 1) $ do
    error "expected: 1"

parseTimestamp :: (HasCallStack) => String -> UTCTime
parseTimestamp timestamp = fromJust $ iso8601ParseM timestamp

withFeatureFlags :: FeatureFlagConfigDbo -> M a -> M a
withFeatureFlags config action = do
  writeNewFeatureFlagsRaw (toJSON config)
  withRecachedFeatureFlags action

writeNewFeatureFlagsRaw :: Aeson.Value -> M ()
writeNewFeatureFlagsRaw config = do
  1 <-
    DB.pgExec
      [pgSQL|
        INSERT INTO feature_flags
          ("config")
          VALUES
          (${config})
      |]
  pure ()

shouldThrow :: M a -> Error -> Expectation
shouldThrow action error' = runTestM $ do
  result <- try action
  case result of
    Left e -> err e `shouldBeM` error'
    Right _ -> liftIO $ expectationFailure "Expected Left, got Right"

withGithubMock :: Lens' GithubInterface g -> g -> M a -> M a
withGithubMock l result action =
  local (#githubInterface . l .~ result) action

repoCollaboratorsLens :: Lens' GithubInterface (InstallationAuth -> GhRepoOwner -> GhRepoName -> M GhCollaborators)
repoCollaboratorsLens = lens _githubInterfaceGetRepoCollaborators (\gi f -> gi {_githubInterfaceGetRepoCollaborators = f})

getRemoteLens :: Lens' GithubInterface (CommitInfo -> M RemoteUrl)
getRemoteLens = lens _githubInterfaceGetRemote (\gi f -> gi {_githubInterfaceGetRemote = f})

newBuildReportLens :: Lens' GithubInterface (RepoInfo -> GhRunReport -> M GhRunId)
newBuildReportLens = lens _githubInterfaceNewBuildReport (\gi f -> gi {_githubInterfaceNewBuildReport = f})

getNixpkgsCommitSha :: (MonadIO m) => m Text
getNixpkgsCommitSha = do
  c <- liftIO $ T.readFile "../flake.lock"
  let rootName = c ^. key "root" . _String
  let nixpkgsName = c ^. key "nodes" . key (fromString $ cs rootName) . key "inputs" . key "nixpkgs" . _String
  let sha = c ^. key "nodes" . key (fromString $ cs nixpkgsName) . key "locked" . key "rev" . _String
  pure sha
