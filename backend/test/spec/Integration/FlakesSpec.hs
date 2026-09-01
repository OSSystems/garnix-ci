module Integration.FlakesSpec (spec) where

import Control.Monad
import Cradle
import Data.Aeson (Options (rejectUnknownFields), defaultOptions, genericParseJSON)
import Data.Functor ((<&>))
import Data.IORef.Lifted (newIORef, readIORef)
import Data.IntMap qualified as IntMap
import Data.Yaml
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.API.GhWebhooks (ghWebhookPullRequest)
import Garnix.BuildLogs (processLogsForGithub)
import Garnix.DB qualified as DB
import Garnix.GithubInterface
import Garnix.Monad
import Garnix.Monad.Async (resolve)
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface.Deprecated qualified as Deprecated
import Garnix.TestHelpers.Monad (cleanDbConn, suppressLogsWhenPassing, withDevSecrets, withTestEnvironment)
import Garnix.TestHelpers.NixStore (garbageCollectStorePath)
import Garnix.Types hiding (base, context, description, head, name, packageType, repo)
import GitHub qualified as GH
import GitHub.App.Auth qualified as GHA
import System.Directory
import System.IO.Temp
import System.Process (callProcess)
import Test.Hspec
import Text.Regex.PCRE.Light qualified as R
import Turtle qualified

spec :: Spec
spec = removePrivateSourcePathsFromNixStore $ do
  describe "the flakes spec @slow @skip-ci" $ do
    testDirs <- runIO getTests

    it "has at least one dir" $ do
      null testDirs `shouldBe` False

    forM_ testDirs $ \(dir, fspec) ->
      context dir
        $ it (cs $ description fspec)
        $ do
          testFlakeSpec dir fspec

-- | These are all the private inputs that we use for integration tests.
privateSourceStorePaths :: [FilePath]
privateSourceStorePaths =
  [ -- garnix-testing-org/minimal-collaborators-test#cb20de6727ea9f283a9d11799e79c7b3f42ea8fa
    "/nix/store/kvghk8fpv89i39swh9ap0bfhchvqd3lv-source",
    -- garnix-testing-org/test-repo-private#863d27ecd3f01e1c8d1c6e1620e1cc4b1e130e8c
    "/nix/store/pnsaxvxr87jmcxw619lj7s9kl8mgqz39-source"
  ]

removePrivateSourcePathsFromNixStore :: Spec -> Spec
removePrivateSourcePathsFromNixStore = before_ $ do
  garnixRunDirs <-
    map ("/tmp/" <>)
      . filter ("garnix-runs-" `isPrefixOf`)
      <$> listDirectory "/tmp"
  forM_ garnixRunDirs $ \dir ->
    callProcess "rm" [dir, "-rf"]
  forM_ privateSourceStorePaths $ \path -> do
    garbageCollectStorePath path
    doesExist <- doesDirectoryExist path
    when doesExist $ do
      error "private source path still in store"

data FlakeSpec = FlakeSpec
  { description :: Text,
    repo :: Maybe Text,
    prFromRepo :: Maybe Text,
    skipPrivateInputsCheck :: Maybe Bool,
    results :: [SubSpec]
  }
  deriving stock (Eq, Show, Generic)

strictOptions :: Options
strictOptions =
  defaultOptions
    { rejectUnknownFields = True
    }

instance FromJSON FlakeSpec where
  parseJSON = genericParseJSON strictOptions

data SubSpec = SubSpec
  { name :: Text,
    result :: Maybe Text,
    outputRegex :: Maybe Regex,
    outputLocation :: OutputLocation,
    index :: ResultIndex
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON SubSpec where
  parseJSON = genericParseJSON strictOptions

newtype Regex = Regex {getRegex :: R.Regex}
  deriving stock (Eq, Show, Generic)

instance FromJSON Regex where
  parseJSON = withText "Regex" $ \t -> case R.compileM (cs t) [] of
    Left e -> fail e
    Right v -> pure $ Regex v

data OutputLocation = Github | Website | Logs
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data ResultIndex = First | Last | Nowhere | BeforeLast
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

base :: FilePath
base = "test/spec/Integration"

getTests :: IO [(FilePath, FlakeSpec)]
getTests = do
  contents <- listDirectory base
  dirs <- forM contents $ \c -> do
    let file = base </> c </> "spec.yaml"
    keep <- doesFileExist file
    if keep
      then do
        eFlakeSpec <- decodeFileEither file
        case eFlakeSpec of
          Left e -> error $ "Could not decode " <> cs file <> ": " <> show e
          Right flakeSpec -> pure $ Just (c, flakeSpec)
      else pure Nothing
  pure $ catMaybes dirs

testFlakeSpec :: FilePath -> FlakeSpec -> IO ()
testFlakeSpec dir fspec = do
  buildRef <- newIORef mempty
  withSystemTempDirectory "garnix-test" $ \tmp -> do
    Turtle.cptree (base </> dir) tmp
    withTestEnvironment tmp $ \baseEnv -> do
      ghInterface <-
        Deprecated.testGithubInterface tmp buildRef <&> \ghi ->
          ghi
            { _githubInterfaceGetAccessToken = \iAuth -> do
                mgr <- view #manager
                liftIO
                  $ GHA.obtainAccessToken mgr iAuth
                  >>= \case
                    Left e -> error $ show e
                    Right (GH.OAuth v) -> pure $ GhToken $ cs v
                    Right _ -> error "Unexpected auth token type",
              _githubInterfaceGetRepoCollaborators =
                _githubInterfaceGetRepoCollaborators realGithubInterface,
              _githubInterfaceGetRepoPublicity =
                _githubInterfaceGetRepoPublicity realGithubInterface
            }
      env <- do
        return
          $ baseEnv
          & (#githubInterface .~ ghInterface)
      run_ $ cmd "git" & silenceStdout & setWorkingDir tmp & addArgs ["init" :: String]
      run_ $ cmd "git" & silenceStdout & setWorkingDir tmp & addArgs ["add", "." :: String]
      run_ $ cmd "git" & silenceStdout & setWorkingDir tmp & addArgs ["commit", "-am", "Initial commit" :: String]
      commit <-
        CommitHash
          . cs
          . fromStdoutTrimmed
          <$> run (cmd "git" & setWorkingDir tmp & addArgs ["rev-parse", "HEAD" :: String])
      result <- runM env $ withDevSecrets $ suppressLogsWhenPassing $ do
        void $ DB.pgExec [pgSQL| TRUNCATE repo_config |]
        case (repo fspec, skipPrivateInputsCheck fspec) of
          (Just repository, Just True) ->
            let (repoUser, repoName) = parseRepo repository
             in void
                  $ DB.pgExec
                    [pgSQL|
              INSERT INTO repo_config
                (repo_user, repo_name, skip_private_inputs_check_for_collaborators)
                VALUES (${repoUser}, ${repoName}, TRUE)
            |]
          _ -> pure ()
        case prFromRepo fspec of
          Nothing -> do
            let event =
                  defaultEvent
                    & maybe identity (\repo -> eventRepoName .~ parseRepo repo) (repo fspec)
                    & installation
                    . _Just
                    . id
                    .~ garnixIoTestAppInstallationId
                    & eventCommit
                    .~ commit
            notifyOfCommit event `catchError` const (pure ())
          Just fromRepo -> do
            toRepo <- case repo fspec of
              Nothing -> error "when using prFromRepo, please also specify repo"
              Just r -> pure r
            notifyOfPr commit "test-branch" fromRepo toRepo garnixIoTestAppInstallationId
              `catchError` const (pure ())
        allBuilds <- readIORef buildRef
        testBuilds (join $ IntMap.elems allBuilds)
      cleanDbConn env
      case result of
        Right () -> pure ()
        Left err -> error $ showDebug err
  where
    testBuild :: Bool -> SubSpec -> (Text, RunReportStatus, RawLogs) -> M ()
    testBuild shouldFail ss (title, status, logs) = case outputLocation ss of
      Github -> do
        let status' = case status of
              RunReportStatusInProgress -> Nothing
              RunReportStatusSuccess -> Just "success"
              RunReportStatusFailure -> Just "failure"
              RunReportStatusTimeout -> Just "timeout"
              RunReportStatusCancelled -> Just "cancelled"
        let output =
              RunOutput
                { _runOutputTitle = title,
                  _runOutputSummary = "",
                  _runOutputText = processLogsForGithub logs
                }
        let succeed =
              when shouldFail
                $ liftIO
                $ expectationFailure
                $ cs
                $ "Got a match when expecting none. Run: "
                <> show output
            failWith msg = unless shouldFail $ liftIO $ expectationFailure msg
        liftIO $ status' `shouldBe` result ss
        case outputRegex ss of
          (Just (Regex re')) -> case R.match re' (cs $ output ^. text) [] of
            Nothing ->
              failWith
                $ "Regex did not match."
                <> "\nRegex:\n"
                <> cs (show re')
                <> "\nOutput:\n"
                <> cs (pShow output)
            Just [] ->
              failWith
                $ "Regex did not match."
                <> "\nRegex:\n"
                <> cs (show re')
                <> "\nOutput:\n"
                <> cs (pShow output)
            Just _ -> succeed
          Nothing -> succeed
      loc ->
        liftIO
          . expectationFailure
          . cs
          $ "Unexpected output location: "
          <> show loc

    testBuilds :: [(Text, RunReportStatus, RawLogs)] -> M ()
    testBuilds builds = forM_ (results fspec) $ \ss -> do
      case filter
        (\(title, _, _) -> name ss == title)
        builds of
        [] -> case index ss of
          Nowhere -> pure ()
          _ ->
            liftIO
              . expectationFailure
              . cs
              $ "Found an expectation that matches no build: "
              <> pShow ss
              <> "\nBuilds are: "
              <> pShow builds
        relevant -> do
          case index ss of
            -- The builds are in reverse order
            Last -> testBuild False ss $ head relevant
            BeforeLast -> testBuild False ss $ relevant !! 1
            First -> testBuild False ss $ last relevant
            Nowhere -> mapM_ (testBuild True ss) relevant

-- This is the installation id of our test app for garnix-io/garnix.
-- See here: https://github.com/settings/installations/63238749
garnixIoTestAppInstallationId :: Int
garnixIoTestAppInstallationId = 63238749

notifyOfPr :: CommitHash -> Branch -> Text -> Text -> Int -> M ()
notifyOfPr commit branch fromRepo toRepo id = do
  mFlakePromise <- do
    ghWebhookPullRequest $ mkPullRequestEvent commit branch fromRepo toRepo id
  resolve mFlakePromise
