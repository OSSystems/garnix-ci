module Garnix.API.Builds where

import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Garnix.API.Builds.Types
import Garnix.API.Commits (GetCommit, ListCommits, getCommitsForUser, getSingleCommit)
import Garnix.Access (Access (..), getBuildWithAccess)
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Orchestrator qualified as Orchestrator
import Garnix.Prelude
import Garnix.Reporters.OpenSearchReporter (openSearchReporter)
import Garnix.Types
import Garnix.UserLogs (getLogLines)
import GitHub.Data.Id (Id (Id))
import Servant.API (Put)
import Servant.API.ContentTypes
import Servant.API.Stream
import Servant.Auth.Server
import Servant.Types.SourceT

data BuildAPI route = BuildAPI
  { _buildAPIgetLogs :: route :- Capture "buildId" BuildId :> "logs" :> QueryParam "after" UTCTime :> Get '[JSON] BuildLogs,
    _buildAPIgetLogsRaw :: route :- Capture "buildId" BuildId :> "logs" :> "raw" :> StreamGet NewlineFraming PlainText (SourceT IO Text),
    _buildAPIgetBuild :: route :- Capture "buildId" BuildId :> Get '[JSON] BuildResponse,
    _buildAPIupdateBuild :: route :- Capture "buildId" BuildId :> ReqBody '[JSON] BuildUpdate :> Put '[JSON] (),
    _buildAPIlistCommits :: route :- "commits" :> Get '[JSON] ListCommits,
    _buildAPIgetCommit :: route :- "commit" :> Capture "commit" CommitHash :> Get '[JSON] GetCommit,
    _buildAPIsubmitTestBuild :: route :- "submit" :> ReqBody '[JSON] SubmitTestBuild :> Post '[JSON] ()
  }
  deriving (Generic)

buildAPI :: AuthResult AuthJwtPayload -> BuildAPI (AsServerT M)
buildAPI (Authenticated ((^. #user) -> user')) =
  BuildAPI
    { _buildAPIgetLogs = getBuildLogs (Just user'),
      _buildAPIgetLogsRaw = getLogsRaw (Just user'),
      _buildAPIgetBuild = getBuild' (Just user'),
      _buildAPIupdateBuild = updateBuild user',
      _buildAPIlistCommits = getCommitsForUser user',
      _buildAPIgetCommit = getSingleCommit (Just user'),
      _buildAPIsubmitTestBuild = submitTestBuild
    }
buildAPI _ =
  BuildAPI
    { _buildAPIgetLogs = getBuildLogs Nothing,
      _buildAPIgetLogsRaw = getLogsRaw Nothing,
      _buildAPIgetBuild = getBuild' Nothing,
      _buildAPIupdateBuild = \_ _ -> throw Unauthorized,
      _buildAPIlistCommits = throw Unauthorized,
      _buildAPIgetCommit = getSingleCommit Nothing,
      _buildAPIsubmitTestBuild = submitTestBuild
    }

data SubmitTestBuild = SubmitTestBuild
  { owner :: GhRepoOwner,
    repo :: GhRepoName,
    testCommit :: CommitHash
  }
  deriving (Generic)

instance FromJSON SubmitTestBuild

submitTestBuild :: SubmitTestBuild -> M ()
submitTestBuild SubmitTestBuild {owner, repo, testCommit} = do
  installationId <- getGarnixInstallationId owner repo
  case installationId of
    Nothing -> throw NotFound
    Just id -> do
      iAuth <- getInstallation (Id $ fromInteger id)
      tok <- getAccessToken iAuth
      let commitInfo =
            CommitInfo
              { _commitInfoReqUser = "garnix-io",
                _commitInfoRepoPublicity = RepoIsPublic False,
                _commitInfoRepoInfo = RepoInfo iAuth tok owner repo,
                _commitInfoBranch = Nothing,
                _commitInfoPrFromFork = Just $ PrFromFork $ getGhLogin (getGhRepoOwner owner) <> "/" <> getGhRepoName repo,
                _commitInfoCommit = testCommit
              }
      void $ Orchestrator.handleCommit openSearchReporter True commitInfo

getBuildLogs :: (HasCallStack) => Maybe User -> BuildId -> Maybe UTCTime -> M BuildLogs
getBuildLogs user' buildId mAfter = do
  build <- getBuildWithAccess Read user' buildId
  let maxResults = 4096
  logs <- getLogLines build maxResults mAfter
  let finished = isJust (build ^. status) && length logs < maxResults
  pure $ BuildLogs finished maxResults logs

getLogsRaw :: (HasCallStack) => Maybe User -> BuildId -> M (SourceT IO Text)
getLogsRaw user buildId = fromStepT <$> getNextStep Nothing
  where
    getNextStep :: Maybe UTCTime -> M (StepT IO Text)
    getNextStep mAfter = do
      env :: Env <- ask
      BuildLogs finished _ logs <- getBuildLogs user buildId mAfter
      let logsText = T.intercalate "\n" $ map formatLogLine logs
          lastLogLine = snd <$> unsnoc logs
          nextStep = case (finished, lastLogLine) of
            (True, _) -> Stop
            (False, Nothing) -> Stop
            (False, Just lastLogLine) -> Effect $ do
              result <- runM env $ getNextStep $ Just $ lastLogLine ^. timestamp
              pure $ case result of
                Left err -> Servant.Types.SourceT.Error $ cs $ userMessage $ toErrorDetails err
                Right x -> x
      pure $ Yield logsText nextStep
    formatLogLine :: OpenSearchMessage -> Text
    formatLogLine (OpenSearchMessage timestamp package phase message) =
      show timestamp
        <> maybe "" (\p -> " " <> getPackageName p) package
        <> maybe "" (\p -> " (" <> p <> ")") phase
        <> "> "
        <> message

getBuild' :: Maybe User -> BuildId -> M BuildResponse
getBuild' user' buildId = do
  b <- getBuildWithAccess Read user' buildId
  originalBuild <- case b ^. drvPath of
    Just drv | b ^. alreadyBuilt == Just True -> DB.getOriginalBuildForDrvPath user' drv
    _ -> pure Nothing
  pure
    $ BuildResponse
      { _buildResponseId = b ^. id,
        _buildResponseRepoUser = b ^. repoUser,
        _buildResponseRepoName = b ^. repoName,
        _buildResponseGitCommit = b ^. gitCommit,
        _buildResponseBranch = b ^. branch,
        _buildResponsePackage = b ^. package,
        _buildResponsePackageType = b ^. packageType,
        _buildResponseSystem = b ^. system,
        _buildResponseReqUser = b ^. reqUser,
        _buildResponseStatus = b ^. status,
        _buildResponseStartTime = b ^. startTime,
        _buildResponseEndTime = b ^. endTime,
        _buildResponseGithubRunId = b ^. githubRunId,
        _buildResponseOriginalBuild = originalBuild,
        _buildResponseRelatedBuilds = maybeToList originalBuild
      }

updateBuild :: User -> BuildId -> BuildUpdate -> M ()
updateBuild user' buildId buildUpdate = do
  case buildUpdate ^. status of
    Just Cancelled -> do
      b <- getBuildWithAccess Cancel (Just user') buildId
      if isJust $ b ^. status
        then throw (BuildAlreadyStopped buildId)
        else do
          buildEnd <- liftIO getCurrentTime
          let build =
                b
                  & status
                  ?~ Cancelled
                  & endTime
                  ?~ buildEnd
          DB.reportBuildResultDB build
    Just _ -> throw (InvalidBuildUpdate buildUpdate)
    Nothing -> pure ()

data NewLogsEvent = NewLogsEvent
  { _newLogsEventPackage :: Maybe Text,
    _newLogsEventContent :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON NewLogsEvent where
  toEncoding = ourToEncoding
  toJSON = ourToJSON
