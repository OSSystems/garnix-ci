module Garnix.API.Runs where

import Garnix.API.Builds.Types
import Garnix.Access (Access (..), getRunWithAccess, withSessionUser)
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
import Garnix.UserLogs (getRunLogLines)
import Servant.Auth.Server (AuthResult (..))

data RunAPI route = RunAPI
  { _runAPIGetRun :: route :- Capture "runId" RunId :> Get '[JSON] RunSummary,
    _runAPIGetLogs :: route :- Capture "runId" RunId :> "logs" :> QueryParam "after" UTCTime :> Get '[JSON] BuildLogs
  }
  deriving stock (Generic)

runAPI :: AuthResult AuthJwtPayload -> RunAPI (AsServerT M)
runAPI auth =
  RunAPI
    { _runAPIGetRun = \runId -> withSessionUser auth $ \user -> getRun user runId,
      _runAPIGetLogs = \runId after -> withSessionUser auth $ \user -> getRunLogs user runId after
    }

getRun :: Maybe User -> RunId -> M RunSummary
getRun mUser runId = do
  run <- getRunWithAccess Read mUser runId
  pure $ toRunSummary run

getRunLogs :: Maybe User -> RunId -> Maybe UTCTime -> M BuildLogs
getRunLogs mUser runId mAfter = do
  run <- getRunWithAccess Read mUser runId
  let maxResults = 4096
  logs <- getRunLogLines run maxResults mAfter
  let finished = isJust (run ^. status) && length logs < maxResults
  pure $ BuildLogs finished maxResults logs

data RunSummary = RunSummary
  { _runSummaryId :: Text,
    _runSummaryName :: Text,
    _runSummaryForge :: ForgeSlug,
    _runSummaryRepoUser :: GhRepoOwner,
    _runSummaryRepoName :: GhRepoName,
    _runSummaryGitCommit :: CommitHash,
    _runSummaryBranch :: Maybe Branch,
    _runSummaryStatus :: Maybe Status,
    _runSummaryStartTime :: UTCTime,
    _runSummaryEndTime :: Maybe UTCTime
  }
  deriving (Eq, Show, Generic)

toRunSummary :: Run -> RunSummary
toRunSummary run@(Run {_runId, _runName, _runStatus, _runStartTime, _runEndTime}) =
  RunSummary
    { _runSummaryId = getRunId _runId ^. re hashIdText,
      _runSummaryName = _runName,
      _runSummaryForge = run ^. forge,
      _runSummaryRepoUser = run ^. repoUser,
      _runSummaryRepoName = run ^. repoName,
      _runSummaryGitCommit = run ^. gitCommit,
      _runSummaryBranch = run ^. branch,
      _runSummaryStatus = _runStatus,
      _runSummaryStartTime = _runStartTime,
      _runSummaryEndTime = _runEndTime
    }

instance ToJSON RunSummary where
  toEncoding = ourToEncoding
  toJSON = ourToJSON
