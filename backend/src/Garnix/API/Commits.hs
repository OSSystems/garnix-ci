module Garnix.API.Commits where

import Data.Maybe (listToMaybe)
import Garnix.API.Runs (RunSummary, toRunSummary)
import Garnix.Access (githubRepoIdFromRoute, hasAccessTo, hasAccessToRepo)
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
import Servant.Auth.Server

data CommitAPI route = CommitAPI
  { -- | The forge-less form predates multi-forge support and names a
    -- github.com repository; kept as an alias so API clients keep working.
    _commitAPIgetCommitsForRepo :: route :- "repo" :> Capture "owner" GhRepoOwner :> Capture "repo" GhRepoName :> Get '[JSON] ListCommits,
    _commitAPIgetCommitsForForgeRepo :: route :- "repo" :> Capture "forge" ForgeSlug :> Capture "owner" GhRepoOwner :> Capture "repo" GhRepoName :> Get '[JSON] ListCommits,
    _commitAPIgetCommitsForUser :: route :- Get '[JSON] ListCommits,
    _commitAPIgetSingleCommit :: route :- Capture "commit" CommitHash :> Get '[JSON] GetCommit
  }
  deriving (Generic)

commitAPI :: AuthResult AuthJwtPayload -> CommitAPI (AsServerT M)
commitAPI (Authenticated ((^. #user) -> user')) =
  CommitAPI
    { _commitAPIgetCommitsForRepo = \owner name -> getCommitsForRepo (Just user') (RepoId githubForge owner name),
      _commitAPIgetCommitsForForgeRepo = \slug owner name -> getCommitsForRepo (Just user') =<< githubRepoIdFromRoute slug owner name,
      _commitAPIgetCommitsForUser = getCommitsForUser user',
      _commitAPIgetSingleCommit = getSingleCommit (Just user')
    }
commitAPI _ =
  CommitAPI
    { _commitAPIgetCommitsForRepo = \owner name -> getCommitsForRepo Nothing (RepoId githubForge owner name),
      _commitAPIgetCommitsForForgeRepo = \slug owner name -> getCommitsForRepo Nothing =<< githubRepoIdFromRoute slug owner name,
      _commitAPIgetCommitsForUser = throw Unauthorized,
      _commitAPIgetSingleCommit = getSingleCommit Nothing
    }

data ListCommits = ListCommits
  { _listCommitsCommits :: [CommitSummary]
  }
  deriving (Eq, Show, Generic)

instance ToJSON ListCommits where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

data GetCommit = GetCommit
  { _getCommitSummary :: CommitSummary,
    _getCommitBuilds :: [Build],
    _getCommitRuns :: [RunSummary]
  }
  deriving (Eq, Show, Generic)

instance ToJSON GetCommit where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

getCommitsForRepo :: (HasCallStack) => Maybe User -> RepoId -> M ListCommits
getCommitsForRepo user repo@(RepoId _forge repoOwner repoName) = do
  credentials' <-
    resolveCredentials repo
      >>= maybe (throw $ NoSuchRepo {_owner = repoOwner, _name = repoName}) pure
  repoPublicity <- getRepoPublicity credentials' repo
  hasAccess <- hasAccessToRepo user repoPublicity repo
  when (not hasAccess) $ throw NoSuchRepo {_owner = repoOwner, _name = repoName}
  ListCommits <$> DB.getCommitsByOwnerAndRepo repo

getCommitsForUser :: User -> M ListCommits
getCommitsForUser user = do
  commits <- DB.getCommitsForReqUser user
  pure $ ListCommits {_listCommitsCommits = commits}

getSingleCommit :: Maybe User -> CommitHash -> M GetCommit
getSingleCommit user' commit = do
  summaries <- DB.getCommitSummaries commit
  visible <- filterM (\s -> hasAccessTo user' (s ^. repoIsPublic) (s ^. reqUser) (commitSummaryRepoId s)) summaries
  summary <- maybe (throw $ NoSuchCommit commit) pure (listToMaybe visible)
  result <- DB.getBuildsAndRunsByCommit (commitSummaryRepoId summary) commit
  pure $ case result of
    CommitEvaluating -> GetCommit summary [] []
    CommitEvaluated _ builds runs ->
      GetCommit
        summary
        (filter (\b -> b ^. packageType /= TypeOverall) builds)
        (map toRunSummary runs)
