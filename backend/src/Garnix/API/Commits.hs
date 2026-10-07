module Garnix.API.Commits where

import Garnix.API.Runs (RunSummary, toRunSummary)
import Garnix.Access (hasAccessTo, hasAccessToRepo)
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
import Servant.Auth.Server

data CommitAPI route = CommitAPI
  { _commitAPIgetCommitsForRepo :: route :- "repo" :> Capture "owner" GhRepoOwner :> Capture "repo" GhRepoName :> Get '[JSON] ListCommits,
    _commitAPIgetCommitsForUser :: route :- Get '[JSON] ListCommits,
    _commitAPIgetSingleCommit :: route :- Capture "commit" CommitHash :> Get '[JSON] GetCommit
  }
  deriving (Generic)

commitAPI :: AuthResult AuthJwtPayload -> CommitAPI (AsServerT M)
commitAPI (Authenticated ((^. #user) -> user')) =
  CommitAPI
    { _commitAPIgetCommitsForRepo = \owner name -> getCommitsForRepo (Just user') (RepoId githubForge owner name),
      _commitAPIgetCommitsForUser = getCommitsForUser user',
      _commitAPIgetSingleCommit = getSingleCommit (Just user')
    }
commitAPI _ =
  CommitAPI
    { _commitAPIgetCommitsForRepo = \owner name -> getCommitsForRepo Nothing (RepoId githubForge owner name),
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
  summary <- DB.getCommitSummary commit
  hasAccess <- hasAccessTo user' (summary ^. repoIsPublic) (summary ^. reqUser) (RepoId githubForge (summary ^. repoOwner) (summary ^. repoName))
  when (not hasAccess) $ throw (NoSuchCommit commit)
  result <- DB.getBuildsAndRunsByCommit (RepoId githubForge (summary ^. repoOwner) (summary ^. repoName)) commit
  pure $ case result of
    CommitEvaluating -> GetCommit summary [] []
    CommitEvaluated _ builds runs ->
      GetCommit
        summary
        (filter (\b -> b ^. packageType /= TypeOverall) builds)
        (map toRunSummary runs)
