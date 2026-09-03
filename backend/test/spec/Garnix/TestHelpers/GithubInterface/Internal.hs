module Garnix.TestHelpers.GithubInterface.Internal where

import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Cradle qualified
import Data.Map
import Data.Maybe (fromJust)
import Garnix.GithubInterface.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers.Common (commitAll)
import Garnix.TestInstances ()
import Garnix.Types
import GitHub.App.Auth qualified as GHA
import System.Directory (doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import Test.HUnit (assertFailure)

data TestRepo = TestRepo
  { publicity :: RepoPublicity,
    collaborators :: [GhLogin],
    localPath :: Maybe FilePath,
    defaultBranch :: Maybe Branch,
    pullRequestBranch :: Maybe Branch
  }
  deriving stock (Generic)

newtype RepoCollection = RepoCollection (TVar (Map (GhRepoOwner, GhRepoName) TestRepo))

newRepoCollection :: M RepoCollection
newRepoCollection = liftIO $ RepoCollection <$> newTVarIO mempty

lookupRepoImpl :: RepoCollection -> GhRepoOwner -> GhRepoName -> M (Maybe TestRepo)
lookupRepoImpl (RepoCollection rc) owner name = do
  repos <- liftIO $ readTVarIO rc
  pure $ repos !? (owner, name)

updateRepo :: RepoCollection -> GhRepoOwner -> GhRepoName -> (TestRepo -> TestRepo) -> M ()
updateRepo (RepoCollection rc) owner name modify =
  liftIO
    $ atomically
    $ modifyTVar rc
    $ Data.Map.alter mergeTestRepos (owner, name)
  where
    mergeTestRepos :: Maybe TestRepo -> Maybe TestRepo
    mergeTestRepos = \case
      Nothing -> Just $ modify $ TestRepo (RepoIsPublic True) [] Nothing Nothing Nothing
      Just repo -> Just $ modify repo

setRepoImpl :: RepoCollection -> GhRepoOwner -> GhRepoName -> (TestRepo -> TestRepo) -> M ()
setRepoImpl repoCollection owner name modify = updateRepo repoCollection owner name $ const $ modify $ TestRepo (RepoIsPublic True) [] Nothing Nothing Nothing

withLocalRepoImpl :: RepoCollection -> GhRepoOwner -> GhRepoName -> CommitInfo -> (FilePath -> M ()) -> (CommitInfo -> M a) -> M a
withLocalRepoImpl rc owner name commitInfo setup action = do
  withSystemTempDirectory "garnix-test" $ \mockGithubRepo -> do
    setup mockGithubRepo
    let defBranch = fromJust $ commitInfo ^. branch
    Cradle.run_
      $ Cradle.cmd "git"
      & Cradle.setWorkingDir mockGithubRepo
      & Cradle.addArgs ["init", "." :: String]
      & Cradle.silenceStdout
      & Cradle.silenceStderr
    Cradle.run_
      $ Cradle.cmd "git"
      & Cradle.setWorkingDir mockGithubRepo
      & Cradle.addArgs ["checkout", "-b", getBranch defBranch]
      & Cradle.silenceStderr
    commit' <- commitAll mockGithubRepo
    updateRepo
      rc
      owner
      name
      ( (#localPath ?~ mockGithubRepo)
          . (#defaultBranch ?~ defBranch)
      )
    action (commitInfo & (commit .~ commit'))

data ReportCollection = ReportCollection
  { reports :: TVar (Map GhRunId [(RepoInfo, GhRunReport)]),
    nextGhRunId :: TVar GhRunId
  }

newReportCollection :: M ReportCollection
newReportCollection = liftIO $ ReportCollection <$> newTVarIO mempty <*> newTVarIO 0

appendNewReport :: ReportCollection -> RepoInfo -> GhRunReport -> M GhRunId
appendNewReport ReportCollection {..} repoInfo runReport = liftIO $ do
  id <- atomically $ do
    id <- readTVar nextGhRunId
    writeTVar nextGhRunId (id + 1)
    pure id

  atomically
    $ modifyTVar reports
    $ Data.Map.insert id [(repoInfo, runReport)]

  pure id

updateReport :: ReportCollection -> GhRunId -> GhRunReport -> RepoInfo -> M ()
updateReport ReportCollection {..} ghRunId runReport repoInfo =
  liftIO
    $ atomically
    $ modifyTVar reports
    $ Data.Map.insertWith (\new old -> old <> new) ghRunId [(repoInfo, runReport)]

getReportsImpl :: ReportCollection -> M [[(RepoInfo, GhRunReport)]]
getReportsImpl ReportCollection {..} =
  liftIO $ Data.Map.elems <$> readTVarIO reports

-- | Comments the fake github interface was asked to post on pull requests,
-- oldest first.
newtype CommentCollection = CommentCollection (TVar [(RepoInfo, GhPullRequestId, Text)])

newCommentCollection :: M CommentCollection
newCommentCollection = liftIO $ CommentCollection <$> newTVarIO []

appendComment :: CommentCollection -> RepoInfo -> GhPullRequestId -> Text -> M ()
appendComment (CommentCollection cc) repoInfo prId body =
  liftIO $ atomically $ modifyTVar cc (<> [(repoInfo, prId, body)])

getCommentsImpl :: CommentCollection -> M [(RepoInfo, GhPullRequestId, Text)]
getCommentsImpl (CommentCollection cc) = liftIO $ readTVarIO cc

newtype OrgMembersCollection = OrgMembersCollection (TVar [GhUserOrgMembership])

newOrgMembersCollection :: M OrgMembersCollection
newOrgMembersCollection = liftIO $ OrgMembersCollection <$> newTVarIO []

addOrgMembersImpl :: OrgMembersCollection -> [GhUserOrgMembership] -> M ()
addOrgMembersImpl (OrgMembersCollection oc) toAdd =
  liftIO
    $ atomically
    $ modifyTVar
      oc
      (<> toAdd)

getOrgMembers :: OrgMembersCollection -> M [GhUserOrgMembership]
getOrgMembers (OrgMembersCollection oc) = liftIO $ readTVarIO oc

data GithubFakeState = GithubFakeState
  { repoCollection :: RepoCollection,
    reportCollection :: ReportCollection,
    orgMembersCollection :: OrgMembersCollection,
    commentCollection :: CommentCollection
  }

mkFakeGithubInterface :: M (GithubFakeState, GithubInterface)
mkFakeGithubInterface = do
  repoCollection <- newRepoCollection
  reportCollection <- newReportCollection
  orgMembersCollection <- newOrgMembersCollection
  commentCollection <- newCommentCollection
  let notImplemented methodName = error $ methodName <> " not implemented in mkFakeGithubInterface"
  pure
    ( GithubFakeState
        { repoCollection = repoCollection,
          reportCollection = reportCollection,
          orgMembersCollection = orgMembersCollection,
          commentCollection = commentCollection
        },
      GithubInterface
        { _githubInterfaceGetAccessToken = \_ -> pure $ notImplemented "_githubInterfaceGetAccessToken",
          _githubInterfaceGetDefaultBranch = \_ repoOwner repoName -> do
            repo <- lookupRepoImpl repoCollection repoOwner repoName
            pure $ repo >>= \r -> r ^. #defaultBranch,
          _githubInterfaceGetHeadCommit = \_ repoOwner repoName branch -> do
            repo <- lookupRepoImpl repoCollection repoOwner repoName
            case repo of
              Nothing ->
                throw
                  $ OtherError
                  $ "fakeGithubInterrface/getHeadCommit: could not find repository "
                  <> getGhLogin (getGhRepoOwner repoOwner)
                  <> "/"
                  <> getGhRepoName repoName
              Just repo -> do
                when (repo ^. #defaultBranch /= Just branch)
                  $ throw
                  $ OtherError
                  $ "fakeGithubInterface/getHeadCommit: can only get head commit for default branch ("
                  <> show (repo ^. #defaultBranch)
                  <> ") but got "
                  <> show branch

                case repo ^. #localPath of
                  Nothing -> throw $ OtherError "fakeGithubInterface/getHeadCommit: can only get head commit for local repos (localPath must be set up)"
                  Just path -> do
                    commitHasFlakeNix <-
                      Cradle.run
                        $ Cradle.cmd "git"
                        & Cradle.setWorkingDir path
                        & Cradle.addArgs ["rev-parse", getBranch branch]
                        & Cradle.silenceStderr
                    case commitHasFlakeNix of
                      (Cradle.ExitFailure _, _) -> liftIO $ assertFailure "could not find git branch"
                      (Cradle.ExitSuccess, Cradle.StdoutTrimmed stdout) -> do
                        pure $ CommitHash $ cs stdout,
          _githubInterfaceNewBuildReport = appendNewReport reportCollection,
          _githubInterfaceUpdateBuildReport = updateReport reportCollection,
          _githubInterfaceDoesRepoFileExist = \ci relativePath -> do
            let ri = ci ^. repoInfo
            repo <- lookupRepoImpl repoCollection (ri ^. ghRepoOwner) (ri ^. ghRepoName)
            case repo >>= \r -> r ^. #localPath of
              Nothing ->
                liftIO
                  $ assertFailure
                  $ cs
                  $ "Trying to access mocked repository '"
                  <> getGhLogin (getGhRepoOwner (ri ^. ghRepoOwner))
                  <> "/"
                  <> getGhRepoName (ri ^. ghRepoName)
                  <> "' at path '"
                  <> cs relativePath
                  <> "' without setting it."
              Just basePath ->
                liftIO (doesFileExist (basePath </> relativePath)) >>= \case
                  True -> pure FileExists
                  False -> pure FileDoesntExist,
          _githubInterfaceGetInstalledOrgs = \_tok -> getOrgMembers orgMembersCollection,
          _githubInterfaceGetRemote = \ci -> do
            let ri = ci ^. repoInfo
            repo <- lookupRepoImpl repoCollection (ri ^. ghRepoOwner) (ri ^. ghRepoName)
            case repo >>= \r -> r ^. #localPath of
              Nothing ->
                liftIO
                  $ assertFailure
                  $ cs
                  $ "Trying to access mocked repository remote for '"
                  <> getGhLogin (getGhRepoOwner (ri ^. ghRepoOwner))
                  <> "/"
                  <> getGhRepoName (ri ^. ghRepoName)
              Just basePath -> pure $ RemoteUrl ("file:///" <> cs basePath <> "/.git"),
          _githubInterfaceGetInstallation = \id' -> do
            appAuth <- view #githubAppAuth
            liftIO $ GHA.mkInstallationAuth appAuth id',
          _githubInterfaceGetInstallations = const $ pure [],
          _githubInterfaceGetGarnixInstallationId = \_ _ -> pure $ Just 1,
          _githubInterfaceGetRepoPublicity = \_ owner name -> do
            repo <- lookupRepoImpl repoCollection owner name
            case repo of
              Just repo -> pure $ repo ^. #publicity
              Nothing -> throw $ NoSuchRepo {_owner = owner, _name = name},
          _githubInterfaceGetRepoCollaborators = \_iAuth owner repo -> do
            repo <- lookupRepoImpl repoCollection owner repo
            case repo of
              Nothing -> pure RepoNotFound
              Just r -> do
                -- Github returns the owner in the collaborators list
                pure $ GhCollaborators (getGhRepoOwner owner : r ^. #collaborators),
          _githubInterfaceGetReposInInstallationAccessibleTo = \_ _ -> pure [],
          _githubInterfaceOpenGithubPullRequest = \owner@(GhRepoOwner (GhLogin o)) repo@(GhRepoName r) pr -> do
            updateRepo repoCollection owner repo (#pullRequestBranch ?~ (pr ^. headBranch))

            repo <- lookupRepoImpl repoCollection owner repo
            case repo of
              Nothing -> throw NotFound
              Just _ ->
                pure $ PullRequestResult $ cs o <> "/" <> cs r <> "/pulls/1",
          _githubInterfaceExchangeOauthCode = \_ _ -> throw $ OtherError "exchangeOauthCode is not faked",
          _githubInterfaceRefreshUserCredentials = \_ -> throw $ OtherError "refreshUserCredentials is not faked",
          -- A repo set up with 'pullRequestBranch' pretends to have exactly one
          -- open pull request, number 1, containing every commit.
          _githubInterfaceGetPullRequestsForCommit = \ri _commit -> do
            repo <- lookupRepoImpl repoCollection (ri ^. ghRepoOwner) (ri ^. ghRepoName)
            pure $ case repo >>= \r -> r ^. #pullRequestBranch of
              Nothing -> []
              Just _ -> [GhPullRequestId 1],
          _githubInterfaceCommentOnPullRequest = appendComment commentCollection
        }
    )
