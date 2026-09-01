module Garnix.TestHelpers.GithubInterface.Deprecated where

import Data.IORef.Lifted (IORef, atomicModifyIORef')
import Data.IntMap qualified as IntMap
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestInstances ()
import Garnix.Types
import GitHub.App.Auth qualified as GHA
import System.Directory (doesFileExist)

defaultCommitHash :: CommitHash
defaultCommitHash = CommitHash "aaaa"

-- | deprecated: consider using mkFakeGithubInterface instead
testGithubInterface ::
  FilePath -> IORef (IntMap.IntMap [(Text, RunReportStatus, RawLogs)]) -> IO GithubInterface
testGithubInterface tmp buildRef = do
  pure
    $ GithubInterface
      { _githubInterfaceGetInstallation = \id' -> do
          appAuth <- view #githubAppAuth
          liftIO $ GHA.mkInstallationAuth appAuth id',
        _githubInterfaceGetInstallations = const $ pure [],
        _githubInterfaceGetGarnixInstallationId = \_ _ -> pure $ Just 1,
        _githubInterfaceGetAccessToken = const $ pure (GhToken "test-token"),
        _githubInterfaceGetDefaultBranch = \_ _ _ -> pure (Just $ Branch "main"),
        _githubInterfaceGetHeadCommit = \_ _ _ _ -> pure defaultCommitHash,
        _githubInterfaceGetRemote = \_ -> do
          pure $ RemoteUrl ("file:///" <> cs tmp <> "/.git"),
        _githubInterfaceDoesRepoFileExist = \_ path -> liftIO $ do
          doesFileExist (tmp </> path) >>= \case
            True -> pure FileExists
            False -> pure FileDoesntExist,
        _githubInterfaceNewBuildReport = \_ runReport -> do
          let next x = case IntMap.lookupMax x of
                Nothing -> 0
                Just (n, _) -> succ n
          int <-
            atomicModifyIORef'
              buildRef
              (\x -> (IntMap.insert (next x) [(runReport ^. name, runReport ^. status, RawLogs "")] x, next x))
          pure $ GhRunId $ fromIntegral int,
        _githubInterfaceUpdateBuildReport = \(GhRunId runId) runReport _ -> do
          let logs = _ghRunReportLogs runReport
          atomicModifyIORef' buildRef (\x -> (IntMap.insertWith (++) (fromIntegral runId) [(runReport ^. name, runReport ^. status, logs)] x, ())),
        _githubInterfaceGetRepoCollaborators = \_ _ _ -> pure $ GhCollaborators [],
        _githubInterfaceGetRepoPublicity = \_ _ _ -> return $ RepoIsPublic True,
        _githubInterfaceGetInstalledOrgs = \_ -> pure [],
        _githubInterfaceGetReposInInstallationAccessibleTo = \_ _ -> pure [],
        _githubInterfaceOpenGithubPullRequest = \_ _ _ -> pure $ PullRequestResult "",
        _githubInterfaceExchangeOauthCode = \_ _ -> throw $ OtherError "exchangeOauthCode is not faked",
        _githubInterfaceRefreshUserCredentials = \_ -> throw $ OtherError "refreshUserCredentials is not faked"
      }
