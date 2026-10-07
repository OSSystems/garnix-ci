module Garnix.Access
  ( Access (..),
    getBuildWithAccess,
    getRunWithAccess,
    hasAccessTo,
    hasAccessToRepo,
  )
where

import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types as Types

data Access = Read | Cancel

getRunWithAccess :: Access -> Maybe User -> RunId -> M Run
getRunWithAccess access user' runId = do
  let accessCheck = case access of
        Read -> hasAccessTo
        Cancel -> canCancelBuild
  run' <- DB.getRun runId
  run <- case run' of
    Just run -> pure run
    Nothing -> throw (NoSuchRun runId)
  let runRepo = RepoId githubForge (run ^. repoUser) (run ^. repoName)
  credentials' <-
    resolveCredentials runRepo
      >>= maybe (throw $ OtherError "Failed to look up the repository's credentials") pure
  repoPublicity <- getRepoPublicity credentials' runRepo
  hasAccess <- accessCheck user' repoPublicity (run ^. reqUser) runRepo
  when (not hasAccess) $ throw (NoSuchRun runId)
  pure run

getBuildWithAccess :: Access -> Maybe User -> BuildId -> M Build
getBuildWithAccess access user' buildId = do
  let accessCheck = case access of
        Read -> hasAccessTo
        Cancel -> canCancelBuild
  build <- DB.getBuild buildId
  hasAccess <- accessCheck user' (build ^. repoIsPublic) (build ^. reqUser) (RepoId githubForge (build ^. repoUser) (build ^. repoName))
  when (not hasAccess) $ throw (NoSuchBuild buildId)
  pure build

hasAccessTo :: Maybe User -> RepoPublicity -> GhLogin -> RepoId -> M Bool
hasAccessTo user' repoIsPublic reqUser repo
  | user' ^? _Just . githubLogin == Just reqUser = pure True
  | otherwise = hasAccessToRepo user' repoIsPublic repo

hasAccessToRepo :: Maybe User -> RepoPublicity -> RepoId -> M Bool
hasAccessToRepo user' repoIsPublic repo
  | isRepoPublic repoIsPublic = pure True
  | user' ^? _Just . subscriptionType == Just Admin = pure True
  | otherwise = case user' of
      Nothing -> pure False
      Just user -> do
        collaborators <- getCollaborators repo
        case collaborators of
          RepoNotFound -> pure False
          GhCollaborators collaborators' -> pure $ (user ^. githubLogin) `elem` collaborators'

getCollaborators :: RepoId -> M GhCollaborators
getCollaborators repo = do
  resolveCredentials repo >>= \case
    Nothing -> pure RepoNotFound
    Just credentials' -> getRepoCollaborators credentials' repo

canCancelBuild :: Maybe User -> RepoPublicity -> GhLogin -> RepoId -> M Bool
canCancelBuild user' _ reqUser repo
  | user' ^? _Just . subscriptionType == Just Admin = pure True
  | user' ^? _Just . githubLogin == Just reqUser = pure True
  | otherwise = case user' of
      Nothing -> pure False
      Just user -> do
        collaborators <- getCollaborators repo
        case collaborators of
          RepoNotFound -> pure False
          GhCollaborators collaborators' -> pure $ (user ^. githubLogin) `elem` collaborators'
