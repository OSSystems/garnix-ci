module Garnix.Access
  ( Access (..),
    canCancelBuild,
    getBuildWithAccess,
    getRunWithAccess,
    hasAccessTo,
    hasAccessToRepo,
    repoIdFromRoute,
    routeRepoId,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types as Types

data Access = Read | Cancel

-- | The repository a URL names, if its forge is configured.
routeRepoId :: Map ForgeSlug a -> ForgeSlug -> GhRepoOwner -> GhRepoName -> Maybe RepoId
routeRepoId configured slug owner name = RepoId slug owner name <$ guard (Map.member slug configured)

-- | 'routeRepoId' against 'Env.forges'. A URL naming no repository is a 404,
-- like a repository that does not exist, rather than the server error the
-- forge lookup would otherwise raise.
repoIdFromRoute :: ForgeSlug -> GhRepoOwner -> GhRepoName -> M RepoId
repoIdFromRoute slug owner name = do
  configured <- view #forges
  orNotFound $ routeRepoId configured slug owner name

orNotFound :: Maybe a -> M a
orNotFound = maybe (throw NotFound) pure

getRunWithAccess :: Access -> Maybe User -> RunId -> M Run
getRunWithAccess access user' runId = do
  let accessCheck = case access of
        Read -> hasAccessTo
        Cancel -> canCancelBuild
  run' <- DB.getRun runId
  run <- case run' of
    Just run -> pure run
    Nothing -> throw (NoSuchRun runId)
  let runRepo = runRepoId run
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
  hasAccess <- accessCheck user' (build ^. repoIsPublic) (build ^. reqUser) (buildRepoId build)
  when (not hasAccess) $ throw (NoSuchBuild buildId)
  pure build

-- | @reqUser@ is the account that requested the build, which lives on the
-- repository's forge.
hasAccessTo :: Maybe User -> RepoPublicity -> GhLogin -> RepoId -> M Bool
hasAccessTo user' repoIsPublic reqUser repo
  | isRequester user' reqUser repo = pure True
  | otherwise = hasAccessToRepo user' repoIsPublic repo

isRequester :: Maybe User -> GhLogin -> RepoId -> Bool
isRequester user' reqUser repo = loginOnRepoForge user' repo == Just reqUser

-- | Admins of one forge instance are not admins of another one's repositories.
isAdminOf :: Maybe User -> RepoId -> Bool
isAdminOf user' repo =
  isJust (loginOnRepoForge user' repo) && user' ^? _Just . subscriptionType == Just Admin

loginOnRepoForge :: Maybe User -> RepoId -> Maybe GhLogin
loginOnRepoForge user' repo = loginOnForgeOf repo . userForgeLogin =<< user'

hasAccessToRepo :: Maybe User -> RepoPublicity -> RepoId -> M Bool
hasAccessToRepo user' repoIsPublic repo
  | isRepoPublic repoIsPublic = pure True
  | isAdminOf user' repo = pure True
  | otherwise = case user' of
      Nothing -> pure False
      Just user -> do
        collaborators <- getCollaborators repo
        case collaborators of
          RepoNotFound -> pure False
          GhCollaborators collaborators' -> pure $ isCollaborator user repo collaborators'

-- | Collaborators are listed by login on the repository's forge.
isCollaborator :: User -> RepoId -> [GhLogin] -> Bool
isCollaborator user repo collaborators =
  maybe False (`elem` collaborators) (loginOnForgeOf repo (userForgeLogin user))

getCollaborators :: RepoId -> M GhCollaborators
getCollaborators repo = do
  resolveCredentials repo >>= \case
    Nothing -> pure RepoNotFound
    Just credentials' -> getRepoCollaborators credentials' repo

canCancelBuild :: Maybe User -> RepoPublicity -> GhLogin -> RepoId -> M Bool
canCancelBuild user' _ reqUser repo
  | isAdminOf user' repo = pure True
  | isRequester user' reqUser repo = pure True
  | otherwise = case user' of
      Nothing -> pure False
      Just user -> do
        collaborators <- getCollaborators repo
        case collaborators of
          RepoNotFound -> pure False
          GhCollaborators collaborators' -> pure $ isCollaborator user repo collaborators'
