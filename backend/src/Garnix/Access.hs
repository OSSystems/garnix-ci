module Garnix.Access
  ( Access (..),
    getBuildWithAccess,
    getRunWithAccess,
    hasAccessTo,
    hasAccessToRepo,
    githubRepoId,
    githubRepoIdFromRoute,
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

-- | The repository a URL names, if it is on github.com.
--
-- Until the tables are keyed by forge, keys, builds and commits are looked up
-- by owner and name alone. A route serving them for another forge would hand
-- out github.com @o/r@'s key, commits or status under that forge's @o/r@, after
-- an access check made against the other forge. Such routes use this instead
-- of 'routeRepoId' until then.
githubRepoId :: ForgeSlug -> GhRepoOwner -> GhRepoName -> Maybe RepoId
githubRepoId slug owner name = RepoId githubForge owner name <$ guard (slug == githubForge)

-- | 'routeRepoId' against 'Env.forges'. A URL naming no repository is a 404,
-- like a repository that does not exist, rather than the server error the
-- forge lookup would otherwise raise.
repoIdFromRoute :: ForgeSlug -> GhRepoOwner -> GhRepoName -> M RepoId
repoIdFromRoute slug owner name = do
  configured <- view #forges
  orNotFound $ routeRepoId configured slug owner name

-- | 'githubRepoId', with a 404 for any other forge.
githubRepoIdFromRoute :: ForgeSlug -> GhRepoOwner -> GhRepoName -> M RepoId
githubRepoIdFromRoute slug owner name = orNotFound $ githubRepoId slug owner name

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
