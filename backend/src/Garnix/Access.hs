module Garnix.Access
  ( Access (..),
    administeredForges,
    canCancelBuild,
    getBuildWithAccess,
    getRunWithAccess,
    hasAccessTo,
    hasAccessToRepo,
    identityAdministers,
    liveAccount,
    repoIdFromRoute,
    routeRepoId,
    githubIdentity,
    mainIdentity,
    requireGithubIdentity,
    sessionUserOf,
    webSessionUser,
    webSessionUserOr,
    withRequiredUser,
    withSessionUser,
  )
where

import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe)
import Data.Map.Strict qualified as Map
import Garnix.DB qualified as DB
import Garnix.Forge.Registered (ForgeSource (..), withLiveIdentities)
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types as Types
import Servant.Auth.Server (AuthResult (Authenticated))

data Access = Read | Cancel

-- | The account of an authenticated request, with its identities as they are
-- now ('liveAccount'); 'Nothing' for anybody else, including a session of an
-- account that no longer exists or has no identity left on a forge garnix is
-- active on.
sessionUserOf :: AuthResult AuthJwtPayload -> M (Maybe User)
sessionUserOf = \case
  Authenticated session -> maybe (pure Nothing) liveAccount =<< DB.getUserById (sessionUserId session)
  _ -> pure Nothing

-- | The account with only its identities on forges garnix is active on
-- ('withLiveIdentities').
liveAccount :: User -> M (Maybe User)
liveAccount user = do
  live <- filterM (fmap isJust . lookupActiveForge . (^. forge)) (user ^. identities)
  pure $ withLiveIdentities live user

-- | Runs a handler with 'sessionUserOf'.
withSessionUser :: AuthResult AuthJwtPayload -> (Maybe User -> M a) -> M a
withSessionUser auth = (sessionUserOf auth >>=)

-- | Runs a handler that needs an account; 401 for anybody else.
withRequiredUser :: AuthResult AuthJwtPayload -> (User -> M a) -> M a
withRequiredUser auth action = sessionUserOf auth >>= maybe (throw Unauthorized) action

-- | The account of a browser session; 403 for the programmatic api, 401 for
-- anybody else.
webSessionUser :: AuthResult AuthJwtPayload -> M User
webSessionUser = webSessionUserOr Unauthorized

-- | 'webSessionUser', refusing anybody without an account with the given
-- error instead.
webSessionUserOr :: Error -> AuthResult AuthJwtPayload -> M User
webSessionUserOr refusal = \case
  Authenticated (ApiSession _) -> throw $ ForbiddenWithMessage "This endpoint is not available through the programmatic api."
  auth@(Authenticated (WebSession _)) -> sessionUserOf auth >>= maybe (throw refusal) pure
  _ -> throw refusal

-- | The identity that names the account where one name is shown: its github
-- one if it has one, else its first.
mainIdentity :: User -> Maybe ForgeIdentity
mainIdentity user = identityOn githubForge user <|> listToMaybe (user ^. identities)

-- | The account's github identity, which GitHub-only features (usage, hosts,
-- modules) act as. Their listings are empty without one.
githubIdentity :: User -> Maybe ForgeLogin
githubIdentity = loginOn githubForge

-- | 'githubIdentity' for GitHub-only actions; the same 403 for every one of
-- them without one.
requireGithubIdentity :: User -> M ForgeLogin
requireGithubIdentity =
  maybe (throw $ ForbiddenWithMessage "This needs a GitHub identity, and this account has none.") pure
    . githubIdentity

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

-- | @reqUser@ is the login that requested the build, on the repository's
-- forge.
hasAccessTo :: Maybe User -> RepoPublicity -> GhLogin -> RepoId -> M Bool
hasAccessTo user' repoIsPublic reqUser repo
  | isRequester user' reqUser repo = pure True
  | otherwise = hasAccessToRepo user' repoIsPublic repo

-- | Whichever of the account's identities asked: one per forge, so the one on
-- the repository's forge.
isRequester :: Maybe User -> GhLogin -> RepoId -> Bool
isRequester user' reqUser repo = loginOnRepoForge user' repo == Just reqUser

-- | Whether an identity administers every repository of a forge. A forge from
-- the configuration lists its admins by login; what the forge itself says
-- ('_forgeIdentityIsForgeAdmin') counts for nothing there, as on github.com it
-- means GitHub staff. A forge registered through the UI has no such list:
-- there the forge's own administrators are garnix's.
identityAdministers :: ForgeSource -> ForgeConfig -> ForgeIdentity -> Bool
identityAdministers source config identity =
  identity ^. forge == config ^. slug && case source of
    Configured -> identity ^. ghLogin `elem` config ^. admins
    Registered -> identity ^. isForgeAdmin

-- | Admins of one forge instance are not admins of another one's
-- repositories, and a forge garnix is not active on has no admins.
isAdminOf :: Maybe User -> RepoId -> M Bool
isAdminOf user' repo = (repo ^. forge `elem`) <$> administeredForges user'

-- | The forges whose every repository the account administers. A registered
-- forge only while it is active.
administeredForges :: Maybe User -> M [ForgeSlug]
administeredForges user' =
  fmap catMaybes
    $ forM (user' ^. _Just . identities)
    $ \identity -> do
      found <- lookupActiveForge (identity ^. forge)
      pure
        $ identity
        ^. forge
        <$ guard (any (\(source, instance') -> identityAdministers source (_forgeInstanceConfig instance') identity) found)

loginOnRepoForge :: Maybe User -> RepoId -> Maybe GhLogin
loginOnRepoForge user' repo = (^. ghLogin) <$> (loginOn (repo ^. forge) =<< user')

hasAccessToRepo :: Maybe User -> RepoPublicity -> RepoId -> M Bool
hasAccessToRepo user' repoIsPublic repo
  | isRepoPublic repoIsPublic = pure True
  | otherwise =
      isAdminOf user' repo >>= \case
        True -> pure True
        False -> isCollaboratorOn user' repo

-- | Collaborators are listed by login on the repository's forge. A forge
-- garnix is not active on lists nobody: one no longer configured, or a
-- registration that is pending or disabled.
isCollaboratorOn :: Maybe User -> RepoId -> M Bool
isCollaboratorOn user' repo = do
  configured <- isJust <$> lookupActiveForge (repo ^. forge)
  case loginOnRepoForge user' repo of
    Just login' | configured -> isListedCollaborator login' repo
    _ -> pure False

isListedCollaborator :: GhLogin -> RepoId -> M Bool
isListedCollaborator login' repo = do
  collaborators <- getCollaborators repo
  pure $ case collaborators of
    RepoNotFound -> False
    GhCollaborators collaborators' -> login' `elem` collaborators'

getCollaborators :: RepoId -> M GhCollaborators
getCollaborators repo = do
  resolveCredentials repo >>= \case
    Nothing -> pure RepoNotFound
    Just credentials' -> getRepoCollaborators credentials' repo

canCancelBuild :: Maybe User -> RepoPublicity -> GhLogin -> RepoId -> M Bool
canCancelBuild user' _ reqUser repo
  | isRequester user' reqUser repo = pure True
  | otherwise =
      isAdminOf user' repo >>= \case
        True -> pure True
        False -> isCollaboratorOn user' repo
