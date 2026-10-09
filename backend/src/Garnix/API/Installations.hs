-- | Installing garnix on a Gitea/Forgejo account, organisation or instance,
-- and choosing the repositories each installation builds, under
-- @/api/forges/:slug/@:
--
--   - @GET installable@: what the caller may install on, and the
--     installation already there, if any.
--   - @GET installations@: the installations the caller may manage.
--   - @POST installations@ (@{ "kind", "target" }@): installs.
--   - @POST installations/:id/reinstall@: gives an installation a new
--     webhook, with the caller as its installer; the only way out of "needs
--     reinstall".
--   - @DELETE installations/:id@: uninstalls, deleting the webhook as far as
--     it can; builds stay.
--   - @GET installations/:id/repos@: a page of the repositories it covers,
--     which it builds, and how many it covers in all.
--   - @PUT@ / @DELETE installations/:id/repos/:owner/:name@: enables and
--     disables one.
--
-- Whether the caller may install on a target, and so manage the installation
-- there, is asked of the forge, with the caller's own token, on every
-- request. Only from a browser session: an API token installs nothing.
module Garnix.API.Installations
  ( InstallationsAPI (..),
    installationsAPI,
    InstallRequest (..),
    Installable (..),
    InstallationRepo (..),
    InstallationRepos (..),
  )
where

import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Garnix.Access (webSessionUser)
import Garnix.DB.Installations qualified as DB
import Garnix.Forge.Gitea (createGiteaHook, deleteGiteaHook, giteaInstallTargets, giteaMayInstallOn, giteaRepoExists, giteaReposOf)
import Garnix.Forge.Installation
import Garnix.Forge.Registered (ForgeSource (..))
import Garnix.GithubUserToken (encryptSecret, withUserToken)
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types hiding (login)
import Servant (Put)
import Servant.Auth.Server (AuthResult)

data InstallRequest = InstallRequest {installKind :: Text, installTarget :: Text}

instance FromJSON InstallRequest where
  parseJSON = Aeson.withObject "InstallRequest" $ \o ->
    InstallRequest <$> o .: "kind" <*> (fromMaybe "" <$> o Aeson..:? "target")

-- | A target the caller may install on.
data Installable = Installable InstallTarget (Maybe ForgeInstallationId)

instance ToJSON Installable where
  toJSON (Installable target installation) =
    Aeson.object
      [ "kind" .= renderTargetKind target,
        "target" .= targetName target,
        "installation" .= installation
      ]

-- | A repository an installation covers.
data InstallationRepo = InstallationRepo
  { installationRepoId :: RepoId,
    installationRepoPrivate :: Bool,
    -- | The installation it is enabled in, if any: this one or another.
    installationRepoEnabledIn :: Maybe ForgeInstallationId
  }

instance ToJSON InstallationRepo where
  toJSON repo =
    Aeson.object
      [ "owner" .= getGhRepoOwner (installationRepoId repo ^. repoUser),
        "name" .= (installationRepoId repo ^. repoName),
        "private" .= installationRepoPrivate repo,
        "enabledIn" .= installationRepoEnabledIn repo
      ]

-- | One page of the repositories an installation covers, and how many it
-- covers in all.
data InstallationRepos = InstallationRepos [InstallationRepo] Int

instance ToJSON InstallationRepos where
  toJSON (InstallationRepos repos total) = Aeson.object ["repos" .= repos, "total" .= total]

data InstallationsAPI r = InstallationsAPI
  { installationsInstallable :: r :- "installable" :> Get '[JSON] [Installable],
    installationsList :: r :- "installations" :> Get '[JSON] [ForgeInstallation],
    installationsInstall :: r :- "installations" :> ReqBody '[JSON] InstallRequest :> Post '[JSON] ForgeInstallation,
    installationsReinstall :: r :- "installations" :> Capture "installation" ForgeInstallationId :> "reinstall" :> Post '[JSON] ForgeInstallation,
    installationsUninstall :: r :- "installations" :> Capture "installation" ForgeInstallationId :> Delete '[JSON] (),
    -- | One page (from 1) of the repositories, as Gitea pages them: at most
    -- @limit@ (50 unless asked, which is also the most) per page, with how
    -- many there are in all.
    installationsRepos :: r :- "installations" :> Capture "installation" ForgeInstallationId :> "repos" :> QueryParam "page" Int :> QueryParam "limit" Int :> Get '[JSON] InstallationRepos,
    installationsEnable :: r :- "installations" :> Capture "installation" ForgeInstallationId :> "repos" :> Capture "owner" GhRepoOwner :> Capture "name" GhRepoName :> Put '[JSON] (),
    installationsDisable :: r :- "installations" :> Capture "installation" ForgeInstallationId :> "repos" :> Capture "owner" GhRepoOwner :> Capture "name" GhRepoName :> Delete '[JSON] ()
  }
  deriving stock (Generic)

installationsAPI :: ForgeSlug -> AuthResult AuthJwtPayload -> InstallationsAPI (AsServerT M)
installationsAPI slug' authResult =
  InstallationsAPI
    { installationsInstallable = do
        caller <- callerOn slug' authResult
        targets <- asCaller caller giteaInstallTargets
        forM targets $ \target -> Installable target . fmap forgeInstallationId <$> DB.getForgeInstallationOn slug' target,
      installationsList = do
        caller <- callerOn slug' authResult
        targets <- asCaller caller giteaInstallTargets
        filter (\installation -> any (sameInstallTarget $ forgeInstallationTarget installation) targets) <$> DB.listForgeInstallations slug',
      installationsInstall = \(InstallRequest kind' name) -> do
        caller <- callerOn slug' authResult
        target <- either (throw . BadRequest) pure $ parseInstallTarget kind' name
        mayInstall caller target
        DB.getForgeInstallationOn slug' target >>= \case
          Just existing -> throw $ alreadyInstalled existing
          Nothing -> pure ()
        (secret, encrypted) <- newSecret
        installation <-
          DB.insertForgeInstallation slug' target (callerLogin caller) encrypted
            >>= maybe (DB.getForgeInstallationOn slug' target >>= throw . maybe (ConflictWithMessage "garnix was installed there meanwhile") alreadyInstalled) pure
        -- Whatever fails, a garnix error or an exception (a connection
        -- reset, the guard's refusal), the row goes, or it would hold the
        -- target with no webhook.
        hookId <-
          asCaller caller (\config token -> webhookUrl installation >>= \url -> createGiteaHook config token target url secret)
            `whenErrorEither` const (DB.deleteForgeInstallation installation)
        DB.setForgeInstallationHook installation hookId
        log Notice $ "installed garnix on " <> describeTarget target <> " of " <> getForgeSlug slug' <> " for " <> getGhLogin (callerLogin caller)
        getInstallation' installation,
      installationsReinstall = \installation -> do
        caller <- callerOn slug' authResult
        existing <- managed caller installation
        (secret, encrypted) <- newSecret
        url <- webhookUrl installation
        hookId <- asCaller caller $ \config token -> createGiteaHook config token (forgeInstallationTarget existing) url secret
        -- Only over the installation as it was read: of two reinstalls at
        -- once, one wins, and the other deletes the webhook it created, as
        -- a reinstall that fails after creating it does.
        won <-
          DB.reinstallForgeInstallation existing (callerLogin caller) hookId encrypted
            `whenErrorEither` const (deleteHookBestEffort caller existing hookId)
        unless won $ do
          deleteHookBestEffort caller existing hookId
          throw $ ConflictWithMessage $ "garnix was reinstalled on " <> describeTarget (forgeInstallationTarget existing) <> " meanwhile"
        -- The old webhook signs with the old secret, which no longer
        -- verifies; deleting it only keeps the forge from retrying it.
        forM_ (forgeInstallationHookId existing) $ \old ->
          unless (old == hookId) $ void $ deleteHookBestEffort caller existing old
        log Notice $ "reinstalled garnix on " <> describeTarget (forgeInstallationTarget existing) <> " of " <> getForgeSlug slug' <> " for " <> getGhLogin (callerLogin caller)
        getInstallation' installation,
      installationsUninstall = \installation -> do
        caller <- callerOn slug' authResult
        existing <- managed caller installation
        forM_ (forgeInstallationHookId existing) $ deleteHookBestEffort caller existing
        DB.deleteForgeInstallation installation
        log Notice $ "uninstalled garnix from " <> describeTarget (forgeInstallationTarget existing) <> " of " <> getForgeSlug slug',
      installationsRepos = \installation page limit -> do
        caller <- callerOn slug' authResult
        existing <- managed caller installation
        let page' = max 1 (fromMaybe 1 page)
            limit' = max 1 (min 50 (fromMaybe 50 limit))
        (covered, total) <- asCaller caller $ \config token -> giteaReposOf config token (forgeInstallationTarget existing) page' limit'
        -- Gitea compares names without regard to case, and so does the
        -- list of enabled repositories.
        let key' = T.toLower . showRepoId
        enabled <- Map.fromList . map (first key') <$> DB.listEnabledRepos slug'
        pure
          $ InstallationRepos
            [ InstallationRepo repo private (Map.lookup (key' repo) enabled)
            | (repo, private) <- covered
            ]
            total,
      installationsEnable = \installation owner name -> do
        caller <- callerOn slug' authResult
        existing <- managed caller installation
        let repo = RepoId slug' owner name
        unless (coversRepo (forgeInstallationTarget existing) repo)
          $ throw
          $ BadRequest
          $ notCoveredMessage repo (forgeInstallationTarget existing)
        unlessM (asCaller caller $ \config token -> giteaRepoExists config token repo) $ throw NotFound
        DB.enableRepo installation repo >>= \case
          Right () -> pure ()
          Left other -> do
            holder <- DB.getForgeInstallation other
            throw $ ConflictWithMessage $ heldElsewhereMessage repo other holder,
      installationsDisable = \installation owner name -> do
        caller <- callerOn slug' authResult
        void $ managed caller installation
        DB.disableRepo installation (RepoId slug' owner name)
    }
  where
    alreadyInstalled existing =
      ConflictWithMessage
        $ "garnix is already installed on "
        <> describeTarget (forgeInstallationTarget existing)
        <> " (installation "
        <> show (getForgeInstallationId $ forgeInstallationId existing)
        <> "); reinstall it instead"
    newSecret = do
      secret <- randomBase64 32
      (secret,) <$> encryptSecret secret
    webhookUrl installation = do
      fromRelativeUrl <- relativeUrlConverter
      pure $ fromRelativeUrl $ installationWebhookPath slug' installation
    getInstallation' installation = DB.getForgeInstallation installation >>= maybe (throw NotFound) pure
    -- The installation, if the caller may manage it.
    managed caller installation = do
      existing <- DB.getForgeInstallation installation >>= maybe (throw NotFound) pure
      unless (forgeInstallationForge existing == slug') $ throw NotFound
      mayInstall caller (forgeInstallationTarget existing)
      pure existing
    mayInstall caller target =
      unlessM (asCaller caller $ \config token -> giteaMayInstallOn config token target)
        $ throw
        $ ForbiddenWithMessage
        $ notAllowedMessage slug' target
    -- A webhook left behind signs with a secret garnix no longer has, so it
    -- only fails to deliver.
    deleteHookBestEffort caller existing hookId = do
      deleted <- tryEither $ asCaller caller $ \config token -> deleteGiteaHook config token (forgeInstallationTarget existing) hookId
      case deleted of
        Right True -> pure ()
        _ -> log Notice $ "could not delete the webhook " <> show hookId <> " of " <> describeTarget (forgeInstallationTarget existing) <> "; it fails to deliver from now on"

-- | Why the caller may not install on the target.
notAllowedMessage :: ForgeSlug -> InstallTarget -> Text
notAllowedMessage slug' = \case
  InstallOnUser login' -> "only " <> getGhLogin login' <> " may install garnix on their account"
  InstallOnOrg org -> "only owners of " <> getGhLogin org <> " may install garnix on it"
  InstallOnInstance -> "only administrators of " <> getForgeSlug slug' <> " may install garnix on all of it"

notCoveredMessage :: RepoId -> InstallTarget -> Text
notCoveredMessage repo target = showRepoId repo <> " is not on " <> describeTarget target

-- | Why a repository cannot be enabled: another installation, if it still
-- exists, builds it, or would once reinstalled.
heldElsewhereMessage :: RepoId -> ForgeInstallationId -> Maybe ForgeInstallation -> Text
heldElsewhereMessage repo other holder =
  showRepoId repo
    <> " is already enabled in the installation "
    <> show (getForgeInstallationId other)
    <> " (on "
    <> maybe "another installation" (describeTarget . forgeInstallationTarget) holder
    <> ")"
    <> case forgeInstallationStatus <$> holder of
      Just (NeedsReinstall _) -> ", which needs a reinstall; reinstall it, or disable it there first"
      _ -> "; disable it there first"

-- | Who asks, on which forge.
data Caller = Caller
  { callerSource :: ForgeSource,
    callerConfig :: ForgeConfig,
    callerLogin :: GhLogin
  }

-- | The session's identity on an active Gitea forge.
callerOn :: ForgeSlug -> AuthResult AuthJwtPayload -> M Caller
callerOn slug' authResult = do
  user <- webSessionUser authResult
  (source, instance') <-
    lookupActiveForge slug' >>= \case
      Just found@(_, instance') | _forgeInstanceConfig instance' ^. kind == GiteaForgeKind -> pure found
      _ -> throw NotFound
  identity <-
    maybe (throw $ ForbiddenWithMessage $ "connect your " <> getForgeSlug slug' <> " account to garnix first") pure
      $ identityOn slug' user
  pure $ Caller source (_forgeInstanceConfig instance') (identity ^. ghLogin)

-- | Calls the forge with the caller's own token, renewed as needed, through
-- the outbound guard for a registered forge.
asCaller :: Caller -> (ForgeConfig -> GhToken -> M a) -> M a
asCaller caller action = do
  registration <- view #forgeRegistration
  guarded <- case (callerSource caller, registration) of
    (Configured, _) -> pure identity
    (Registered, Just registration') -> pure $ local (#manager .~ _forgeRegistrationManager registration')
    -- Never the plain manager for a host someone registered.
    (Registered, Nothing) -> throw $ OtherError "registered forges are off"
  guarded
    $ withUserToken (ForgeLogin (callerConfig caller ^. slug) (callerLogin caller))
    $ action (callerConfig caller)
