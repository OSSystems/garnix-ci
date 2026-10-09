-- | Forges registered through the UI, as 'ForgeInstance's: read from the
-- @forges@ table, their secrets decrypted, and every call they make sent
-- through the outbound guard.
module Garnix.Forge.Registry
  ( newForgeRegistration,
    ForgeCache,
    newForgeCache,
    cachedForgeSlugs,
    newForgeRegistrationWithCache,
    registeredForgeInstance,
    registeredForgeConfig,
    loginThrough,
    activatingRegistration,
    configuredOnHost,
    registrationCookieName,
    registrationTokenFromCookies,
    hashRegistrationToken,
  )
where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text.Encoding qualified as T
import Garnix.DB qualified as DB (withinTransaction)
import Garnix.DB.Forges qualified as DB
import Garnix.Forge.Gitea (giteaForgeApi, instanceHost)
import Garnix.Forge.Registered (forgetOnActivation, pendingLogin)
import Garnix.GithubUserToken (decryptSecret)
import Garnix.Monad
import Garnix.Prelude
import Garnix.RateLimit (newRateLimiter)
import Garnix.Types
import Network.HTTP.Client (Manager)
import Web.Cookie (parseCookiesText)

-- | The forges built from rows, with the @updated_at@ of the row each was
-- built from. Decrypting a secret runs age, so a forge is kept until its row
-- changes; one whose row is gone, disabled or stale is dropped, secrets and
-- all, the next time it is looked up or the active forges are listed.
newtype ForgeCache = ForgeCache (MVar (Map ForgeSlug (UTCTime, ForgeInstance)))

newForgeCache :: IO ForgeCache
newForgeCache = ForgeCache <$> newMVar mempty

cachedForgeSlugs :: ForgeCache -> IO [ForgeSlug]
cachedForgeSlugs (ForgeCache cache) = Map.keys <$> readMVar cache

-- | Registration as the server runs it. The manager is the guarded one; the
-- flag allows plain http forge URLs, for tests.
newForgeRegistration :: Bool -> Manager -> IO ForgeRegistration
newForgeRegistration allowHttp manager' = do
  cache <- newForgeCache
  newForgeRegistrationWithCache cache allowHttp manager'

newForgeRegistrationWithCache :: ForgeCache -> Bool -> Manager -> IO ForgeRegistration
newForgeRegistrationWithCache (ForgeCache cache) allowHttp manager' = do
  startLimit <- newRateLimiter 30 60
  registerLimit <- newRateLimiter 10 (60 * 60)
  pure
    ForgeRegistration
      { -- The row itself is read on every lookup: it is cheap, and it lets
        -- another process's change apply at once.
        _forgeRegistrationLookup = \slug' ->
          shadowedByConfigured slug' >>= \case
            True -> pure Nothing
            False ->
              DB.getLiveRegisteredForge slug' >>= \case
                Nothing -> do
                  liftIO $ modifyMVar_ cache (pure . Map.delete slug')
                  pure Nothing
                Just row ->
                  Just
                    . RegisteredForge (DB.rowStatus row) (DB.rowRegistrationTokenHash row)
                    <$> cachedInstance cache manager' row,
        _forgeRegistrationListActive = do
          rows <- filterM (fmap not . shadowedByConfigured . DB.rowSlug) =<< DB.listActiveRegisteredForges
          liftIO $ modifyMVar_ cache (pure . (`Map.restrictKeys` Set.fromList (map DB.rowSlug rows)))
          -- One forge that cannot be built (its secret no longer decrypts)
          -- must not hide all the others.
          fmap catMaybes $ forM rows $ \row ->
            try (cachedInstance cache manager' row) >>= \case
              Right instance' -> pure $ Just instance'
              Left e -> do
                log Error $ "skipping the registered forge " <> getForgeSlug (DB.rowSlug row) <> ": " <> showPretty (err e)
                pure Nothing,
        _forgeRegistrationManager = manager',
        _forgeRegistrationAllowHttp = allowHttp,
        _forgeRegistrationStartLimit = startLimit,
        _forgeRegistrationRegisterLimit = registerLimit
      }

-- | Whether a configured forge takes the host a registered forge is named
-- after: the operator configuring an instance someone had registered takes it
-- over.
shadowedByConfigured :: ForgeSlug -> M Bool
shadowedByConfigured (ForgeSlug host) = isJust . configuredOnHost host <$> view #forges

-- | The configured forge that takes a host: one served on it, or one whose
-- slug is the host. The one rule for registered forges and registrations a
-- configured forge shadows.
configuredOnHost :: Text -> Map ForgeSlug ForgeInstance -> Maybe ForgeSlug
configuredOnHost host configured =
  listToMaybe
    [ slug'
    | (slug', instance') <- Map.toList configured,
      slug' == ForgeSlug host || instanceHost (_forgeInstanceConfig instance') == Just host
    ]

cachedInstance :: MVar (Map ForgeSlug (UTCTime, ForgeInstance)) -> Manager -> DB.RegisteredForgeRow -> M ForgeInstance
cachedInstance cache manager' row = do
  cached <- liftIO $ Map.lookup (DB.rowSlug row) <$> readMVar cache
  case cached of
    Just (updatedAt, instance') | updatedAt == DB.rowUpdatedAt row -> pure instance'
    _ -> do
      instance' <- registeredForgeInstance manager' <$> registeredForgeConfig row
      liftIO $ modifyMVar_ cache (pure . Map.insert (DB.rowSlug row) (DB.rowUpdatedAt row, instance'))
      pure instance'

-- | A row as the 'ForgeConfig' the Gitea forge is built from. Registered
-- forges have no bot token and no configured admins: the identities their
-- forge calls administrators count instead ('Garnix.Access.identityAdministers',
-- 'Garnix.Forge.Registered.mayManageRegisteredForge').
registeredForgeConfig :: DB.RegisteredForgeRow -> M ForgeConfig
registeredForgeConfig row = do
  clientSecret <- decryptSecret (DB.rowOAuthClientSecret row)
  webhookSecret' <- decryptSecret (DB.rowWebhookSecret row)
  pure
    ForgeConfig
      { _forgeConfigSlug = DB.rowSlug row,
        _forgeConfigKind = GiteaForgeKind,
        _forgeConfigWebUrl = DB.rowWebUrl row,
        _forgeConfigApiUrl = DB.rowApiUrl row,
        _forgeConfigWebhookSecret = T.encodeUtf8 webhookSecret',
        _forgeConfigOAuthClientId = DB.rowOAuthClientId row,
        _forgeConfigOAuthClientSecret = clientSecret,
        _forgeConfigApiToken = Nothing,
        _forgeConfigAdmins = []
      }

-- | A registered forge: the Gitea forge a configured instance gets, with two
-- differences. Every call it makes goes through the guarded manager. And it
-- is login-only: it resolves no repository and hands out no clone URL, so no
-- URL of it ever reaches git or nix, which resolve names themselves and could
-- be led to a private address.
registeredForgeInstance :: Manager -> ForgeConfig -> ForgeInstance
registeredForgeInstance manager' config =
  ForgeInstance
    { _forgeInstanceConfig = config,
      _forgeInstanceForge =
        Forge
          { _forgeResolveCredentials = \_ -> pure Nothing,
            _forgeResolveRepo = \_ -> pure Nothing,
            _forgeGetDefaultBranch = \credentials' repo -> guarded $ _forgeGetDefaultBranch gitea credentials' repo,
            _forgeGetHeadCommit = \token repo branch -> guarded $ _forgeGetHeadCommit gitea token repo branch,
            _forgeNewBuildReport = \repoInfo' report -> guarded $ _forgeNewBuildReport gitea repoInfo' report,
            _forgeUpdateBuildReport = \runId' report repoInfo' -> guarded $ _forgeUpdateBuildReport gitea runId' report repoInfo',
            _forgeDoesRepoFileExist = \commitInfo path -> guarded $ _forgeDoesRepoFileExist gitea commitInfo path,
            _forgeGetRemote = \_ -> loginOnly "clone",
            _forgeGetRepoCollaborators = \credentials' repo -> guarded $ _forgeGetRepoCollaborators gitea credentials' repo,
            _forgeGetRepoPublicity = \credentials' repo -> guarded $ _forgeGetRepoPublicity gitea credentials' repo,
            _forgeOpenPullRequest = \_ _ -> loginOnly "open pull requests on",
            _forgeExchangeOauthCode = \callbackUrl code -> guarded $ _forgeExchangeOauthCode gitea callbackUrl code,
            _forgeRefreshUserCredentials = \token -> guarded $ _forgeRefreshUserCredentials gitea token,
            _forgeGetCurrentUser = \token -> guarded $ _forgeGetCurrentUser gitea token,
            _forgeGetPullRequestsForCommit = \repoInfo' commit' -> guarded $ _forgeGetPullRequestsForCommit gitea repoInfo' commit',
            _forgeCommentOnPullRequest = \repoInfo' prId body' -> guarded $ _forgeCommentOnPullRequest gitea repoInfo' prId body'
          }
    }
  where
    gitea = giteaForgeApi config
    guarded :: M a -> M a
    guarded = local (#manager .~ manager')
    loginOnly :: Text -> M a
    loginOnly what =
      throw
        $ OtherError
        $ "garnix does not "
        <> what
        <> " repositories of "
        <> getForgeSlug (config ^. slug)
        <> ": forges registered through the UI are only used to log in"

-- | Whether a login or a connect through the forge, from a browser with
-- these cookies, may go on: refused ('Garnix.Forge.Registered.pendingLogin'),
-- or the hash of the registration token it completes, if it completes a
-- pending registration. A configured forge, or a slug naming no forge,
-- completes nothing (an unknown slug is refused elsewhere, as a page that does
-- not exist).
loginThrough :: ForgeSlug -> Maybe Text -> M (Maybe Text)
loginThrough slug' cookies =
  lookupRegisteredForge slug' >>= \case
    Nothing -> pure Nothing
    Just registered ->
      either throw pure
        $ pendingLogin
          slug'
          (_registeredForgeStatus registered)
          (_registeredForgeTokenHash registered)
          (hashRegistrationToken <$> registrationTokenFromCookies slug' cookies)

-- | Runs a login or a connect through the forge, which answers the account it
-- logged in to. When it completes a pending registration ('loginThrough'), it
-- runs in one transaction with the registration's activation. Unless the
-- registration brings back a forge disabled within the quarantine
-- ('forgetOnActivation'), the identities on the forge from before are
-- forgotten first ('DB.forgetIdentitiesOn'), so that whoever registered its
-- host again lands in none of their accounts. Then the login runs, and the
-- forge becomes active, with the logged-in account as its registrant. A
-- registration that changed meanwhile undoes all of it. The login should run
-- nothing but SQL: the rows it touches stay locked until it ends.
activatingRegistration :: ForgeSlug -> Maybe Text -> M (UserId, a) -> M a
activatingRegistration slug' completes login' = case completes of
  Nothing -> snd <$> login'
  Just tokenHash -> DB.withinTransaction $ do
    lastDisabled' <- DB.lastDisabled slug'
    when (forgetOnActivation lastDisabled') $ DB.forgetIdentitiesOn slug'
    (registrant, result) <- login'
    activated <- DB.activatePendingForge slug' registrant tokenHash
    unless activated
      $ throw
      $ ConflictWithMessage
      $ "the registration of "
      <> getForgeSlug slug'
      <> " changed meanwhile; register it again"
    log Notice
      $ "the registered forge "
      <> getForgeSlug slug'
      <> " is now active, registered by the account "
      <> show (getUserId registrant)
      <> if forgetOnActivation lastDisabled' then "" else ", with everyone who logged in through it before it was disabled"
    pure result

-- | The cookie that holds, in the browser that submitted a registration, the
-- token proving it did.
registrationCookieName :: ForgeSlug -> Text
registrationCookieName slug' = "garnix-forge-registration-" <> getForgeSlug slug'

-- | The registration token for the forge in a @Cookie@ header.
registrationTokenFromCookies :: ForgeSlug -> Maybe Text -> Maybe Text
registrationTokenFromCookies slug' header =
  lookup (registrationCookieName slug') . parseCookiesText . T.encodeUtf8 =<< header

-- | Only the hash of a token is stored.
hashRegistrationToken :: Text -> Text
hashRegistrationToken = show . hashWith SHA256 . T.encodeUtf8
