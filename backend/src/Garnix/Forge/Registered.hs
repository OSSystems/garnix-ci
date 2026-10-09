-- | Gitea/Forgejo instances people register through the garnix UI, as opposed
-- to the ones the operator configures in @GARNIX_FORGES_FILE@: the rules that
-- need neither the database nor the network.
module Garnix.Forge.Registered
  ( ForgeSource (..),
    renderForgeSource,
    ForgeStatus (..),
    renderForgeStatus,
    parseForgeStatus,
    RegistrationUrl (..),
    normaliseRegistrationUrl,
    pendingForgeTtl,
    pendingForgeLock,
    disabledForgeQuarantine,
    LastDisabled (..),
    forgetOnActivation,
    mayManageRegisteredForge,
    pendingLoginRefusal,
    pendingLogin,
    withLiveIdentities,
  )
where

import Data.Char (isAsciiLower, isDigit)
import Data.Text qualified as T
import Garnix.Prelude
import Garnix.Types
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI)

-- | Where a forge instance garnix talks to came from.
data ForgeSource
  = -- | The forges file, or github.com from the GitHub App settings. Trusted:
    -- the operator chose them.
    Configured
  | -- | Registered by someone through the UI. Untrusted: every connection to
    -- it is checked, and nothing of it is handed to git or nix.
    Registered
  deriving stock (Eq, Show)

renderForgeSource :: ForgeSource -> Text
renderForgeSource = \case
  Configured -> "configured"
  Registered -> "registered"

instance ToJSON ForgeSource where
  toJSON = toJSON . renderForgeSource

-- | A registered forge is pending until an OAuth through it succeeds, which
-- proves the client id and secret it was registered with work on it.
-- Disabling one keeps its row, its identities and the history that names its
-- slug.
data ForgeStatus = ForgePending | ForgeActive | ForgeDisabled
  deriving stock (Eq, Show, Enum, Bounded)

renderForgeStatus :: ForgeStatus -> Text
renderForgeStatus = \case
  ForgePending -> "pending"
  ForgeActive -> "active"
  ForgeDisabled -> "disabled"

parseForgeStatus :: Text -> Maybe ForgeStatus
parseForgeStatus text' = find ((== text') . renderForgeStatus) [minBound .. maxBound]

-- | How long a registration may wait for its first successful OAuth.
pendingForgeTtl :: NominalDiffTime
pendingForgeTtl = 24 * 60 * 60

-- | How long a registration holds its host against registrations from other
-- browsers: long enough to complete its OAuth, short enough that one never
-- finished does not keep the host from others for long.
pendingForgeLock :: NominalDiffTime
pendingForgeLock = 15 * 60

-- | How long a disabled forge stays the forge everyone logged in through:
-- registered again within it, it comes back with everyone. Past it, its host
-- may well have changed hands (a domain expires, and is bought), and a new
-- registration forgets everyone who logged in through it before.
disabledForgeQuarantine :: NominalDiffTime
disabledForgeQuarantine = 30 * 24 * 60 * 60

-- | When the forge a registration takes over was last disabled, against
-- 'disabledForgeQuarantine', on the database's clock.
data LastDisabled
  = -- | Never: the host is new, or its row is gone (a registration that was
    -- never completed is purged, and so is one that replaced a forge
    -- disabled longer ago than the quarantine).
    NeverDisabled
  | DisabledWithinQuarantine
  | DisabledBeforeQuarantine
  deriving stock (Eq, Show, Enum, Bounded)

-- | Whether activating a registration forgets the identities on its forge,
-- and the accounts left with none ('Garnix.DB.Forges.forgetIdentitiesOn'):
-- unless it brings back a forge disabled within the quarantine. Identities on
-- a host never registered before are those of a configured forge the
-- operator removed, which vouch for nobody on it now.
forgetOnActivation :: LastDisabled -> Bool
forgetOnActivation = \case
  NeverDisabled -> True
  DisabledWithinQuarantine -> False
  DisabledBeforeQuarantine -> True

-- | A forge URL as someone typed it, normalised.
data RegistrationUrl = RegistrationUrl
  { -- | The host, which is also the slug of a forge registered under it.
    registrationHost :: Text,
    -- | @scheme://host[:port]@, without a trailing slash.
    registrationWebUrl :: Text
  }
  deriving stock (Eq, Show)

-- | Normalises a forge URL: https only (unless the first argument allows
-- plain http, for tests), a lower-case host, no trailing slash. A forge
-- served under a sub-path is refused: registered forges are named by their
-- host alone, and a forge under a path has to be configured by the operator.
normaliseRegistrationUrl :: Bool -> Text -> Either Text RegistrationUrl
normaliseRegistrationUrl allowHttp raw = do
  uri <- maybe (Left $ "not an absolute URL: " <> raw) Right $ parseAbsoluteURI (cs $ T.strip raw)
  let scheme = T.toLower (cs $ uriScheme uri)
  unless (scheme == "https:" || (allowHttp && scheme == "http:"))
    $ Left "garnix only registers forges served over https"
  authority <- maybe (Left "the URL has no host") Right $ uriAuthority uri
  unless (null $ uriUserInfo authority) $ Left "the URL must not contain a user name or password"
  let host = T.dropWhileEnd (== '.') $ T.toLower $ cs $ uriRegName authority
  when (T.null host) $ Left "the URL has no host"
  unless (T.all (\c -> isAsciiLower c || isDigit c || c `elem` ['.', '-']) host)
    $ Left "the host must be a DNS name (letters, digits, '.' and '-')"
  when (ForgeSlug host == githubForge) $ Left "the host \"github\" is reserved"
  unless (null (uriQuery uri) && null (uriFragment uri))
    $ Left "the URL must not have a query or a fragment"
  unless (uriPath uri `elem` ["", "/"])
    $ Left
    $ "garnix only registers a forge served at the root of its host, not under the path "
    <> cs (uriPath uri)
    <> ". A forge under a sub-path can still be configured by whoever runs this garnix (services.garnixServer.forges)."
  pure
    RegistrationUrl
      { registrationHost = host,
        registrationWebUrl = scheme <> "//" <> host <> cs (uriPort authority)
      }

-- | Who may replace a registered forge's client secret (which re-enables a
-- disabled one), or disable it: the account that registered it (the one that
-- completed its first OAuth), or an account whose identity on that very forge
-- the forge calls an administrator. What another forge says of an identity
-- there never counts.
mayManageRegisteredForge ::
  ForgeSlug ->
  -- | The account that registered it
  Maybe UserId ->
  -- | The account asking
  UserId ->
  -- | Its identity on that forge, if it has one
  Maybe ForgeIdentity ->
  Bool
mayManageRegisteredForge slug' registeredBy caller identity =
  registeredBy
    == Just caller
    || any (\identity' -> identity' ^. forge == slug' && identity' ^. isForgeAdmin) identity

-- | The account with only the given identities, the ones on forges garnix is
-- active on: an identity on a registered forge that is pending, disabled, or
-- ignored while registration is off grants nothing, and names nothing. An
-- account left with none has no session: disabling a forge, or turning
-- registration off, ends the sessions of the accounts that only log in
-- through it.
withLiveIdentities :: [ForgeIdentity] -> User -> Maybe User
withLiveIdentities live user = (user & identities .~ live) <$ guard (not $ null live)

-- | Why a login through a forge, from a browser that holds the token of its
-- registration or not, must not go on: a registered forge still pending logs
-- in only whoever registered it, whose login is what activates it. Anybody
-- else would get an account through a forge nobody vouched for yet.
pendingLoginRefusal :: ForgeSlug -> ForgeStatus -> Bool -> Maybe Error
pendingLoginRefusal slug' status holdsToken = case status of
  ForgeActive -> Nothing
  ForgePending
    | holdsToken -> Nothing
    | otherwise ->
        Just
          $ ForbiddenWithMessage
          $ getForgeSlug slug'
          <> " is still waiting for whoever registered it to log in through it. Try again once they have."
  ForgeDisabled -> Just NotFound

-- | What a login or a connect through a registered forge does, from its
-- status, the hash of its registration token and the hash of the token the
-- browser presents: refused ('pendingLoginRefusal'), or let through, and if
-- it completes a pending registration, the hash that registration is
-- activated with.
pendingLogin :: ForgeSlug -> ForgeStatus -> Maybe Text -> Maybe Text -> Either Error (Maybe Text)
pendingLogin slug' status registered presented =
  maybe (Right activates) Left $ pendingLoginRefusal slug' status holdsToken
  where
    holdsToken = isJust presented && presented == registered
    activates = case status of
      ForgePending | holdsToken -> presented
      ForgePending -> Nothing
      ForgeActive -> Nothing
      ForgeDisabled -> Nothing
