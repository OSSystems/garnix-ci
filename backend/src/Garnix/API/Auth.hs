module Garnix.API.Auth where

import Control.Lens
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import Data.Char (isAlphaNum, isAscii)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Garnix.Access (administeredForges, mainIdentity, webSessionUser, webSessionUserOr)
import Garnix.AccessToken
import Garnix.AccessToken.Types
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.GithubUserToken
import Garnix.Monad
import Garnix.ParseHttpBasicAuth
import Garnix.Prelude
import Garnix.Types hiding (login)
import Network.OAuth2 qualified as OA
import Servant.API (noHeader)
import Servant.Auth.Server
  ( Auth,
    AuthResult (Authenticated),
    Cookie,
    CookieSettings (..),
    IsSecure (..),
    JWT,
    acceptLogin,
    clearSession,
  )
import Servant.Auth.Server.Internal.JWT (makeJWT, verifyJWT)
import Web.Cookie

sessionExpiresAt :: M UTCTime
sessionExpiresAt = do
  lifetime <- view #sessionLifetime
  addTime lifetime <$> liftIO getCurrentTime

sessionCookieSettings :: M CookieSettings
sessionCookieSettings = do
  transportSettings <- view #cookieSettings
  lifetime <- view #sessionLifetime
  expiresAt <- sessionExpiresAt
  pure
    transportSettings
      { cookieExpires = Just expiresAt,
        cookieMaxAge = Just $ realToFrac $ toSeconds lifetime
      }

data IdentityDto = IdentityDto
  { _identityDtoForge :: ForgeSlug,
    _identityDtoLogin :: GhLogin
  }
  deriving stock (Generic)

instance ToJSON IdentityDto where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

-- | @username@ and @forge@ name the account's main identity, its github one if
-- it has one, for the frontend that predates accounts with several;
-- @identities@ lists them all. @is_admin@ is about github.com, whose admins
-- may set up the GitHub App: admins of another forge are not admins there.
data UserDto = UserDto
  { _userDtoUsername :: GhLogin,
    _userDtoForge :: ForgeSlug,
    _userDtoEmail :: Email,
    _userDtoIsAdmin :: Bool,
    _userDtoIdentities :: [IdentityDto]
  }
  deriving stock (Generic)

instance ToJSON UserDto where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

whoAmIAPI :: AuthResult AuthJwtPayload -> M (Maybe UserDto)
whoAmIAPI (Authenticated session) =
  DB.getUserById (sessionUserId session) >>= \case
    Nothing -> pure Nothing
    Just user -> do
      administered <- administeredForges (Just user)
      pure
        $ mainIdentity user
        <&> \main ->
          UserDto
            { _userDtoUsername = main ^. ghLogin,
              _userDtoForge = main ^. forge,
              _userDtoEmail = user ^. email,
              _userDtoIsAdmin = githubForge `elem` administered,
              _userDtoIdentities = [IdentityDto (i ^. forge) (i ^. ghLogin) | i <- user ^. identities]
            }
whoAmIAPI _ = pure Nothing

data AuthJwtAPI route = AuthJwtAPI
  { jwt :: route :- Header "Authorization" Text :> Auth '[JWT, Cookie] AuthJwtPayload :> Post '[JSON] AuthJwtDto
  }
  deriving stock (Generic)

data AuthJwtDto = AuthJwtDto
  { token :: Text,
    expiresAt :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

authJwtAPI :: AuthJwtAPI (AsServerT M)
authJwtAPI =
  AuthJwtAPI
    { jwt = getJwt
    }

-- | The user name is any identity of the account, as 'forgeLoginText'.
getJwt :: Maybe Text -> AuthResult AuthJwtPayload -> M AuthJwtDto
getJwt mAuthHeader authResult = do
  authHeader <- case (mAuthHeader, authResult) of
    (_, Authenticated _) -> throw $ ForbiddenWithMessage "Creating JWTs is only allowed with the api access tokens."
    (Nothing, _) -> throw $ UnauthorizedWithMessage "Missing Authorization header"
    (Just authHeader, _) -> pure authHeader
  (username, password) <- case parseBasicAuth authHeader of
    Left err -> throw $ BadRequest $ cs err
    Right creds -> pure creds
  userId <-
    maybe (throw Unauthorized) pure
      =<< maybe (pure Nothing) DB.lookupIdentityOwner (parseForgeLoginText username)
  isValid <- isAccessTokenValid userId (AccessToken password) (^. #api)
  when (not isValid) $ do
    throw Unauthorized
  jwtSettings' <- view #jwtSettings
  expiresAt <- sessionExpiresAt
  jwt <- liftIO $ makeJWT (ApiSession userId) jwtSettings' (Just expiresAt)
  jwt <- case jwt of
    Left err -> throw $ OtherError $ "Failed to create JWT: " <> show err
    Right jwt -> pure jwt
  pure
    $ AuthJwtDto
      { token = cs jwt,
        expiresAt
      }

type SessionCookies a =
  Headers
    '[ Header "Set-Cookie" SetCookie,
       Header "Set-Cookie" SetCookie
     ]
    a

-- | What a login or a connect answers: the account's main identity, as
-- @whoami@ names it, and whether another account has the email of an account
-- this login just created, so the person may already have an account to
-- connect this forge to instead. It never says which.
data LoginResult = LoginResult
  { username :: GhLogin,
    emailAlreadyUsed :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

type CallbackCookies a =
  Headers
    '[ Header "Set-Cookie" SetCookie,
       Header "Set-Cookie" SetCookie,
       Header "Set-Cookie" SetCookie
     ]
    a

data LoginAPI route = LoginAPI
  { _loginAPILogin :: route :- Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] LoginLinks),
    _loginAPILogout :: route :- Delete '[JSON] (SessionCookies ()),
    -- | Where the forge sends the browser back to, for a login and for a
    -- connect alike: the @state@ tells them apart (see 'connectStatePrefix').
    _loginAPILoginCallback ::
      route
        :- "cb"
        :> QueryParam "code" OAuthCode
        :> QueryParam "state" Text
        :> Header "Cookie" Text
        :> Auth '[Cookie] AuthJwtPayload
        :> Get '[JSON] (CallbackCookies LoginResult)
  }
  deriving (Generic)

-- | Logging in through one forge instance. The first login of an identity
-- garnix does not know yet creates its account.
loginAPI :: ForgeSlug -> LoginAPI (AsServerT M)
loginAPI slug =
  LoginAPI
    { _loginAPILogin = login slug,
      _loginAPILogout = logout,
      _loginAPILoginCallback = loginCallback slug
    }

-- | The link is under the @github@ key whichever forge it points to, so that
-- the response keeps one shape across forges.
login :: ForgeSlug -> M (Headers '[Header "Set-Cookie" SetCookie] LoginLinks)
login slug = do
  nonce <- newOAuthNonce
  link <- authorizeLink slug (getOAuthNonce nonce)
  cookie <- oauthStateCookie nonce
  pure $ addHeader cookie $ LoginLinks {_loginLinksGithub = link}

-- * The OAuth state
--
-- A login or a connect puts a fresh nonce in the OAuth @state@ and sets a
-- cookie named by it in the browser that starts it, so several flows may be
-- in flight in one browser. The callback only goes on when this browser holds
-- the state's cookie, and spends it whatever happens: a callback URL somebody
-- else made, with their own code, logs nobody into their account, and a
-- state that leaks attaches nothing once its browser has used it.

-- | The nonce of one flow, as 'newOAuthNonce' makes it. Only a parsed nonce
-- names a cookie: the state in a callback URL may hold anything.
newtype OAuthNonce = OAuthNonce {getOAuthNonce :: Text}
  deriving stock (Eq, Show)

instance ToJSON OAuthNonce where
  toJSON = toJSON . getOAuthNonce

instance FromJSON OAuthNonce where
  parseJSON value = parseJSON value >>= maybe (fail "not an OAuth nonce") pure . parseOAuthNonce . Just

oauthNonceLength :: Int
oauthNonceLength = 32

parseOAuthNonce :: Maybe Text -> Maybe OAuthNonce
parseOAuthNonce state = do
  nonce <- state
  guard $ T.length nonce == oauthNonceLength && T.all (\c -> isAscii c && isAlphaNum c) nonce
  pure $ OAuthNonce nonce

newOAuthNonce :: M OAuthNonce
newOAuthNonce = do
  candidate <- T.take oauthNonceLength . T.filter isAlphaNum <$> randomBase64 48
  maybe newOAuthNonce pure $ parseOAuthNonce (Just candidate)

oauthStateCookieName :: OAuthNonce -> ByteString
oauthStateCookieName nonce = "garnix-oauth-state-" <> cs (getOAuthNonce nonce)

oauthStateLifetime :: Duration
oauthStateLifetime = fromMinutes @Int 10

-- | The forge's authorize link. Without a state of the library's own: it
-- would base58-encode ours, and ours is URL-safe as it is.
authorizeLink :: ForgeSlug -> Text -> M Text
authorizeLink slug state = do
  oauth <- forgeOauth slug
  link <- OA.getAuthorize OA.OAuthStateless oauth ""
  pure $ link <> "&state=" <> state

oauthStateCookie :: OAuthNonce -> M SetCookie
oauthStateCookie nonce = do
  transport <- view #cookieSettings
  pure
    defaultSetCookie
      { setCookieName = oauthStateCookieName nonce,
        setCookieValue = "1",
        setCookiePath = Just $ fromMaybe "/" $ cookiePath transport,
        setCookieMaxAge = Just $ realToFrac $ toSeconds oauthStateLifetime,
        setCookieHttpOnly = True,
        setCookieSecure = cookieIsSecure transport == Secure,
        setCookieSameSite = Just sameSiteLax
      }

spentOAuthStateCookie :: OAuthNonce -> M SetCookie
spentOAuthStateCookie nonce = do
  cookie <- oauthStateCookie nonce
  pure cookie {setCookieValue = "", setCookieMaxAge = Just 0, setCookieExpires = Just $ posixSecondsToUTCTime 0}

-- | Whether this browser started the flow whose nonce the state carries.
holdsOAuthNonce :: Maybe Text -> OAuthNonce -> Bool
holdsOAuthNonce cookies nonce =
  any ((== oauthStateCookieName nonce) . fst) (maybe [] (parseCookies . cs) cookies)

-- | Refuses a callback unless this browser started the flow the state names.
checkOAuthNonce :: Maybe Text -> OAuthNonce -> M ()
checkOAuthNonce cookies nonce = unless (holdsOAuthNonce cookies nonce) $ throw notStartedHere

-- | A state that is no nonce of ours is refused before it names any cookie.
parsedOAuthNonce :: Maybe Text -> M OAuthNonce
parsedOAuthNonce = maybe (throw notStartedHere) pure . parseOAuthNonce

notStartedHere :: Error
notStartedHere = ForbiddenWithMessage "This login was not started in this browser, or it took too long. Start again."

-- | Runs a callback and spends its nonce's cookie, on an error response too.
spendingOAuthNonce :: OAuthNonce -> M (SessionCookies a) -> M (CallbackCookies a)
spendingOAuthNonce nonce callback = do
  spent <- spentOAuthStateCookie nonce
  try callback >>= \case
    Right response -> pure $ addHeader spent response
    Left e -> throwError e {err = WithSetCookies [cs $ toLazyByteString $ renderSetCookie spent] e}

logout :: M (SessionCookies ())
logout = do
  cookieSettings' <- view #cookieSettings
  return $ clearSession cookieSettings' ()

loginCallback ::
  ForgeSlug ->
  Maybe OAuthCode ->
  Maybe Text ->
  Maybe Text ->
  AuthResult AuthJwtPayload ->
  M (CallbackCookies LoginResult)
loginCallback slug code state cookies session = do
  -- A slug naming no forge is a page that does not exist (see 'forgeOauth').
  _ <- forgeOauth slug
  case T.stripPrefix connectStatePrefix =<< state of
    Just connectState -> connectCallback slug code connectState cookies session
    Nothing -> do
      nonce <- parsedOAuthNonce state
      spendingOAuthNonce nonce $ do
        checkOAuthNonce cookies nonce
        (identity, email', credentials) <- callbackHelper slug code
        (user, emailAlreadyUsed) <- loginOrCreateAccount identity email'
        storeCredentialsFor (identityForgeLogin identity) credentials
          <?> "storing the forge credentials"
        cookieSettings' <- sessionCookieSettings
        jwtSettings' <- view #jwtSettings
        mApplyCookies <-
          liftIO (acceptLogin cookieSettings' jwtSettings' (WebSession (user ^. id)))
            <?> "calling acceptLogin"
        case mApplyCookies of
          Nothing -> throw Unauthorized
          Just applyCookies -> applyCookies <$> loginResult user emailAlreadyUsed

loginResult :: User -> DB.EmailAlreadyUsed -> M LoginResult
loginResult user emailAlreadyUsed = do
  main <- maybe (throw $ OtherError "an account without identities") pure $ mainIdentity user
  pure LoginResult {username = main ^. ghLogin, emailAlreadyUsed = DB.getEmailAlreadyUsed emailAlreadyUsed}

-- | The account of a known identity, or a new account for an unknown one,
-- with the email the forge reported (see 'LoginResult').
loginOrCreateAccount :: ForgeIdentity -> Email -> M (User, DB.EmailAlreadyUsed)
loginOrCreateAccount identity email' =
  loginOrCreate identity email' =<< DB.lookupIdentityOwner (identityForgeLogin identity)

-- | 'loginOrCreateAccount' once the identity's owner was looked up, which
-- another callback of the same first login (a double click, a second tab)
-- may have changed since: the account it created is logged in to.
loginOrCreate :: ForgeIdentity -> Email -> Maybe UserId -> M (User, DB.EmailAlreadyUsed)
loginOrCreate identity email' = \case
  Just owner -> logInTo owner
  Nothing ->
    DB.createAccount identity email' >>= \case
      Right created -> pure created
      Left (DB.OwnedBy owner) -> logInTo owner
      Left clash -> throw $ IdentityConflict $ DB.identityClashMessage login' clash
  where
    login' = identityForgeLogin identity
    logInTo owner = do
      DB.setIdentityIsForgeAdmin login' (identity ^. isForgeAdmin)
      user <- DB.getUserById owner >>= maybe (throw $ OtherError "the identity's account is gone") pure
      pure (user, DB.EmailAlreadyUsed False)

callbackHelper :: ForgeSlug -> Maybe OAuthCode -> M (ForgeIdentity, Email, GhUserCredentials Text)
callbackHelper _ Nothing = throw $ OtherError "'code' param missing"
callbackHelper slug (Just code) = do
  oauth <- forgeOauth slug
  credentials <-
    exchangeOauthCode slug (OA.oauthCallback oauth) code
      <?> "exchanging the oauth code"
  (login', email', isForgeAdmin') <- getCurrentUser slug (credentials ^. accessToken)
  pure (ForgeIdentity slug login' isForgeAdmin', email', credentials)

-- * Connecting another forge to the account

-- | What a connect puts in the OAuth @state@, signed: the account and forge it
-- was started for. The callback only attaches an identity for the session that
-- started it, so a link or code from somebody else attaches nothing.
data ConnectState = ConnectState
  { connectUser :: UserId,
    connectForge :: ForgeSlug,
    -- | The nonce of the browser that started it (see 'checkOAuthNonce').
    connectNonce :: OAuthNonce
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJWT, FromJWT)

-- | Marks the @state@ of a connect. A login's state never starts with it.
connectStatePrefix :: Text
connectStatePrefix = "connect."

data ConnectAPI route = ConnectAPI
  { -- | The link to the forge's OAuth app; it calls back to the login callback.
    _connectAPIConnect ::
      route
        :- "connect"
        :> Auth '[JWT, Cookie] AuthJwtPayload
        :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] LoginLinks),
    -- | Detaches the account's identity on this forge. The body is @{}@, or
    -- @{"confirmDeleteModuleSettings": true}@ to also delete the module
    -- settings saved through it.
    _connectAPIDisconnect ::
      route
        :- "identity"
        :> Auth '[JWT, Cookie] AuthJwtPayload
        :> ReqBody '[JSON] DisconnectIdentity
        :> Delete '[JSON] NoContent
  }
  deriving (Generic)

data DisconnectIdentity = DisconnectIdentity
  { confirmDeleteModuleSettings :: Maybe Bool
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

connectAPI :: ForgeSlug -> ConnectAPI (AsServerT M)
connectAPI slug =
  ConnectAPI
    { _connectAPIConnect = connect slug,
      _connectAPIDisconnect = disconnect slug
    }

connect :: ForgeSlug -> AuthResult AuthJwtPayload -> M (Headers '[Header "Set-Cookie" SetCookie] LoginLinks)
connect slug session = do
  user <- webSessionUser session
  -- An unknown slug is a 404 before anything is signed.
  _ <- forgeOauth slug
  nonce <- newOAuthNonce
  jwtSettings' <- view #jwtSettings
  expiresAt <- addTime oauthStateLifetime <$> liftIO getCurrentTime
  state <-
    liftIO (makeJWT (ConnectState (user ^. id) slug nonce) jwtSettings' (Just expiresAt)) >>= \case
      Left e -> throw $ OtherError $ "Failed to sign the connect state: " <> show e
      Right state -> pure $ cs state
  link <- authorizeLink slug (connectStatePrefix <> state)
  cookie <- oauthStateCookie nonce
  pure $ addHeader cookie $ LoginLinks {_loginLinksGithub = link}

-- | A connect's state names the account and forge of the callback's session
-- and route.
validateConnect :: ConnectState -> UserId -> ForgeSlug -> Either Error ()
validateConnect state userId slug
  | connectUser state /= userId || connectForge state /= slug =
      Left $ ForbiddenWithMessage "This connect link was started for another session. Start connecting again."
  | otherwise = Right ()

-- | What a connect does with the identity the forge vouches for, from who
-- holds it: an identity of another account is never moved.
data ConnectAction = Refresh | Attach | Refuse DB.IdentityClash
  deriving stock (Eq, Show)

connectAction :: UserId -> Maybe UserId -> ConnectAction
connectAction userId = \case
  Nothing -> Attach
  Just owner
    | owner == userId -> Refresh
    | otherwise -> Refuse (DB.OwnedBy owner)

-- | Attaches the identity the forge vouches for to the session's account,
-- which keeps one identity per forge.
connectCallback ::
  ForgeSlug ->
  Maybe OAuthCode ->
  Text ->
  Maybe Text ->
  AuthResult AuthJwtPayload ->
  M (CallbackCookies LoginResult)
connectCallback slug code signedState cookies session = do
  jwtSettings' <- view #jwtSettings
  state <-
    liftIO (verifyJWT jwtSettings' (cs signedState))
      >>= maybe (throw $ ForbiddenWithMessage "This connect link is invalid or has expired. Start connecting again.") pure
  spendingOAuthNonce (connectNonce state) $ do
    -- Not a 401, which would log the browser out of what it came back to.
    user <- webSessionUserOr (ForbiddenWithMessage "Log in to connect a forge to your account.") session
    either throw pure $ validateConnect state (user ^. id) slug
    checkOAuthNonce cookies (connectNonce state)
    (identity, _, credentials) <- callbackHelper slug code
    let login' = identityForgeLogin identity
        refuse = throw . IdentityConflict . DB.identityClashMessage login'
    owner <- DB.lookupIdentityOwner login'
    case connectAction (user ^. id) owner of
      Refresh -> DB.setIdentityIsForgeAdmin login' (identity ^. isForgeAdmin)
      Attach -> DB.tryAddIdentity (user ^. id) identity >>= either refuse pure
      Refuse clash -> refuse clash
    storeCredentialsFor login' credentials <?> "storing the forge credentials"
    connected <- DB.getUserById (user ^. id) >>= maybe (throw Unauthorized) pure
    noHeader . noHeader <$> loginResult connected (DB.EmailAlreadyUsed False)

-- | Builds and other history the identity requested stay; its credentials go,
-- and so do module settings saved through it, once the request confirms it.
disconnect :: ForgeSlug -> AuthResult AuthJwtPayload -> DisconnectIdentity -> M NoContent
disconnect slug session body = do
  user <- webSessionUser session
  let moduleSettings
        | confirmDeleteModuleSettings body == Just True = DB.DeleteModuleSettings
        | otherwise = DB.KeepModuleSettings
  DB.removeIdentity (user ^. id) slug moduleSettings
    >>= either (throw . removalRefusalError slug) (const $ pure NoContent)

removalRefusalError :: ForgeSlug -> DB.RemovalRefusal -> Error
removalRefusalError slug = \case
  DB.NoSuchIdentity -> NotFound
  DB.LastIdentity ->
    IdentityConflict
      $ getForgeSlug slug
      <> " is the only forge your account logs in with. Connect another forge before disconnecting it."
  DB.HasModuleSettings ->
    IdentityConflict
      $ "Disconnecting "
      <> getForgeSlug slug
      <> " deletes the module settings saved through it. Confirm with confirmDeleteModuleSettings to disconnect anyway."

-- | The OAuth app of one forge instance.
forgeOauth :: ForgeSlug -> M OA.OAuth2
forgeOauth slug = do
  -- The slug comes from the URL: one that names no configured instance is a
  -- page that does not exist, not a server error.
  config' <- forgeConfigFor slug >>= maybe (throw NotFound) pure
  fromRelativeUrl <- relativeUrlConverter
  pure $ forgeOAuth2 fromRelativeUrl slug config'

-- | GitHub and Gitea both serve the OAuth app under @/login/oauth/@ on their
-- web host. The first argument turns a path into an absolute garnix URL.
forgeOAuth2 :: (Text -> Text) -> ForgeSlug -> ForgeConfig -> OA.OAuth2
forgeOAuth2 fromRelativeUrl slug config' =
  OA.OAuth2
    { oauthClientId = config' ^. oAuthClientId,
      oauthClientSecret = config' ^. oAuthClientSecret,
      oauthOAuthorizeEndpoint = webUrl' <> "/login/oauth/authorize",
      oauthAccessTokenEndpoint = webUrl' <> "/login/oauth/access_token",
      oauthCallback = fromRelativeUrl (oauthCallbackPath slug),
      oauthScopes = []
    }
  where
    webUrl' = config' ^. Garnix.Types.webUrl

-- | Where the forge sends the browser back to, after a login or a connect.
-- GitHub keeps the callback its OAuth app has always been registered with;
-- every other forge calls back under its own slug.
oauthCallbackPath :: ForgeSlug -> Text
oauthCallbackPath slug
  | slug == githubForge = "login/cb"
  | otherwise = "auth/" <> getForgeSlug slug <> "/login/cb"
