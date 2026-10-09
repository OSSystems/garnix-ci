module Garnix.API.Auth where

import Control.Lens
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import Data.Char (isAlphaNum, isAscii)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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
import Servant.Auth.Server.Internal.JWT (makeJWT)
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

data UserDto = UserDto
  { _userDtoUsername :: GhLogin,
    _userDtoForge :: ForgeSlug,
    _userDtoEmail :: Email,
    _userDtoIsAdmin :: Bool
  }
  deriving stock (Generic)

instance ToJSON UserDto where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

whoAmIAPI :: AuthResult AuthJwtPayload -> M (Maybe UserDto)
whoAmIAPI (Authenticated ((^. #user) -> user)) = do
  pure
    $ Just
    $ UserDto
      (user ^. githubLogin)
      (user ^. forge)
      (user ^. email)
      (user ^. subscriptionType == Admin)
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

getJwt :: Maybe Text -> AuthResult AuthJwtPayload -> M AuthJwtDto
getJwt mAuthHeader authResult = do
  authHeader <- case (mAuthHeader, authResult) of
    (_, Authenticated _) -> throw $ ForbiddenWithMessage "Creating JWTs is only allowed with the api access tokens."
    (Nothing, _) -> throw $ UnauthorizedWithMessage "Missing Authorization header"
    (Just authHeader, _) -> pure authHeader
  (username, password) <- case parseBasicAuth authHeader of
    Left err -> throw $ BadRequest $ cs err
    Right creds -> pure creds
  user <-
    withError
      ( errLens %~ \case
          NoSuchUser _ -> Unauthorized
          err -> err
      )
      $ DB.getUser
      =<< maybe (throw Unauthorized) pure (parseForgeLoginText username)
  isValid <- isAccessTokenValid (user ^. id) (AccessToken password) (^. #api)
  when (not isValid) $ do
    throw Unauthorized
  jwtSettings' <- view #jwtSettings
  expiresAt <- sessionExpiresAt
  jwt <- liftIO $ makeJWT (ApiSession user) jwtSettings' (Just expiresAt)
  jwt <- case jwt of
    Left err -> throw $ OtherError $ "Failed to create JWT: " <> show err
    Right jwt -> pure jwt
  pure
    $ AuthJwtDto
      { token = cs jwt,
        expiresAt
      }

type CallbackCookies a =
  Headers
    '[ Header "Set-Cookie" SetCookie,
       Header "Set-Cookie" SetCookie,
       Header "Set-Cookie" SetCookie
     ]
    a

data LoginAPI route = LoginAPI
  { _loginAPILogin :: route :- Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] LoginLinks),
    _loginAPILogout ::
      route
        :- Delete
             '[JSON]
             ( Headers
                 '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
                 ()
             ),
    _loginAPILoginCallback ::
      route
        :- "cb"
        :> QueryParam "code" OAuthCode
        :> QueryParam "state" Text
        :> Header "Cookie" Text
        :> Get '[JSON] (CallbackCookies GhLogin)
  }
  deriving (Generic)

data SignupAPI route = SignupAPI
  { _signupAPISignup :: route :- Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] SignupLinks),
    _signupAPISignupCallback ::
      route
        :- "fill"
        :> QueryParam "code" OAuthCode
        :> QueryParam "state" Text
        :> Header "Cookie" Text
        :> Get '[JSON] (CallbackCookies (CreatingUser ())),
    _signupAPIFinishSignup ::
      route
        :- Auth '[Cookie] (CreatingUser (GhUserCredentials Text))
        :> ReqBody '[JSON] CreateUser
        :> Post
             '[JSON]
             ( Headers
                 '[ Header "Set-Cookie" SetCookie,
                    Header "Set-Cookie" SetCookie
                  ]
                 GhLogin
             )
  }
  deriving (Generic)

-- | Logging in through one forge instance. The account it yields is the
-- 'ForgeLogin' on that forge: the same name on another forge is somebody else.
loginAPI :: ForgeSlug -> LoginAPI (AsServerT M)
loginAPI slug =
  LoginAPI
    { _loginAPILogin = login slug,
      _loginAPILogout = logout,
      _loginAPILoginCallback = loginCallback slug
    }

signupAPI :: ForgeSlug -> SignupAPI (AsServerT M)
signupAPI slug =
  SignupAPI
    { _signupAPISignup = signup slug,
      _signupAPISignupCallback = signupCallback slug,
      _signupAPIFinishSignup = finishSignup
    }

-- | The link is under the @github@ key whichever forge it points to, so that
-- the response keeps one shape across forges.
login :: ForgeSlug -> M (Headers '[Header "Set-Cookie" SetCookie] LoginLinks)
login slug = do
  nonce <- newOAuthNonce
  link <- authorizeLink slug LoginFlow (getOAuthNonce nonce)
  cookie <- oauthStateCookie nonce
  pure $ addHeader cookie $ LoginLinks {_loginLinksGithub = link}

-- * The OAuth state
--
-- A login or a signup puts a fresh nonce in the OAuth @state@ and sets a
-- cookie named by it in the browser that starts it, so several flows may be
-- in flight in one browser. The callback only goes on when this browser holds
-- the state's cookie, and spends it whatever happens: a callback URL somebody
-- else made, with their own code, logs nobody into their account, and a
-- state that leaks is worth nothing once its browser has used it.

-- | The nonce of one flow, as 'newOAuthNonce' makes it. Only a parsed nonce
-- names a cookie: the state in a callback URL may hold anything.
newtype OAuthNonce = OAuthNonce {getOAuthNonce :: Text}
  deriving stock (Eq, Show)

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
authorizeLink :: ForgeSlug -> OAuthFlow -> Text -> M Text
authorizeLink slug flow state = do
  oauth <- forgeOauth slug flow
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
spendingOAuthNonce ::
  OAuthNonce ->
  M (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] a) ->
  M (CallbackCookies a)
spendingOAuthNonce nonce callback = do
  spent <- spentOAuthStateCookie nonce
  try callback >>= \case
    Right response -> pure $ addHeader spent response
    Left e -> throwError e {err = WithSetCookies [cs $ toLazyByteString $ renderSetCookie spent] e}

logout ::
  M
    ( Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        ()
    )
logout = do
  cookieSettings' <- view #cookieSettings
  return $ clearSession cookieSettings' ()

signup :: ForgeSlug -> M (Headers '[Header "Set-Cookie" SetCookie] SignupLinks)
signup slug = do
  nonce <- newOAuthNonce
  link <- authorizeLink slug SignupFlow (getOAuthNonce nonce)
  cookie <- oauthStateCookie nonce
  pure
    $ addHeader cookie
    $ SignupLinks
      { _signupLinksGithub = link
      }

loginCallback ::
  ForgeSlug ->
  Maybe OAuthCode ->
  Maybe Text ->
  Maybe Text ->
  M (CallbackCookies GhLogin)
loginCallback slug code state cookies = do
  -- A slug naming no forge is a page that does not exist (see 'forgeOauth').
  _ <- forgeOauth slug LoginFlow
  nonce <- parsedOAuthNonce state
  spendingOAuthNonce nonce $ loginCallback' slug code cookies nonce

loginCallback' ::
  ForgeSlug ->
  Maybe OAuthCode ->
  Maybe Text ->
  OAuthNonce ->
  M
    ( Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        GhLogin
    )
loginCallback' slug code cookies nonce = do
  checkOAuthNonce cookies nonce
  (login', _, credentials) <- callbackHelper slug LoginFlow code
  cookieSettings' <- sessionCookieSettings
  jwtSettings' <- view #jwtSettings
  user <- DB.getUser login' <?> "calling getUser"
  storeCredentialsFor (user ^. id) credentials <?> "storing the github credentials"
  mApplyCookies <-
    liftIO (acceptLogin cookieSettings' jwtSettings' (WebSession user))
      <?> "calling acceptLogin"
  case mApplyCookies of
    Nothing -> throw Unauthorized
    Just applyCookies ->
      return
        $ applyCookies
        $ user
        ^. githubLogin

signupCallback ::
  ForgeSlug ->
  Maybe OAuthCode ->
  Maybe Text ->
  Maybe Text ->
  M (CallbackCookies (CreatingUser ()))
signupCallback slug code state cookies = do
  _ <- forgeOauth slug SignupFlow
  nonce <- parsedOAuthNonce state
  spendingOAuthNonce nonce $ signupCallback' slug code cookies nonce

signupCallback' ::
  ForgeSlug ->
  Maybe OAuthCode ->
  Maybe Text ->
  OAuthNonce ->
  M
    ( Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        (CreatingUser ())
    )
signupCallback' slug code cookies nonce = do
  checkOAuthNonce cookies nonce
  (login', email', credentials) <- callbackHelper slug SignupFlow code
  eUser <- try $ DB.getUser login' <?> "calling getUser"
  exists <- case eUser of
    Right _ -> pure True
    Left ErrorWithContext {err = NoSuchUser {}} -> pure False
    Left e -> throwError e
  let creatingUser =
        CreatingUser
          { _creatingUserExists = exists,
            _creatingUserForge = login' ^. forge,
            _creatingUserGithubLogin = login' ^. ghLogin,
            _creatingUserEmail = email',
            _creatingUserGithubToken = credentials
          }
  cookieSettings' <- sessionCookieSettings
  jwtSettings' <- view #jwtSettings
  mApplyCookies <- case eUser of
    Right user -> do
      storeCredentialsFor (user ^. id) credentials
      liftIO $ acceptLogin cookieSettings' jwtSettings' (WebSession user)
    _ -> liftIO $ acceptLogin cookieSettings' jwtSettings' creatingUser
  case mApplyCookies of
    Nothing -> throw Unauthorized
    Just applyCookies -> return $ applyCookies (void creatingUser)

callbackHelper :: ForgeSlug -> OAuthFlow -> Maybe OAuthCode -> M (ForgeLogin, Email, GhUserCredentials Text)
callbackHelper _ _ Nothing = throw $ OtherError "'code' param missing"
callbackHelper slug flow (Just code) = do
  oauth <- forgeOauth slug flow
  credentials <-
    exchangeOauthCode slug (OA.oauthCallback oauth) code
      <?> "exchanging the oauth code"
  (login', email') <- getCurrentUser slug (credentials ^. accessToken)
  pure (ForgeLogin slug login', email', credentials)

finishSignup ::
  AuthResult (CreatingUser (GhUserCredentials Text)) ->
  CreateUser ->
  M (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] GhLogin)
finishSignup (Authenticated cUser) addenda = do
  -- The things in AuthResult we can trust, because we put them there
  admins' <- (^. admins) . _forgeInstanceConfig <$> forgeInstanceFor (cUser ^. forge)
  user <-
    DB.newUser
      (ForgeLogin (cUser ^. forge) (cUser ^. githubLogin))
      (addenda ^. email)
      (subscriptionFor admins' (cUser ^. githubLogin))
      (addenda ^. agreeToEmails)
  storeCredentialsFor (user ^. id) (cUser ^. githubToken)
  cookieSettings' <- sessionCookieSettings
  jwtSettings' <- view #jwtSettings
  mApplyCookies <- liftIO $ acceptLogin cookieSettings' jwtSettings' (WebSession user)
  case mApplyCookies of
    Nothing -> throw Unauthorized
    Just applyCookies -> return $ applyCookies $ user ^. githubLogin
finishSignup _ _ = throw $ OtherError "Did not receive expected user info"

-- | A new account administers garnix when its forge instance lists its login
-- among the admins.
subscriptionFor :: [GhLogin] -> GhLogin -> SubscriptionType
subscriptionFor admins' login'
  | login' `elem` admins' = Admin
  | otherwise = FreeSubscription

data OAuthFlow = LoginFlow | SignupFlow

-- | The OAuth app of one forge instance.
forgeOauth :: ForgeSlug -> OAuthFlow -> M OA.OAuth2
forgeOauth slug flow = do
  -- The slug comes from the URL: one that names no configured instance is a
  -- page that does not exist, not a server error.
  configured <- view #forges
  config' <- maybe (throw NotFound) (pure . _forgeInstanceConfig) (Map.lookup slug configured)
  fromRelativeUrl <- relativeUrlConverter
  pure $ forgeOAuth2 fromRelativeUrl slug config' flow

-- | GitHub and Gitea both serve the OAuth app under @/login/oauth/@ on their
-- web host. The first argument turns a path into an absolute garnix URL.
forgeOAuth2 :: (Text -> Text) -> ForgeSlug -> ForgeConfig -> OAuthFlow -> OA.OAuth2
forgeOAuth2 fromRelativeUrl slug config' flow =
  OA.OAuth2
    { oauthClientId = config' ^. oAuthClientId,
      oauthClientSecret = config' ^. oAuthClientSecret,
      oauthOAuthorizeEndpoint = webUrl' <> "/login/oauth/authorize",
      oauthAccessTokenEndpoint = webUrl' <> "/login/oauth/access_token",
      oauthCallback = fromRelativeUrl (oauthCallbackPath slug flow),
      oauthScopes = []
    }
  where
    webUrl' = config' ^. Garnix.Types.webUrl

-- | Where the forge sends the browser back to. GitHub keeps the callbacks its
-- OAuth app has always been registered with; every other forge calls back
-- under its own slug.
oauthCallbackPath :: ForgeSlug -> OAuthFlow -> Text
oauthCallbackPath slug flow
  | slug == githubForge = case flow of
      LoginFlow -> "login/cb"
      SignupFlow -> "signup/fill"
  | otherwise = case flow of
      LoginFlow -> "auth/" <> getForgeSlug slug <> "/login/cb"
      SignupFlow -> "auth/" <> getForgeSlug slug <> "/signup/fill"
