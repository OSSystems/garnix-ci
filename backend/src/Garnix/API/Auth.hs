module Garnix.API.Auth where

import Control.Lens
import Garnix.AccessToken
import Garnix.AccessToken.Types
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.GithubUserToken
import Garnix.Monad
import Garnix.ParseHttpBasicAuth
import Garnix.Prelude
import Garnix.Types hiding (login)
import GitHub qualified as GH
import Network.OAuth2 qualified as OA
import Servant.Auth.Server
  ( Auth,
    AuthResult (Authenticated),
    Cookie,
    CookieSettings (..),
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
      $ GhLogin username
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

data LoginAPI route = LoginAPI
  { _loginAPILogin :: route :- Get '[JSON] LoginLinks,
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
        :> Get
             '[JSON]
             ( Headers
                 '[ Header "Set-Cookie" SetCookie,
                    Header "Set-Cookie" SetCookie
                  ]
                 GhLogin
             )
  }
  deriving (Generic)

data SignupAPI route = SignupAPI
  { _signupAPISignup :: route :- Get '[JSON] SignupLinks,
    _signupAPISignupCallback ::
      route
        :- "fill"
        :> QueryParam "code" OAuthCode
        :> Get
             '[JSON]
             ( Headers
                 '[ Header "Set-Cookie" SetCookie,
                    Header "Set-Cookie" SetCookie
                  ]
                 (CreatingUser ())
             ),
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

loginAPI :: LoginAPI (AsServerT M)
loginAPI =
  LoginAPI
    { _loginAPILogin = login,
      _loginAPILogout = logout,
      _loginAPILoginCallback = loginCallback
    }

signupAPI :: SignupAPI (AsServerT M)
signupAPI =
  SignupAPI
    { _signupAPISignup = signup,
      _signupAPISignupCallback = signupCallback,
      _signupAPIFinishSignup = finishSignup
    }

login :: M LoginLinks
login = do
  oaState <- OA.newOAuthState
  ghOauth <- githubOauthLogin
  githubLink <- OA.getAuthorize oaState ghOauth "foo"
  return $ LoginLinks {_loginLinksGithub = githubLink}

logout ::
  M
    ( Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        ()
    )
logout = do
  cookieSettings' <- view #cookieSettings
  return $ clearSession cookieSettings' ()

signup :: M SignupLinks
signup = do
  oaState <- OA.newOAuthState
  ghOauth <- githubOauthSignup
  githubLink <- OA.getAuthorize oaState ghOauth "foo"
  return
    $ SignupLinks
      { _signupLinksGithub = githubLink
      }

loginCallback ::
  Maybe OAuthCode ->
  M
    ( Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        GhLogin
    )
loginCallback code = do
  (login', _, credentials) <- callbackHelper githubOauthLogin code
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
  Maybe OAuthCode ->
  M
    ( Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        (CreatingUser ())
    )
signupCallback code = do
  (login', email', credentials) <- callbackHelper githubOauthSignup code
  eUser <- try $ DB.getUser login' <?> "calling getUser"
  creatingUser <- case eUser of
    Right _ ->
      pure
        $ CreatingUser
          { _creatingUserExists = True,
            _creatingUserGithubLogin = login',
            _creatingUserEmail = email',
            _creatingUserGithubToken = credentials
          }
    Left ErrorWithContext {err = NoSuchUser {}} ->
      pure
        $ CreatingUser
          { _creatingUserExists = False,
            _creatingUserGithubLogin = login',
            _creatingUserEmail = email',
            _creatingUserGithubToken = credentials
          }
    Left e -> throwError e
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

callbackHelper :: M OA.OAuth2 -> Maybe OAuthCode -> M (GhLogin, Email, GhUserCredentials Text)
callbackHelper _ Nothing = throw $ OtherError "'code' param missing"
callbackHelper githubOauth (Just code) = do
  ghOauth <- githubOauth
  credentials <-
    exchangeOauthCode (OA.oauthCallback ghOauth) code
      <?> "exchanging the oauth code"
  let auth = GH.OAuth $ cs $ credentials ^. accessToken
  eGhUser <- liftIO (GH.github auth GH.userInfoCurrentR) <?> "calling userInfoCurrentR"
  case eGhUser of
    Left e -> throw $ OtherError $ show e
    Right ghUser -> do
      e <- getEmail auth ghUser <?> "calling getEmail"
      pure
        ( GhLogin . GH.untagName $ GH.userLogin ghUser,
          e,
          credentials
        )
  where
    getEmail auth ghUser = case GH.userEmail ghUser of
      Just e -> pure $ Email e
      Nothing -> do
        emails <-
          liftIO (GH.github auth $ GH.currentUserEmailsR GH.FetchAll)
            <?> "calling currentUserEmailsR"
        case find GH.emailPrimary <$> emails of
          Right (Just e') -> pure $ Email $ GH.emailAddress e'
          _ -> throw $ OtherError "No email address"

finishSignup ::
  AuthResult (CreatingUser (GhUserCredentials Text)) ->
  CreateUser ->
  M (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] GhLogin)
finishSignup (Authenticated cUser) addenda = do
  -- The things in AuthResult we can trust, because we put them there
  mAdminLogin <- view #adminGithubLogin
  let subType =
        if Just (cUser ^. githubLogin) == mAdminLogin
          then Admin
          else FreeSubscription
  user <-
    DB.newUser
      (cUser ^. githubLogin)
      (addenda ^. email)
      subType
      (addenda ^. agreeToEmails)
  storeCredentialsFor (user ^. id) (cUser ^. githubToken)
  cookieSettings' <- sessionCookieSettings
  jwtSettings' <- view #jwtSettings
  mApplyCookies <- liftIO $ acceptLogin cookieSettings' jwtSettings' (WebSession user)
  case mApplyCookies of
    Nothing -> throw Unauthorized
    Just applyCookies -> return $ applyCookies $ user ^. githubLogin
finishSignup _ _ = throw $ OtherError "Did not receive expected user info"

githubOauthLogin :: M OA.OAuth2
githubOauthLogin = do
  clientId <- view #githubClientId
  ghClientSecret <- view #githubClientSecret
  fromRelativeUrl <- relativeUrlConverter
  pure
    $ OA.OAuth2
      { oauthClientId = clientId,
        oauthClientSecret = ghClientSecret,
        oauthOAuthorizeEndpoint = "https://github.com/login/oauth/authorize",
        oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token",
        oauthCallback = fromRelativeUrl "login/cb",
        oauthScopes = []
      }

githubOauthSignup :: M OA.OAuth2
githubOauthSignup = do
  clientId <- view #githubClientId
  ghClientSecret <- view #githubClientSecret
  fromRelativeUrl <- relativeUrlConverter
  pure
    $ OA.OAuth2
      { oauthClientId = clientId,
        oauthClientSecret = ghClientSecret,
        oauthOAuthorizeEndpoint = "https://github.com/login/oauth/authorize",
        oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token",
        oauthCallback = fromRelativeUrl "signup/fill",
        oauthScopes = []
      }
