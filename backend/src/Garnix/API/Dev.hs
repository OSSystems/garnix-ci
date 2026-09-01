module Garnix.API.Dev where

import Data.Row (Rec, (.==), type (.==))
import Data.Set qualified
import Garnix.API.Auth (sessionCookieSettings)
import Garnix.DB qualified as DB
import Garnix.GithubUserToken
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
import Servant.Auth.Server (SetCookie, acceptLogin)

data DevAPI route = DevAPI
  { _devAPILogMeIn :: route :- "log-me-in" :> Get '[JSON] LogMeInResponse
  }
  deriving (Generic)

type LogMeInResponse =
  Headers
    '[ Header "Set-Cookie" SetCookie,
       Header "Set-Cookie" SetCookie
     ]
    (Rec ("success" .== Bool))

devAPI :: M LogMeInResponse
devAPI = do
  testFeatures <- view #testFeatures
  when (not (DevApi `Data.Set.member` testFeatures)) $ do
    throw DevModeOnly
  user <- getTestUser
  cookieSettings' <- sessionCookieSettings
  jwtSettings' <- view #jwtSettings
  storeCredentialsFor
    (user ^. id)
    GhUserCredentials
      { _ghUserCredentialsAccessToken = "tok",
        _ghUserCredentialsAccessTokenExpiresAt = Nothing,
        _ghUserCredentialsRefreshToken = Nothing,
        _ghUserCredentialsRefreshTokenExpiresAt = Nothing
      }
  mApplyCookies <- liftIO $ acceptLogin cookieSettings' jwtSettings' (WebSession user)
  case mApplyCookies of
    Nothing -> throw Unauthorized
    Just applyCookies -> pure $ applyCookies (#success .== True)

getTestUser :: M User
getTestUser = do
  existing <- try $ DB.getUser (GhLogin "dev-user")
  case existing of
    Right user -> return user
    Left (ErrorWithContext {err = NoSuchUser {}}) -> do
      DB.newUser
        (GhLogin "dev-user")
        (Email "dev-user@example.com")
        FreeSubscription
        True
    Left e -> throwError e
