{-# LANGUAGE OverloadedRecordDot #-}

module Garnix.API.AuthSpec where

import Control.Lens
import Crypto.JOSE as Jose
import Crypto.JWT (ClaimsSet, JWTError (..), defaultJWTValidationSettings, verifyClaimsAt)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens
import Data.ByteString qualified
import Data.ByteString.Base64 qualified as Base64
import Data.String.Interpolate (i)
import Garnix.AccessToken.Types
import Garnix.Build (buildFlake)
import Garnix.DB qualified as DB
import Garnix.Duration (addTime, fromMinutes)
import Garnix.Monad
import Garnix.Monad.Async
import Garnix.Prelude
import Garnix.Reporters.OpenSearchReporter (openSearchReporter)
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.WithServer
import Garnix.Types
import Network.HTTP.Types (forbidden403)
import Network.Wreq
import Servant.Auth.Server (validationKeys)
import Servant.Auth.Server.Internal.JWT (makeJWT)
import Test.Hspec
import Web.Cookie (parseSetCookie, setCookieName, setCookieValue)

spec :: Spec
spec = inM $ beforeM_ truncateDBM $ aroundM_ suppressLogs $ do
  describe "/api/auth/jwt" $ do
    let encodeAuthHeader :: Text -> Text -> Text
        encodeAuthHeader username password = cs $ "Basic " <> Base64.encode (cs username <> ":" <> cs password)

    let createApiAccessToken :: M (User, AccessToken)
        createApiAccessToken = do
          withServer $ \server -> do
            user <- server.login
            res <- assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "test token", scopes: { api: true } } |]
            pure (user, AccessToken $ res ^?! responseBody . key "token" . _String)

    it "generates valid JWTs for the given user" $ do
      (user, accessToken) <- createApiAccessToken
      withServer $ \server -> do
        res <- assert200 $ server.postWithHeaders "/api/auth/jwt" [("Authorization", cs $ encodeAuthHeader (user ^. githubLogin . to getGhLogin) (getAccessTokenText accessToken))] [aesonQQ| null |]
        let jwt = res ^?! responseBody . key "token" . _String
        res <- assert200 $ server.getWithHeaders "/api/whoami" [("Authorization", cs $ "Bearer " <> jwt)]
        Aeson.decode (res ^. responseBody)
          `shouldBeM` Just
            [aesonQQ|
              {
                username: #{user ^. githubLogin},
                email: #{user ^. email},
                is_admin: false
              }
            |]

    it "creates JWTs that expire after the session lifetime" $ do
      (user, accessToken) <- createApiAccessToken
      withServer $ \server -> do
        res <- assert200 $ server.postWithHeaders "/api/auth/jwt" [("Authorization", cs $ encodeAuthHeader (user ^. githubLogin . to getGhLogin) (getAccessTokenText accessToken))] ""
        now <- liftIO getCurrentTime
        lifetime <- view #sessionLifetime
        let expiresAt = res ^?! responseBody . key "expiresAt" . _String . to cs . to parseTimestamp
        expiresAt `shouldSatisfyM` (<= addTime lifetime now)
        let jwt = res ^?! responseBody . key "token" . _String
        keys <- view #jwtSettings >>= liftIO . validationKeys
        let verify :: UTCTime -> M (Either JWTError ClaimsSet)
            verify time = liftIO $ Jose.runJOSE $ do
              signed <- Jose.decodeCompact (cs jwt)
              verifyClaimsAt (defaultJWTValidationSettings (error "not used")) keys time signed
        claimsSet <- verify now
        claimsSet `shouldSatisfyM` isRight
        verify (addUTCTime 1 $ addTime lifetime now) `shouldReturnM` Left JWTExpired

    it "returns unauthorized for non-existing users and does not expose why authentication failed to the user" $ do
      (_user, accessToken) <- createApiAccessToken
      withServer $ \server -> do
        res <- server.postWithHeaders "/api/auth/jwt" [("Authorization", cs $ encodeAuthHeader "no-such-user" (getAccessTokenText accessToken))] [aesonQQ| null |]
        res `shouldHaveStatusCode` 401
        res ^. responseBody `shouldBeM` "Unauthorized"

    it "returns unauthorized for bad access tokens and does not expose why authentication failed to the user" $ do
      (user, _accessToken) <- createApiAccessToken
      withServer $ \server -> do
        res <- server.postWithHeaders "/api/auth/jwt" [("Authorization", cs $ encodeAuthHeader (user ^. githubLogin . to getGhLogin) "bad-access-token")] [aesonQQ| null |]
        res `shouldHaveStatusCode` 401
        res ^. responseBody `shouldBeM` "Unauthorized"

    it "does not allow to use JWTs to create new session access tokens" $ do
      (user, accessToken) <- createApiAccessToken
      withServer $ \server -> do
        res <- assert200 $ server.postWithHeaders "/api/auth/jwt" [("Authorization", cs $ encodeAuthHeader (user ^. githubLogin . to getGhLogin) (getAccessTokenText accessToken))] ""
        let jwt = res ^?! responseBody . key "token" . _String
        res <- server.postWithHeaders "/api/account/tokens" [("Authorization", cs $ "Bearer " <> jwt)] [aesonQQ| { name: "test token", scopes: { api: true } } |]
        res ^. responseStatus `shouldBeM` forbidden403
        res ^. responseBody `shouldBeM` "Forbidden: This endpoint is not available through the programmatic api."

    it "does not allow to use JWTs to create new JWTs" $ do
      (user, accessToken) <- createApiAccessToken
      withServer $ \server -> do
        res <- assert200 $ server.postWithHeaders "/api/auth/jwt" [("Authorization", cs $ encodeAuthHeader (user ^. githubLogin . to getGhLogin) (getAccessTokenText accessToken))] ""
        let jwt = res ^?! responseBody . key "token" . _String
        res <- server.postWithHeaders "/api/auth/jwt" [("Authorization", cs $ "Bearer " <> jwt)] [aesonQQ| null |]
        res ^. responseStatus `shouldBeM` forbidden403
        res ^. responseBody `shouldBeM` "Forbidden: Creating JWTs is only allowed with the api access tokens."

    it "allows retrieving build statuses and logs" $ GH.withFakeGithubInterface $ \ghState -> do
      let flake =
            cs
              [i|
                {
                  outputs = {self}: {
                    packages.x86_64-linux.test-pkg = derivation {
                      name = "test-pkg";
                      builder = "/bin/sh";
                      args = ["-c" "echo some-build-output"];
                      system = "x86_64-linux";
                    };
                  };
                }
              |]
      (user, accessToken) <- createApiAccessToken
      withServer $ \server -> do
        res <-
          assert200
            $ server.postWithHeaders
              "/api/auth/jwt"
              [("Authorization", cs $ encodeAuthHeader (user ^. githubLogin . to getGhLogin) (getAccessTokenText accessToken))]
              [aesonQQ| null |]
        let jwt = res ^?! responseBody . key "token" . _String
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
          resolve =<< buildFlake openSearchReporter (commitInfo & reqUser .~ (user ^. githubLogin))
          build <- fromSingleton . filter (\x -> x ^. packageType == TypePackage) <$> DB.getBuilds user
          res <-
            assert200
              $ server.getWithHeaders
                ("/api/build/" <> cs (getHashId $ getBuildId $ build ^. id))
                [("Authorization", cs $ "Bearer " <> jwt)]
          (res ^?! responseBody . key "status" . _String) `shouldBeM` "Failure"
          res <-
            assert200
              $ server.getWithHeaders
                ("/api/build/" <> cs (getHashId $ getBuildId $ build ^. id) <> "/logs")
                [("Authorization", cs $ "Bearer " <> jwt)]
          (res ^? responseBody . key "finished" . _Bool) `shouldBeM` Just True
          cs (show (res ^?! responseBody . key "logs")) `shouldContainM` "some-build-output"

  describe "web session cookies" $ do
    let sessionUser :: M User
        sessionUser =
          DB.newUser
            (GhLogin "session-user")
            (Email "session-user@example.com")
            FreeSubscription
            True

    let forgedSessionCookie :: User -> UTCTime -> M Data.ByteString.ByteString
        forgedSessionCookie user expiresAt = do
          jwtSettings' <- view #jwtSettings
          eJwt <- liftIO $ makeJWT (WebSession user) jwtSettings' (Just expiresAt)
          case eJwt of
            Left err -> error $ "could not forge a session JWT: " <> show err
            Right jwt -> pure $ "JWT-Cookie=" <> cs jwt

    let whoAmIWithCookie :: TestServer -> Data.ByteString.ByteString -> M (Maybe Text)
        whoAmIWithCookie server cookie = do
          res <- assert200 $ server.getWithHeaders "/api/whoami" [("Cookie", cookie)]
          pure $ res ^? responseBody . key "username" . _String

    it "accepts a session cookie whose exp is still in the future" $ do
      user <- sessionUser
      withServer $ \server -> do
        now <- liftIO getCurrentTime
        cookie <- forgedSessionCookie user (addUTCTime 60 now)
        whoAmIWithCookie server cookie
          `shouldReturnM` Just (user ^. githubLogin . to getGhLogin)
        res <- server.getWithHeaders "/api/account/tokens" [("Cookie", cookie)]
        res `shouldHaveStatusCode` 200

    it "refuses a session cookie whose exp has already passed" $ do
      user <- sessionUser
      withServer $ \server -> do
        now <- liftIO getCurrentTime
        cookie <- forgedSessionCookie user (addUTCTime (-60) now)
        whoAmIWithCookie server cookie `shouldReturnM` Nothing
        res <- server.getWithHeaders "/api/account/tokens" [("Cookie", cookie)]
        res `shouldHaveStatusCode` 401

    let loginSessionJwt :: TestServer -> M Text
        loginSessionJwt server = do
          res <- assert200 $ server.get "/api/dev/log-me-in"
          let setCookies = res ^.. responseHeaders . traverse . filtered ((== "Set-Cookie") . fst) . _2
          pure
            $ maybe (error "no session cookie in the login response") (cs . setCookieValue)
            $ find ((== "JWT-Cookie") . setCookieName)
            $ parseSetCookie
            <$> setCookies

    let verifyAt :: Text -> UTCTime -> M (Either JWTError ClaimsSet)
        verifyAt jwt time = do
          keys <- view #jwtSettings >>= liftIO . validationKeys
          liftIO $ Jose.runJOSE $ do
            signed <- Jose.decodeCompact (cs jwt)
            verifyClaimsAt (defaultJWTValidationSettings (error "not used")) keys time signed

    it "mints web session cookies that stop verifying after the session lifetime" $ do
      withServer $ \server -> do
        jwt <- loginSessionJwt server
        now <- liftIO getCurrentTime
        lifetime <- view #sessionLifetime
        claimsSet <- verifyAt jwt now
        claimsSet `shouldSatisfyM` isRight
        verifyAt jwt (addUTCTime 1 $ addTime lifetime now) `shouldReturnM` Left JWTExpired

    it "mints session cookies with the lifetime configured in the environment" $ do
      let configuredLifetime = fromMinutes @Int 5
      local (#sessionLifetime .~ configuredLifetime) $ withServer $ \server -> do
        jwt <- loginSessionJwt server
        now <- liftIO getCurrentTime
        claimsSet <- verifyAt jwt now
        claimsSet `shouldSatisfyM` isRight
        verifyAt jwt (addUTCTime 1 $ addTime configuredLifetime now) `shouldReturnM` Left JWTExpired
