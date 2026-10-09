{-# LANGUAGE OverloadedRecordDot #-}

module Garnix.API.AuthSpec where

import Control.Lens
import Crypto.JOSE as Jose
import Crypto.JWT (ClaimsSet, JWTError (..), defaultJWTValidationSettings, verifyClaimsAt)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens
import Data.ByteString qualified
import Data.ByteString.Base64 qualified as Base64
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Database.PostgreSQL.Typed (pgSQL)
import Data.Text qualified as T
import Garnix.API.Auth (ConnectAction (..), ConnectState (..), OAuthNonce (..), connectAction, forgeOAuth2, holdsOAuthNonce, loginOrCreate, oauthCallbackPath, parseOAuthNonce, validateConnect)
import Garnix.AccessToken.Types
import Garnix.Build (buildFlake)
import Garnix.DB qualified as DB
import Garnix.Duration (addTime, fromMinutes)
import Garnix.GithubUserToken (userTokenFor)
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
import Network.OAuth2 qualified as OA
import Network.Wreq
import Servant.Auth.Server (validationKeys)
import Servant.Auth.Server.Internal.JWT (makeJWT)
import Test.Hspec
import Web.Cookie (parseSetCookie, setCookieName, setCookieValue)

spec :: Spec
spec = oauthSpec >> serverSpec

serverSpec :: Spec
serverSpec = inM $ beforeM_ truncateDBM $ aroundM_ suppressLogs $ do
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
        res <- assert200 $ server.postWithHeaders "/api/auth/jwt" [("Authorization", cs $ encodeAuthHeader (user ^. soleLogin . to getGhLogin) (getAccessTokenText accessToken))] [aesonQQ| null |]
        let jwt = res ^?! responseBody . key "token" . _String
        res <- assert200 $ server.getWithHeaders "/api/whoami" [("Authorization", cs $ "Bearer " <> jwt)]
        Aeson.decode (res ^. responseBody)
          `shouldBeM` Just
            [aesonQQ|
              {
                username: #{user ^. soleLogin},
                email: #{user ^. email},
                forge: "github",
                is_admin: false,
                identities: [{ forge: "github", login: #{user ^. soleLogin} }]
              }
            |]

    it "creates JWTs that expire after the session lifetime" $ do
      (user, accessToken) <- createApiAccessToken
      withServer $ \server -> do
        res <- assert200 $ server.postWithHeaders "/api/auth/jwt" [("Authorization", cs $ encodeAuthHeader (user ^. soleLogin . to getGhLogin) (getAccessTokenText accessToken))] ""
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
        res <- server.postWithHeaders "/api/auth/jwt" [("Authorization", cs $ encodeAuthHeader (user ^. soleLogin . to getGhLogin) "bad-access-token")] [aesonQQ| null |]
        res `shouldHaveStatusCode` 401
        res ^. responseBody `shouldBeM` "Unauthorized"

    it "does not allow to use JWTs to create new session access tokens" $ do
      (user, accessToken) <- createApiAccessToken
      withServer $ \server -> do
        res <- assert200 $ server.postWithHeaders "/api/auth/jwt" [("Authorization", cs $ encodeAuthHeader (user ^. soleLogin . to getGhLogin) (getAccessTokenText accessToken))] ""
        let jwt = res ^?! responseBody . key "token" . _String
        res <- server.postWithHeaders "/api/account/tokens" [("Authorization", cs $ "Bearer " <> jwt)] [aesonQQ| { name: "test token", scopes: { api: true } } |]
        res ^. responseStatus `shouldBeM` forbidden403
        res ^. responseBody `shouldBeM` "Forbidden: This endpoint is not available through the programmatic api."

    it "does not allow to use JWTs to create new JWTs" $ do
      (user, accessToken) <- createApiAccessToken
      withServer $ \server -> do
        res <- assert200 $ server.postWithHeaders "/api/auth/jwt" [("Authorization", cs $ encodeAuthHeader (user ^. soleLogin . to getGhLogin) (getAccessTokenText accessToken))] ""
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
              [("Authorization", cs $ encodeAuthHeader (user ^. soleLogin . to getGhLogin) (getAccessTokenText accessToken))]
              [aesonQQ| null |]
        let jwt = res ^?! responseBody . key "token" . _String
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
          resolve =<< buildFlake openSearchReporter (commitInfo & reqUser .~ ForgeLogin githubForge (user ^. soleLogin))
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
            (ForgeLogin githubForge (GhLogin "session-user"))
            (Email "session-user@example.com")

    let forgedSessionCookie :: User -> UTCTime -> M Data.ByteString.ByteString
        forgedSessionCookie user expiresAt = do
          jwtSettings' <- view #jwtSettings
          eJwt <- liftIO $ makeJWT (WebSession (user ^. id)) jwtSettings' (Just expiresAt)
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
          `shouldReturnM` Just (user ^. soleLogin . to getGhLogin)
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

  describe "accounts with identities on several forges" $ do
    let otherForge = ForgeSlug "git.example"
        -- A forge whose OAuth app takes the code "<login>" for a token of that
        -- login, whose email and admin flag 'people' gives.
        fakeForge :: ForgeSlug -> Map.Map Text (Text, Bool) -> ForgeInstance
        fakeForge slug' people =
          ForgeInstance
            { _forgeInstanceConfig =
                ForgeConfig
                  { _forgeConfigSlug = slug',
                    _forgeConfigKind = GiteaForgeKind,
                    _forgeConfigWebUrl = "https://" <> getForgeSlug slug',
                    _forgeConfigApiUrl = "https://" <> getForgeSlug slug' <> "/api/v1",
                    _forgeConfigWebhookSecret = "webhook-secret",
                    _forgeConfigOAuthClientId = getForgeSlug slug' <> "-client-id",
                    _forgeConfigOAuthClientSecret = "client-secret",
                    _forgeConfigApiToken = Nothing,
                    _forgeConfigAdmins = []
                  },
              _forgeInstanceForge =
                githubForgeApi
                  { _forgeExchangeOauthCode = \_ (OAuthCode code) ->
                      if Map.member code people
                        then pure $ tokenOf slug' code
                        else throw Unauthorized,
                    _forgeGetCurrentUser = \token -> case T.stripPrefix (getForgeSlug slug' <> "/") token of
                      Just login'
                        | Just (email', isAdmin') <- Map.lookup login' people ->
                            pure (GhLogin login', Email email', isAdmin')
                      _ -> throw Unauthorized
                  }
            }
        tokenOf :: ForgeSlug -> Text -> GhUserCredentials Text
        tokenOf slug' login' =
          GhUserCredentials
            { _ghUserCredentialsAccessToken = getForgeSlug slug' <> "/" <> login',
              _ghUserCredentialsAccessTokenExpiresAt = Nothing,
              _ghUserCredentialsRefreshToken = Nothing,
              _ghUserCredentialsRefreshTokenExpiresAt = Nothing
            }
        githubPeople =
          Map.fromList
            [ ("alice", ("alice@example.com", False)),
              ("staff", ("staff@github.example", True))
            ]
        otherPeople =
          Map.fromList
            [ ("alice", ("alice@example.com", False)),
              ("alice-git", ("ALICE@example.com", False)),
              ("bob", ("bob@git.example", True)),
              ("carol", ("carol@git.example", False))
            ]
        withForges :: M a -> M a
        withForges =
          local
            $ (#forges %~ Map.insert githubForge (fakeForge githubForge githubPeople))
            . (#forges %~ Map.insert otherForge (fakeForge otherForge otherPeople))
        whoami :: TestServer -> M (Response LazyByteString)
        whoami server = assert200 $ server.get "/api/whoami"
        stateOf :: Text -> Text
        stateOf link = case T.breakOn "&state=" link of
          (_, rest) | not (T.null rest) -> T.drop (T.length "&state=") rest
          _ -> error $ "no state in " <> cs link
        loginPath :: ForgeSlug -> String
        loginPath slug'
          | slug' == githubForge = "/api/login"
          | otherwise = "/api/auth/" <> cs (getForgeSlug slug') <> "/login"
        -- A login as a browser makes it: the link sets the state cookie, and
        -- the forge calls back with that state.
        loginState :: TestServer -> ForgeSlug -> M Text
        loginState server slug' = do
          res <- assert200 $ server.get (loginPath slug')
          pure $ stateOf $ res ^?! responseBody . key "github" . _String
        callbackWith :: TestServer -> ForgeSlug -> Text -> Text -> M (Response LazyByteString)
        callbackWith server slug' code state =
          server.get (loginPath slug' <> "/cb?code=" <> cs code <> "&state=" <> cs state)
        loginAs :: TestServer -> ForgeSlug -> Text -> M (Response LazyByteString)
        loginAs server slug' code = callbackWith server slug' code =<< loginState server slug'
        connectState :: TestServer -> ForgeSlug -> M Text
        connectState server slug' = do
          res <- assert200 $ server.get ("/api/auth/" <> cs (getForgeSlug slug') <> "/connect")
          pure $ stateOf $ res ^?! responseBody . key "github" . _String
        connectWith = callbackWith
        messageOf :: Response LazyByteString -> Maybe Text
        messageOf res = res ^? responseBody . key "message" . _String
        sessionCookieFor :: User -> M Data.ByteString.ByteString
        sessionCookieFor user = do
          jwtSettings' <- view #jwtSettings
          now <- liftIO getCurrentTime
          jwt <- liftIO (makeJWT (WebSession (user ^. id)) jwtSettings' (Just $ addUTCTime 60 now)) >>= either (error . show) pure
          pure $ "JWT-Cookie=" <> cs jwt
        disconnectBody = Aeson.object []

    it "links to each forge's own OAuth app, calling back to the login callback" $ withForges $ withServer $ \server -> do
      res <- assert200 $ server.get "/api/auth/git.example/login"
      let link = res ^?! responseBody . key "github" . _String
      ("https://git.example/login/oauth/authorize?" `T.isPrefixOf` link) `shouldBeM` True
      ("client_id=git.example-client-id" `T.isInfixOf` link) `shouldBeM` True
      ("/auth/git.example/login/cb&" `T.isInfixOf` link) `shouldBeM` True
      -- The state is a nonce this browser keeps in a cookie.
      let setCookies = parseSetCookie <$> res ^.. responseHeaders . traverse . filtered ((== "Set-Cookie") . fst) . _2
      (setCookieValue <$> find ((== "garnix-oauth-state-" <> cs (stateOf link)) . setCookieName) setCookies) `shouldBeM` Just "1"

    it "refuses a login callback this browser did not start" $ withForges $ do
      state <- withServer $ \server -> loginState server githubForge
      withServer $ \server -> do
        forM_ [state, "foo", ""] $ \state' -> do
          res <- callbackWith server githubForge "alice" state'
          res `shouldHaveStatusCode` 403
        whoami server >>= \r -> (r ^. responseBody) `shouldBeM` "null"
      try (DB.getUser (ForgeLogin githubForge "alice")) >>= liftIO . (`shouldSatisfy` isLeft @ErrorWithContext @User)

    it "spends the state of a login once it is used" $ withForges $ withServer $ \server -> do
      state <- loginState server githubForge
      _ <- assert200 $ callbackWith server githubForge "alice" state
      _ <- assert200 $ server.delete "/api/login"
      res <- callbackWith server githubForge "alice" state
      res `shouldHaveStatusCode` 403

    it "creates one account at the first login on each forge" $ withForges $ do
      withServer $ \server -> do
        login' <- assert200 $ loginAs server githubForge "alice"
        (login' ^? responseBody . key "emailAlreadyUsed" . _Bool) `shouldBeM` Just False
        res <- whoami server
        (res ^? responseBody . key "username" . _String) `shouldBeM` Just "alice"
        (res ^? responseBody . key "forge" . _String) `shouldBeM` Just "github"
      withServer $ \server -> do
        _ <- assert200 $ loginAs server otherForge "bob"
        res <- whoami server
        (res ^? responseBody . key "username" . _String) `shouldBeM` Just "bob"
        (res ^? responseBody . key "forge" . _String) `shouldBeM` Just "git.example"
      onGithub <- DB.getUser (ForgeLogin githubForge "alice")
      onOther <- DB.getUser (ForgeLogin otherForge "bob")
      (onGithub ^. id == onOther ^. id) `shouldBeM` False
      (onGithub ^. email) `shouldBeM` Email "alice@example.com"
      (onOther ^. identities) `shouldBeM` [ForgeIdentity otherForge "bob" True]
      userTokenFor (ForgeLogin otherForge "bob") `shouldReturnM` GhToken "git.example/bob"

    it "logs a known identity in again, to the same account" $ withForges $ do
      user <- DB.newUser (ForgeLogin otherForge "bob") (Email "bob@git.example")
      withServer $ \server -> do
        _ <- assert200 $ loginAs server otherForge "bob"
        res <- whoami server
        (res ^? responseBody . key "username" . _String) `shouldBeM` Just "bob"
      -- The forge says bob administers it now; the next login records that.
      DB.getUser (ForgeLogin otherForge "bob") `shouldReturnM` (user & identities .~ [ForgeIdentity otherForge "bob" True])

    it "lets several logins be in flight in one browser" $ withForges $ withServer $ \server -> do
      first <- loginState server githubForge
      second <- loginState server githubForge
      _ <- assert200 $ callbackWith server githubForge "alice" first
      _ <- assert200 $ callbackWith server githubForge "alice" second
      pure ()

    it "spends the state of a refused callback too" $ withForges $ withServer $ \server -> do
      state <- loginState server githubForge
      refused <- callbackWith server githubForge "nobody" state
      (refused ^. responseStatus . Network.Wreq.statusCode) `shouldSatisfyM` (>= 400)
      let setCookies = parseSetCookie <$> refused ^.. responseHeaders . traverse . filtered ((== "Set-Cookie") . fst) . _2
      (setCookieValue <$> find ((== "garnix-oauth-state-" <> cs state) . setCookieName) setCookies) `shouldBeM` Just ""
      res <- callbackWith server githubForge "alice" state
      res `shouldHaveStatusCode` 403

    it "names no cookie after a state that is no nonce of ours" $ withForges $ withServer $ \server -> do
      forM_ ["a;%20Domain=garnix.example;%20Path=/api", "a=b", "a%0d%0aSet-Cookie:%20JWT-Cookie=x"] $ \state -> do
        res <- callbackWith server githubForge "alice" state
        res `shouldHaveStatusCode` 403
        (res ^.. responseHeaders . traverse . filtered ((== "Set-Cookie") . fst)) `shouldBeM` []

    it "logs in to the account another callback of the same first login just created" $ withForges $ do
      alice <- DB.newUser (ForgeLogin githubForge "alice") (Email "alice@example.com")
      -- The owner was looked up before the other callback attached the identity.
      (user, used) <- loginOrCreate (ForgeIdentity githubForge "alice" False) (Email "alice@example.com") Nothing
      (user ^. id) `shouldBeM` (alice ^. id)
      used `shouldBeM` DB.EmailAlreadyUsed False

    it "creates an account at a first login whose email another account has, and says it was in use" $ withForges $ do
      existing <- DB.newUser (ForgeLogin githubForge "alice") (Email "alice@example.com")
      withServer $ \server -> do
        res <- assert200 $ loginAs server otherForge "alice-git"
        (res ^? responseBody . key "emailAlreadyUsed" . _Bool) `shouldBeM` Just True
        (res ^? responseBody . key "username" . _String) `shouldBeM` Just "alice-git"
        -- It never says which account.
        ("alice\"" `T.isInfixOf` cs (res ^. responseBody)) `shouldBeM` False
        ids <- whoami server
        (ids ^.. responseBody . key "identities" . values . key "login" . _String) `shouldBeM` ["alice-git"]
      created <- DB.getUser (ForgeLogin otherForge "alice-git")
      (created ^. id == existing ^. id) `shouldBeM` False
      (created ^. email) `shouldBeM` Email "ALICE@example.com"
      DB.getUser (ForgeLogin githubForge "alice") `shouldReturnM` existing

    it "connects another forge to the account that started it" $ withForges $ withServer $ \server -> do
      _ <- assert200 $ loginAs server githubForge "alice"
      state <- connectState server otherForge
      ("connect." `T.isPrefixOf` state) `shouldBeM` True
      res <- assert200 $ connectWith server otherForge "carol" state
      -- The account's name, not the connected login's: the page that gets
      -- this shows it as who is logged in.
      (res ^? responseBody . key "username" . _String) `shouldBeM` Just "alice"
      (res ^? responseBody . key "emailAlreadyUsed" . _Bool) `shouldBeM` Just False
      account <- DB.getUser (ForgeLogin githubForge "alice")
      DB.getUser (ForgeLogin otherForge "carol") `shouldReturnM` account
      (account ^.. identities . traverse . to identityForgeLogin)
        `shouldBeM` [ForgeLogin githubForge "alice", ForgeLogin otherForge "carol"]
      userTokenFor (ForgeLogin githubForge "alice") `shouldReturnM` GhToken "github/alice"
      userTokenFor (ForgeLogin otherForge "carol") `shouldReturnM` GhToken "git.example/carol"
      ids <- whoami server
      (ids ^.. responseBody . key "identities" . values . key "forge" . _String) `shouldBeM` ["github", "git.example"]
      -- The session keeps naming the github identity first.
      (ids ^? responseBody . key "username" . _String) `shouldBeM` Just "alice"

    it "refuses to connect an identity that belongs to another account" $ withForges $ do
      bob <- DB.newUser (ForgeLogin otherForge "bob") (Email "bob@git.example")
      withServer $ \server -> do
        _ <- assert200 $ loginAs server githubForge "alice"
        state <- connectState server otherForge
        res <- connectWith server otherForge "bob" state
        res `shouldHaveStatusCode` 409
        messageOf res `shouldBeM` Just "The git.example identity bob already belongs to another garnix account."
      DB.getUser (ForgeLogin otherForge "bob") `shouldReturnM` bob
      ((^. identities) <$> DB.getUser (ForgeLogin githubForge "alice")) `shouldReturnM` [ForgeIdentity githubForge "alice" False]

    it "refuses a second identity on a forge the account is connected to" $ withForges $ withServer $ \server -> do
      _ <- assert200 $ loginAs server githubForge "alice"
      _ <- assert200 . connectWith server otherForge "carol" =<< connectState server otherForge
      res <- connectWith server otherForge "bob" =<< connectState server otherForge
      res `shouldHaveStatusCode` 409
      messageOf res `shouldBeM` Just "Your account is already connected to git.example as carol. Disconnect it first."
      try (DB.getUser (ForgeLogin otherForge "bob")) >>= liftIO . (`shouldSatisfy` isLeft @ErrorWithContext @User)

    it "attaches nothing for a connect another session started" $ withForges $ do
      state <- withServer $ \server -> do
        _ <- assert200 $ loginAs server githubForge "staff"
        connectState server otherForge
      withServer $ \server -> do
        _ <- assert200 $ loginAs server githubForge "alice"
        res <- connectWith server otherForge "carol" state
        res `shouldHaveStatusCode` 403
      withServer $ \server -> do
        -- Not a 401, which logs the frontend out.
        res <- connectWith server otherForge "carol" state
        res `shouldHaveStatusCode` 403
        (res ^. responseBody) `shouldBeM` "Forbidden: Log in to connect a forge to your account."
      withServer $ \server -> do
        _ <- assert200 $ loginAs server githubForge "alice"
        res <- connectWith server otherForge "carol" "connect.forged"
        res `shouldHaveStatusCode` 403
      try (DB.getUser (ForgeLogin otherForge "carol")) >>= liftIO . (`shouldSatisfy` isLeft @ErrorWithContext @User)

    it "spends the state of a connect once it is used" $ withForges $ withServer $ \server -> do
      _ <- assert200 $ loginAs server githubForge "alice"
      state <- connectState server otherForge
      _ <- assert200 $ connectWith server otherForge "carol" state
      _ <- assert200 $ server.deleteWithBody "/api/auth/git.example/identity" disconnectBody
      res <- connectWith server otherForge "carol" state
      res `shouldHaveStatusCode` 403
      ((^. identities) <$> DB.getUser (ForgeLogin githubForge "alice")) `shouldReturnM` [ForgeIdentity githubForge "alice" False]

    it "refuses a connect callback this browser did not start" $ withForges $ do
      user <- DB.newUser (ForgeLogin githubForge "alice") (Email "alice@example.com")
      cookie <- sessionCookieFor user
      -- The state leaked from the browser that started it; this one holds the
      -- session but not the state's cookie.
      state <- withServer $ \server -> do
        _ <- assert200 $ server.getWithHeaders "/api/whoami" [("Cookie", cookie)]
        res <- assert200 $ server.getWithHeaders "/api/auth/git.example/connect" [("Cookie", cookie)]
        pure $ stateOf $ res ^?! responseBody . key "github" . _String
      withServer $ \server -> do
        res <- server.getWithHeaders ("/api/auth/git.example/login/cb?code=carol&state=" <> cs state) [("Cookie", cookie)]
        res `shouldHaveStatusCode` 403
      try (DB.getUser (ForgeLogin otherForge "carol")) >>= liftIO . (`shouldSatisfy` isLeft @ErrorWithContext @User)

    it "says is_admin only of github.com's admins" $ withForges $ do
      let withAdmins slug' admins' =
            local $ #forges %~ Map.adjust (\fi -> fi {_forgeInstanceConfig = (_forgeInstanceConfig fi) {_forgeConfigAdmins = admins'}}) slug'
      gitAdmin <- DB.newUser (ForgeLogin otherForge "bob") (Email "bob@git.example")
      githubAdmin <- DB.newUser (ForgeLogin githubForge "alice") (Email "alice@example.com")
      withAdmins otherForge ["bob"] $ withAdmins githubForge ["alice"] $ do
        forM_ [(gitAdmin, False), (githubAdmin, True)] $ \(user, expected) -> do
          cookie <- sessionCookieFor user
          res <- withServer $ \server -> assert200 $ server.getWithHeaders "/api/whoami" [("Cookie", cookie)]
          (res ^? responseBody . key "username" . _String, res ^? responseBody . key "is_admin" . _Bool)
            `shouldBeM` (Just (user ^. soleLogin . to getGhLogin), Just expected)

    it "disconnects an identity, but never the last one" $ withForges $ withServer $ \server -> do
      _ <- assert200 $ loginAs server githubForge "alice"
      res <- server.deleteWithBody "/api/auth/github/identity" disconnectBody
      res `shouldHaveStatusCode` 409
      messageOf res `shouldBeM` Just "github is the only forge your account logs in with. Connect another forge before disconnecting it."
      _ <- assert200 . connectWith server otherForge "carol" =<< connectState server otherForge
      build <- testBuild $ (forge .~ otherForge) . (reqUser .~ "carol")
      _ <- assert200 $ server.deleteWithBody "/api/auth/git.example/identity" disconnectBody
      ((^. identities) <$> DB.getUser (ForgeLogin githubForge "alice")) `shouldReturnM` [ForgeIdentity githubForge "alice" False]
      try (userTokenFor (ForgeLogin otherForge "carol")) >>= liftIO . (`shouldSatisfy` isLeft @ErrorWithContext @GhToken)
      -- History stays.
      ((^. reqUser) <$> DB.getBuild (build ^. id)) `shouldReturnM` "carol"
      res' <- server.deleteWithBody "/api/auth/git.example/identity" disconnectBody
      res' `shouldHaveStatusCode` 404

    it "warns before deleting the module settings of an identity it disconnects" $ withForges $ withServer $ \server -> do
      _ <- assert200 $ loginAs server githubForge "alice"
      _ <- assert200 . connectWith server otherForge "carol" =<< connectState server otherForge
      void
        $ DB.pgExec
          [pgSQL|
            INSERT INTO module_user_repo (forge, github_login, repo_user, repo_name)
              VALUES ('github', 'alice', 'alice', 'site')
          |]
      res <- server.deleteWithBody "/api/auth/github/identity" disconnectBody
      res `shouldHaveStatusCode` 409
      messageOf res `shouldBeM` Just "Disconnecting github deletes the module settings saved through it. Confirm with confirmDeleteModuleSettings to disconnect anyway."
      DB.identityHasModuleSettings (ForgeLogin githubForge "alice") `shouldReturnM` True
      _ <- assert200 $ server.deleteWithBody "/api/auth/github/identity" (Aeson.object ["confirmDeleteModuleSettings" Aeson..= True])
      DB.identityHasModuleSettings (ForgeLogin githubForge "alice") `shouldReturnM` False
      ((^. identities) <$> DB.getUser (ForgeLogin otherForge "carol")) `shouldReturnM` [ForgeIdentity otherForge "carol" False]

    it "lists the builds every identity of the account requested" $ withForges $ withServer $ \server -> do
      _ <- assert200 $ loginAs server githubForge "alice"
      _ <- assert200 . connectWith server otherForge "carol" =<< connectState server otherForge
      void $ testBuild $ (gitCommit .~ "aaaaaa") . (reqUser .~ "alice")
      void $ testBuild $ (gitCommit .~ "bbbbbb") . (forge .~ otherForge) . (reqUser .~ "carol")
      -- Same login on another forge: somebody else.
      void $ testBuild $ (gitCommit .~ "cccccc") . (forge .~ otherForge) . (reqUser .~ "alice")
      res <- assert200 $ server.get "/api/commits"
      sort (res ^.. responseBody . key "commits" . values . key "git_commit" . _String)
        `shouldBeM` ["aaaaaa", "bbbbbb"]

    it "keeps GitHub-only features on the account's github identity: empty listings without one, refused actions" $ withForges $ withServer $ \server -> do
      _ <- assert200 $ loginAs server otherForge "carol"
      forM_
        [ ("/api/account/usage", Aeson.object ["by_org" Aeson..= Aeson.object []]),
          ("/api/account/repos", Aeson.object ["repos" Aeson..= ([] :: [Text])]),
          ("/api/hosts", Aeson.toJSON ([] :: [Text]))
        ]
        $ \(path, expected) -> do
          res <- server.get path
          (path, res ^. responseStatus . Network.Wreq.statusCode, Aeson.decode (res ^. responseBody))
            `shouldBeM` (path, 200, Just expected)
      let refused = "Forbidden: This needs a GitHub identity, and this account has none."
      forM_ [("/api/modules" :: Text, server.get "/api/modules"), ("/api/account/usage/acme", server.get "/api/account/usage/acme"), ("DELETE /api/hosts/:id", server.delete ("/api/hosts/" <> cs (getHashId (review hashIdInt 1))))] $ \(path, request) -> do
        res <- request
        (path, res ^. responseStatus . Network.Wreq.statusCode, res ^. responseBody) `shouldBeM` (path, 403, refused)

    it "accepts sessions minted before accounts held several identities" $ withForges $ do
      user <- DB.newUser (ForgeLogin githubForge "alice") (Email "alice@example.com")
      now <- liftIO getCurrentTime
      let oldPayloads =
            [ Aeson.object
                [ "id" Aeson..= (user ^. id),
                  "github_login" Aeson..= ("alice" :: Text),
                  "email" Aeson..= ("alice@example.com" :: Text),
                  "subscription_type" Aeson..= ("free" :: Text),
                  "created_at" Aeson..= now,
                  "session_kind" Aeson..= ("web" :: Text)
                ],
              Aeson.object
                [ "id" Aeson..= (user ^. id),
                  "forge" Aeson..= ("github" :: Text),
                  "github_login" Aeson..= ("alice" :: Text),
                  "email" Aeson..= ("alice@example.com" :: Text),
                  "subscription_type" Aeson..= ("admin" :: Text),
                  "created_at" Aeson..= now,
                  "session_kind" Aeson..= ("web" :: Text)
                ]
            ]
      forM_ oldPayloads $ \payload -> do
        jwtSettings' <- view #jwtSettings
        jwt <- liftIO (makeJWT (OldSession payload) jwtSettings' (Just $ addUTCTime 60 now)) >>= either (error . show) pure
        withServer $ \server -> do
          res <- assert200 $ server.getWithHeaders "/api/whoami" [("Cookie", "JWT-Cookie=" <> cs jwt)]
          (res ^? responseBody . key "username" . _String) `shouldBeM` Just "alice"

    it "answers 404 for a slug that names no forge" $ withServer $ \server -> do
      forM_ ["/api/auth/nowhere/login", "/api/auth/nowhere/login/cb?code=c"] $ \path -> do
        res <- server.get path
        (path, res ^. responseStatus . Network.Wreq.statusCode) `shouldBeM` (path, 404)
      _ <- server.login
      res <- server.get "/api/auth/nowhere/connect"
      res `shouldHaveStatusCode` 404

-- | A session as garnix minted it before accounts held several identities.
newtype OldSession = OldSession Aeson.Value
  deriving newtype (ToJSON)

instance ToJWT OldSession

oauthSpec :: Spec
oauthSpec = do
  let gitExample = ForgeSlug "git.example"

  describe "connectAction" $ do
    let self = UserId 1
        other = UserId 2
    it "attaches an identity nobody holds" $ connectAction self Nothing `shouldBe` Attach
    it "refreshes the account's own identity" $ connectAction self (Just self) `shouldBe` Refresh
    it "never moves another account's identity" $ connectAction self (Just other) `shouldBe` Refuse (DB.OwnedBy other)

  describe "validateConnect" $ do
    let state = ConnectState (UserId 1) gitExample (OAuthNonce "nonce")
    it "accepts the session and forge the connect was started for" $ do
      validateConnect state (UserId 1) gitExample `shouldBe` Right ()
    it "refuses another session or forge" $ do
      isLeft (validateConnect state (UserId 2) gitExample) `shouldBe` True
      isLeft (validateConnect state (UserId 1) githubForge) `shouldBe` True

  describe "parseOAuthNonce" $ do
    let nonce = T.replicate 32 "a"
    it "accepts a nonce as garnix makes them" $ do
      getOAuthNonce <$> parseOAuthNonce (Just nonce) `shouldBe` Just nonce
    it "refuses anything else, which could name or set another cookie" $ do
      forM_ [Nothing, Just "", Just (T.take 31 nonce), Just (nonce <> "a"), Just (T.take 31 nonce <> ";"), Just (T.take 31 nonce <> "="), Just (T.take 30 nonce <> "\r\n"), Just (T.take 31 nonce <> "é")] $ \state ->
        (state, getOAuthNonce <$> parseOAuthNonce state) `shouldBe` (state, Nothing)

  describe "holdsOAuthNonce" $ do
    let a = OAuthNonce (T.replicate 32 "a")
        b = OAuthNonce (T.replicate 32 "b")
        cookieOf (OAuthNonce nonce) = "garnix-oauth-state-" <> nonce <> "=1"
    it "finds the nonce's own cookie among others" $ do
      holdsOAuthNonce (Just $ cookieOf a <> "; JWT-Cookie=x; " <> cookieOf b) b `shouldBe` True
    it "refuses a nonce without its cookie, or none at all" $ do
      holdsOAuthNonce (Just $ cookieOf a) b `shouldBe` False
      holdsOAuthNonce Nothing a `shouldBe` False

  describe "oauthCallbackPath" $ do
    it "keeps the callback GitHub's OAuth app is registered with" $ do
      oauthCallbackPath githubForge `shouldBe` "login/cb"

    it "calls any other forge back under its slug" $ do
      oauthCallbackPath gitExample `shouldBe` "auth/git.example/login/cb"

  describe "forgeOAuth2" $ do
    it "points at the forge's web host and calls back to garnix" $ do
      let config' =
            ForgeConfig
              { _forgeConfigSlug = gitExample,
                _forgeConfigKind = GiteaForgeKind,
                _forgeConfigWebUrl = "https://git.example",
                _forgeConfigApiUrl = "https://git.example/api/v1",
                _forgeConfigWebhookSecret = "webhook-secret",
                _forgeConfigOAuthClientId = "client-id",
                _forgeConfigOAuthClientSecret = "client-secret",
                _forgeConfigApiToken = Nothing,
                _forgeConfigAdmins = []
              }
          oauth = forgeOAuth2 ("https://garnix.example/" <>) gitExample config'
      OA.oauthClientId oauth `shouldBe` "client-id"
      OA.oauthClientSecret oauth `shouldBe` "client-secret"
      OA.oauthOAuthorizeEndpoint oauth `shouldBe` "https://git.example/login/oauth/authorize"
      OA.oauthAccessTokenEndpoint oauth `shouldBe` "https://git.example/login/oauth/access_token"
      OA.oauthCallback oauth `shouldBe` "https://garnix.example/auth/git.example/login/cb"
