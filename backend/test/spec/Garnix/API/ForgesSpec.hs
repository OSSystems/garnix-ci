{-# LANGUAGE OverloadedRecordDot #-}

module Garnix.API.ForgesSpec (spec) where

import Control.Lens ((^?!))
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Lens (key, values, _String)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.API.Forges (clientAddress)
import Garnix.Access (administeredForges, hasAccessToRepo, liveAccount)
import Garnix.AccessToken (generateToken)
import Garnix.AccessToken.Types (AccessToken (..), AccessTokenScopes (..))
import Garnix.DB qualified as DB
import Garnix.DB.Forges qualified as Forges
import Garnix.Forge.OutboundGuard (GuardOptions (..), defaultGuardOptions, guardedManager, isPublicAddress)
import Garnix.Forge.Registered (ForgeStatus (..))
import Garnix.Forge.Registry (ForgeCache, cachedForgeSlugs, hashRegistrationToken, newForgeCache, newForgeRegistrationWithCache)
import Garnix.GithubUserToken (encryptSecret)
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.WithServer
import Garnix.Types hiding (message, slug, status, statusCode)
import Network.HTTP.Types (hAuthorization, parseSimpleQuery, status200, status400, status401, status404)
import Network.Socket (SockAddr (..), tupleToHostAddress, tupleToHostAddress6)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wreq (Response, responseBody, responseStatus, statusCode)
import Test.Hspec

-- | The OAuth client secret every registration in these specs uses: no
-- answer may contain it.
clientSecret :: Text
clientSecret = "client-secret-value"

-- | A Gitea whose OAuth app is @client-id@ / 'clientSecret'. The code
-- @alice-code@ logs in @alice@, @bob-code@ logs in @bob@, and @root-code@
-- logs in @root@, an administrator of the instance.
fakeGitea :: Wai.Application
fakeGitea request respond = do
  body <- Wai.strictRequestBody request
  let form = parseSimpleQuery (BSL.toStrict body)
      -- No keep-alive: every request garnix makes opens a new connection, as
      -- the specs that change what names resolve to need.
      json status' value = respond $ Wai.responseLBS status' [("Content-Type", "application/json"), ("Connection", "close")] (encode value)
      token login' = object ["access_token" .= (login' <> "-token" :: Text)]
      user login' = object ["login" .= login', "email" .= (login' <> "@forge.test" :: Text), "is_admin" .= (login' == "root")]
  case Wai.pathInfo request of
    ["api", "v1", "version"] -> json status200 $ object ["version" .= ("1.22.0" :: Text)]
    ["login", "oauth", "access_token"]
      | lookup "client_id" form == Just "client-id" && lookup "client_secret" form == Just (cs clientSecret) ->
          case lookup "code" form of
            Just "alice-code" -> json status200 $ token "alice"
            Just "bob-code" -> json status200 $ token "bob"
            Just "root-code" -> json status200 $ token "root"
            _ -> json status400 $ object ["error" .= ("invalid_grant" :: Text)]
      | otherwise -> json status401 $ object ["error" .= ("invalid_client" :: Text)]
    ["api", "v1", "user"] -> case lookup hAuthorization (Wai.requestHeaders request) of
      Just "token alice-token" -> json status200 $ user "alice"
      Just "token bob-token" -> json status200 $ user "bob"
      Just "token root-token" -> json status200 $ user "root"
      _ -> json status401 $ object []
    _ -> json status404 $ object []

-- | Runs the action with 'fakeGitea' served at the URL it is given.
withFakeGitea :: (Int -> M a) -> M a
withFakeGitea = liftBaseOp (testWithApplication (pure fakeGitea))

fakeUrl :: Int -> Text
fakeUrl port = "http://localhost:" <> show port

loopback :: Int -> SockAddr
loopback port = SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1))

-- | The fake Gitea's loopback address stands in for a public one: every
-- other non-public address is still refused.
allowedInSpecs :: SockAddr -> Bool
allowedInSpecs = \case
  SockAddrInet _ a | a == tupleToHostAddress (127, 0, 0, 1) -> True
  addr -> isPublicAddress addr

-- | Registration turned on, with every name resolving to what the IORef
-- holds (the fake Gitea, unless a spec changes it).
withRegistration :: Bool -> IORef (Int -> [SockAddr]) -> M a -> M a
withRegistration allowHttp resolution action = do
  cache <- liftIO newForgeCache
  withRegistrationCache cache allowHttp resolution action

withRegistrationCache :: ForgeCache -> Bool -> IORef (Int -> [SockAddr]) -> M a -> M a
withRegistrationCache cache allowHttp resolution action = do
  manager' <- liftIO $ guardedManager defaultGuardOptions {guardAllowPlainHttp = allowHttp} allowedInSpecs (\_ port -> ($ port) <$> readIORef resolution)
  registration <- liftIO $ newForgeRegistrationWithCache cache allowHttp manager'
  local (#forgeRegistration ?~ registration) action

-- | Registration on, plain http allowed, names resolving to the fake.
withHttpRegistration :: M a -> M a
withHttpRegistration action = do
  resolution <- liftIO $ newIORef (pure . loopback)
  withRegistration True resolution action

slug :: ForgeSlug
slug = ForgeSlug "localhost"

registerBody :: Int -> Value
registerBody port = object ["url" .= fakeUrl port, "clientId" .= ("client-id" :: Text), "clientSecret" .= clientSecret]

startBody :: Text -> Value
startBody url = object ["url" .= url]

status :: Response a -> Int
status response = response ^. responseStatus . statusCode

message :: Response LazyByteString -> Text
message response = fromMaybe (cs $ response ^. responseBody) (response ^? responseBody . key "message" . _String)

statusOf :: ForgeSlug -> M (Maybe ForgeStatus)
statusOf slug' = fmap Forges.rowStatus <$> Forges.getRegisteredForge slug'

-- | Logs @login@ in through the registered forge as a browser does: the
-- login link sets the OAuth state's cookie, and the forge calls back with
-- that state. The first login creates the account.
logInThroughForge :: TestServer -> Text -> M (Response LazyByteString)
logInThroughForge server login' = do
  started <- server.get "/api/auth/localhost/login"
  case T.breakOn "&state=" <$> (started ^? responseBody . key "github" . _String) of
    Just (_, state)
      | not (T.null state) ->
          server.get $ "/api/auth/localhost/login/cb?code=" <> cs login' <> "-code&state=" <> cs (T.drop (T.length "&state=") state)
    -- The forge is not one to log in through: the answer says why.
    _ -> pure started

-- | The forge calling back after @login@ authorized garnix through the given
-- authorize link, with the link's state.
callbackFrom :: TestServer -> Text -> Text -> M (Response LazyByteString)
callbackFrom server link login' =
  server.get $ "/api/auth/localhost/login/cb?code=" <> cs login' <> "-code&state=" <> cs (T.drop (T.length "&state=") (snd $ T.breakOn "&state=" link))

-- | The account holding @login@'s identity on the registered forge.
accountOf :: Text -> M (Maybe UserId)
accountOf login' = DB.lookupIdentityOwner (ForgeLogin slug (GhLogin login'))

registeredBy :: ForgeSlug -> M (Maybe UserId)
registeredBy slug' = (Forges.rowRegisteredBy =<<) <$> Forges.getRegisteredForge slug'

-- | Exchanges an api token of the account behind @login@ (as
-- 'forgeLoginText' writes it) for an api session, from a browser of its own.
exchangeApiToken :: Text -> AccessToken -> M (Response LazyByteString)
exchangeApiToken login' token = withServer $ \server ->
  server.postWithHeaders
    "/api/auth/jwt"
    [("Authorization", "Basic " <> Base64.encode (cs login' <> ":" <> cs (getAccessTokenText token)))]
    (object [])

-- | Backdates when the registered forge was disabled.
disabledDaysAgo :: Double -> M ()
disabledDaysAgo days =
  void $ DB.pgExec [pgSQL| UPDATE forges SET disabled_at = now() - make_interval(secs => ${days * 24 * 60 * 60}) |]

disabledAt :: M [Maybe UTCTime]
disabledAt = DB.pgQuery [pgSQL| SELECT disabled_at FROM forges |]

-- | Gives the account behind @login@'s identity on the registered forge an
-- identity on github.com too, so that its sessions outlive the forge.
alsoOnGithub :: Text -> M UserId
alsoOnGithub login' = do
  account <- accountOf login' >>= maybe (error "no account") pure
  (isRight <$> DB.tryAddIdentity account (ForgeIdentity githubForge (GhLogin $ login' <> "-on-github") False)) `shouldReturnM` True
  pure account

-- | Whether the registered forge's registrant is @login@'s account.
shouldBeRegisteredBy :: Text -> M ()
shouldBeRegisteredBy login' = do
  account <- accountOf login'
  isJust account `shouldBeM` True
  registeredBy slug `shouldReturnM` account

spec :: Spec
spec = inM $ beforeM_ truncateDBM $ aroundM_ suppressLogsWhenPassing $ do
  describe "registering a forge" $ do
    it "stores it pending, and activates it once an OAuth through it succeeds" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      registered <- assert200 $ server.post "/api/forges" (registerBody port)
      let link = registered ^?! responseBody . key "login" . _String
      ((fakeUrl port <> "/login/oauth/authorize?") `T.isPrefixOf` link) `shouldBeM` True
      ("client_id=client-id" `T.isInfixOf` link) `shouldBeM` True
      statusOf slug `shouldReturnM` Just ForgePending
      forges <- assert200 $ server.get "/api/forges"
      (forges ^.. responseBody . values . key "slug" . _String) `shouldBeM` ["github"]

      -- The link it answered is the one to follow: this browser holds the
      -- cookie of its OAuth state.
      _ <- assert200 $ callbackFrom server link "alice"
      row <- Forges.getRegisteredForge slug
      (Forges.rowStatus <$> row) `shouldBeM` Just ForgeActive
      shouldBeRegisteredBy "alice"
      forges' <- assert200 $ server.get "/api/forges"
      let listed = [entry | entry <- forges' ^.. responseBody . values, entry ^? key "slug" . _String == Just "localhost"]
      map (^? key "source" . _String) listed `shouldBeM` [Just "registered"]
      map (^? key "web_url" . _String) listed `shouldBeM` [Just (fakeUrl port)]
      map (^? key "name" . _String) listed `shouldBeM` [Just "localhost"]

    it "is activated by connecting it to the account the browser is logged in to" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      account <- server.login
      _ <- assert200 $ server.post "/api/forges" (registerBody port)
      started <- assert200 $ server.get "/api/auth/localhost/connect"
      _ <- assert200 $ callbackFrom server (started ^. responseBody . key "github" . _String) "alice"
      statusOf slug `shouldReturnM` Just ForgeActive
      registeredBy slug `shouldReturnM` Just (account ^. id)
      accountOf "alice" `shouldReturnM` Just (account ^. id)

    it "leaves it pending when the OAuth fails" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      _ <- assert200 $ server.post "/api/forges" (registerBody port)
      failed <- server.get "/api/auth/localhost/login/cb?code=wrong-code"
      (status failed >= 400) `shouldBeM` True
      statusOf slug `shouldReturnM` Just ForgePending
      started <- assert200 $ server.post "/api/auth/start" (startBody $ fakeUrl port)
      (started ^? responseBody . key "register" . key "slug" . _String) `shouldBeM` Just "localhost"

    it "leaves it pending when it was registered with a wrong client secret" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      _ <- assert200 $ server.post "/api/forges" (object ["url" .= fakeUrl port, "clientId" .= ("client-id" :: Text), "clientSecret" .= ("wrong-secret" :: Text)])
      failed <- logInThroughForge server "alice"
      (status failed >= 400) `shouldBeM` True
      statusOf slug `shouldReturnM` Just ForgePending
      registeredBy slug `shouldReturnM` Nothing

    it "is only activated by the browser that registered it" $ withHttpRegistration $ withFakeGitea $ \port -> do
      withServer $ \registrant -> do
        _ <- assert200 $ registrant.post "/api/forges" (registerBody port)
        -- Someone else completing an OAuth through the same app, from
        -- another browser, neither takes the registration nor gets an
        -- account through a forge nobody vouched for yet.
        withServer $ \stranger -> do
          refused <- logInThroughForge stranger "bob"
          status refused `shouldBeM` 403
          ("still waiting for whoever registered it" `T.isInfixOf` message refused) `shouldBeM` True
        statusOf slug `shouldReturnM` Just ForgePending
        accountOf "bob" `shouldReturnM` Nothing
        _ <- assert200 $ logInThroughForge registrant "alice"
        statusOf slug `shouldReturnM` Just ForgeActive
        shouldBeRegisteredBy "alice"

    it "keeps a pending registration from other browsers for 15 minutes" $ withHttpRegistration $ withFakeGitea $ \port -> do
      withServer $ \registrant -> do
        _ <- assert200 $ registrant.post "/api/forges" (registerBody port)
        withServer $ \other -> do
          refused <- other.post "/api/forges" (object ["url" .= fakeUrl port, "clientId" .= ("someone-elses" :: Text), "clientSecret" .= clientSecret])
          status refused `shouldBeM` 409
          ("a registration of localhost is already in progress" `T.isInfixOf` message refused) `shouldBeM` True
          (fmap Forges.rowOAuthClientId <$> Forges.getRegisteredForge slug) `shouldReturnM` Just "client-id"
          -- Re-submitting from the registering browser does not extend the
          -- lock.
          void $ DB.pgExec [pgSQL| UPDATE forges SET created_at = now() - interval '14 minutes' |]
          _ <- assert200 $ registrant.post "/api/forges" (registerBody port)
          stillRefused <- other.post "/api/forges" (registerBody port)
          status stillRefused `shouldBeM` 409
          void $ DB.pgExec [pgSQL| UPDATE forges SET created_at = now() - interval '16 minutes' |]
          _ <- assert200 $ other.post "/api/forges" (registerBody port)
          -- The first browser's token no longer counts.
          stale <- logInThroughForge registrant "alice"
          status stale `shouldBeM` 403
          statusOf slug `shouldReturnM` Just ForgePending
          _ <- assert200 $ logInThroughForge other "bob"
          shouldBeRegisteredBy "bob"

    it "replaces a pending registration on the same host" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      _ <- assert200 $ server.post "/api/forges" (object ["url" .= fakeUrl port, "clientId" .= ("someone-elses" :: Text), "clientSecret" .= ("whatever" :: Text)])
      _ <- assert200 $ server.post "/api/forges" (registerBody port)
      (fmap Forges.rowOAuthClientId <$> Forges.getRegisteredForge slug) `shouldReturnM` Just "client-id"
      _ <- assert200 $ logInThroughForge server "alice"
      statusOf slug `shouldReturnM` Just ForgeActive
      again <- server.post "/api/forges" (registerBody port)
      status again `shouldBeM` 409

    it "purges a registration that stayed pending too long" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      _ <- assert200 $ server.post "/api/forges" (registerBody port)
      void $ DB.pgExec [pgSQL| UPDATE forges SET updated_at = now() - interval '25 hours' |]
      late <- server.get "/api/auth/localhost/login/cb?code=alice-code"
      status late `shouldBeM` 404
      started <- assert200 $ server.post "/api/auth/start" (startBody $ fakeUrl port)
      (started ^? responseBody . key "register" . key "slug" . _String) `shouldBeM` Just "localhost"
      statusOf slug `shouldReturnM` Nothing

    it "refuses a URL that does not answer like Gitea" $ withHttpRegistration $ withServer $ \server -> do
      -- garnix itself, which has no /api/v1/version
      refused <- server.post "/api/forges" (object ["url" .= T.dropEnd 4 server.apiUrl, "clientId" .= ("client-id" :: Text), "clientSecret" .= clientSecret])
      status refused `shouldBeM` 400
      ("does not answer like a Gitea or Forgejo instance" `T.isInfixOf` message refused) `shouldBeM` True
      statusOf slug `shouldReturnM` Nothing

    it "rate-limits registrations per client" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      let from client = server.postWithHeaders "/api/forges" [("X-Forwarded-For", "198.51.100.1, " <> client)] (registerBody port)
      replicateM_ 10 $ assert200 $ from "203.0.113.7"
      limitedOut <- from "203.0.113.7"
      status limitedOut `shouldBeM` 429
      void $ assert200 $ from "203.0.113.8"

  describe "/api/auth/start" $ do
    let withGitExample = local (#forges %~ Map.insert "git.example" (testForgeInstance "git.example" GiteaForgeKind))

    it "answers a login link for a configured forge, whatever its URL looks like" $ withGitExample $ withHttpRegistration $ withServer $ \server -> do
      forM_ ["https://git.example", "https://GIT.example/", "http://git.example/some/path"] $ \url -> do
        started <- assert200 $ server.post "/api/auth/start" (startBody url)
        let link = started ^? responseBody . key "login" . _String
        (url, ("https://git.example/login/oauth/authorize?" `T.isPrefixOf`) <$> link) `shouldBeM` (url, Just True)

    it "answers a login link for an active registered forge" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      _ <- assert200 $ server.post "/api/forges" (registerBody port)
      _ <- assert200 $ logInThroughForge server "alice"
      withServer $ \other -> do
        started <- assert200 $ other.post "/api/auth/start" (startBody $ fakeUrl port <> "/")
        let link = started ^. responseBody . key "login" . _String
        ((fakeUrl port <> "/login/oauth/authorize?") `T.isPrefixOf` link) `shouldBeM` True
        -- It carries the cookie of its OAuth state, so a browser can follow it.
        loggedIn <- assert200 $ callbackFrom other link "bob"
        (loggedIn ^? responseBody . key "username" . _String) `shouldBeM` Just "bob"

    it "asks to register an unknown or pending forge, with the callback to give its OAuth app" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      unknown <- assert200 $ server.post "/api/auth/start" (startBody $ fakeUrl port)
      (unknown ^? responseBody . key "register" . key "slug" . _String) `shouldBeM` Just "localhost"
      (unknown ^? responseBody . key "register" . key "callback" . _String) `shouldBeM` Just "https://garnix.io/auth/localhost/login/cb"
      _ <- assert200 $ server.post "/api/forges" (registerBody port)
      pending <- assert200 $ server.post "/api/auth/start" (startBody $ fakeUrl port)
      (pending ^? responseBody . key "register" . key "slug" . _String) `shouldBeM` Just "localhost"

    it "refuses a forge under a sub-path, pointing to the configuration" $ withHttpRegistration $ withServer $ \server -> do
      refused <- server.post "/api/auth/start" (startBody "https://forge.test/gitea")
      status refused `shouldBeM` 400
      ("services.garnixServer.forges" `T.isInfixOf` message refused) `shouldBeM` True

    it "refuses plain http" $ do
      resolution <- liftIO $ newIORef (pure . loopback)
      withRegistration False resolution $ withServer $ \server -> do
        refused <- server.post "/api/auth/start" (startBody "http://forge.test")
        status refused `shouldBeM` 400
        ("https" `T.isInfixOf` message refused) `shouldBeM` True
        refused' <- server.post "/api/forges" (object ["url" .= ("http://forge.test" :: Text), "clientId" .= ("client-id" :: Text), "clientSecret" .= clientSecret])
        status refused' `shouldBeM` 400

    it "only answers configured forges when registration is off" $ withGitExample $ withFakeGitea $ \port -> withServer $ \server -> do
      configured <- assert200 $ server.post "/api/auth/start" (startBody "https://git.example")
      isJust (configured ^? responseBody . key "login" . _String) `shouldBeM` True
      unknown <- server.post "/api/auth/start" (startBody $ fakeUrl port)
      status unknown `shouldBeM` 404
      ("services.garnixServer.forges" `T.isInfixOf` message unknown) `shouldBeM` True
      registering <- server.post "/api/forges" (registerBody port)
      status registering `shouldBeM` 404
      replacing <- server.put "/api/forges/localhost/secret" (object ["clientSecret" .= ("new" :: Text)])
      status replacing `shouldBeM` 404
      removing <- server.delete "/api/forges/localhost"
      status removing `shouldBeM` 404

    it "ignores registered forges when registration is off" $ withFakeGitea $ \port -> do
      withHttpRegistration $ withServer $ \server -> do
        _ <- assert200 $ server.post "/api/forges" (registerBody port)
        void $ assert200 $ logInThroughForge server "alice"
      withServer $ \server -> do
        forges <- assert200 $ server.get "/api/forges"
        (forges ^.. responseBody . values . key "slug" . _String) `shouldBeM` ["github"]
        login' <- server.get "/api/auth/localhost/login"
        status login' `shouldBeM` 404

    it "rate-limits per client" $ withHttpRegistration $ withServer $ \server -> do
      let from client = server.postWithHeaders "/api/auth/start" [("X-Forwarded-For", client)] (startBody "https://forge.test")
      replicateM_ 30 $ assert200 $ from "203.0.113.7"
      limitedOut <- from "203.0.113.7"
      status limitedOut `shouldBeM` 429
      void $ assert200 $ from "203.0.113.8"

  describe "configured and registered forges on the same slug" $ do
    it "lets the configured one win" $ withHttpRegistration $ do
      secret <- encryptSecret "registered-secret"
      Forges.upsertPendingForge
        Forges.PendingForge
          { pendingSlug = "git.example",
            pendingWebUrl = "https://registered.example",
            pendingApiUrl = "https://registered.example/api/v1",
            pendingOAuthClientId = "registered-client",
            pendingOAuthClientSecret = secret,
            pendingWebhookSecret = secret,
            pendingTokenHash = hashRegistrationToken "token"
          }
        Nothing
        `shouldReturnM` True
      Right (mallory, _) <- DB.createAccount (ForgeIdentity githubForge "mallory" False) (Email "mallory@example.com")
      Forges.activatePendingForge "git.example" (mallory ^. id) (hashRegistrationToken "token") `shouldReturnM` True
      local (#forges %~ Map.insert "git.example" (testForgeInstance "git.example" GiteaForgeKind)) $ do
        instance' <- forgeInstanceFor "git.example"
        (_forgeInstanceConfig instance' ^. webUrl) `shouldBeM` "https://git.example"
        withServer $ \server -> do
          forges <- assert200 $ server.get "/api/forges"
          let gitExample = [entry | entry <- forges ^.. responseBody . values, entry ^? key "slug" . _String == Just "git.example"]
          map (^? key "source" . _String) gitExample `shouldBeM` [Just "configured"]
          map (^? key "web_url" . _String) gitExample `shouldBeM` [Just "https://git.example"]
          managing <- server.delete "/api/forges/git.example"
          status managing `shouldBeM` 404

  describe "administrators of a registered forge" $ do
    it "administer its repositories, while it is active" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      _ <- assert200 $ server.post "/api/forges" (registerBody port)
      _ <- assert200 $ logInThroughForge server "alice"
      withServer $ \server' -> void $ assert200 $ logInThroughForge server' "root"
      root <- accountOf "root" >>= maybe (error "no account") DB.getUserById >>= maybe (error "no account") pure
      alice <- accountOf "alice" >>= maybe (error "no account") DB.getUserById >>= maybe (error "no account") pure
      administeredForges (Just root) `shouldReturnM` [slug]
      hasAccessToRepo (Just root) (RepoIsPublic False) (RepoId slug "alice" "app") `shouldReturnM` True
      administeredForges (Just alice) `shouldReturnM` []
      _ <- assert200 $ server.delete "/api/forges/localhost"
      administeredForges (Just root) `shouldReturnM` []
      (isNothing <$> liveAccount root) `shouldReturnM` True

  describe "a configured forge on the host of a registered one" $ do
    it "hides the registered one, whatever its slug" $ withHttpRegistration $ withFakeGitea $ \port -> do
      withServer $ \server -> do
        _ <- assert200 $ server.post "/api/forges" (registerBody port)
        void $ assert200 $ logInThroughForge server "alice"
      let base = testForgeInstance "cb" GiteaForgeKind
          configured = base {_forgeInstanceConfig = _forgeInstanceConfig base & webUrl .~ "https://localhost"}
      local (#forges %~ Map.insert "cb" configured) $ do
        (isNothing <$> lookupForge slug) `shouldReturnM` True
        withServer $ \server -> do
          forges <- assert200 $ server.get "/api/forges"
          (forges ^.. responseBody . values . key "slug" . _String) `shouldBeM` ["cb", "github"]

  describe "the forges built from registrations" $ do
    it "drops a forge, and its decrypted secrets, once it is disabled or purged" $ withFakeGitea $ \port -> do
      cache <- liftIO newForgeCache
      resolution <- liftIO $ newIORef (pure . loopback)
      withRegistrationCache cache True resolution $ withServer $ \server -> do
        _ <- assert200 $ server.post "/api/forges" (registerBody port)
        _ <- assert200 $ logInThroughForge server "alice"
        liftIO (cachedForgeSlugs cache) `shouldReturnM` [slug]
        _ <- assert200 $ server.delete "/api/forges/localhost"
        _ <- lookupForge slug
        liftIO (cachedForgeSlugs cache) `shouldReturnM` []

        _ <- assert200 $ server.post "/api/forges" (registerBody port)
        _ <- lookupForge slug
        liftIO (cachedForgeSlugs cache) `shouldReturnM` [slug]
        void $ DB.pgExec [pgSQL| DELETE FROM forges |]
        _ <- listActiveForges
        liftIO (cachedForgeSlugs cache) `shouldReturnM` []

  describe "clientAddress" $ do
    let v4 a b c d = SockAddrInet 0 (tupleToHostAddress (a, b, c, d))
        v6 tuple = SockAddrInet6 0 0 (tupleToHostAddress6 tuple) 0

    it "trusts X-Forwarded-For from a proxy on a non-public address only" $ do
      clientAddress (v4 127 0 0 1) (Just "1.2.3.4, 203.0.113.7") `shouldBeM` "203.0.113.7"
      clientAddress (v4 10 0 0 5) (Just "203.0.113.7") `shouldBeM` "203.0.113.7"
      clientAddress (v4 93 184 216 34) (Just "203.0.113.7") `shouldBeM` "93.184.216.34"

    it "counts an IPv6 client as its /64" $ do
      clientAddress (v4 10 0 0 5) (Just "2606:4700:1:2:aaaa::1") `shouldBeM` clientAddress (v4 10 0 0 5) (Just "2606:4700:1:2:bbbb:cccc:dddd:eeee")
      (clientAddress (v4 10 0 0 5) (Just "2606:4700:1:2::1") /= clientAddress (v4 10 0 0 5) (Just "2606:4700:1:3::1")) `shouldBeM` True
      clientAddress (v6 (0x2606, 0x4700, 1, 2, 3, 4, 5, 6)) Nothing `shouldBeM` clientAddress (v6 (0x2606, 0x4700, 1, 2, 9, 9, 9, 9)) Nothing

    it "counts an IPv4 client mapped into IPv6 as itself" $ do
      clientAddress (v6 (0, 0, 0, 0, 0, 0xffff, 0x5db8, 0xd822)) Nothing `shouldBeM` "93.184.216.34"
      clientAddress (v6 (0, 0, 0, 0, 0, 0xffff, 0x5db8, 0xd823)) Nothing `shouldBeM` "93.184.216.35"
      clientAddress (v4 10 0 0 5) (Just "::ffff:203.0.113.7") `shouldBeM` "203.0.113.7"

    it "ignores the port of an X-Forwarded-For entry" $ do
      clientAddress (v4 10 0 0 5) (Just "203.0.113.7:51234") `shouldBeM` "203.0.113.7"
      clientAddress (v4 10 0 0 5) (Just "[2606:4700:1:2::1]:443") `shouldBeM` clientAddress (v4 10 0 0 5) (Just "2606:4700:1:2::1")

  describe "outbound connections" $ do
    it "refuses to register a forge whose host resolves to a private address" $ withFakeGitea $ \port -> do
      forM_ [SockAddrInet 443 (tupleToHostAddress (10, 0, 0, 1)), SockAddrInet 443 (tupleToHostAddress (169, 254, 169, 254))] $ \target -> do
        resolution <- liftIO $ newIORef (const [target])
        withRegistration True resolution $ withServer $ \server -> do
          refused <- server.post "/api/forges" (registerBody port)
          status refused `shouldBeM` 400
          ("not public" `T.isInfixOf` message refused) `shouldBeM` True
      statusOf slug `shouldReturnM` Nothing

    it "checks again on every connection, so a host rebound to a private address is refused" $ withFakeGitea $ \port -> do
      resolution <- liftIO $ newIORef (pure . loopback)
      withRegistration True resolution $ withServer $ \server -> do
        _ <- assert200 $ server.post "/api/forges" (registerBody port)
        liftIO $ writeIORef resolution (const [SockAddrInet 80 (tupleToHostAddress (169, 254, 169, 254))])
        failed <- logInThroughForge server "alice"
        (status failed >= 400) `shouldBeM` True
        statusOf slug `shouldReturnM` Just ForgePending

    it "never resolves a repository on a registered forge, so nothing of it reaches git or nix" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      _ <- assert200 $ server.post "/api/forges" (registerBody port)
      _ <- assert200 $ logInThroughForge server "alice"
      (isNothing <$> resolveRepo (RepoId slug "alice" "app")) `shouldReturnM` True
      (isNothing <$> resolveCredentials (RepoId slug "alice" "app")) `shouldReturnM` True

  describe "managing a registered forge" $ do
    let activeForge server port = do
          _ <- assert200 $ server.post "/api/forges" (registerBody port)
          void $ assert200 $ logInThroughForge server "alice"

    it "lets whoever registered it replace its secret and disable it, ending logins and sessions through it" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      activeForge server port
      _ <- assert200 $ server.put "/api/forges/localhost/secret" (object ["clientSecret" .= clientSecret])
      whoami <- assert200 $ server.get "/api/whoami"
      (whoami ^? responseBody . key "username" . _String) `shouldBeM` Just "alice"
      _ <- assert200 $ server.get "/api/account/tokens"
      alice <- accountOf "alice" >>= maybe (error "no account") pure
      token <- generateToken alice "spec" AccessTokenScopes {api = True, cache = False}
      _ <- assert200 $ exchangeApiToken "alice@localhost" token

      _ <- assert200 $ server.delete "/api/forges/localhost"
      statusOf slug `shouldReturnM` Just ForgeDisabled
      login' <- server.get "/api/auth/localhost/login"
      status login' `shouldBeM` 404
      hook <- server.postWithHeaders "/api/forges/localhost/webhook" [("X-Gitea-Event", "push")] (object [])
      status hook `shouldBeM` 404
      -- The account stays, but it was its only identity: its sessions end,
      -- and its api tokens log nobody in.
      whoami' <- assert200 $ server.get "/api/whoami"
      (whoami' ^. responseBody) `shouldBeM` "null"
      tokens <- server.get "/api/account/tokens"
      status tokens `shouldBeM` 401
      creating <- server.post "/api/account/tokens" (object ["name" .= ("t" :: Text), "scopes" .= object ["api" .= True]])
      status creating `shouldBeM` 401
      exchanged <- exchangeApiToken "alice@localhost" token
      status exchanged `shouldBeM` 401
      replacing <- server.put "/api/forges/localhost/secret" (object ["clientSecret" .= clientSecret])
      status replacing `shouldBeM` 401
      -- Disabling deletes nothing: the identity, and its account, stay,
      -- for the forge to come back.
      accountOf "alice" `shouldReturnM` Just alice
      (fmap (map (^. forge) . (^. identities)) <$> DB.getUserById alice) `shouldReturnM` Just [slug]

    it "ends the sessions through registered forges when registration is turned off" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      activeForge server port
      alice <- accountOf "alice" >>= maybe (error "no account") DB.getUserById >>= maybe (error "no account") pure
      (isJust <$> liveAccount alice) `shouldReturnM` True
      local (#forgeRegistration .~ Nothing) $ do
        (isNothing <$> liveAccount alice) `shouldReturnM` True
        token <- generateToken (alice ^. id) "spec" AccessTokenScopes {api = True, cache = False}
        exchanged <- exchangeApiToken "alice@localhost" token
        status exchanged `shouldBeM` 401

    it "is not managed through an api token" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      activeForge server port
      alice <- accountOf "alice" >>= maybe (error "no account") pure
      token <- generateToken alice "spec" AccessTokenScopes {api = True, cache = False}
      jwt <- (^?! responseBody . key "token" . _String) <$> assert200 (exchangeApiToken "alice@localhost" token)
      withServer $ \api -> do
        replacing <- api.putWithHeaders "/api/forges/localhost/secret" [("Authorization", cs $ "Bearer " <> jwt)] (object ["clientSecret" .= clientSecret])
        status replacing `shouldBeM` 403
      statusOf slug `shouldReturnM` Just ForgeActive

    it "comes back, disabled, by being registered again, owned by whoever completes it" $ withHttpRegistration $ withFakeGitea $ \port -> do
      withServer $ \server -> do
        activeForge server port
        void $ assert200 $ server.delete "/api/forges/localhost"
      withServer $ \server -> do
        started <- assert200 $ server.post "/api/auth/start" (startBody $ fakeUrl port)
        (started ^? responseBody . key "register" . key "slug" . _String) `shouldBeM` Just "localhost"
        _ <- assert200 $ server.post "/api/forges" (registerBody port)
        statusOf slug `shouldReturnM` Just ForgePending
        registeredBy slug `shouldReturnM` Nothing
        _ <- assert200 $ logInThroughForge server "bob"
        statusOf slug `shouldReturnM` Just ForgeActive
        shouldBeRegisteredBy "bob"

    it "comes back with everyone when it is registered again within 30 days of being disabled" $ withHttpRegistration $ withFakeGitea $ \port -> do
      withServer $ \server -> do
        activeForge server port
        withServer $ \server' -> void $ assert200 $ logInThroughForge server' "root"
        void $ assert200 $ server.delete "/api/forges/localhost"
      alice <- accountOf "alice" >>= maybe (error "no account") pure
      root <- accountOf "root" >>= maybe (error "no account") pure
      _ <- generateToken alice "spec" AccessTokenScopes {api = True, cache = False}
      disabledDaysAgo 29
      withServer $ \server -> do
        _ <- assert200 $ server.post "/api/forges" (registerBody port)
        _ <- assert200 $ logInThroughForge server "bob"
        statusOf slug `shouldReturnM` Just ForgeActive
        shouldBeRegisteredBy "bob"
      disabledAt `shouldReturnM` [Nothing]
      accountOf "alice" `shouldReturnM` Just alice
      accountOf "root" `shouldReturnM` Just root
      (length <$> DB.getAccessTokensForUser alice) `shouldReturnM` 1
      withServer $ \server -> do
        _ <- assert200 $ logInThroughForge server "alice"
        accountOf "alice" `shouldReturnM` Just alice

    it "still brings everyone back after a registration of it that was never completed" $ withHttpRegistration $ withFakeGitea $ \port -> do
      withServer $ \server -> do
        activeForge server port
        void $ assert200 $ server.delete "/api/forges/localhost"
      alice <- accountOf "alice" >>= maybe (error "no account") pure
      withServer $ \server -> void $ assert200 $ server.post "/api/forges" (registerBody port)
      void $ DB.pgExec [pgSQL| UPDATE forges SET updated_at = now() - interval '25 hours', created_at = now() - interval '25 hours' |]
      withServer $ \server -> do
        started <- assert200 $ server.post "/api/auth/start" (startBody $ fakeUrl port)
        (started ^? responseBody . key "register" . key "slug" . _String) `shouldBeM` Just "localhost"
        statusOf slug `shouldReturnM` Just ForgePending
        _ <- assert200 $ server.post "/api/forges" (registerBody port)
        _ <- assert200 $ logInThroughForge server "bob"
        statusOf slug `shouldReturnM` Just ForgeActive
      accountOf "alice" `shouldReturnM` Just alice

    it "forgets the identities from before it once it is registered again 30 days or more after it was disabled, and the accounts left with none" $ withHttpRegistration $ withFakeGitea $ \port -> do
      withServer $ \server -> do
        activeForge server port
        withServer $ \server' -> void $ assert200 $ logInThroughForge server' "root"
        void $ assert200 $ server.delete "/api/forges/localhost"
      disabledDaysAgo 30
      alice <- accountOf "alice" >>= maybe (error "no account") pure
      root <- accountOf "root" >>= maybe (error "no account") pure
      -- root also logs in through github; alice only through the forge, and
      -- has an api token and a build it requested.
      (isRight <$> DB.tryAddIdentity root (ForgeIdentity githubForge "root-on-github" False)) `shouldReturnM` True
      _ <- generateToken alice "spec" AccessTokenScopes {api = True, cache = False}
      _ <- testBuild $ \build -> build {_buildForge = slug, _buildReqUser = "alice"}
      withServer $ \server -> do
        _ <- assert200 $ server.post "/api/forges" (registerBody port)
        _ <- assert200 $ logInThroughForge server "bob"
        statusOf slug `shouldReturnM` Just ForgeActive
      accountOf "alice" `shouldReturnM` Nothing
      accountOf "root" `shouldReturnM` Nothing
      (isNothing <$> DB.getUserById alice) `shouldReturnM` True
      DB.getAccessTokensForUser alice `shouldReturnM` []
      (fmap (map (^. forge) . (^. identities)) <$> DB.getUserById root) `shouldReturnM` Just [githubForge]
      builds <- DB.pgQuery [pgSQL| SELECT count(*) FROM builds WHERE forge = 'localhost' AND req_user = 'alice' |]
      builds `shouldBeM` [Just (1 :: Int64)]

    it "hands nobody the accounts of its old identities, when whoever holds its host next registers it again" $ withHttpRegistration $ withFakeGitea $ \port -> do
      -- alice logs in through git.x; its registrant disables it (say, its
      -- domain is about to expire).
      oldAlice <- withServer $ \server -> do
        activeForge server port
        oldAlice <- accountOf "alice" >>= maybe (error "no account") pure
        void $ assert200 $ server.delete "/api/forges/localhost"
        pure oldAlice
      -- The domain expires, which takes longer than the quarantine. Whoever
      -- bought it runs a Gitea with a user alice, and registers it again
      -- from another account.
      disabledDaysAgo 30
      withServer $ \server -> do
        _ <- server.login
        _ <- assert200 $ server.post "/api/forges" (registerBody port)
        _ <- assert200 $ logInThroughForge server "alice"
        statusOf slug `shouldReturnM` Just ForgeActive
      newAlice <- accountOf "alice" >>= maybe (error "no account") pure
      (newAlice /= oldAlice) `shouldBeM` True
      -- The old account had no other identity: it is gone.
      (isNothing <$> DB.getUserById oldAlice) `shouldReturnM` True

    it "is re-enabled at once, with everyone, by a new secret from whoever registered it" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      activeForge server port
      alice <- alsoOnGithub "alice"
      withServer $ \server' -> void $ assert200 $ logInThroughForge server' "bob"
      bob <- accountOf "bob" >>= maybe (error "no account") pure
      _ <- assert200 $ server.delete "/api/forges/localhost"
      statusOf slug `shouldReturnM` Just ForgeDisabled
      (all isJust <$> disabledAt) `shouldReturnM` True
      _ <- assert200 $ server.put "/api/forges/localhost/secret" (object ["clientSecret" .= clientSecret])
      statusOf slug `shouldReturnM` Just ForgeActive
      disabledAt `shouldReturnM` [Nothing]
      registeredBy slug `shouldReturnM` Just alice
      withServer $ \server' -> do
        _ <- assert200 $ logInThroughForge server' "bob"
        accountOf "bob" `shouldReturnM` Just bob

    it "is re-enabled by an administrator of the instance, and by nobody else" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      activeForge server port
      withServer $ \asRoot -> withServer $ \asBob -> withServer $ \asOther -> do
        _ <- assert200 $ logInThroughForge asRoot "root"
        _ <- assert200 $ logInThroughForge asBob "bob"
        _ <- asOther.login
        mapM_ alsoOnGithub ["root", "bob"]
        _ <- assert200 $ server.delete "/api/forges/localhost"
        forM_ [asBob, asOther] $ \stranger -> do
          refused <- stranger.put "/api/forges/localhost/secret" (object ["clientSecret" .= ("stolen" :: Text)])
          status refused `shouldBeM` 403
        withServer $ \anonymous -> do
          refused <- anonymous.put "/api/forges/localhost/secret" (object ["clientSecret" .= ("stolen" :: Text)])
          status refused `shouldBeM` 401
        statusOf slug `shouldReturnM` Just ForgeDisabled
        _ <- assert200 $ asRoot.put "/api/forges/localhost/secret" (object ["clientSecret" .= clientSecret])
        statusOf slug `shouldReturnM` Just ForgeActive

    it "lets an administrator of the instance manage it" $ withHttpRegistration $ withFakeGitea $ \port -> do
      withServer $ \server -> activeForge server port
      withServer $ \server -> do
        _ <- assert200 $ logInThroughForge server "root"
        _ <- assert200 $ server.put "/api/forges/localhost/secret" (object ["clientSecret" .= clientSecret])
        _ <- assert200 $ server.delete "/api/forges/localhost"
        statusOf slug `shouldReturnM` Just ForgeDisabled

    it "refuses anyone else" $ withHttpRegistration $ withFakeGitea $ \port -> do
      withServer $ \server -> activeForge server port
      withServer $ \server -> do
        _ <- assert200 $ logInThroughForge server "bob"
        refused <- server.delete "/api/forges/localhost"
        status refused `shouldBeM` 403
        refused' <- server.put "/api/forges/localhost/secret" (object ["clientSecret" .= ("stolen" :: Text)])
        status refused' `shouldBeM` 403
      withServer $ \server -> do
        _ <- server.login
        refused <- server.delete "/api/forges/localhost"
        status refused `shouldBeM` 403
      withServer $ \server -> do
        anonymous <- server.delete "/api/forges/localhost"
        status anonymous `shouldBeM` 401
        -- Authentication comes before looking the slug up.
        anonymous' <- server.delete "/api/forges/nowhere.example"
        status anonymous' `shouldBeM` 401
      statusOf slug `shouldReturnM` Just ForgeActive

    it "keeps the account an identity on an active forge when another one is disconnected" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      activeForge server port
      alice <- alsoOnGithub "alice"
      let disconnectGithub = server.deleteWithBody "/api/auth/github/identity" (object [])
          forgesOf = fmap (map (^. forge) . (^. identities)) <$> DB.getUserById alice
      _ <- assert200 $ server.delete "/api/forges/localhost"
      -- Its identity on the disabled forge logs nobody in: without GitHub
      -- the account would have no way in until the forge comes back.
      refused <- disconnectGithub
      status refused `shouldBeM` 409
      (refused ^? responseBody . key "reason" . _String) `shouldBeM` Just "last_identity"
      forgesOf `shouldReturnM` Just [slug, githubForge]
      _ <- assert200 $ server.put "/api/forges/localhost/secret" (object ["clientSecret" .= clientSecret])
      _ <- assert200 disconnectGithub
      forgesOf `shouldReturnM` Just [slug]

    it "never answers the client secret" $ withHttpRegistration $ withFakeGitea $ \port -> withServer $ \server -> do
      responses <-
        sequence
          [ server.post "/api/auth/start" (startBody $ fakeUrl port),
            server.post "/api/forges" (registerBody port),
            server.post "/api/auth/start" (startBody $ fakeUrl port),
            logInThroughForge server "alice",
            server.post "/api/auth/start" (startBody $ fakeUrl port),
            server.get "/api/forges",
            server.get "/api/auth/localhost/login",
            server.put "/api/forges/localhost/secret" (object ["clientSecret" .= clientSecret]),
            server.delete "/api/forges/localhost",
            server.post "/api/forges" (registerBody port)
          ]
      forM_ responses $ \response ->
        (clientSecret `T.isInfixOf` cs (response ^. responseBody)) `shouldBeM` False
      -- Nor is it stored in the clear.
      stored <- Forges.getRegisteredForge slug
      ((clientSecret `T.isInfixOf`) . cs . getEncryptedText . Forges.rowOAuthClientSecret <$> stored) `shouldBeM` Just False
