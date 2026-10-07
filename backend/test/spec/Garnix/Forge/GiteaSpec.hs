module Garnix.Forge.GiteaSpec (spec) where

import Control.Concurrent (modifyMVar, modifyMVar_, newMVar, readMVar)
import Cradle
import Crypto.Hash (Digest, SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.Lens (key, _Object, _String)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isAlphaNum)
import Data.Containers.ListUtils (nubOrd)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Garnix.Build.Checkout (withAuthorization)
import Garnix.Build.Helpers (withInternalCacheToken, withPrivateNixXdgCache)
import Garnix.FlakeInputAuthorization (GithubVisibility (..), InputAuthorization (..), checkAuthorization, _githubVisibility)
import Garnix.Forge.Gitea
import Garnix.Hosting.Deploy (deployerGithubKeys)
import Garnix.Monad
import Garnix.NixConfig (NetRcEntry (..), addNixConfigEnvironment, getNetRcFileSetting, nixConfDefaults)
import Garnix.Prelude
import Garnix.Sandbox (inNixSandbox)
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.WithServer qualified as WithServer
import Garnix.Types hiding (body)
import Network.HTTP.Types (hAuthorization, status200, status201, status404)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wreq qualified as Wreq
import Test.Hspec
import Text.Read (readMaybe)

-- | A request the fake Gitea received.
data Received = Received
  { method :: Text,
    path :: Text,
    query :: [(Text, Text)],
    authorization :: Maybe Text,
    body :: LazyByteString
  }
  deriving stock (Show)

-- | Answers to the fake Gitea's routes, by method and path.
type Routes = Text -> Text -> [(Text, Text)] -> (HTTP.Status, Value)

-- | Runs the action with a Gitea instance @git.example@ served by the given
-- routes, and returns what the instance received.
withFakeGitea :: Routes -> (ForgeConfig -> M a) -> M (a, [Received])
withFakeGitea routes =
  withFakeGiteaRaw $ \r ->
    let (status', answer) = routes (method r) (path r) (query r)
     in pure (status', [("Content-Type", "application/json")], encode answer)

-- | 'withFakeGitea', with routes that see the whole request and answer any
-- body.
withFakeGiteaRaw :: (Received -> IO (HTTP.Status, HTTP.ResponseHeaders, LazyByteString)) -> (ForgeConfig -> M a) -> M (a, [Received])
withFakeGiteaRaw routes action = do
  received <- liftIO $ newMVar []
  let app request respond = do
        requestBody <- Wai.strictRequestBody request
        let method' = T.decodeUtf8 $ Wai.requestMethod request
            path' = "/" <> T.intercalate "/" (Wai.pathInfo request)
            query' = [(T.decodeUtf8 k, maybe "" T.decodeUtf8 v) | (k, v) <- Wai.queryString request]
        let this =
              Received
                { method = method',
                  path = path',
                  query = query',
                  authorization = T.decodeUtf8 <$> lookup hAuthorization (Wai.requestHeaders request),
                  body = requestBody
                }
        modifyMVar_ received $ \rs -> pure $ rs <> [this]
        (status', headers, answer) <- routes this
        respond $ Wai.responseLBS status' headers answer
  result <- liftBaseOp (testWithApplication (pure app)) $ \port ->
    action (giteaConfig $ "http://localhost:" <> show port)
  (result,) <$> liftIO (readMVar received)

giteaConfig :: Text -> ForgeConfig
giteaConfig webUrl' =
  ForgeConfig
    { _forgeConfigSlug = ForgeSlug "git.example",
      _forgeConfigKind = GiteaForgeKind,
      _forgeConfigWebUrl = webUrl',
      _forgeConfigApiUrl = webUrl' <> "/api/v1",
      _forgeConfigWebhookSecret = "webhook-secret",
      _forgeConfigOAuthClientId = "client-id",
      _forgeConfigOAuthClientSecret = "client-secret",
      _forgeConfigApiToken = Just (GhToken "bot-token"),
      _forgeConfigAdmins = []
    }

-- | Runs the action with the instance registered in 'forges'.
withInstance :: ForgeConfig -> M a -> M a
withInstance config = local (#forges %~ Map.insert (config ^. slug) (ForgeInstance config (giteaForgeApi config)))

appRepo :: RepoId
appRepo = RepoId (ForgeSlug "git.example") "acme" "app"

repoAnswer :: Bool -> Value
repoAnswer canPush =
  object
    [ "full_name" .= ("acme/app" :: Text),
      "private" .= True,
      "default_branch" .= ("main" :: Text),
      "permissions" .= object ["admin" .= False, "push" .= canPush, "pull" .= True]
    ]

report :: RunReportStatus -> GhRunReport
report status' =
  GhRunReport
    { _ghRunReportName = "package x86_64-linux.default",
      _ghRunReportCommit = "2eba238e33607c1fa49253182e9fff42baafa1eb",
      _ghRunReportUrl = Just "/build/abc",
      _ghRunReportStatus = status',
      _ghRunReportTitle = "package x86_64-linux.default",
      _ghRunReportSummary = "package x86_64-linux.default succeeded",
      _ghRunReportLogs = RawLogs "some logs"
    }

notFound :: (HTTP.Status, Value)
notFound = (status404, object ["message" .= ("not found" :: Text)])

spec :: Spec
spec = do
  clientSpec
  webhookSpec
  privateInputsSpec

-- | The API client.
clientSpec :: Spec
clientSpec = do
  describe "giteaStatusState" $ do
    it "maps build outcomes to commit status states" $ do
      map giteaStatusState [RunReportStatusInProgress, RunReportStatusSuccess, RunReportStatusFailure, RunReportStatusTimeout, RunReportStatusCancelled]
        `shouldBe` ["pending", "success", "failure", "error", "error"]

  describe "giteaCommitStatus" $ do
    it "names the status after the report and links to the build in garnix" $ do
      giteaCommitStatus ("https://garnix.example" <>) (report RunReportStatusSuccess)
        `shouldBe` object
          [ "state" .= ("success" :: Text),
            "context" .= ("package x86_64-linux.default" :: Text),
            "description" .= ("package x86_64-linux.default succeeded" :: Text),
            "target_url" .= ("https://garnix.example/build/abc" :: Text)
          ]

  inM $ aroundM_ suppressLogsWhenPassing $ do
    describe "resolveRepo" $ do
      it "hands out the bot token when the bot can push to the repository" $ do
        (resolved, received) <- withFakeGitea (\_ _ _ -> (status200, repoAnswer True)) $ \config ->
          withInstance config $ resolveRepo appRepo
        fmap (view ghToken) resolved `shouldBeM` Just (GhToken "bot-token")
        map (\r -> (method r, path r, authorization r)) received
          `shouldBeM` [("GET", "/api/v1/repos/acme/app", Just "token bot-token")]

      it "treats a repository the bot can only read as not installed" $ do
        (resolved, _) <- withFakeGitea (\_ _ _ -> (status200, repoAnswer False)) $ \config ->
          withInstance config $ resolveRepo appRepo
        isNothing resolved `shouldBeM` True

      it "treats a repository the bot cannot see as not installed" $ do
        (resolved, _) <- withFakeGitea (\_ _ _ -> notFound) $ \config ->
          withInstance config $ resolveRepo appRepo
        isNothing resolved `shouldBeM` True

    describe "lists" $ do
      let collaborators = [object ["login" .= ("user" <> show n :: Text)] | n <- [1 .. 30 :: Int]]
          -- Gitea with MAX_RESPONSE_ITEMS = 20, below the 50 garnix asks for.
          cappedPage withTotal r
            | path r == "/api/v1/repos/acme/app/collaborators" =
                let page = fromMaybe 1 (lookup "page" (query r) >>= readMaybe . cs)
                 in pure
                      ( status200,
                        [("X-Total-Count", "30") | withTotal],
                        encode (take 20 (drop ((page - 1) * 20) collaborators))
                      )
            | otherwise = pure (status404, [], "{}")
          loginsOf = \case
            GhCollaborators logins -> Just logins
            RepoNotFound -> Nothing

      it "reads every page, even when the instance caps the page size below the size asked for" $ do
        (result, received) <- withFakeGiteaRaw (cappedPage False) $ \config ->
          withInstance config $ getRepoCollaborators ApiTokenCredentials appRepo
        fmap length (loginsOf result) `shouldBeM` Just 30
        length received `shouldBeM` 3

      it "stops when the instance ignores the page asked for" $ do
        (result, received) <- withFakeGiteaRaw (\_ -> pure (status200, [], encode (take 20 collaborators))) $ \config ->
          withInstance config $ getRepoCollaborators ApiTokenCredentials appRepo
        fmap length (loginsOf result) `shouldBeM` Just 20
        length received `shouldBeM` 2

      it "fails when a later page of a list disappears" $ do
        let firstPageOnly r
              | lookup "page" (query r) `elem` [Nothing, Just "1"] = cappedPage False r
              | otherwise = pure (status404, [], "{}")
        (result, _) <- withFakeGiteaRaw firstPageOnly $ \config ->
          withInstance config $ try (getRepoCollaborators ApiTokenCredentials appRepo)
        either (Just . show . err) (const Nothing) result `shouldSatisfyM` maybe False ("404 for page 2" `T.isInfixOf`)

      it "stops once it has as many items as the instance counts" $ do
        (result, received) <- withFakeGiteaRaw (cappedPage True) $ \config ->
          withInstance config $ getRepoCollaborators ApiTokenCredentials appRepo
        fmap length (loginsOf result) `shouldBeM` Just 30
        length received `shouldBeM` 2

    describe "server errors" $ do
      it "retries a request the instance failed" $ do
        failures <- liftIO $ newMVar (1 :: Int)
        let flaky _ = do
              failing <- modifyMVar failures $ \n -> pure (max 0 (n - 1), n > 0)
              pure
                $ if failing
                  then (HTTP.status502, [], "")
                  else (status200, [("Content-Type", "application/json")], encode (repoAnswer True))
        (resolved, received) <- withFakeGiteaRaw flaky $ \config ->
          withInstance config $ resolveRepo appRepo
        fmap (view ghToken) resolved `shouldBeM` Just (GhToken "bot-token")
        length received `shouldBeM` 2

    describe "build reports" $ do
      let statusRoutes "POST" "/api/v1/repos/acme/app/statuses/2eba238e33607c1fa49253182e9fff42baafa1eb" _ =
            (status201, object ["id" .= (42 :: Int)])
          statusRoutes _ _ _ = notFound
          repoInfo' = RepoInfo ApiTokenCredentials (GhToken "bot-token") appRepo

      it "posts a pending commit status when a build starts" $ do
        (runId, received) <- withFakeGitea statusRoutes $ \config ->
          withInstance config $ newBuildReport repoInfo' (report RunReportStatusInProgress)
        runId `shouldBeM` GhRunId (-42)
        [r] <- pure received
        authorization r `shouldBeM` Just "token bot-token"
        let posted = decode @Value (body r)
        (posted >>= (^? key "state" . _String)) `shouldBeM` Just "pending"
        (posted >>= (^? key "context" . _String)) `shouldBeM` Just "package x86_64-linux.default"
        (posted >>= (^? key "target_url" . _String)) `shouldBeM` Just "https://garnix.io/build/abc"

      it "posts the outcome when a build finishes" $ do
        (_, received) <- withFakeGitea statusRoutes $ \config ->
          withInstance config $ updateBuildReport (GhRunId (-42)) (report RunReportStatusFailure) repoInfo'
        map (\r -> decode @Value (body r) >>= (^? key "state" . _String)) received `shouldBeM` [Just "failure"]

      it "does not post anything while a build runs" $ do
        (_, received) <- withFakeGitea statusRoutes $ \config ->
          withInstance config $ updateBuildReport (GhRunId (-42)) (report RunReportStatusInProgress) repoInfo'
        length received `shouldBeM` 0

    describe "getRemote" $ do
      it "clones the repository with the bot token" $ do
        let config = giteaConfig "https://git.example.com"
        RemoteUrl url <- withInstance config $ getRemote (commitInfoOn appRepo Nothing)
        url `shouldBeM` "https://x-access-token:bot-token@git.example.com/acme/app.git"

      it "clones a fork anonymously" $ do
        let config = giteaConfig "https://git.example.com"
        RemoteUrl url <- withInstance config $ getRemote (commitInfoOn appRepo (Just $ PrFromFork "someone/app"))
        url `shouldBeM` "https://git.example.com/someone/app.git"

    describe "getPullRequestsForCommit" $ do
      it "finds the open pull requests whose head is the commit" $ do
        let routes "GET" "/api/v1/repos/acme/app/pulls" query'
              | ("page", "1") `elem` query' =
                  ( status200,
                    toJSON
                      [ object ["number" .= (3 :: Int), "head" .= object ["sha" .= ("aaa" :: Text)]],
                        object ["number" .= (4 :: Int), "head" .= object ["sha" .= ("bbb" :: Text)]]
                      ]
                  )
              | otherwise = (status200, toJSON ([] :: [Value]))
            routes _ _ _ = notFound
        (prs, received) <- withFakeGitea routes $ \config ->
          withInstance config $ getPullRequestsForCommit (RepoInfo ApiTokenCredentials (GhToken "bot-token") appRepo) "bbb"
        prs `shouldBeM` [GhPullRequestId 4]
        map (lookup "state" . query) received `shouldBeM` [Just "open", Just "open"]

    describe "repository contents" $ do
      let routes "GET" "/api/v1/repos/acme/app/branches/feature/x" _ =
            (status200, object ["name" .= ("feature/x" :: Text), "commit" .= object ["id" .= ("2eba238e33607c1fa49253182e9fff42baafa1eb" :: Text)]])
          routes "GET" "/api/v1/repos/acme/app/contents/garnix.yaml" query'
            | lookup "ref" query' == Just "2eba238e33607c1fa49253182e9fff42baafa1eb" = (status200, object ["type" .= ("file" :: Text)])
          routes _ _ _ = notFound

      it "reads the head commit of a branch" $ do
        (head', _) <- withFakeGitea routes $ \config ->
          withInstance config $ getHeadCommit (GhToken "bot-token") appRepo "feature/x"
        head' `shouldBeM` "2eba238e33607c1fa49253182e9fff42baafa1eb"

      it "tells whether a file exists at a commit" $ do
        (found, _) <- withFakeGitea routes $ \config ->
          withInstance config $ (,) <$> doesRepoFileExist (commitInfoOn appRepo Nothing) "garnix.yaml" <*> doesRepoFileExist (commitInfoOn appRepo Nothing) "missing.yaml"
        found `shouldBeM` (FileExists, FileDoesntExist)

    describe "OAuth" $ do
      let routes "POST" "/login/oauth/access_token" _ =
            (status200, object ["access_token" .= ("user-token" :: Text), "token_type" .= ("bearer" :: Text), "expires_in" .= (3600 :: Int), "refresh_token" .= ("refresh-token" :: Text)])
          routes "GET" "/api/v1/user" _ =
            (status200, object ["login" .= ("alice" :: Text), "email" .= ("alice@example.com" :: Text)])
          routes _ _ _ = notFound

      it "exchanges a code for the user's tokens at the instance's token endpoint" $ do
        (credentials', received) <- withFakeGitea routes $ \config ->
          withInstance config $ exchangeOauthCode (config ^. slug) "https://garnix.example/callback" (OAuthCode "the-code")
        credentials' ^. #_ghUserCredentialsAccessToken `shouldBeM` "user-token"
        credentials' ^. #_ghUserCredentialsRefreshToken `shouldBeM` Just "refresh-token"
        [r] <- pure received
        sort (T.splitOn "&" (cs (body r)))
          `shouldBeM` sort
            [ "client_id=client-id",
              "client_secret=client-secret",
              "code=the-code",
              "grant_type=authorization_code",
              "redirect_uri=https%3A%2F%2Fgarnix.example%2Fcallback"
            ]

      it "refreshes the user's tokens at the instance's token endpoint" $ do
        (credentials', received) <- withFakeGitea routes $ \config ->
          withInstance config $ refreshUserCredentials (config ^. slug) "old-refresh-token"
        credentials' ^. #_ghUserCredentialsAccessToken `shouldBeM` "user-token"
        [r] <- pure received
        sort (T.splitOn "&" (cs (body r)))
          `shouldBeM` sort
            [ "client_id=client-id",
              "client_secret=client-secret",
              "grant_type=refresh_token",
              "refresh_token=old-refresh-token"
            ]

      it "retries the token endpoint after a server error" $ do
        failures <- liftIO $ newMVar (1 :: Int)
        let flaky r = do
              failing <- modifyMVar failures $ \n -> pure (max 0 (n - 1), n > 0)
              pure
                $ if failing
                  then (HTTP.status502, [], "")
                  else let (status', answer) = routes (method r) (path r) (query r) in (status', [("Content-Type", "application/json")], encode answer)
        (credentials', received) <- withFakeGiteaRaw flaky $ \config ->
          withInstance config $ exchangeOauthCode (config ^. slug) "https://garnix.example/callback" (OAuthCode "the-code")
        credentials' ^. #_ghUserCredentialsAccessToken `shouldBeM` "user-token"
        length received `shouldBeM` 2

      it "asks the instance who a token belongs to" $ do
        (user, received) <- withFakeGitea routes $ \config ->
          withInstance config $ getCurrentUser (config ^. slug) "user-token"
        user `shouldBeM` ("alice", Email "alice@example.com")
        map authorization received `shouldBeM` [Just "token user-token"]

-- | Builds triggered by an instance's webhooks.
webhookSpec :: Spec
webhookSpec = inM $ aroundM_ suppressLogsWhenPassing $ do
  describe "builds requested from an instance" $ do
    let netRcOf = view #userNixConfig >>= traverse (liftIO . T.readFile . getNetRcFile) . getNetRcFileSetting

    it "get a cache token of their own, apart from the GitHub user with the same login" $ do
      netRc <- withInternalCacheToken (ForgeLogin githubForge "alice") netRcOf
      fmap ("machine cache.garnix.io\nlogin alice\n" `T.isInfixOf`) netRc `shouldBeM` Just True
      fromInstance <- withInternalCacheToken (ForgeLogin (ForgeSlug "git.example") "alice") netRcOf
      fmap ("machine cache.garnix.io\nlogin alice@git.example\n" `T.isInfixOf`) fromInstance `shouldBeM` Just True
      let password = fmap (filter ("password " `T.isPrefixOf`) . T.lines)
      (password fromInstance /= password netRc) `shouldBeM` True

    it "authorize no deployer GitHub keys, as the GitHub account of the same login is somebody else" $ do
      deployerGithubKeys (ForgeLogin githubForge "alice") `shouldBeM` Just "alice"
      deployerGithubKeys (ForgeLogin (ForgeSlug "git.example") "alice") `shouldBeM` Nothing

  describe "the webhook route" $ do
    let post' config headers payload =
          withInstance config $ WithServer.withServer $ \server ->
            WithServer.postWithHeaders server ("/api/forges/" <> cs (getForgeSlug $ config ^. slug) <> "/webhook") headers payload
        signed payload = cs (show (hmacHex "webhook-secret" (encode payload)))
        pushPayload :: Value
        pushPayload =
          object
            [ "ref" .= ("refs/heads/main" :: Text),
              "after" .= ("2eba238e33607c1fa49253182e9fff42baafa1eb" :: Text),
              "repository" .= object ["name" .= ("app" :: Text), "private" .= True, "full_name" .= ("acme/app" :: Text), "owner" .= object ["login" .= ("acme" :: Text)]],
              "sender" .= object ["login" .= ("alice" :: Text)]
            ]

    it "accepts a correctly signed delivery" $ do
      -- The bot cannot push to the repository, so the push is not built;
      -- the delivery is still accepted.
      (response, received) <- withFakeGitea (\_ _ _ -> (status200, repoAnswer False)) $ \config ->
        post' config [("X-Gitea-Event", "push"), ("X-Gitea-Signature", signed pushPayload)] pushPayload
      response `WithServer.shouldHaveStatusCode` 200
      map path received `shouldBeM` ["/api/v1/repos/acme/app"]

    it "accepts Forgejo's headers" $ do
      (response, _) <- withFakeGitea (\_ _ _ -> (status200, repoAnswer False)) $ \config ->
        post' config [("X-Forgejo-Event", "push"), ("X-Forgejo-Signature", signed pushPayload)] pushPayload
      response `WithServer.shouldHaveStatusCode` 200

    it "rejects a delivery with a wrong signature" $ do
      (response, received) <- withFakeGitea (\_ _ _ -> (status200, repoAnswer True)) $ \config ->
        post' config [("X-Gitea-Event", "push"), ("X-Gitea-Signature", cs (T.replicate 64 "0"))] pushPayload
      response `WithServer.shouldHaveStatusCode` 401
      length received `shouldBeM` 0

    it "rejects a delivery without a signature" $ do
      (response, received) <- withFakeGitea (\_ _ _ -> (status200, repoAnswer True)) $ \config ->
        post' config [("X-Gitea-Event", "push")] pushPayload
      response `WithServer.shouldHaveStatusCode` 401
      length received `shouldBeM` 0

    it "answers 404 for an unknown forge" $ do
      response <- WithServer.withServer $ \server ->
        WithServer.postWithHeaders server "/api/forges/nowhere/webhook" [("X-Gitea-Event", "push")] pushPayload
      response `WithServer.shouldHaveStatusCode` 404
  where
    hmacHex :: StrictByteString -> LazyByteString -> Digest SHA256
    hmacHex secret payload = hmacGetDigest (hmac secret (BSL.toStrict payload) :: HMAC SHA256)

-- | Private flake inputs on an instance.
privateInputsSpec :: Spec
privateInputsSpec = do
  describe "giteaNetRcEntry" $ do
    it "is for the instance's host, with the token as password" $ do
      giteaNetRcEntry (giteaConfig "https://git.example.com:3000/gitea") (GhToken "bot-token")
        `shouldBe` Just (NetRcEntry "git.example.com" "x-access-token" "bot-token")

  inM $ aroundM_ suppressLogsWhenPassing $ do
    describe "private flake inputs" $ do
      let -- A flake with one non-flake git input on the fake instance, and a
          -- lock file that pins it, so that nix never fetches it.
          writeFlake config input = writeFlakeWith config input []
          -- Extra inputs are github: ones, also pinned.
          writeFlakeWith config input githubInputs =
            writeLockedFlake
              ( gitInput "dep" (config ^. webUrl <> "/" <> input <> ".git")
                  : [githubInput owner repo' | (owner, repo') <- githubInputs]
              )
              "_: { }"
          -- Every repository on the instance but acme/app is private, and
          -- alice is the only collaborator of each.
          routesWith appIsPublic "GET" path' query'
            | "/collaborators" `T.isSuffixOf` path' =
                -- One page, like Gitea: the pages after it are empty.
                (status200, toJSON [object ["login" .= ("alice" :: Text)] | lookup "page" query' `elem` [Nothing, Just "1"]])
            | path' == "/api/v1/repos/acme/app" = (status200, repoAnswer True & key "private" .~ Bool (not appIsPublic))
            | otherwise = (status200, repoAnswer True)
          routesWith _ _ _ _ = notFound
          privateRoutes = routesWith False
          authorizationOf = fmap (\a -> (authorizationNixConfig a, authorizationNetRc a))
          checkWith repoConfig config =
            withInstance config $ withPrivateNixXdgCache $ checkAuthorization (FlakeDir ".") repoConfig (commitInfoOn appRepo Nothing)
          check = checkWith defaultRepoConfig
          messageOf action =
            try action <&> \case
              Left e -> Just $ show (err e)
              Right _ -> Nothing

      it "hands nix the bot token over netrc for an input of the same owner" $ do
        ((nixConfig, netRc), received) <- withFakeGitea privateRoutes $ \config -> do
          writeFlake config "acme/private-lib"
          authorizationOf $ check config
        netRc `shouldBeM` [NetRcEntry "localhost" "x-access-token" "bot-token"]
        getNixConfig nixConfig `shouldBeM` mempty
        sort (nubOrd (map path received))
          `shouldBeM` [ "/api/v1/repos/acme/app",
                        "/api/v1/repos/acme/app/collaborators",
                        "/api/v1/repos/acme/private-lib",
                        "/api/v1/repos/acme/private-lib/collaborators"
                      ]

      describe "github: inputs" $ do
        -- Every test asks about repositories of its own: answers are cached
        -- in the environment the tests share.
        let checkAsking token answer githubInputs config = do
              asked <- liftIO $ newMVar []
              writeFlakeWith config "acme/private-lib" githubInputs
              result <-
                local (#githubInterface %~ githubAnswering asked answer)
                  $ withServerGithubToken token
                  $ messageOf
                  $ check config
              (result,) <$> liftIO (readMVar asked)
            privateMessage input = "github:" <> input <> " is private or doesn't exist. A repository on git.example can only use public GitHub repositories"
            retryMessage input = "Could not tell whether github:" <> input <> " is public"

        it "are accepted when public, asking GitHub anonymously without a server token" $ do
          -- The repository's Gitea credentials mean nothing on GitHub.
          ((message, asked), _) <- withFakeGitea privateRoutes $ checkAsking Nothing (\_ -> pure False) [("NixOS", "public-anonymously")]
          message `shouldBeM` Nothing
          asked `shouldBeM` [(Nothing, "public-anonymously")]

        it "are refused when the server's token sees them as private, asking with that token" $ do
          -- That token is what nix would fetch them with.
          ((message, asked), _) <- withFakeGitea privateRoutes $ checkAsking (Just $ GhToken "server-token") (\_ -> pure True) [("corp", "private-to-the-token")]
          fmap (privateMessage "corp/private-to-the-token" `T.isInfixOf`) message `shouldBeM` Just True
          asked `shouldBeM` [(Just (GhToken "server-token"), "private-to-the-token")]

        it "are refused when GitHub does not show them" $ do
          ((message, _), _) <- withFakeGitea privateRoutes $ checkAsking Nothing (\repo' -> throw $ NoSuchRepo "corp" repo') [("corp", "not-found")]
          fmap (privateMessage "corp/not-found" `T.isInfixOf`) message `shouldBeM` Just True

        it "fail as retryable, not as private, under GitHub's rate limit" $ do
          ((message, _), _) <- withFakeGitea privateRoutes $ checkAsking Nothing (\repo' -> throw $ GarnixAppUnauthorized "NixOS" repo') [("NixOS", "rate-limited")]
          fmap (retryMessage "NixOS/rate-limited" `T.isInfixOf`) message `shouldBeM` Just True
          fmap ("rate limit" `T.isInfixOf`) message `shouldBeM` Just True
          fmap ("retry the build later" `T.isInfixOf`) message `shouldBeM` Just True
          fmap ("is private" `T.isInfixOf`) message `shouldBeM` Just False

        it "fail as retryable, not as private, when GitHub times out" $ do
          ((message, _), _) <- withFakeGitea privateRoutes $ checkAsking Nothing (\_ -> throw GithubRequestTimeout) [("NixOS", "timed-out")]
          fmap (retryMessage "NixOS/timed-out" `T.isInfixOf`) message `shouldBeM` Just True
          fmap ("did not answer in time" `T.isInfixOf`) message `shouldBeM` Just True

        it "are asked about once for a while, but again after GitHub failed to answer" $ do
          (asked, _) <- withFakeGitea privateRoutes $ \config -> do
            (_, failed) <- checkAsking Nothing (\_ -> throw GithubRequestTimeout) [("NixOS", "cached")] config
            (first', answered) <- checkAsking Nothing (\_ -> pure False) [("NixOS", "cached")] config
            (second', cached) <- checkAsking Nothing (\_ -> pure False) [("NixOS", "cached")] config
            pure (length failed, length answered, length cached, first', second')
          asked `shouldBeM` (1, 1, 0, Nothing, Nothing)

        it "tell GitHub's answers apart" $ do
          let answerTo action = _githubVisibility <$> tryEither action
          answerTo (pure False) `shouldReturnM` GithubPublic
          answerTo (pure True) `shouldReturnM` GithubNotVisible
          answerTo (throw $ NoSuchRepo "o" "r") `shouldReturnM` GithubNotVisible
          let unknown visibility = case visibility of
                GithubUnknown _ -> True
                _ -> False
          (unknown <$> answerTo (throw $ GarnixAppUnauthorized "o" "r")) `shouldReturnM` True
          (unknown <$> answerTo (throw GithubRequestTimeout)) `shouldReturnM` True
          (unknown <$> answerTo (throw $ OtherError "500")) `shouldReturnM` True
          (unknown <$> answerTo (liftIO $ ioError $ userError "connection reset")) `shouldReturnM` True

      it "leaves a GitHub repository's inputs as they were, with an instance configured" $ do
        -- An input on the instance's host that names no repository is
        -- refused for a repository on the instance only.
        (result, received) <- withFakeGitea privateRoutes $ \config -> do
          writeLockedFlake [gitInput "dep" (config ^. webUrl <> "/acme/app/../../someone-else/lib.git")] "_: { }"
          let githubRepo = RepoId githubForge "acme" "app"
          local (#githubInterface %~ \gi -> gi {_githubInterfaceGetRepoPublicity = \_ _ -> pure (RepoIsPublic False)})
            $ withInstance config
            $ withPrivateNixXdgCache
            $ authorizationOf
            $ checkAuthorization (FlakeDir ".") defaultRepoConfig (commitInfoOn githubRepo Nothing)
        first getNixConfig result `shouldBeM` (mempty, [])
        length received `shouldBeM` 0

      it "compares a GitHub repository's owners exactly, as before" $ do
        let githubRepo = RepoId githubForge "acme" "app"
            allPrivate gi =
              gi
                { _githubInterfaceGetRepoPublicity = \_ _ -> pure (RepoIsPublic False),
                  _githubInterfaceGetRepoCollaborators = \_ _ -> pure (GhCollaborators ["alice"])
                }
        (message, _) <- withFakeGitea privateRoutes $ \config -> do
          writeLockedFlake [githubInput "ACME" "lib"] "_: { }"
          local (#githubInterface %~ allPrivate)
            $ withInstance config
            $ withPrivateNixXdgCache
            $ messageOf
            $ checkAuthorization (FlakeDir ".") defaultRepoConfig (commitInfoOn githubRepo Nothing)
        fmap ("github:ACME/lib is private or doesn't exist" `T.isInfixOf`) message `shouldBeM` Just True

      it "names the input whose collaborators it cannot read" $ do
        let routes method' path' query'
              | path' == "/api/v1/repos/acme/private-lib/collaborators" = notFound
              | otherwise = privateRoutes method' path' query'
        (message, _) <- withFakeGitea routes $ \config -> do
          writeFlake config "acme/private-lib"
          messageOf $ check config
        fmap ("repo acme/private-lib not found" `T.isInfixOf`) message `shouldBeM` Just True

      it "refuses a private input of another owner, even one the bot can see" $ do
        (message, _) <- withFakeGitea privateRoutes $ \config -> do
          writeFlake config "someone-else/lib"
          messageOf $ check config
        fmap ("git.example:someone-else/lib is private or doesn't exist" `T.isInfixOf`) message `shouldBeM` Just True

      it "refuses a private input of another owner even when collaborator checks are skipped" $ do
        -- On GitHub the installation token only reaches the owner's
        -- repositories; the bot token reaches every repository the bot sees.
        (message, _) <- withFakeGitea (routesWith True) $ \config -> do
          writeFlake config "someone-else/lib"
          messageOf $ checkWith (defaultRepoConfig & skipPrivateInputsCheckForCollaborators .~ True) config
        fmap ("git.example:someone-else/lib is private or doesn't exist" `T.isInfixOf`) message `shouldBeM` Just True

      it "matches owners case-insensitively, as Gitea does" $ do
        ((_, netRc), _) <- withFakeGitea privateRoutes $ \config -> do
          writeFlake config "ACME/private-lib"
          authorizationOf $ check config
        netRc `shouldBeM` [NetRcEntry "localhost" "x-access-token" "bot-token"]

      it "checks an input on the instance's host even when its URL names another port" $ do
        -- The netrc entry is for the host, whatever the port: an input there
        -- is fetched with the bot token, so it is checked like any other.
        (message, _) <- withFakeGitea privateRoutes $ \config -> do
          writeLockedFlake
            [ gitInput "dep" (config ^. webUrl <> "/acme/private-lib.git"),
              gitInput "other" "http://localhost:1/someone-else/lib.git"
            ]
            "_: { }"
          messageOf $ check config
        fmap ("git.example:someone-else/lib is private or doesn't exist" `T.isInfixOf`) message `shouldBeM` Just True

      it "refuses an input on the instance whose path resolves elsewhere" $ do
        (message, _) <- withFakeGitea privateRoutes $ \config -> do
          writeLockedFlake
            [ gitInput "dep" (config ^. webUrl <> "/acme/private-lib.git"),
              gitInput "other" (config ^. webUrl <> "/acme/app/../../someone-else/lib.git")
            ]
            "_: { }"
          messageOf $ check config
        fmap ("flake input disallowed" `T.isInfixOf`) message `shouldBeM` Just True

      it "refuses a private input that fetches its submodules" $ do
        -- They would be fetched with the bot token, from wherever the
        -- input's .gitmodules points.
        (message, _) <- withFakeGitea privateRoutes $ \config -> do
          let (name, ref, node) = gitInput "dep" (config ^. webUrl <> "/acme/private-lib.git")
          writeLockedFlake [(name, ref, node & key "locked" . _Object . at "submodules" ?~ Bool True)] "_: { }"
          messageOf $ check config
        fmap ("fetches its submodules" `T.isInfixOf`) message `shouldBeM` Just True

      it "refuses a private input that fetches Git LFS files" $ do
        -- .lfsconfig can name the LFS endpoint of any repository.
        (message, _) <- withFakeGitea privateRoutes $ \config -> do
          let (name, ref, node) = gitInput "dep" (config ^. webUrl <> "/acme/private-lib.git")
          writeLockedFlake [(name, ref, node & key "locked" . _Object . at "lfs" ?~ Bool True)] "_: { }"
          messageOf $ check config
        fmap ("fetches its Git LFS files" `T.isInfixOf`) message `shouldBeM` Just True

      it "fetches the private inputs before evaluation, which runs without the bot token" $ do
        nonce <- T.filter isAlphaNum <$> randomBase64 12
        let libPath repo' file = "/acme/" <> repo' <> "/raw/branch/main/" <> file
            contents :: Text -> Text
            contents name = "\"" <> name <> " " <> nonce <> "\""
            botAuthorization = "Basic " <> T.decodeUtf8 (Base64.encode "x-access-token:bot-token")
            fileRoutes r
              | path r == libPath "private-lib" "lib.nix" = ifBot r (contents "private")
              | path r == libPath "private-lib" "other.nix" = ifBot r (contents "leaked")
              | path r == libPath "public-lib" "lib.nix" = (status200, [], cs (contents "public"))
              | path r == "/evaluation-starts" = (status200, [], "")
              | method r == "GET" && path r == "/api/v1/repos/acme/public-lib" = json (repoAnswer False & key "private" .~ Bool False)
              | otherwise = json (snd $ privateRoutes (method r) (path r) (query r))
            ifBot r answer
              | authorization r == Just botAuthorization = (status200, [], cs answer)
              | otherwise = (HTTP.status401, [], "")
            json answer = (status200, [("Content-Type", "application/json")], encode answer)
        ((both, leak, netRcDuringEvaluation), received) <- withFakeGiteaRaw (pure . fileRoutes) $ \config -> do
          let urlOf repo' file = config ^. webUrl <> libPath repo' file
          privateHash <- narHashOf (contents "private")
          publicHash <- narHashOf (contents "public")
          leakedHash <- narHashOf (contents "leaked")
          writeLockedFlake
            [ fileInput "priv" (urlOf "private-lib" "lib.nix") privateHash,
              fileInput "pub" (urlOf "public-lib" "lib.nix") publicHash
            ]
            $ "{ priv, pub, ... }: { both = import priv + import pub; leak = import (builtins.fetchTree { type = \"file\"; url = \""
            <> urlOf "private-lib" "other.nix"
            <> "\"; narHash = \""
            <> leakedHash
            <> "\"; }); }"
          withInstance config $ withPrivateNixXdgCache $ do
            withAuthorization (FlakeDir ".") defaultRepoConfig (commitInfoOn appRepo Nothing) $ do
              netRc <- getNetRcFileSetting <$> view #userNixConfig
              netRcContents <- liftIO $ traverse (T.readFile . getNetRcFile) netRc
              void $ liftIO $ Wreq.get (cs $ config ^. webUrl <> "/evaluation-starts")
              both <- evaluate "both"
              leak <- evaluate "leak"
              pure (both, leak, netRcContents)
        both `shouldBeM` Right ("private " <> nonce <> "public " <> nonce)
        isLeft leak `shouldBeM` True
        fmap ("bot-token" `T.isInfixOf`) netRcDuringEvaluation `shouldSatisfyM` (/= Just True)
        let (prefetch, evaluation) = break ((== "/evaluation-starts") . path) received
            fileRequests = map (\r -> (path r, authorization r)) . filter (("/raw/" `T.isInfixOf`) . path)
        -- Only the private input is fetched with the token, before evaluation.
        fileRequests prefetch `shouldBeM` [(libPath "private-lib" "lib.nix", Just botAuthorization)]
        -- Evaluation finds it in the store, fetches the public one without
        -- credentials, and cannot fetch another private file.
        fileRequests evaluation
          `shouldBeM` [ (libPath "public-lib" "lib.nix", Nothing),
                        (libPath "private-lib" "other.nix", Nothing)
                      ]

      it "refuses private inputs of a public repository" $ do
        (message, _) <- withFakeGitea (routesWith True) $ \config -> do
          writeFlake config "acme/private-lib"
          messageOf $ check config
        fmap ("Public repository has private dependencies" `T.isInfixOf`) message `shouldBeM` Just True
  where
    -- Writes a flake whose lock file pins the given inputs, as (name, flake
    -- reference, lock node), with the given outputs function.
    writeLockedFlake :: [(Text, Text, Value)] -> Text -> M ()
    writeLockedFlake inputs outputs = do
      tmp <- view #workingDir
      liftIO $ do
        T.writeFile (tmp </> "flake.nix")
          $ "{ "
          <> T.concat ["inputs." <> name <> " = { url = \"" <> ref <> "\"; flake = false; }; " | (name, ref, _) <- inputs]
          <> "outputs = "
          <> outputs
          <> "; }\n"
        BSL.writeFile (tmp </> "flake.lock")
          $ encode
          $ object
            [ "nodes"
                .= object
                  ( ("root" .= object ["inputs" .= object [fromText name .= name | (name, _, _) <- inputs]])
                      : [fromText name .= node | (name, _, node) <- inputs]
                  ),
              "root" .= ("root" :: Text),
              "version" .= (7 :: Int)
            ]
    gitInput name url =
      ( name,
        "git+" <> url,
        object
          [ "flake" .= False,
            "locked"
              .= object
                [ "lastModified" .= (1759860000 :: Int),
                  "narHash" .= ("sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=" :: Text),
                  "ref" .= ("refs/heads/main" :: Text),
                  "rev" .= ("a98284025e86bd5c37771f1a2e917332b9f4d1d4" :: Text),
                  "revCount" .= (1 :: Int),
                  "type" .= ("git" :: Text),
                  "url" .= url
                ],
            "original" .= object ["type" .= ("git" :: Text), "url" .= url]
          ]
      )
    githubInput owner repo' =
      ( repo',
        "github:" <> owner <> "/" <> repo',
        object
          [ "flake" .= False,
            "locked"
              .= object
                [ "lastModified" .= (1759381078 :: Int),
                  "narHash" .= ("sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=" :: Text),
                  "owner" .= owner,
                  "repo" .= repo',
                  "rev" .= ("7df7ff7d8e00218376575f0acdcc5d66741351ee" :: Text),
                  "type" .= ("github" :: Text)
                ],
            "original" .= object ["owner" .= owner, "repo" .= repo', "type" .= ("github" :: Text)]
          ]
      )
    fileInput name url narHash =
      ( name,
        "file+" <> url,
        object
          [ "flake" .= False,
            "locked" .= object ["narHash" .= narHash, "type" .= ("file" :: Text), "url" .= url],
            "original" .= object ["type" .= ("file" :: Text), "url" .= url]
          ]
      )
    -- The hash nix pins a @file@ input with that content by.
    narHashOf :: Text -> M Text
    narHashOf content = do
      tmp <- view #workingDir
      let file = tmp </> "content-to-hash"
      liftIO $ T.writeFile file content
      StdoutTrimmed hash <-
        liftIO
          $ run
          $ cmd "nix"
          & addArgs ["--extra-experimental-features", "nix-command", "hash", "path", file]
      pure $ cs hash
    -- Evaluates an attribute of the flake in the working directory, as the
    -- build does: in the sandbox, with the nix settings the build has.
    evaluate :: Text -> M (Either Text Text)
    evaluate attribute = do
      cacheDir <- getNixXdgCacheDir
      nixConfig <- view #userNixConfig
      curDir <- view #workingDir
      (exitCode, StdoutTrimmed out, StderrRaw err') <-
        (>>= run)
          $ cmd "nix"
          & addArgs ["eval", "--raw", ".#" <> attribute]
          & nixConfDefaults
          & addNixConfigEnvironment nixConfig
          & setWorkingDir curDir
          & pure
          & inNixSandbox [] (Just cacheDir)
      pure $ case exitCode of
        ExitSuccess -> Right (cs out)
        ExitFailure _ -> Left (cs err')
    -- GitHub, answering only whether a repository is private, with the
    -- given function of its name. Who asked about what is recorded; asked any
    -- other way, it fails the test.
    githubAnswering asked answer gi =
      gi
        { _githubInterfaceGetRepoPrivate = \token repo' -> do
            liftIO $ modifyMVar_ asked (pure . (<> [(token, getGhRepoName $ repo' ^. repoName)]))
            answer (repo' ^. repoName),
          _githubInterfaceGetDefaultBranch = \_ _ -> throw $ OtherError "GitHub was asked for a default branch",
          _githubInterfaceGetRepoPublicity = \_ _ -> throw $ OtherError "GitHub was asked about an input with credentials",
          _githubInterfaceGetRepoCollaborators = \_ _ -> throw $ OtherError "GitHub was asked about an input with credentials"
        }
    -- The token nix holds for github.com, if any.
    withServerGithubToken token =
      local
        $ #userNixConfig
        %~ NixConfig
        . maybe (Map.delete accessTokensSetting) (\(GhToken token') -> Map.insert accessTokensSetting ("github.com=" <> cs token')) token
        . getNixConfig

commitInfoOn :: RepoId -> Maybe PrFromFork -> CommitInfo
commitInfoOn repo' fork =
  CommitInfo
    { _commitInfoReqUser = ForgeLogin (repo' ^. forge) "alice",
      _commitInfoRepoPublicity = RepoIsPublic False,
      _commitInfoRepoInfo = RepoInfo ApiTokenCredentials (GhToken "bot-token") repo',
      _commitInfoBranch = Just "main",
      _commitInfoPrFromFork = fork,
      _commitInfoCommit = "2eba238e33607c1fa49253182e9fff42baafa1eb"
    }
