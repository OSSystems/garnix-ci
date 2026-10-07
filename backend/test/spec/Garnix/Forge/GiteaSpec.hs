module Garnix.Forge.GiteaSpec (spec) where

import Control.Concurrent (modifyMVar, modifyMVar_, newMVar, readMVar)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Lens (key, _String)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Garnix.Forge.Gitea
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers.Monad
import Garnix.Types hiding (body)
import Network.HTTP.Types (hAuthorization, status200, status201, status404)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
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
spec = clientSpec

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
