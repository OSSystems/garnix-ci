module Garnix.Forge.GiteaSpec (spec) where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Cradle
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.Lens (key, values, _Integer, _Object, _String)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isAlphaNum)
import Data.Containers.ListUtils (nubOrd)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.Build.Checkout (withAuthorization)
import Garnix.Build.Helpers (withInternalCacheToken, withPrivateNixXdgCache)
import Garnix.DB qualified as DB
import Garnix.DB.Installations qualified as Installations
import Garnix.Forge.Installation
import Garnix.FlakeInputAuthorization (GithubVisibility (..), InputAuthorization (..), checkAuthorization, _githubVisibility)
import Garnix.Forge.Gitea
import Garnix.Hosting.Deploy (deployerGithubKeys)
import Garnix.Monad
import Garnix.NixConfig (NetRcEntry (..), addNixConfigEnvironment, getNetRcFileSetting, nixConfDefaults)
import Garnix.Prelude
import Garnix.Sandbox (inNixSandbox)
import Garnix.TestHelpers (truncateDBM)
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
        if lookup dropConnection headers == Just "1"
          then -- Closes the connection without answering.
            respond $ Wai.responseRaw (\_ _ -> pure ()) (Wai.responseLBS status' [] "")
          else respond $ Wai.responseLBS status' headers answer
  result <- liftBaseOp (testWithApplication (pure app)) $ \port ->
    action (giteaConfig $ "http://localhost:" <> show port)
  (result,) <$> liftIO (readMVar received)

-- | A header that makes 'withFakeGiteaRaw' close the connection instead of
-- answering.
dropConnection :: HTTP.HeaderName
dropConnection = "X-Fake-Drop-Connection"

giteaConfig :: Text -> ForgeConfig
giteaConfig webUrl' =
  ForgeConfig
    { _forgeConfigSlug = ForgeSlug "git.example",
      _forgeConfigKind = GiteaForgeKind,
      _forgeConfigWebUrl = webUrl',
      _forgeConfigApiUrl = webUrl' <> "/api/v1",
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
  installationsSpec

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
        (publicity, received) <- withFakeGiteaRaw flaky $ \config ->
          withInstance config $ getRepoPublicity ApiTokenCredentials appRepo
        publicity `shouldBeM` RepoIsPublic False
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
            (status200, object ["login" .= ("alice" :: Text), "email" .= ("alice@example.com" :: Text), "is_admin" .= True])
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
        user `shouldBeM` ("alice", Email "alice@example.com", True)
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

-- | What goes wrong on the instance of 'installationsSpec'.
data Trouble
  = -- | It no longer accepts the login's token, and refuses to renew it.
    Revoked Text
  | -- | Its token endpoint is down: it renews no token, and refuses none.
    TokenEndpointDown
  | -- | The login left the organisation @acme@.
    LeftAcme Text
  | -- | Creating a webhook breaks the connection.
    HookCreationBreaks
  deriving stock (Eq)

-- | The instance of 'installationsSpec': @alice@ owns the organisation
-- @acme@ (with the repositories @app@ and @lib@) and three of their own
-- (@one@, @two@, @three@), @bob@ is only a member of @acme@, and @root@
-- administers the instance and owns @acme@ too. Each logs in with the code @<login>-code@, and calls
-- the instance with the token @<login>-token@, renewed with
-- @<login>-refresh@, unless something goes wrong (the first 'MVar').
-- Webhooks get ids counting from 100 (the second).
installationRoutes :: MVar [Trouble] -> MVar Integer -> Received -> IO (HTTP.Status, HTTP.ResponseHeaders, LazyByteString)
installationRoutes troubles hookIds r = do
  troubles' <- readMVar troubles
  let revoked' = [who | Revoked who <- troubles']
      inAcme who = who `elem` ["alice", "bob", "root"] && LeftAcme who `notElem` troubles'
      json status' value = pure (status', [("Content-Type", "application/json")], encode value)
      caller = authorization r >>= T.stripPrefix "token " >>= T.stripSuffix "-token"
      live = caller >>= \who -> who <$ guard (who `notElem` revoked')
      form = HTTP.parseSimpleQuery (BSL.toStrict (body r))
      tokens who = object ["access_token" .= (who <> "-token"), "refresh_token" .= (who <> "-refresh"), "expires_in" .= (3600 :: Int), "token_type" .= ("bearer" :: Text)]
      firstPage items = if lookup "page" (query r) `elem` [Nothing, Just "1"] then items else []
      -- A page of a list, as asked, with the length of the whole list.
      paged items =
        let page = fromMaybe 1 $ lookup "page" (query r) >>= readMaybe . cs
            limit = fromMaybe 50 $ lookup "limit" (query r) >>= readMaybe . cs
         in pure (status200, [("Content-Type", "application/json"), ("X-Total-Count", cs (show (length items) :: Text))], encode $ take limit $ drop ((page - 1) * limit) items)
      repoOf :: Text -> Text -> Value
      repoOf owner name' = object ["name" .= name', "full_name" .= (owner <> "/" <> name'), "private" .= True, "owner" .= object ["login" .= owner], "permissions" .= object ["push" .= True]]
      repo = repoOf "acme"
  case (method r, T.splitOn "/" (T.drop 1 (path r))) of
    ("POST", ["login", "oauth", "access_token"])
      | TokenEndpointDown `elem` troubles' -> json HTTP.status502 (object [])
      | otherwise ->
      case (lookup "grant_type" form, T.decodeUtf8 <$> lookup "code" form, T.decodeUtf8 <$> lookup "refresh_token" form) of
        (Just "authorization_code", Just code, _) | Just who <- T.stripSuffix "-code" code -> json status200 (tokens who)
        (Just "refresh_token", _, Just refresh) | Just who <- T.stripSuffix "-refresh" refresh, who `notElem` revoked' -> json status200 (tokens who)
        _ -> json HTTP.status400 (object ["error" .= ("invalid_grant" :: Text)])
    (_, "api" : "v1" : rest) -> case live of
      Nothing -> json HTTP.status401 (object ["message" .= ("token is required" :: Text)])
      Just who -> case (method r, rest) of
        ("GET", ["user"]) -> json status200 $ object ["login" .= who, "email" .= (who <> "@example.com"), "is_admin" .= (who == "root")]
        ("GET", ["user", "orgs"]) -> json status200 $ toJSON (firstPage [object ["name" .= ("acme" :: Text)] | inAcme who])
        -- Gitea compares names case-insensitively.
        ("GET", ["users", user, "orgs", org, "permissions"]) | T.toLower org == "acme" -> json status200 $ object ["is_owner" .= (user `elem` ["alice", "root"] && user == who && inAcme who)]
        ("POST", hooks)
          | lastMay hooks == Just "hooks" && HookCreationBreaks `elem` troubles' -> pure (status201, [(dropConnection, "1")], "")
          | lastMay hooks == Just "hooks" -> do
              hookId <- modifyMVar hookIds $ \n -> pure (n + 1, n)
              json status201 $ object ["id" .= hookId]
        ("DELETE", _) -> pure (HTTP.status204, [], "")
        ("GET", ["repos", owner, name'])
          | T.toLower owner == "acme" && T.toLower name' == "gone" -> json status404 (object [])
          -- The bot token sees every repository.
          | T.toLower owner == "acme" && LeftAcme who `notElem` troubles' -> json status200 $ repo (T.toLower name')
          | T.toLower owner == "acme" -> json status404 (object [])
        ("GET", ["orgs", "acme", "repos"]) | LeftAcme who `notElem` troubles' -> paged [repo "app", repo "lib"]
        ("GET", ["users", owner, "repos"]) | owner == "alice" -> paged [repoOf "alice" "one", repoOf "alice" "two", repoOf "alice" "three"]
        -- What the user owns, and what they reach through @acme@ too.
        ("GET", ["user", "repos"]) | who == "alice" -> json status200 $ toJSON (firstPage [repo "app", repo "lib", repoOf "alice" "one", repoOf "alice" "two", repoOf "alice" "three"])
        _ -> pure (status404, [], "{}")
    _ -> pure (status404, [], "{}")
  where
    lastMay = \case
      [] -> Nothing
      xs -> Just (last xs)

-- | Installing garnix on the instance, and building through installations.
installationsSpec :: Spec
installationsSpec = inM $ beforeM_ truncateDBM $ aroundM_ suppressLogsWhenPassing $ do
  let -- The instance, with no bot token unless a spec gives it one, and a
      -- server talking to it; the action gets what the instance received so
      -- far, and the 'MVar' of what goes wrong on it.
      withGitea :: (ForgeConfig -> ForgeConfig) -> (WithServer.TestServer -> M [Received] -> MVar [Trouble] -> M a) -> M a
      withGitea configure action = do
        revoked <- liftIO $ newMVar []
        hookIds <- liftIO $ newMVar 100
        seen <- liftIO $ newMVar []
        let routes r = modifyMVar_ seen (pure . (<> [r])) >> installationRoutes revoked hookIds r
        fmap fst $ withFakeGiteaRaw routes $ \config -> do
          let config' = configure (config & apiToken .~ Nothing)
          withInstance config' $ WithServer.withServer $ \server -> action server (liftIO $ readMVar seen) revoked
      -- Logs @who@ in through the instance, as a browser does.
      logIn :: WithServer.TestServer -> Text -> M ()
      logIn server who = do
        started <- WithServer.get server "/api/auth/git.example/login"
        let state = T.drop (T.length "&state=") $ snd $ T.breakOn "&state=" $ fromMaybe "" $ started ^? Wreq.responseBody . key "github" . _String
        response <- WithServer.get server $ "/api/auth/git.example/login/cb?code=" <> cs who <> "-code&state=" <> cs state
        response `WithServer.shouldHaveStatusCode` 200
      install :: WithServer.TestServer -> Text -> Text -> M (Wreq.Response LazyByteString)
      install server kind' target = WithServer.post server "/api/forges/git.example/installations" (object ["kind" .= kind', "target" .= target])
      installed :: WithServer.TestServer -> Text -> Text -> M Integer
      installed server kind' target = do
        response <- install server kind' target
        response `WithServer.shouldHaveStatusCode` 200
        maybe (error "no installation id") pure $ response ^? Wreq.responseBody . key "id" . _Integer
      installationPath :: Integer -> String
      installationPath installation = "/api/forges/git.example/installations/" <> cs (show installation)
      -- The hook creations the instance received, as (path, token, config).
      hooksCreated :: [Received] -> [(Text, Maybe Text, Value)]
      hooksCreated received =
        [ (path r, authorization r, fromMaybe Null $ decode @Value (body r) >>= (^? key "config"))
        | r <- received,
          method r == "POST",
          "/hooks" `T.isSuffixOf` path r
        ]
      secretOf :: Value -> StrictByteString
      secretOf hookConfig = T.encodeUtf8 $ fromMaybe "" $ hookConfig ^? key "secret" . _String
      pushOf :: Text -> Value
      pushOf name' =
        object
          [ "ref" .= ("refs/heads/main" :: Text),
            "after" .= ("2eba238e33607c1fa49253182e9fff42baafa1eb" :: Text),
            "repository" .= object ["name" .= name', "private" .= True, "full_name" .= ("acme/" <> name'), "owner" .= object ["login" .= ("acme" :: Text)]],
            "sender" .= object ["login" .= ("alice" :: Text)]
          ]
      deliver :: WithServer.TestServer -> Integer -> StrictByteString -> Value -> M (Wreq.Response LazyByteString)
      deliver server installation secret payload =
        WithServer.postWithHeaders
          server
          (installationPath installation <> "/webhook")
          [("X-Gitea-Event", "push"), ("X-Gitea-Signature", cs (show (hmacGetDigest (hmac secret (BSL.toStrict $ encode payload) :: HMAC SHA256))))]
          payload
      -- A repository garnix may not build: a delivery that reaches the
      -- build is refused there (403), before anything runs, while one that
      -- garnix ignores is answered 200.
      denylist :: Text -> M ()
      denylist name' = void $ DB.pgExec [pgSQL| INSERT INTO denylist (forge, repo_user, repo_name) VALUES ('git.example', 'acme', ${name'}) |]
      resolvedWith :: [Received] -> [Maybe Text]
      resolvedWith received = [authorization r | r <- received, method r == "GET", path r == "/api/v1/repos/acme/app"]
      statusOf' :: Integer -> M (Maybe ForgeInstallationStatus)
      statusOf' installation = fmap forgeInstallationStatus <$> Installations.getForgeInstallation (ForgeInstallationId $ fromInteger installation)

  describe "installing garnix" $ do
    it "installs on the caller's account with a user webhook, signed with a secret of its own" $ withGitea identity $ \server received _ -> do
      logIn server "alice"
      installation <- installed server "user" "alice"
      [(hookPath, token, hookConfig)] <- hooksCreated <$> received
      hookPath `shouldBeM` "/api/v1/user/hooks"
      token `shouldBeM` Just "token alice-token"
      (hookConfig ^? key "url" . _String) `shouldBeM` Just ("https://garnix.io/api/forges/git.example/installations/" <> show installation <> "/webhook")
      (hookConfig ^? key "content_type" . _String) `shouldBeM` Just "json"
      (T.length <$> hookConfig ^? key "secret" . _String) `shouldSatisfyM` maybe False (>= 32)
      listed <- WithServer.get server "/api/forges/git.example/installations"
      (listed ^.. Wreq.responseBody . values . key "status" . _String) `shouldBeM` ["active"]
      (listed ^.. Wreq.responseBody . values . key "installer" . _String) `shouldBeM` ["alice"]

    it "installs on an organisation for its owner, and refuses a member who does not own it" $ withGitea identity $ \server received _ -> do
      logIn server "bob"
      refused <- install server "org" "acme"
      refused `WithServer.shouldHaveStatusCode` 403
      (hooksCreated <$> received) `shouldReturnM` []
      logIn server "alice"
      _ <- installed server "org" "acme"
      (map (\(p, t, _) -> (p, t)) . hooksCreated <$> received) `shouldReturnM` [("/api/v1/orgs/acme/hooks", Just "token alice-token")]

    it "installs on the whole instance only for a site administrator, with a system webhook" $ withGitea identity $ \server received _ -> do
      logIn server "alice"
      refused <- install server "instance" ""
      refused `WithServer.shouldHaveStatusCode` 403
      logIn server "root"
      _ <- installed server "instance" ""
      [(hookPath, _, hookConfig)] <- hooksCreated <$> received
      hookPath `shouldBeM` "/api/v1/admin/hooks"
      (hookConfig ^? key "is_system_webhook" . _String) `shouldBeM` Just "true"

    it "gives every installation a secret of its own, and refuses a second one on the same target" $ withGitea identity $ \server received _ -> do
      logIn server "root"
      _ <- installed server "org" "acme"
      _ <- installed server "instance" ""
      again <- install server "org" "ACME"
      again `WithServer.shouldHaveStatusCode` 409
      [(_, _, first'), (_, _, second')] <- hooksCreated <$> received
      (secretOf first' /= secretOf second') `shouldBeM` True

    it "lists what the caller may install on" $ withGitea identity $ \server _ _ -> do
      let targetsOf response = zip (response ^.. Wreq.responseBody . values . key "kind" . _String) (response ^.. Wreq.responseBody . values . key "target" . _String)
      logIn server "bob"
      (targetsOf <$> WithServer.get server "/api/forges/git.example/installable") `shouldReturnM` [("user", "bob")]
      logIn server "root"
      (targetsOf <$> WithServer.get server "/api/forges/git.example/installable") `shouldReturnM` [("user", "root"), ("org", "acme"), ("instance", "")]

    it "enables nothing until the caller enables each repository, and a repository in one installation only" $ withGitea identity $ \server _ _ -> do
      logIn server "root"
      org <- installed server "org" "acme"
      instance' <- installed server "instance" ""
      repos <- WithServer.get server (installationPath org <> "/repos")
      (repos ^.. Wreq.responseBody . key "repos" . values . key "name" . _String) `shouldBeM` ["app", "lib"]
      (repos ^.. Wreq.responseBody . key "repos" . values . key "enabledIn") `shouldBeM` [Null, Null]
      enabled <- WithServer.put server (installationPath org <> "/repos/acme/app") Null
      enabled `WithServer.shouldHaveStatusCode` 200
      twice <- WithServer.put server (installationPath instance' <> "/repos/acme/app") Null
      twice `WithServer.shouldHaveStatusCode` 409
      (("installation " <> show org) `T.isInfixOf` cs (twice ^. Wreq.responseBody)) `shouldBeM` True
      repos' <- WithServer.get server (installationPath org <> "/repos")
      (repos' ^.. Wreq.responseBody . key "repos" . values . key "enabledIn") `shouldBeM` [toJSON org, Null]

    it "uninstalls: deletes the webhook and the installation, and keeps the builds" $ withGitea identity $ \server received _ -> do
      logIn server "alice"
      installation <- installed server "org" "acme"
      _ <- WithServer.put server (installationPath installation <> "/repos/acme/app") Null
      -- Pushes outlive the database resets between specs: a commit of its
      -- own.
      commit' <- CommitHash <$> randomBase64 20
      DB.registerPush appRepo commit' "main" `shouldReturnM` DB.NewPush
      bob <- WithServer.withServer $ \other -> logIn other "bob" >> WithServer.delete other (installationPath installation)
      bob `WithServer.shouldHaveStatusCode` 403
      uninstalled <- WithServer.delete server (installationPath installation)
      uninstalled `WithServer.shouldHaveStatusCode` 200
      deletes <- map path . filter ((== "DELETE") . method) <$> received
      deletes `shouldBeM` ["/api/v1/orgs/acme/hooks/100"]
      statusOf' installation `shouldReturnM` Nothing
      Installations.enabledInstallation appRepo >>= (`shouldBeM` True) . isNothing
      DB.registerPush appRepo commit' "main" `shouldReturnM` DB.AlreadyPushed

    it "lists a repository enabled under another case as enabled" $ withGitea identity $ \server _ _ -> do
      logIn server "alice"
      org <- installed server "org" "acme"
      enabled <- WithServer.put server (installationPath org <> "/repos/ACME/App") Null
      enabled `WithServer.shouldHaveStatusCode` 200
      repos <- WithServer.get server (installationPath org <> "/repos")
      (repos ^.. Wreq.responseBody . key "repos" . values . key "enabledIn") `shouldBeM` [toJSON org, Null]

    it "leaves no installation behind when creating its webhook breaks the connection" $ withGitea identity $ \server _ troubles -> do
      logIn server "alice"
      liftIO $ modifyMVar_ troubles (pure . (HookCreationBreaks :))
      broken <- install server "org" "acme"
      (broken ^. Wreq.responseStatus . Wreq.statusCode >= 500) `shouldBeM` True
      Installations.listForgeInstallations (ForgeSlug "git.example") >>= (`shouldBeM` 0) . length
      liftIO $ modifyMVar_ troubles (const $ pure [])
      void $ installed server "org" "acme"

    it "answers which installation holds a repository, or enables it, even when that changes meanwhile" $ withGitea identity $ \server _ _ -> do
      logIn server "root"
      org <- ForgeInstallationId . fromInteger <$> installed server "org" "acme"
      instance' <- ForgeInstallationId . fromInteger <$> installed server "instance" ""
      Installations.enableRepo org appRepo `shouldReturnM` Right ()
      -- The organisation's installation disables it right after the
      -- instance's tried to enable it: a trigger stands in for that
      -- concurrent request.
      void
        $ DB.pgExec
          [pgSQL|
            CREATE FUNCTION spec_disable_in_org() RETURNS trigger AS $$
            BEGIN
              DELETE FROM forge_installation_repos
              WHERE installation_id IN (SELECT id FROM forge_installations WHERE target_kind = 'org');
              RETURN NULL;
            END;
            $$ LANGUAGE plpgsql
          |]
      void $ DB.pgExec [pgSQL| CREATE TRIGGER spec_disable_in_org AFTER INSERT ON forge_installation_repos FOR EACH STATEMENT EXECUTE FUNCTION spec_disable_in_org() |]
      result <- tryEither $ Installations.enableRepo instance' appRepo
      void $ DB.pgExec [pgSQL| DROP TRIGGER spec_disable_in_org ON forge_installation_repos |]
      void $ DB.pgExec [pgSQL| DROP FUNCTION spec_disable_in_org() |]
      holder <- fmap forgeInstallationId <$> Installations.enabledInstallation appRepo
      -- Either it was still the organisation's, and says so, or it is now
      -- the instance's: never enabled nowhere while answering it is.
      case result of
        Right (Left other) -> other `shouldBeM` org
        Right (Right ()) -> holder `shouldBeM` Just instance'
        Left e -> liftIO $ expectationFailure $ cs (show e :: Text)

    it "keeps a repository in an installation that needs a reinstall, and says so to one enabling it elsewhere" $ withGitea identity $ \server _ _ -> do
      logIn server "alice"
      org <- installed server "org" "acme"
      _ <- WithServer.put server (installationPath org <> "/repos/acme/app") Null
      -- alice is gone: the organisation's installation needs a reinstall.
      void $ DB.pgExec [pgSQL| DELETE FROM users WHERE id IN (SELECT user_id FROM forge_identities WHERE forge = 'git.example' AND login = 'alice') |]
      WithServer.withServer $ \other -> do
        logIn other "root"
        instance' <- installed other "instance" ""
        refused <- WithServer.put other (installationPath instance' <> "/repos/acme/app") Null
        refused `WithServer.shouldHaveStatusCode` 409
        (("is already enabled in the installation " <> show org <> " (on the organisation acme), which needs a reinstall") `T.isInfixOf` cs (refused ^. Wreq.responseBody))
          `shouldBeM` True
        holder <- fmap forgeInstallationId <$> Installations.enabledInstallation appRepo
        holder `shouldBeM` Just (ForgeInstallationId $ fromInteger org)

    it "lists one page of the repositories at a time, as asked" $ withGitea identity $ \server received _ -> do
      logIn server "root"
      org <- installed server "org" "acme"
      seen <- length <$> received
      page2 <- WithServer.get server (installationPath org <> "/repos?page=2&limit=10")
      page2 `WithServer.shouldHaveStatusCode` 200
      (page2 ^.. Wreq.responseBody . key "repos" . values . key "name" . _String) `shouldBeM` []
      (page2 ^? Wreq.responseBody . key "total" . _Integer) `shouldBeM` Just 2
      listed <- (\rs -> [sort (query r) | r <- drop seen rs, path r == "/api/v1/orgs/acme/repos"]) <$> received
      listed `shouldBeM` [[("limit", "10"), ("page", "2")]]

    it "lists one page of a user's own repositories, and how many they own" $ withGitea identity $ \server received _ -> do
      logIn server "alice"
      user <- installed server "user" "alice"
      page2 <- WithServer.get server (installationPath user <> "/repos?page=2&limit=2")
      page2 `WithServer.shouldHaveStatusCode` 200
      (page2 ^.. Wreq.responseBody . key "repos" . values . key "name" . _String) `shouldBeM` ["three"]
      (page2 ^? Wreq.responseBody . key "total" . _Integer) `shouldBeM` Just 3
      (filter (== "/api/v1/user/repos") . map path <$> received) `shouldReturnM` []

    it "reinstalls only the installation as it was read" $ withGitea identity $ \server _ _ -> do
      logIn server "alice"
      installation <- ForgeInstallationId . fromInteger <$> installed server "org" "acme"
      read' <- Installations.getForgeInstallation installation >>= maybe (error "not installed") pure
      Installations.reinstallForgeInstallation read' "root" 200 (EncryptedText "") `shouldReturnM` True
      -- A second reinstall over the same read lost to the first.
      Installations.reinstallForgeInstallation read' "bob" 300 (EncryptedText "") `shouldReturnM` False
      (fmap forgeInstallationInstaller <$> Installations.getForgeInstallation installation) `shouldReturnM` Just (Just "root")

    it "deletes the webhook a reinstall created when the reinstall then fails" $ withGitea identity $ \server received _ -> do
      logIn server "alice"
      installation <- installed server "org" "acme"
      void
        $ DB.pgExec
          [pgSQL|
            CREATE FUNCTION spec_fail_update() RETURNS trigger AS $$
            BEGIN
              RAISE EXCEPTION 'spec: the update fails';
            END;
            $$ LANGUAGE plpgsql
          |]
      void $ DB.pgExec [pgSQL| CREATE TRIGGER spec_fail_update BEFORE UPDATE ON forge_installations FOR EACH ROW EXECUTE FUNCTION spec_fail_update() |]
      failed <- WithServer.post server (installationPath installation <> "/reinstall") Null
      void $ DB.pgExec [pgSQL| DROP TRIGGER spec_fail_update ON forge_installations |]
      void $ DB.pgExec [pgSQL| DROP FUNCTION spec_fail_update() |]
      (failed ^. Wreq.responseStatus . Wreq.statusCode >= 500) `shouldBeM` True
      deletes <- map path . filter ((== "DELETE") . method) <$> received
      deletes `shouldBeM` ["/api/v1/orgs/acme/hooks/101"]

    it "deletes the webhook a reinstall created when another reinstall won meanwhile" $ withGitea identity $ \server received _ -> do
      logIn server "alice"
      installation <- installed server "org" "acme"
      -- The update finds no row as it was read: another reinstall changed it.
      void
        $ DB.pgExec
          [pgSQL|
            CREATE FUNCTION spec_skip_update() RETURNS trigger AS $$
            BEGIN
              RETURN NULL;
            END;
            $$ LANGUAGE plpgsql
          |]
      void $ DB.pgExec [pgSQL| CREATE TRIGGER spec_skip_update BEFORE UPDATE ON forge_installations FOR EACH ROW EXECUTE FUNCTION spec_skip_update() |]
      lost <- WithServer.post server (installationPath installation <> "/reinstall") Null
      void $ DB.pgExec [pgSQL| DROP TRIGGER spec_skip_update ON forge_installations |]
      void $ DB.pgExec [pgSQL| DROP FUNCTION spec_skip_update() |]
      lost `WithServer.shouldHaveStatusCode` 409
      deletes <- map path . filter ((== "DELETE") . method) <$> received
      deletes `shouldBeM` ["/api/v1/orgs/acme/hooks/101"]
      (fmap forgeInstallationHookId <$> Installations.getForgeInstallation (ForgeInstallationId $ fromInteger installation)) `shouldReturnM` Just (Just 100)

    it "keeps a repository's forge that of its installation" $ withGitea identity $ \server _ _ -> do
      logIn server "alice"
      installation <- ForgeInstallationId . fromInteger <$> installed server "org" "acme"
      refused <- tryEither $ DB.pgExec [pgSQL| INSERT INTO forge_installation_repos (installation_id, forge, repo_owner, repo_name) VALUES (${installation}, 'elsewhere', 'acme', 'app') |]
      isLeft refused `shouldBeM` True

  describe "deliveries" $ do
    it "builds a signed delivery for an enabled repository, with the installer's token" $ withGitea identity $ \server received _ -> do
      logIn server "alice"
      installation <- installed server "org" "acme"
      _ <- WithServer.put server (installationPath installation <> "/repos/acme/app") Null
      [(_, _, hookConfig)] <- hooksCreated <$> received
      denylist "app"
      seen <- length <$> received
      delivered <- deliver server installation (secretOf hookConfig) (pushOf "app")
      delivered `WithServer.shouldHaveStatusCode` 403
      (resolvedWith . drop seen <$> received) `shouldReturnM` [Just "token alice-token"]

    it "ignores a delivery for a repository that is not enabled" $ withGitea identity $ \server received _ -> do
      logIn server "alice"
      installation <- installed server "org" "acme"
      [(_, _, hookConfig)] <- hooksCreated <$> received
      denylist "app"
      delivered <- deliver server installation (secretOf hookConfig) (pushOf "app")
      delivered `WithServer.shouldHaveStatusCode` 200
      (resolvedWith <$> received) `shouldReturnM` []

    it "ignores a delivery through an installation the repository is not enabled in" $ withGitea identity $ \server received _ -> do
      logIn server "root"
      org <- installed server "org" "acme"
      instance' <- installed server "instance" ""
      _ <- WithServer.put server (installationPath org <> "/repos/acme/app") Null
      [_, (_, _, instanceHook)] <- hooksCreated <$> received
      denylist "app"
      seen <- length <$> received
      delivered <- deliver server instance' (secretOf instanceHook) (pushOf "app")
      delivered `WithServer.shouldHaveStatusCode` 200
      (resolvedWith . drop seen <$> received) `shouldReturnM` []

    it "refuses a delivery with a wrong signature, or signed with another installation's secret" $ withGitea identity $ \server received _ -> do
      logIn server "root"
      org <- installed server "org" "acme"
      _ <- installed server "instance" ""
      _ <- WithServer.put server (installationPath org <> "/repos/acme/app") Null
      [_, (_, _, instanceHook)] <- hooksCreated <$> received
      seen <- length <$> received
      wrong <- deliver server org "not-the-secret" (pushOf "app")
      wrong `WithServer.shouldHaveStatusCode` 401
      other <- deliver server org (secretOf instanceHook) (pushOf "app")
      other `WithServer.shouldHaveStatusCode` 401
      unsigned <- WithServer.postWithHeaders server (installationPath org <> "/webhook") [("X-Gitea-Event", "push")] (pushOf "app")
      unsigned `WithServer.shouldHaveStatusCode` 401
      unknown <- deliver server 999 "secret" (pushOf "app")
      unknown `WithServer.shouldHaveStatusCode` 404
      (resolvedWith . drop seen <$> received) `shouldReturnM` []

    it "accepts Forgejo's headers" $ withGitea identity $ \server received _ -> do
      logIn server "alice"
      installation <- installed server "org" "acme"
      [(_, _, hookConfig)] <- hooksCreated <$> received
      let payload = pushOf "app"
      delivered <-
        WithServer.postWithHeaders
          server
          (installationPath installation <> "/webhook")
          [("X-Forgejo-Event", "push"), ("X-Forgejo-Signature", cs (show (hmacGetDigest (hmac (secretOf hookConfig) (BSL.toStrict $ encode payload) :: HMAC SHA256))))]
          payload
      delivered `WithServer.shouldHaveStatusCode` 200

  describe "the installer's token" $ do
    it "stops building once the instance no longer accepts it, until someone reinstalls" $ withGitea identity $ \server received revoked -> do
      logIn server "alice"
      installation <- installed server "org" "acme"
      _ <- WithServer.put server (installationPath installation <> "/repos/acme/app") Null
      [(_, _, hookConfig)] <- hooksCreated <$> received
      denylist "app"
      liftIO $ modifyMVar_ revoked (pure . (Revoked "alice" :))
      seen <- length <$> received
      delivered <- deliver server installation (secretOf hookConfig) (pushOf "app")
      delivered `WithServer.shouldHaveStatusCode` 200
      status' <- statusOf' installation
      status' `shouldSatisfyM` \case
        Just (NeedsReinstall reason) -> "alice" `T.isInfixOf` reason
        _ -> False
      -- It asked once, with the revoked token, and renewing it failed.
      (resolvedWith . drop seen <$> received) `shouldReturnM` [Just "token alice-token"]
      listed <- WithServer.withServer $ \other -> logIn other "root" >> WithServer.get other "/api/forges/git.example/installations"
      (listed ^.. Wreq.responseBody . values . key "status" . _String) `shouldBeM` ["needs_reinstall"]
      -- Stopped: the next delivery does not even ask the instance.
      before' <- length . resolvedWith <$> received
      again <- deliver server installation (secretOf hookConfig) (pushOf "app")
      again `WithServer.shouldHaveStatusCode` 200
      (length . resolvedWith <$> received) `shouldReturnM` before'
      -- Another owner of the organisation reinstalls it, and becomes its
      -- installer, with a new webhook and secret.
      WithServer.withServer $ \other -> do
        logIn other "root"
        reinstalled <- WithServer.post other (installationPath installation <> "/reinstall") Null
        reinstalled `WithServer.shouldHaveStatusCode` 200
        (reinstalled ^? Wreq.responseBody . key "status" . _String) `shouldBeM` Just "active"
        (reinstalled ^? Wreq.responseBody . key "installer" . _String) `shouldBeM` Just "root"
      hooks <- hooksCreated <$> received
      [(_, token, newHook)] <- pure (drop 1 hooks)
      token `shouldBeM` Just "token root-token"
      deletes <- map path . filter ((== "DELETE") . method) <$> received
      deletes `shouldBeM` ["/api/v1/orgs/acme/hooks/100"]
      stale <- deliver server installation (secretOf hookConfig) (pushOf "app")
      stale `WithServer.shouldHaveStatusCode` 401
      rebuilt <- deliver server installation (secretOf newHook) (pushOf "app")
      rebuilt `WithServer.shouldHaveStatusCode` 403
      (lastMay' . resolvedWith <$> received) `shouldReturnM` Just (Just "token root-token")

    it "keeps building through an outage of the token endpoint, which refuses nothing" $ withGitea identity $ \server _ troubles -> do
      logIn server "alice"
      installation <- installed server "org" "acme"
      _ <- WithServer.put server (installationPath installation <> "/repos/acme/app") Null
      -- The token ran out, and the endpoint that renews it is down.
      liftIO $ modifyMVar_ troubles (pure . ([Revoked "alice", TokenEndpointDown] <>))
      failed <- tryEither $ resolveRepo appRepo
      isLeft failed `shouldBeM` True
      statusOf' installation `shouldReturnM` Just InstallationActive
      liftIO $ modifyMVar_ troubles (const $ pure [])
      resolved <- resolveRepo appRepo
      fmap (view ghToken) resolved `shouldBeM` Just (GhToken "alice-token")

    it "stops building once the installer may no longer install there, as after leaving the organisation" $ withGitea identity $ \server _ troubles -> do
      logIn server "alice"
      installation <- installed server "org" "acme"
      _ <- WithServer.put server (installationPath installation <> "/repos/acme/app") Null
      -- A repository an owner cannot see is gone, not the installation.
      _ <- Installations.enableRepo (ForgeInstallationId $ fromInteger installation) (RepoId (ForgeSlug "git.example") "acme" "gone")
      resolveRepo (RepoId (ForgeSlug "git.example") "acme" "gone") >>= (`shouldBeM` True) . isNothing
      statusOf' installation `shouldReturnM` Just InstallationActive
      liftIO $ modifyMVar_ troubles (pure . (LeftAcme "alice" :))
      resolveRepo appRepo >>= (`shouldBeM` True) . isNothing
      status' <- statusOf' installation
      status' `shouldSatisfyM` \case
        Just (NeedsReinstall reason) -> "alice" `T.isInfixOf` reason
        _ -> False
      listed <- WithServer.withServer $ \other -> logIn other "root" >> WithServer.get other "/api/forges/git.example/installations"
      (listed ^.. Wreq.responseBody . values . key "status" . _String) `shouldBeM` ["needs_reinstall"]

    it "leaves a reinstalled installation alone when the token of whoever installed it before fails late" $ withGitea identity $ \server _ _ -> do
      logIn server "alice"
      installation <- installed server "org" "acme"
      WithServer.withServer $ \other -> do
        logIn other "root"
        reinstalled <- WithServer.post other (installationPath installation <> "/reinstall") Null
        reinstalled `WithServer.shouldHaveStatusCode` 200
      -- A build that still held alice's token fails only now.
      Installations.markNeedsReinstall (ForgeInstallationId $ fromInteger installation) "alice" "the instance no longer accepts the token of alice"
      statusOf' installation `shouldReturnM` Just InstallationActive

    it "stops building once the installer's identity is gone" $ withGitea identity $ \server _ _ -> do
      logIn server "alice"
      installation <- installed server "org" "acme"
      void $ DB.pgExec [pgSQL| DELETE FROM users WHERE id IN (SELECT user_id FROM forge_identities WHERE forge = 'git.example' AND login = 'alice') |]
      status' <- statusOf' installation
      status' `shouldSatisfyM` \case
        Just (NeedsReinstall _) -> True
        _ -> False
      resolved <- resolveRepo appRepo
      isNothing resolved `shouldBeM` True

    it "gives way to the instance's bot token when it has one" $ withGitea (apiToken ?~ GhToken "bot-token") $ \server received _ -> do
      logIn server "alice"
      installation <- installed server "org" "acme"
      _ <- WithServer.put server (installationPath installation <> "/repos/acme/app") Null
      resolved <- resolveRepo appRepo
      fmap (view ghToken) resolved `shouldBeM` Just (GhToken "bot-token")
      (lastMay' . resolvedWith <$> received) `shouldReturnM` Just (Just "token bot-token")
  where
    lastMay' = \case
      [] -> Nothing
      xs -> Just (last xs)
