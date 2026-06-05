{-# LANGUAGE OverloadedRecordDot #-}

module Garnix.API.AccountSpec where

import Control.Lens (locally, (^?!))
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Lens
import Data.Functor ((<&>))
import Data.Map.Strict (fromList)
import Data.Yaml (decodeThrow)
import Data.Yaml.TH (yamlQQ)
import Garnix.API.Account
  ( EnabledRepos (..),
    OrgUsage (..),
    UsageOverview (..),
    enabledReposOf,
    usageOverview,
  )
import Garnix.AccessToken
import Garnix.AccessToken.Types
import Garnix.Duration
import Garnix.GithubInterface.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.WithServer
import Garnix.Types hiding (Admin, context, head)
import GitHub qualified as GH
import Network.HTTP.Types (badRequest400)
import Network.Wreq.Lens
import Servant.Auth.Server (AuthResult (..))
import Test.Hspec

spec :: Spec
spec = inM $ beforeM_ truncateDBM $ aroundM_ suppressLogsWhenPassing $ do
  describe "AccountAPI" $ do
    let mockGithubInterface :: GhToken -> [GhRepoOwner] -> M a -> M a
        mockGithubInterface expectedToken orgs =
          locally
            #githubInterface
            ( \x ->
                x
                  { _githubInterfaceGetInstalledOrgs = \tok -> do
                      liftIO $ tok `shouldBe` expectedToken
                      pure $ map (`GhUserOrgMembership` Admin) orgs
                  }
            )

    describe "ci minutes" $ do
      it "reports empty usage when the user has no installations" $ do
        mockGithubInterface (GhToken "user-with-no-builds") [] $ do
          testUser <- mkTestUser
          usage <- usageOverview $ pure $ WebSession testUser (GhToken "user-with-no-builds")
          liftIO $ usage `shouldBe` UsageOverview (fromList [("mock-user", OrgUsage emptyDuration emptyDuration NoActiveInstallation)])

      it "reports empty usage when the user has no builds this month" $ do
        monthsAgo <- liftIO getCurrentTime <&> subTime (fromDays @Int 90)
        mockGithubInterface (GhToken "user-with-one-org") [] $ do
          testUser <- mkTestUser
          _ <- addTestBuild "owner" monthsAgo (fromSeconds @Int 100)
          _ <- addTestBuild "owner" monthsAgo (fromSeconds @Int 100)
          usage <- usageOverview $ pure $ WebSession testUser (GhToken "user-with-one-org")
          liftIO $ usage `shouldBe` UsageOverview (fromList [("mock-user", OrgUsage emptyDuration emptyDuration NoActiveInstallation)])

      it "reports usage of all build minutes for the user's installation" $ do
        now <- liftIO getCurrentTime
        mockGithubInterface (GhToken "user-with-many-orgs") ["work-org", "org-with-no-builds"] $ do
          testUser <- mkTestUser
          _ <- addTestBuild "mock-user" now (fromSeconds @Int 100)
          _ <- addTestBuild "mock-user" now (fromSeconds @Int 200)
          _ <- addTestBuild "work-org" now (fromSeconds @Int 400)
          _ <- addTestBuild "unrelated-org" now (fromSeconds @Int 100)
          usage <- usageOverview $ pure $ WebSession testUser (GhToken "user-with-many-orgs")
          liftIO
            $ usage
            `shouldBe` UsageOverview
              ( fromList
                  [ (GhRepoOwner $ GhLogin "org-with-no-builds", OrgUsage emptyDuration emptyDuration NoActiveInstallation),
                    (GhRepoOwner $ GhLogin "mock-user", OrgUsage (fromSeconds @Int 300) emptyDuration NoActiveInstallation),
                    (GhRepoOwner $ GhLogin "work-org", OrgUsage (fromSeconds @Int 400) emptyDuration NoActiveInstallation)
                  ]
              )

    describe "/api/account/tokens" $ do
      it "return 401 status for GET when logged out" $ suppressLogs $ withServer $ \server -> do
        res <- server.get "/api/account/tokens"
        res `shouldHaveStatusCode` 401

      it "return 401 status for POST when logged out" $ suppressLogs $ withServer $ \server -> do
        res <- server.post "/api/account/tokens" [aesonQQ| { name: "my-token" } |]
        res `shouldHaveStatusCode` 401

      it "return 401 status for DELETE when logged out" $ suppressLogs $ withServer $ \server -> do
        res <- server.delete "/api/account/tokens/123"
        res `shouldHaveStatusCode` 401

      it "returns no access tokens when none have been generated yet" $ suppressLogs $ withServer $ \server -> do
        void server.login
        res <- assert200 $ server.get "/api/account/tokens"
        liftIO $ res ^?! responseBody . _Value `shouldBe` [aesonQQ| { tokens: [] } |]

      it "allows generating valid tokens" $ suppressLogs $ withServer $ \server -> do
        user <- server.login
        res <- assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "my-token" } |]
        let token = AccessToken $ res ^?! responseBody . key "token" . _String
        isValid <- isAccessTokenValid (user ^. id) token (^. #cache)
        liftIO $ isValid `shouldBe` True

      it "allows querying generated tokens" $ suppressLogs $ withServer $ \server -> do
        void server.login
        void $ assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "token-a", scopes: { cache: true } } |]
        void $ assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "token-b", scopes: { api: true } } |]
        void $ assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "token-c", scopes: { cache: true, api: true } } |]
        -- for backwards compatibility
        void $ assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "token-d" } |]
        res <-
          assert200 (server.get "/api/account/tokens")
            <&> (^. responseBody)
            <&> key "tokens"
              . _Array
              . mapped
              . _Object
              %~ ( Aeson.delete "created"
                     . Aeson.delete "id"
                 )
        decodeThrow (cs res)
          `shouldReturnM` [yamlQQ|
            tokens:
              - name: token-a
                scopes:
                  cache: true
                  api: false
              - name: token-b
                scopes:
                  cache: false
                  api: true
              - name: token-c
                scopes:
                  cache: true
                  api: true
              - name: token-d
                scopes:
                  cache: true
                  api: false
          |]

      it "errors on access tokens with no scopes" $ suppressLogs $ withServer $ \server -> do
        void server.login
        let cases =
              [ [aesonQQ| { name: "token", scopes: {  } } |],
                [aesonQQ| { name: "token", scopes: { cache: false } } |],
                [aesonQQ| { name: "token", scopes: { cache: false, api: false } } |]
              ]
        forM_ cases $ \body -> do
          res <- server.post "/api/account/tokens" body
          res ^. responseStatus `shouldBeM` badRequest400

      it "allows deleting generated tokens" $ suppressLogs $ withServer $ \server -> do
        void server.login
        void $ assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "token-a" } |]
        void $ assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "token-b" } |]
        void $ assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "token-c" } |]
        res <- assert200 $ server.get "/api/account/tokens"
        let [a, _b, c] = res ^.. responseBody . key "tokens" . _Array . traverse . key "id" . _Integer
        void $ assert200 $ server.delete $ cs ("/api/account/tokens/" <> show a)
        void $ assert200 $ server.delete $ cs ("/api/account/tokens/" <> show c)
        res <- assert200 $ server.get "/api/account/tokens"
        liftIO
          $ sort (res ^.. responseBody . key "tokens" . _Array . traverse . key "name" . _String)
          `shouldBe` ["token-b"]

    describe "getEnabledRepos" $ do
      let mockGithubInterface =
            locally
              #githubInterface
              ( \x ->
                  x
                    { _githubInterfaceGetInstallations =
                        const
                          $ pure
                            [ GH.mkId Proxy 1,
                              GH.mkId Proxy 2
                            ],
                      _githubInterfaceGetReposInInstallationAccessibleTo = \org _ ->
                        pure
                          $ case GH.untagId org of
                            1 -> ["org1/repo1"]
                            2 -> ["org2/repo2"]
                            _ -> []
                    }
              )
      it "lists garnix-enabled repos the user has access to" $ suppressLogs $ do
        mockGithubInterface $ do
          testUser <- mkTestUser
          enabledReposOf (Authenticated $ WebSession testUser (GhToken "user-with-no-builds"))
            `shouldReturnM` EnabledRepos ["org1/repo1", "org2/repo2"]

mkTestUser :: M User
mkTestUser = do
  now <- liftIO getCurrentTime
  pure
    $ User
      { _userId = UserId 1,
        _userGithubLogin = GhLogin "mock-user",
        _userEmail = Email "mock-user@example.com",
        _userSubscriptionType = FreeSubscription,
        _userCreatedAt = now
      }
