{-# LANGUAGE OverloadedRecordDot #-}

module Garnix.API.AccountSpec where

import Control.Lens (locally, (^?!))
import Control.Lens.Unsound (lensProduct)
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Lens
import Data.Functor ((<&>))
import Data.Map.Strict (fromList, (!))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Yaml (decodeThrow)
import Data.Yaml.TH (yamlQQ)
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.API.Account
  ( EnabledRepos (..),
    OrgUsage (..),
    UsageOverview (..),
    enabledReposOf,
    usageOverview,
  )
import Garnix.AccessToken
import Garnix.AccessToken.Types
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Entitlements qualified as Entitlements
import Garnix.GithubInterface.Types
import Garnix.Hosting.ServerPool.Types
import Garnix.Monad
import Garnix.MonetaryCost (usd)
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface qualified as GH
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

    let getDefaultPlan = fromJust <$> Entitlements.getPlanByName Entitlements.defaultPlanName

    describe "ci minutes" $ do
      it "reports empty usage when the user has no installations" $ do
        defaultPlan <- getDefaultPlan
        mockGithubInterface (GhToken "user-with-no-builds") [] $ do
          testUser <- mkTestUser
          usage <- usageOverview $ pure $ WebSession testUser (GhToken "user-with-no-builds")
          liftIO $ usage `shouldBe` UsageOverview (fromList [("mock-user", OrgUsage defaultPlan emptyDuration emptyDuration 0 NoActiveInstallation)])

      it "reports empty usage when the user has no builds this month" $ do
        defaultPlan <- getDefaultPlan
        monthsAgo <- liftIO getCurrentTime <&> subTime (fromDays @Int 90)
        mockGithubInterface (GhToken "user-with-one-org") [] $ do
          testUser <- mkTestUser
          _ <- addTestBuild "owner" monthsAgo (fromSeconds @Int 100)
          _ <- addTestBuild "owner" monthsAgo (fromSeconds @Int 100)
          usage <- usageOverview $ pure $ WebSession testUser (GhToken "user-with-one-org")
          liftIO $ usage `shouldBe` UsageOverview (fromList [("mock-user", OrgUsage defaultPlan emptyDuration emptyDuration 0 NoActiveInstallation)])

      it "reports usage of all build minutes for the user's installation" $ do
        defaultPlan <- getDefaultPlan
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
                  [ (GhRepoOwner $ GhLogin "org-with-no-builds", OrgUsage defaultPlan emptyDuration emptyDuration 0 NoActiveInstallation),
                    (GhRepoOwner $ GhLogin "mock-user", OrgUsage defaultPlan (fromSeconds @Int 300) emptyDuration 0 NoActiveInstallation),
                    (GhRepoOwner $ GhLogin "work-org", OrgUsage defaultPlan (fromSeconds @Int 400) emptyDuration 0 NoActiveInstallation)
                  ]
              )

      it "reports over-time in allotted minutes" $ do
        let planBaseCiTime = fromHours @Int 1000
            extraCiTime = fromHours @Int 20
        Entitlements.setExtraUsageLimits "mock-user" (Entitlements.emptyUsageLimits & #ciTime .~ extraCiTime)
        withTestEntitlement "test" (baseCiTime .~ planBaseCiTime) "mock-user" $ do
          mockGithubInterface (GhToken "mock-user") [] $ do
            testUser <- mkTestUser
            usage <- usageOverview $ pure $ WebSession testUser (GhToken "mock-user")
            usage
              `shouldBeM` UsageOverview
                ( fromList
                    [ ( GhRepoOwner $ GhLogin "mock-user",
                        OrgUsage
                          ( ProductPlan
                              { _productPlanDisplayName = "Plan Title for test",
                                _productPlanDescription = Just "test plan description",
                                _productPlanBaseCiTime = planBaseCiTime,
                                _productPlanMaximumPrDeploymentTime = fromMinutes @Int 100,
                                _productPlanIncludedBranchDeploymentHosts = 2,
                                _productPlanMaximumPackagesPerFlake = 100,
                                _productPlanPackageEvaluationTimeout = 30,
                                _productPlanPackageBuildTimeout = 120,
                                _productPlanExtraUsage = emptyUsageLimits & #ciTime .~ extraCiTime,
                                _productPlanIsPaid = True
                              }
                          )
                          emptyDuration
                          emptyDuration
                          0
                          NoActiveInstallation
                      )
                    ]
                )

    describe "pr deployment minutes" $ do
      it "sums up pr deployment minutes" $ do
        defaultPlan <- getDefaultPlan
        now <- liftIO getCurrentTime
        mockGithubInterface (GhToken "token") ["org"] $ do
          testUser <- mkTestUser
          build <- addTestBuild "mock-user" now emptyDuration
          addServer build (Just 42) now (Just $ fromSeconds @Int 1)
          build <- addTestBuild "org" now emptyDuration
          addServer build (Just 42) now (Just $ fromSeconds @Int 2)
          usage <- usageOverview (pure $ WebSession testUser (GhToken "token"))
          liftIO
            $ usage
            `shouldBe` UsageOverview
              ( fromList
                  [ ( "org",
                      OrgUsage
                        { _orgUsagePlan = defaultPlan,
                          _orgUsageCiTime = emptyDuration,
                          _orgUsagePrDeploymentTime = fromSeconds @Int 2,
                          _orgUsageBranchDeploymentHosts = 0,
                          _orgUsageInstallationStatus = NoActiveInstallation
                        }
                    ),
                    ( "mock-user",
                      OrgUsage
                        { _orgUsagePlan = defaultPlan,
                          _orgUsageCiTime = emptyDuration,
                          _orgUsagePrDeploymentTime = fromSeconds @Int 1,
                          _orgUsageBranchDeploymentHosts = 0,
                          _orgUsageInstallationStatus = NoActiveInstallation
                        }
                    )
                  ]
              )

    describe "branch deployments" $ do
      it "returns the number of running hosts" $ do
        defaultPlan <- getDefaultPlan
        now <- liftIO getCurrentTime
        mockGithubInterface (GhToken "token") ["org"] $ do
          testUser <- mkTestUser
          build <- addTestBuild "mock-user" now emptyDuration
          addServer build Nothing now Nothing
          build <- addTestBuild "org" now emptyDuration
          addServer build Nothing now Nothing
          addServer build Nothing now Nothing
          usage <- usageOverview (pure $ WebSession testUser (GhToken "token"))
          liftIO
            $ usage
            `shouldBe` UsageOverview
              ( fromList
                  [ ( "org",
                      OrgUsage
                        { _orgUsagePlan = defaultPlan,
                          _orgUsageCiTime = emptyDuration,
                          _orgUsagePrDeploymentTime = emptyDuration,
                          _orgUsageBranchDeploymentHosts = 2,
                          _orgUsageInstallationStatus = NoActiveInstallation
                        }
                    ),
                    ( "mock-user",
                      OrgUsage
                        { _orgUsagePlan = defaultPlan,
                          _orgUsageCiTime = emptyDuration,
                          _orgUsagePrDeploymentTime = emptyDuration,
                          _orgUsageBranchDeploymentHosts = 1,
                          _orgUsageInstallationStatus = NoActiveInstallation
                        }
                    )
                  ]
              )

    describe "plans" $ do
      it "returns the current plan of the user"
        $ withTestEntitlement "test" identity "mock-user"
        $ do
          now <- liftIO getCurrentTime
          mockGithubInterface (GhToken "token") [] $ do
            testUser <- mkTestUser
            _ <- addTestBuild "mock-user" now (fromSeconds @Int 50)
            (owner, usage) <-
              usageOverview (pure $ WebSession testUser (GhToken "token"))
                <&> fromSingleton . Map.toList . _usageOverviewByOrg
            liftIO $ do
              owner `shouldBe` GhRepoOwner (GhLogin "mock-user")
              _orgUsagePlan usage
                `shouldBe` ( ProductPlan
                               { _productPlanDisplayName = "Plan Title for test",
                                 _productPlanDescription = Just "test plan description",
                                 _productPlanBaseCiTime = fromMinutes @Int 200000,
                                 _productPlanMaximumPrDeploymentTime = fromMinutes @Int 100,
                                 _productPlanIncludedBranchDeploymentHosts = 2,
                                 _productPlanMaximumPackagesPerFlake = 100,
                                 _productPlanPackageEvaluationTimeout = 30,
                                 _productPlanPackageBuildTimeout = 120,
                                 _productPlanExtraUsage = emptyUsageLimits,
                                 _productPlanIsPaid = True
                               }
                           )

      it "returns the current plan of the user, when there's no builds"
        $ withTestEntitlement "test" identity "mock-user"
        $ do
          mockGithubInterface (GhToken "token") [] $ do
            testUser <- mkTestUser
            (owner, usage) <-
              usageOverview (pure $ WebSession testUser (GhToken "token"))
                <&> fromSingleton . Map.toList . _usageOverviewByOrg
            liftIO $ do
              owner `shouldBe` GhRepoOwner (GhLogin "mock-user")
              _orgUsagePlan usage
                `shouldBe` ( ProductPlan
                               { _productPlanDisplayName = "Plan Title for test",
                                 _productPlanDescription = Just "test plan description",
                                 _productPlanBaseCiTime = fromMinutes @Int 200000,
                                 _productPlanMaximumPrDeploymentTime = fromMinutes @Int 100,
                                 _productPlanIncludedBranchDeploymentHosts = 2,
                                 _productPlanMaximumPackagesPerFlake = 100,
                                 _productPlanPackageEvaluationTimeout = 30,
                                 _productPlanPackageBuildTimeout = 120,
                                 _productPlanExtraUsage = emptyUsageLimits,
                                 _productPlanIsPaid = True
                               }
                           )

      it "returns plans for orgs that the user is an admin for (with usage)" $ do
        withTestEntitlement "test" identity "mock-org" $ do
          now <- liftIO getCurrentTime
          mockGithubInterface (GhToken "token") ["mock-org"] $ do
            testUser <- mkTestUser
            _ <- addTestBuild "mock-org" now (fromSeconds @Int 50)
            usage <-
              usageOverview (pure $ WebSession testUser (GhToken "token"))
                <&> _usageOverviewByOrg
            liftIO $ do
              usage ! "mock-org"
                `shouldBe` OrgUsage
                  ( ProductPlan
                      { _productPlanDisplayName = "Plan Title for test",
                        _productPlanDescription = Just "test plan description",
                        _productPlanBaseCiTime = fromMinutes @Int 200000,
                        _productPlanMaximumPrDeploymentTime = fromMinutes @Int 100,
                        _productPlanIncludedBranchDeploymentHosts = 2,
                        _productPlanMaximumPackagesPerFlake = 100,
                        _productPlanPackageEvaluationTimeout = 30,
                        _productPlanPackageBuildTimeout = 120,
                        _productPlanExtraUsage = emptyUsageLimits,
                        _productPlanIsPaid = True
                      }
                  )
                  (fromSeconds @Int 50)
                  emptyDuration
                  0
                  NoActiveInstallation

      it "returns plans for orgs that the user is an admin for (without usage)" $ do
        withTestEntitlement "test" identity "mock-org" $ do
          mockGithubInterface (GhToken "token") ["mock-org"] $ do
            testUser <- mkTestUser
            usage <-
              usageOverview (pure $ WebSession testUser (GhToken "token"))
                <&> _usageOverviewByOrg
            liftIO $ do
              usage ! "mock-org"
                `shouldBe` OrgUsage
                  ( ProductPlan
                      { _productPlanDisplayName = "Plan Title for test",
                        _productPlanDescription = Just "test plan description",
                        _productPlanBaseCiTime = fromMinutes @Int 200000,
                        _productPlanMaximumPrDeploymentTime = fromMinutes @Int 100,
                        _productPlanIncludedBranchDeploymentHosts = 2,
                        _productPlanMaximumPackagesPerFlake = 100,
                        _productPlanPackageEvaluationTimeout = 30,
                        _productPlanPackageBuildTimeout = 120,
                        _productPlanExtraUsage = emptyUsageLimits,
                        _productPlanIsPaid = True
                      }
                  )
                  emptyDuration
                  emptyDuration
                  0
                  NoActiveInstallation

      it "merges multiple plans into one" $ do
        withTestEntitlement "a" ((includedBranchDeploymentHosts .~ 2) . (maximumPrDeploymentTime .~ fromMinutes @Int 20) . (baseCiTime .~ fromMinutes @Int 200000)) "mock-user" $ do
          withTestEntitlement "b" ((includedBranchDeploymentHosts .~ 3) . (maximumPrDeploymentTime .~ fromMinutes @Int 30) . (baseCiTime .~ fromMinutes @Int 300000)) "mock-user" $ do
            mockGithubInterface (GhToken "token") [] $ do
              testUser <- mkTestUser
              usage <-
                usageOverview (pure $ WebSession testUser (GhToken "token"))
                  <&> _usageOverviewByOrg
              liftIO $ do
                usage
                  `shouldBe` fromList
                    [ ( GhRepoOwner (GhLogin "mock-user"),
                        OrgUsage
                          ( ProductPlan
                              { _productPlanDisplayName = "Plan Title for a, Plan Title for b",
                                _productPlanDescription = Just "a plan description, b plan description",
                                _productPlanBaseCiTime = fromMinutes @Int 300000,
                                _productPlanMaximumPrDeploymentTime = fromMinutes @Int 30,
                                _productPlanIncludedBranchDeploymentHosts = 3,
                                _productPlanMaximumPackagesPerFlake = 100,
                                _productPlanPackageEvaluationTimeout = 30,
                                _productPlanPackageBuildTimeout = 120,
                                _productPlanExtraUsage = emptyUsageLimits,
                                _productPlanIsPaid = True
                              }
                          )
                          emptyDuration
                          emptyDuration
                          0
                          NoActiveInstallation
                      )
                    ]

      context "when there are plans with and without a priceId" $ do
        let wrap :: M () -> M ()
            wrap action = do
              withTestEntitlement "with-price-id" ((includedBranchDeploymentHosts .~ 3) . (maximumPrDeploymentTime .~ fromMinutes @Int 300) . (baseCiTime .~ fromMinutes @Int 700000)) "mock-user"
                $ withTestEntitlement
                  "without-price-id"
                  ((includedBranchDeploymentHosts .~ 4) . (maximumPrDeploymentTime .~ fromMinutes @Int 400) . (baseCiTime .~ fromMinutes @Int 800000) . (isPaid .~ False))
                  "mock-user"
                  action

        aroundM_ wrap $ do
          it "hides plans without price ids" $ do
            testUser <- mkTestUser
            plan <-
              usageOverview (pure $ WebSession testUser (GhToken "token"))
                <&> (^. lensProduct displayName description)
                  . _orgUsagePlan
                  . (! "mock-user")
                  . _usageOverviewByOrg
            liftIO $ plan `shouldBe` ("Plan Title for with-price-id", Just "with-price-id plan description")

          it "returns the maximum for entitlements" $ do
            testUser <- mkTestUser
            entitlements <-
              usageOverview (pure $ WebSession testUser (GhToken "token"))
                <&> ( \p ->
                        ( p ^. includedBranchDeploymentHosts,
                          p ^. maximumPrDeploymentTime,
                          p ^. baseCiTime
                        )
                    )
                  . _orgUsagePlan
                  . (! "mock-user")
                  . _usageOverviewByOrg
            liftIO $ entitlements `shouldBe` (4, fromMinutes @Int 400, fromMinutes @Int 800000)

      context "when there's products without any priceIds" $ do
        it "returns correct entitlements" $ do
          withTestEntitlement "without-price-id" ((maximumPrDeploymentTime .~ fromMinutes @Int 400) . (isPaid .~ False)) "mock-user" $ do
            testUser <- mkTestUser
            entitlements <-
              usageOverview (pure $ WebSession testUser (GhToken "token"))
                <&> ( \p ->
                        ( p ^. includedBranchDeploymentHosts,
                          p ^. maximumPrDeploymentTime,
                          p ^. baseCiTime
                        )
                    )
                  . _orgUsagePlan
                  . (! "mock-user")
                  . _usageOverviewByOrg
            liftIO $ entitlements `shouldBe` (2, fromMinutes @Int 400, fromMinutes @Int 200000)

    describe "setting usage limits" $ do
      let newLimits =
            [aesonQQ| {
              ciTime: #{1234 * 60 :: Int},
              prDeployTime: #{567 * 60 :: Int},
              hostingSpend: #{890 * 100 :: Int}
            } |]

      it "returns 401 when logged out" $ suppressLogs $ withServer $ \server -> do
        res <- server.put "/api/account/usage/some-org" newLimits
        res `shouldHaveStatusCode` 401

      it "returns 401 when making a request to an org that doesn't exist" $ suppressLogs $ withServer $ \server -> do
        void server.login
        res <- server.put "/api/account/usage/some-org" newLimits
        res `shouldHaveStatusCode` 401

      it "returns 400 if the user does not have a plan"
        $ suppressLogs
        $ GH.withFakeGithubInterface
        $ const
        $ withServer
        $ \server -> do
          user <- GhRepoOwner . (^. githubLogin) <$> server.login
          res <- server.put ("/api/account/usage/" <> cs (getGhLogin $ getGhRepoOwner user)) newLimits
          res `shouldHaveStatusCode` 400

      it "returns 401 when making a request to an org that the user is not an admin of"
        $ suppressLogs
        $ GH.withFakeGithubInterface
        $ \st ->
          withServer $ \server ->
            withTestEntitlement "test" identity "some-org" $ do
              void server.login
              GH.addOrgMembers st [GhUserOrgMembership "some-org" (Other "user")]
              res <- server.put "/api/account/usage/some-org" newLimits
              res `shouldHaveStatusCode` 401
              (Entitlements.getPlan "some-org" <&> (^. extraUsage . #ciTime)) `shouldReturnM` fromMinutes @Int 0

      it "returns 400 if any values are negative"
        $ suppressLogs
        $ GH.withFakeGithubInterface
        $ \st ->
          withServer $ \server ->
            withTestEntitlement "test" identity "some-org" $ do
              void server.login
              GH.addOrgMembers st [GhUserOrgMembership "some-org" Admin]
              let assert400 :: Int -> Int -> Int -> String -> M ()
                  assert400 ciTime prDeployTime hostingSpend expectedErr = do
                    let json =
                          [aesonQQ| {
                            ciTime: #{ciTime},
                            prDeployTime: #{prDeployTime},
                            hostingSpend: #{hostingSpend}
                          } |]
                    res <- server.put "/api/account/usage/some-org" json
                    res `shouldHaveStatusCode` 400
                    cs (res ^. responseBody) `shouldContainM` expectedErr
              assert400 (-123) 456 789 "CI time cannot be negative"
              assert400 123 (-456) 789 "PR deploy time cannot be negative"
              assert400 123 456 (-789) "Hosting spend cannot be negative"

      it "updates the extra ci entitlements time"
        $ suppressLogs
        $ GH.withFakeGithubInterface
        $ \st ->
          withServer $ \server ->
            withTestEntitlement "test" identity "some-org" $ do
              void server.login
              GH.addOrgMembers st [GhUserOrgMembership "some-org" Admin]
              void $ assert200 $ server.put "/api/account/usage/some-org" newLimits
              plan <- Entitlements.getPlan "some-org"
              plan ^. extraUsage . #ciTime `shouldBeM` fromMinutes @Int 1234
              plan ^. extraUsage . #prDeployTime `shouldBeM` fromMinutes @Int 567
              plan ^. extraUsage . #hostingSpend `shouldBeM` usd 890

      it "allows a GH repo owner to modify their own usage limits (github does not report these as admin)"
        $ suppressLogs
        $ GH.withFakeGithubInterface
        $ const
        $ withServer
        $ \server -> do
          user <- GhRepoOwner . (^. githubLogin) <$> server.login
          withTestEntitlement "test" identity user $ do
            void $ assert200 $ server.put ("/api/account/usage/" <> cs (getGhLogin $ getGhRepoOwner user)) newLimits
            plan <- Entitlements.getPlan user
            plan ^. extraUsage . #ciTime `shouldBeM` fromMinutes @Int 1234
            plan ^. extraUsage . #prDeployTime `shouldBeM` fromMinutes @Int 567
            plan ^. extraUsage . #hostingSpend `shouldBeM` usd 890

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

addServer :: Build -> Maybe GhPullRequestId -> UTCTime -> Maybe Duration -> M ()
addServer build pr now duration = do
  let (start, end) = case duration of
        Nothing -> (now, Nothing)
        Just duration -> (subTime duration now, Just now)
  res <-
    DB.pgExec
      [pgSQL|
        INSERT INTO servers
          (configuration_build_id, hetzner_id, created_at, ready_at, ended_at, pull_request, ipv4, ipv6, server_tier) VALUES
          (${build ^. id}, 1, ${start}, ${start}, ${end}, ${pr}, '<none>', '<none>', ${def :: ServerTier})
      |]
  case res of
    1 -> pure ()
    n -> throw $ OtherError $ "impossible: " <> show n
