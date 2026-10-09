{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Garnix.API.DevSpec where

import Control.Lens
import Data.Aeson.Lens
import Data.Set (delete)
import Garnix.Monad (TestFeature (DevApi))
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.WithServer hiding (delete)
import Network.Wreq.Lens
import Test.Hspec

spec :: Spec
spec = inM $ beforeM_ truncateDBM $ do
  describe "/dev/log-me-in" $ do
    let test = it
    test "whoami returns null when not logged in" $ withServer $ \testServer -> do
      response <- assert200 $ testServer.get "/api/whoami"
      liftIO $ response ^?! responseBody . _Value `shouldBe` [aesonQQ|null|]

    describe "dev mode" $ do
      it "logs the user in" $ withServer $ \testServer -> do
        _ <- assert200 $ testServer.get "/api/dev/log-me-in"
        response <- assert200 $ testServer.get "/api/whoami"
        liftIO
          $ response
          ^?! responseBody . _Value
          `shouldBe` [aesonQQ|
              {
                username: "dev-user",
                email: "dev-user@example.com",
                forge: "github",
                is_admin: false,
                identities: [{ forge: "github", login: "dev-user" }]
              }
            |]

      it "works if called twice" $ withServer $ \testServer -> do
        _ <- assert200 $ testServer.get "/api/dev/log-me-in"
        _ <- assert200 $ testServer.get "/api/dev/log-me-in"
        response <- assert200 $ testServer.get "/api/whoami"
        liftIO
          $ response
          ^?! responseBody . _Value
          `shouldBe` [aesonQQ|
              {
                username: "dev-user",
                email: "dev-user@example.com",
                forge: "github",
                is_admin: false,
                identities: [{ forge: "github", login: "dev-user" }]
              }
            |]

    aroundM_ suppressLogsWhenPassing $ describe "prod mode" $ do
      it "returns 404" $ do
        local (#testFeatures %~ delete DevApi) $ do
          withServer $ \testServer -> do
            result <- testServer.get "/api/dev/log-me-in"
            result `shouldHaveStatusCode` 404
