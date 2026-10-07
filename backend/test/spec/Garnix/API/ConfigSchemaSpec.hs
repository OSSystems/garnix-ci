{-# LANGUAGE OverloadedRecordDot #-}

module Garnix.API.ConfigSchemaSpec where

import Autodocodec.Schema (JSONSchema)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Map.Strict qualified as Map
import Data.Yaml (decodeThrow)
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.WithServer
import Garnix.Types
import Network.Wreq.Lens
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)

spec :: Spec
spec = do
  describe "/api/forges" $ inM $ aroundM_ suppressLogsWhenPassing $ do
    it "lists github.com" $ withServer $ \testServer -> do
      response <- assertJSON $ assert200 $ testServer.get "/api/forges"
      response ^. responseBody `shouldBeM` [aesonQQ| [{slug: "github", kind: "github", web_url: "https://github.com"}] |]

    it "lists every configured instance, without its secrets" $ do
      local (#forges %~ Map.insert "git.example" (testForgeInstance "git.example" GiteaForgeKind)) $ withServer $ \testServer -> do
        response <- assertJSON $ assert200 $ testServer.get "/api/forges"
        response
          ^. responseBody
          `shouldBeM` [aesonQQ|
            [ {slug: "git.example", kind: "gitea", web_url: "https://git.example"},
              {slug: "github", kind: "github", web_url: "https://github.com"}
            ]
          |]

  describe "/api/garnix-config-schema.json" $ do
    inM $ aroundM_ suppressLogsWhenPassing $ do
      it "returns a json schema for the garnix yaml config" $ withServer $ \testServer -> do
        response <- assert200 $ testServer.get "/api/garnix-config-schema.json"
        _schema :: JSONSchema <- decodeThrow $ cs $ response ^. responseBody
        pure ()

    it "golden test for schema file" $ do
      runTestM $ suppressLogsWhenPassing $ withServer $ \testServer -> do
        response <- assertJSON $ assert200 $ testServer.get "/api/garnix-config-schema.json"
        pure $ defaultGolden "ConfigSchemaSpec/garnix-config-schema.json" $ cs $ encodePretty $ response ^. responseBody
