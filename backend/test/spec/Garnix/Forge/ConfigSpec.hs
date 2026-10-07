module Garnix.Forge.ConfigSpec (spec) where

import Control.Exception (throwIO)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Garnix.Forge.Config
import Garnix.Prelude
import Garnix.Types
import Test.Hspec

-- | Secret files, by path.
secrets :: Map.Map FilePath StrictByteString
secrets =
  Map.fromList
    [ ("/run/secrets/webhook_secret", "webhook-secret\n"),
      ("/run/secrets/oauth_client_secret", "client-secret\n"),
      ("/run/secrets/api_token", "bot-token\n"),
      ("/run/secrets/empty", "\n")
    ]

readSecret :: FilePath -> IO StrictByteString
readSecret path = maybe (throwIO $ ForgesFileError $ "cannot read " <> cs path <> ": no such file") pure $ Map.lookup path secrets

forgesFile :: Text -> [(Text, Text)] -> StrictByteString
forgesFile slug' overrides =
  cs
    $ "{"
    <> show slug'
    <> ": {"
    <> T.intercalate ", " [show k <> ": " <> v | (k, v) <- Map.toList (Map.fromList overrides `Map.union` defaults)]
    <> "}}"
  where
    defaults =
      Map.fromList
        [ ("kind", "\"gitea\""),
          ("webUrl", "\"https://git.example.com/\""),
          ("apiUrl", "\"https://git.example.com/api/v1\""),
          ("webhookSecretFile", "\"/run/secrets/webhook_secret\""),
          ("oauthClientId", "\"client-id\""),
          ("oauthClientSecretFile", "\"/run/secrets/oauth_client_secret\""),
          ("apiTokenFile", "\"/run/secrets/api_token\""),
          ("admins", "[\"alice\"]")
        ]

failsWith :: Text -> IO a -> Expectation
failsWith fragment action =
  action `shouldThrow` \(ForgesFileError message) -> fragment `T.isInfixOf` message

spec :: Spec
spec = do
  describe "forgeInstance" $ do
    it "refuses to make a GitHub instance from a forges file entry" $ do
      [config] <- parseForgesFile readSecret (forgesFile "git.example" [])
      isLeft (forgeInstance config) `shouldBe` False
      isLeft (forgeInstance $ config & kind .~ GithubForgeKind) `shouldBe` True

  describe "parseForgesFile" $ do
    it "reads an instance and its secrets" $ do
      [config] <- parseForgesFile readSecret (forgesFile "git.example" [])
      config ^. slug `shouldBe` ForgeSlug "git.example"
      config ^. kind `shouldBe` GiteaForgeKind
      config ^. webUrl `shouldBe` "https://git.example.com"
      config ^. apiUrl `shouldBe` "https://git.example.com/api/v1"
      config ^. webhookSecret `shouldBe` "webhook-secret"
      config ^. oAuthClientId `shouldBe` "client-id"
      config ^. oAuthClientSecret `shouldBe` "client-secret"
      config ^. apiToken `shouldBe` Just (GhToken "bot-token")
      config ^. admins `shouldBe` ["alice"]

    it "reads several instances" $ do
      configs <-
        parseForgesFile readSecret
          $ "{\"a\": "
          <> innerOf (forgesFile "a" [])
          <> ", \"b\": "
          <> innerOf (forgesFile "b" [("webUrl", "\"https://git.other.example.com\"")])
          <> "}"
      sort (map (view slug) configs) `shouldBe` [ForgeSlug "a", ForgeSlug "b"]

    it "has no admins unless given" $ do
      [config] <- parseForgesFile readSecret (forgesFile "git.example" [("admins", "null")])
      config ^. admins `shouldBe` []

    it "reads an empty file as no instances" $ do
      configs <- parseForgesFile readSecret "{}"
      length configs `shouldBe` 0

    it "rejects the slug github" $ do
      failsWith "reserved for github.com" $ parseForgesFile readSecret (forgesFile "github" [])

    it "rejects two forges on the same host, which netrc could not tell apart" $ do
      let entry url = innerOf (forgesFile "x" [("webUrl", "\"" <> url <> "\"")])
      failsWith "are all on the host git.example.com"
        $ parseForgesFile readSecret
        $ "{\"one\": "
        <> entry "https://git.example.com/one"
        <> ", \"two\": "
        <> entry "https://GIT.example.com:3000/two"
        <> "}"

    it "rejects two forges whose hosts differ only as netrc ignores" $ do
      let entry url = innerOf (forgesFile "x" [("webUrl", "\"" <> url <> "\"")])
      failsWith "are all on the host git.example.com"
        $ parseForgesFile readSecret
        $ "{\"one\": "
        <> entry "https://git.example.com"
        <> ", \"two\": "
        <> entry "https://git.example.com."
        <> "}"

    it "rejects the slug github in any case" $ do
      failsWith "reserved for github.com" $ parseForgesFile readSecret (forgesFile "GitHub" [])

    it "rejects a slug that is not a path segment" $ do
      failsWith "may only contain" $ parseForgesFile readSecret (forgesFile "git/example" [])

    it "rejects an unknown kind" $ do
      failsWith "unknown kind" $ parseForgesFile readSecret (forgesFile "git.example" [("kind", "\"gitlab\"")])

    it "rejects a missing secret file, naming the field" $ do
      failsWith "apiTokenFile: cannot read /run/secrets/missing" $ parseForgesFile readSecret (forgesFile "git.example" [("apiTokenFile", "\"/run/secrets/missing\"")])

    it "rejects an empty secret" $ do
      failsWith "webhookSecretFile: /run/secrets/empty is empty" $ parseForgesFile readSecret (forgesFile "git.example" [("webhookSecretFile", "\"/run/secrets/empty\"")])

    it "rejects a web URL that is not http(s)" $ do
      failsWith "webUrl is not an http(s) URL" $ parseForgesFile readSecret (forgesFile "git.example" [("webUrl", "\"ssh://git.example.com\"")])

    it "rejects a web URL without a host" $ do
      failsWith "webUrl is not an http(s) URL with a host" $ parseForgesFile readSecret (forgesFile "git.example" [("webUrl", "\"https:git.example.com\"")])
      failsWith "webUrl is not an http(s) URL with a host" $ parseForgesFile readSecret (forgesFile "git.example" [("webUrl", "\"https:///git\"")])

    it "rejects a file that is not JSON" $ do
      failsWith "" $ parseForgesFile readSecret "not json"
  where
    -- The instance object of a one-instance forges file.
    innerOf file = cs $ T.dropEnd 1 $ T.drop 1 $ T.dropWhile (/= ':') (cs file :: Text)
