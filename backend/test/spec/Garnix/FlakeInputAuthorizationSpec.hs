module Garnix.FlakeInputAuthorizationSpec where

import Data.Aeson (Key, Value, eitherDecodeFileStrict, object, (.=))
import Data.Aeson.Lens (key)
import Data.Aeson.Types (parseEither)
import Data.Containers.ListUtils (nubOrd)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Yaml.TH (yamlQQ)
import Garnix.FlakeInputAuthorization
import Garnix.NixConfig (NetRcEntry (..))
import Garnix.Prelude
import Garnix.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "_extractPrivateReposFromErrors" $ do
    it "extracts a repo name from from nix error messages for private repos" $ do
      _extractPrivateReposFromErrors "while fetching the input 'github:foo'\n"
        `shouldBe` Just ["github:foo"]

    it "ignores other messages" $ do
      _extractPrivateReposFromErrors "foo\nwhile fetching the input 'github:foo'\nbar\n"
        `shouldBe` Just ["github:foo"]

    it "extracts multiple repo names" $ do
      _extractPrivateReposFromErrors "while fetching the input 'github:foo'\nwhile fetching the input 'github:bar'\n"
        `shouldBe` Just ["github:foo", "github:bar"]

    it "returns `Nothing` when there's no match" $ do
      _extractPrivateReposFromErrors "foo\nbar\n"
        `shouldBe` Nothing

  describe "_parseFlakeInfo" $ do
    let test :: Value -> IO [FlakeInput]
        test json = do
          let result = parseEither _parseFlakeMetaData json
          case result of
            Right inputs -> pure inputs
            Left err -> error $ cs err

    it "parses github inputs" $ do
      inputs <-
        test
          [yamlQQ|
          locks:
            root: root
            nodes:
              root:
                inputs:
                  foo: foo
              foo:
                original:
                  owner: test-owner
                  repo: test-repo
                  type: github
        |]
      inputs `shouldBe` [Github $ GithubFlakeInput "test-owner" "test-repo"]

    it "parses indirect inputs" $ do
      inputs <-
        test
          [yamlQQ|
          locks:
            root: root
            nodes:
              root:
                inputs:
                  foo: foo
              foo:
                locked:
                  owner: test-owner
                  repo: test-repo
                  type: github
                original:
                  id: test-repo
                  type: indirect
        |]
      inputs `shouldBe` [Github $ GithubFlakeInput "test-owner" "test-repo"]

    it "discards other blessed types of flake inputs" $ do
      inputs <-
        test
          [yamlQQ|
          locks:
            root: root
            nodes:
              root:
                inputs:
                  foo: foo
              foo:
                original:
                  url: test-url
                  type: tarball
        |]
      inputs `shouldBe` []

    it "discards indirect inputs of other blessed types" $ do
      inputs <-
        test
          [yamlQQ|
            locks:
              root: root
              nodes:
                root:
                  inputs:
                    pathInput: pathInput
                pathInput:
                  locked:
                    path: test-url
                    type: tarball
                  original:
                    id: pathInput
                    type: indirect
          |]
      inputs `shouldBe` []

    it "extracts transitive flake inputs" $ do
      inputs <-
        test
          [yamlQQ|
            locks:
              root: root
              nodes:
                root:
                  inputs:
                    other-flake: other-flake
                other-flake:
                  inputs:
                    transitive-input: transitive-input
                  original:
                    owner: test-owner
                    repo: test-repo
                    type: github
                transitive-input:
                  locked:
                    path: /test-file-input
                    type: path
                  flake: false
                  original:
                    path: /test-file-input
                    type: path
          |]
      sort inputs `shouldBe` sort [Github $ GithubFlakeInput "test-owner" "test-repo", PathInput "/test-file-input"]

  describe "_parseGiteaInputs" $ do
    let instance' slug' url =
          ForgeConfig
            { _forgeConfigSlug = ForgeSlug slug',
              _forgeConfigKind = GiteaForgeKind,
              _forgeConfigWebUrl = url,
              _forgeConfigApiUrl = url <> "/api/v1",
              _forgeConfigWebhookSecret = "webhook-secret",
              _forgeConfigOAuthClientId = "client-id",
              _forgeConfigOAuthClientSecret = "client-secret",
              _forgeConfigApiToken = Just (GhToken "bot-token"),
              _forgeConfigAdmins = []
            }
        metadataOf :: FilePath -> IO Value
        metadataOf lockFile = do
          lock <- eitherDecodeFileStrict lockFile >>= either fail pure
          pure $ object ["locks" .= (lock :: Value)]

        -- A lock file with one input of the given type, with the given
        -- original reference fields.
        metadataWith :: Text -> [(Key, Text)] -> Value
        metadataWith typ fields =
          object
            [ "locks"
                .= object
                  [ "root" .= ("root" :: Text),
                    "nodes"
                      .= object
                        [ "root" .= object ["inputs" .= object ["lib" .= ("lib" :: Text)]],
                          "lib" .= object ["original" .= object (("type" .= typ) : [k .= v | (k, v) <- fields])]
                        ]
                  ]
            ]
        gitInput url = metadataWith "git" [("url", url)]
        example = instance' "git.example" "https://git.example.com"
        reposIn configs metadata = fmap (map instanceInputRepo . snd) (parseEither (_parseGiteaInputs configs) metadata)
        rejectedIn configs metadata = fmap (map rejectedInputUrl . fst) (parseEither (_parseGiteaInputs configs) metadata)

    it "recognises git and tarball inputs on a configured instance" $ do
      metadata <- metadataOf "test/spec/data/gitea/flake.lock"
      let configs = [example, instance' "git.second" "https://git.second.example.com"]
      fmap (nubOrd . sort) (reposIn configs metadata)
        `shouldBe` Right
          [ RepoId (ForgeSlug "git.example") "acme" "archived",
            RepoId (ForgeSlug "git.example") "acme" "private-lib"
          ]
      rejectedIn configs metadata `shouldBe` Right []

    it "keeps how the lock file pins each input" $ do
      metadata <- metadataOf "test/spec/data/gitea/flake.lock"
      let pinned =
            parseEither (_parseGiteaInputs [example]) metadata <&> \(_, inputs) ->
              nubOrd [(instanceInputRepo i, (^? key "url") =<< instanceInputLocked i) | i <- inputs]
      fmap sort pinned
        `shouldBe` Right
          [ (RepoId (ForgeSlug "git.example") "acme" "archived", Just "https://git.example.com/api/v1/repos/acme/archived/archive/4d1d4146010abf67c005a0d38d0e9f9a015470a4.tar.gz"),
            (RepoId (ForgeSlug "git.example") "acme" "private-lib", Just "https://git.example.com/acme/private-lib.git")
          ]

    it "recognises nothing when no instance is configured" $ do
      metadata <- metadataOf "test/spec/data/gitea/flake.lock"
      parseEither (_parseGiteaInputs []) metadata `shouldBe` Right ([], [])

    it "matches an instance served under a path" $ do
      let config = instance' "example" "https://example.com/gitea"
      reposIn [config] (gitInput "https://example.com/gitea/acme/lib") `shouldBe` Right [RepoId (ForgeSlug "example") "acme" "lib"]

    it "rejects a URL on the instance's host outside its path, since netrc matches the host alone" $ do
      let config = instance' "example" "https://example.com/gitea"
      rejectedIn [config] (gitInput "https://example.com/acme/other") `shouldBe` Right ["https://example.com/acme/other"]

    it "matches the instance's host whatever the port, scheme or user in the URL" $ do
      forM_
        [ "https://git.example.com:443/victim/secret",
          "https://git.example.com:3000/victim/secret.git",
          "http://git.example.com/victim/secret",
          "https://x-access-token@GIT.EXAMPLE.COM./victim/secret"
        ]
        $ \url -> reposIn [example] (gitInput url) `shouldBe` Right [RepoId (ForgeSlug "git.example") "victim" "secret"]

    it "rejects a URL whose path curl and git would resolve to another repository" $ do
      forM_
        [ "https://git.example.com/acme/pub/../../victim/secret.git",
          "https://git.example.com/acme/pub/%2e%2e/%2E%2E/victim/secret.git",
          "https://git.example.com/acme/./pub",
          "https://git.example.com//acme/pub",
          "https://git.example.com/acme%2Fpub/x"
        ]
        $ \url -> rejectedIn [example] (gitInput url) `shouldBe` Right [url]

    it "rejects a URL on the instance's host that names no repository" $ do
      forM_ ["https://git.example.com/explore", "https://git.example.com/", "ftp://git.example.com/acme/lib"] $ \url ->
        rejectedIn [example] (gitInput url) `shouldBe` Right [url]

    it "rejects an input on the instance's host that is not a git, tarball or file input" $ do
      rejectedIn [example] (metadataWith "github" [("owner", "acme"), ("repo", "lib"), ("host", "git.example.com")])
        `shouldBe` Right ["git.example.com"]

    it "leaves ssh URLs alone, since ssh never reads netrc" $ do
      parseEither (_parseGiteaInputs [example]) (gitInput "ssh://git@git.example.com/victim/secret.git") `shouldBe` Right ([], [])

    describe "_privateInputCredentials" $ do
      it "hands a Gitea repository's token over netrc, for its instance's host only" $ do
        let (nixConfig, netRc) = _privateInputCredentials (instance' "git.example" "https://git.example.com") (GhToken "bot-token")
        netRc `shouldBe` [NetRcEntry "git.example.com" "x-access-token" "bot-token"]
        Map.keys (getNixConfig nixConfig) `shouldBe` []

      it "hands a GitHub repository's token over access-tokens, for github.com" $ do
        let github = (instance' "github" "https://github.com") {_forgeConfigKind = GithubForgeKind}
            (nixConfig, netRc) = _privateInputCredentials github (GhToken "installation-token")
        netRc `shouldBe` []
        getNixConfig nixConfig `shouldBe` Map.fromList [(accessTokensSetting, "github.com=installation-token")]
