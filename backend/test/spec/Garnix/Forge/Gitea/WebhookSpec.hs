module Garnix.Forge.Gitea.WebhookSpec (spec) where

import Data.Aeson (Value (..), decode, encode)
import Data.Aeson.Lens (key)
import Data.ByteString.Lazy qualified as BSL
import Garnix.Forge.Gitea.Webhook
import Garnix.Orchestrator (ForgeEvent (..))
import Garnix.Prelude
import Garnix.Types
import Test.Hspec

slug' :: ForgeSlug
slug' = ForgeSlug "git.example"

fixture :: FilePath -> IO LazyByteString
fixture name = BSL.readFile $ "test/spec/data/gitea" </> name

fixtureValue :: FilePath -> IO Value
fixtureValue name = fixture name >>= maybe (fail $ "not JSON: " <> name) pure . decode

spec :: Spec
spec = do
  describe "verifyGiteaSignature" $ do
    let secret = "webhook-secret"
        -- hmac.new(b"webhook-secret", push.json, sha256).hexdigest()
        signature = "b25149d13cf85c213b301e57c7060d2d4bc6a765302f189181dcbd1e8eac9066"

    it "accepts the HMAC-SHA256 of the raw body" $ do
      body <- fixture "push.json"
      verifyGiteaSignature secret (Just signature) body `shouldBe` True

    it "accepts the signature in upper case" $ do
      body <- fixture "push.json"
      verifyGiteaSignature secret (Just "B25149D13CF85C213B301E57C7060D2D4BC6A765302F189181DCBD1E8EAC9066") body `shouldBe` True

    it "rejects a signature made with another secret" $ do
      body <- fixture "push.json"
      verifyGiteaSignature "another-secret" (Just signature) body `shouldBe` False

    it "rejects a signature of another body" $ do
      body <- fixture "push.json"
      verifyGiteaSignature secret (Just signature) (body <> " ") `shouldBe` False

    it "rejects a delivery without a signature" $ do
      body <- fixture "push.json"
      verifyGiteaSignature secret Nothing body `shouldBe` False
      verifyGiteaSignature secret (Just "") body `shouldBe` False

    it "rejects everything when no secret is configured" $ do
      verifyGiteaSignature "" (Just "") "" `shouldBe` False

  describe "parseGiteaWebhook" $ do
    it "reads a push" $ do
      body <- fixture "push.json"
      parseGiteaWebhook slug' "push" body
        `shouldBe` Right
          ( Just
              GiteaPush
                { _giteaWebhookRepo = RepoId slug' "gitea" "webhooks",
                  _giteaWebhookPublicity = RepoIsPublic True,
                  _giteaWebhookSender = "gitea",
                  _giteaWebhookBranch = Just "develop",
                  _giteaWebhookCommit = "bffeb74224043ba2feb48d137756c8a9331c449a"
                }
          )

    it "reads a tag push as a push without a branch" $ do
      body <- fixtureValue "push.json"
      let tagPush = body & key "ref" .~ String "refs/tags/v1.0"
      fmap (fmap _giteaWebhookBranch) (parseGiteaWebhook slug' "push" (encode tagPush))
        `shouldBe` Right (Just Nothing)

    it "ignores a push that deletes a ref" $ do
      body <- fixtureValue "push.json"
      let deletion = body & key "after" .~ String "0000000000000000000000000000000000000000"
      parseGiteaWebhook slug' "push" (encode deletion) `shouldBe` Right Nothing

    it "reads an opened pull request" $ do
      body <- fixture "pull_request_opened.json"
      parseGiteaWebhook slug' "pull_request" body
        `shouldBe` Right
          ( Just
              GiteaPullRequest
                { _giteaWebhookRepo = RepoId slug' "jcitizen" "my-repo",
                  _giteaWebhookPublicity = RepoIsPublic True,
                  _giteaWebhookSender = "jcitizen",
                  _giteaWebhookPrFromFork = Nothing,
                  _giteaWebhookCommit = "2eba238e33607c1fa49253182e9fff42baafa1eb",
                  _giteaWebhookNumber = GhPullRequestId 1
                }
          )

    it "reads a pull request whose head moved" $ do
      body <- fixture "pull_request_synchronized.json"
      fmap (fmap _giteaWebhookCommit) (parseGiteaWebhook slug' "pull_request" body)
        `shouldBe` Right (Just "2eba238e33607c1fa49253182e9fff42baafa1eb")

    it "marks a pull request from another repository as from a fork" $ do
      body <- fixtureValue "pull_request_opened.json"
      let fromFork = body & key "pull_request" . key "head" . key "repo" . key "full_name" .~ String "someone/my-repo"
      fmap (fmap _giteaWebhookPrFromFork) (parseGiteaWebhook slug' "pull_request" (encode fromFork))
        `shouldBe` Right (Just (Just (PrFromFork "someone/my-repo")))

    it "ignores other pull request actions" $ do
      body <- fixture "pull_request_closed.json"
      parseGiteaWebhook slug' "pull_request" body `shouldBe` Right Nothing

    it "ignores other events" $ do
      body <- fixture "push.json"
      parseGiteaWebhook slug' "issues" body `shouldBe` Right Nothing

    it "fails on a payload it cannot read" $ do
      parseGiteaWebhook slug' "push" "{}" `shouldSatisfy` isLeft

  describe "giteaForgeEvent" $ do
    let repoInfo' = RepoInfo ApiTokenCredentials (GhToken "bot-token") (RepoId slug' "jcitizen" "my-repo")

    it "builds a push as a pushed commit, requested by its sender" $ do
      body <- fixture "push.json"
      Right (Just hook) <- pure $ parseGiteaWebhook slug' "push" body
      case giteaForgeEvent repoInfo' hook of
        CommitPushed allowDuplicateRun info -> do
          allowDuplicateRun `shouldBe` False
          info ^. reqUser `shouldBe` ForgeLogin slug' "gitea"
          info ^. branch `shouldBe` Just "develop"
          info ^. commit `shouldBe` "bffeb74224043ba2feb48d137756c8a9331c449a"
          info ^. prFromFork `shouldBe` Nothing
          info ^. repoInfo . repoId `shouldBe` repoInfo' ^. repoId
        _ -> expectationFailure "expected CommitPushed"

    it "builds a pull request as an updated pull request" $ do
      body <- fixture "pull_request_opened.json"
      Right (Just hook) <- pure $ parseGiteaWebhook slug' "pull_request" body
      case giteaForgeEvent repoInfo' hook of
        PullRequestUpdated info prId -> do
          prId `shouldBe` GhPullRequestId 1
          info ^. reqUser `shouldBe` ForgeLogin slug' "jcitizen"
          info ^. branch `shouldBe` Nothing
          info ^. commit `shouldBe` "2eba238e33607c1fa49253182e9fff42baafa1eb"
        _ -> expectationFailure "expected PullRequestUpdated"
