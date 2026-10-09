module Garnix.TypesSpec where

import Control.Lens
import Data.Aeson hiding (Error)
import Data.Aeson.Lens
import Data.String.Interpolate (i)
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestInstances ()
import Garnix.Types
import Servant (ServerError (..))
import Test.Hspec

spec :: Spec
spec = describe "Types" $ do
  describe "forgeLoginText" $ do
    it "names github accounts by their bare login" $ do
      forgeLoginText (ForgeLogin githubForge "alice") `shouldBe` "alice"
      parseForgeLoginText "alice" `shouldBe` Just (ForgeLogin githubForge "alice")

    it "qualifies accounts on other forges with their slug" $ do
      let login = ForgeLogin (ForgeSlug "git.example") "alice"
      forgeLoginText login `shouldBe` "alice@git.example"
      parseForgeLoginText (forgeLoginText login) `shouldBe` Just login

    it "keeps a slug that itself contains an @" $ do
      let login = ForgeLogin (ForgeSlug "git@corp") "bob"
      forgeLoginText login `shouldBe` "bob@git@corp"
      parseForgeLoginText (forgeLoginText login) `shouldBe` Just login

    it "rejects an empty login or slug" $ do
      parseForgeLoginText "" `shouldBe` Nothing
      parseForgeLoginText "@git.example" `shouldBe` Nothing
      parseForgeLoginText "bob@" `shouldBe` Nothing

  describe "AuthJwtPayload" $ do
    it "names only the account" $ do
      toJSON (WebSession (UserId 123))
        `shouldBe` [aesonQQ| { "id": 123, "session_kind": "web" } |]

    it "roundtrips both session kinds" $ do
      forM_ [WebSession (UserId 123), ApiSession (UserId 123)] $ \payload ->
        eitherDecode' (encode payload) `shouldBe` Right payload

    it "reads sessions minted when an account was one github login" $ do
      now <- getCurrentTime
      let json =
            [i|
              {
                "id": 123,
                "github_login": "some-user",
                "email": "foo@example.org",
                "subscription_type": "free",
                "created_at": #{encode now},
                "session_kind": "web"
              }
            |]
      eitherDecode' (cs json) `shouldBe` Right (WebSession (UserId 123))

    it "reads sessions minted when an account was one login on one forge" $ do
      now <- getCurrentTime
      let json =
            [i|
              {
                "id": 123,
                "forge": "git.example",
                "github_login": "some-user",
                "email": "foo@example.org",
                "subscription_type": "admin",
                "created_at": #{encode now},
                "session_kind": "api"
              }
            |]
      eitherDecode' (cs json) `shouldBe` Right (ApiSession (UserId 123))

    it "refuses sessions minted before the github token moved to the database" $ do
      now <- getCurrentTime
      let json =
            [i|
              {
                "id": 123,
                "github_login": "some-user",
                "email": "foo@example.org",
                "subscription_type": "free",
                "created_at": #{encode now},
                "github_token": "tok"
              }
            |]
      (eitherDecode' (cs json) :: Either String AuthJwtPayload) `shouldSatisfy` isLeft

  describe "asPackageType" $ do
    it "roundtrips correctly"
      $ forM_ [minBound .. maxBound]
      $ \pkgType ->
        review asPackageType pkgType ^? asPackageType `shouldBe` Just pkgType

  describe "servantizeError" $ do
    describe "NoSuchError" $ do
      it "contains the user as a field" $ do
        let error = wrapError Error $ NoSuchUser "test-user"
            Right (json :: Value) = eitherDecode' $ errBody $ servantizeError error
            user = json ^?! key "garnixUser"
        user `shouldBe` "test-user"

  describe "errorDetails" $ do
    it "obfuscates github tokens in `RunProcessError`" $ do
      let token = "ghs_puMag5LeethueBee2oof"
          error = wrapError Error $ RunProcessError "git" ["clone", token] ("stderr: " <> token) ("stdout: " <> token) 1
      userMessage (toErrorDetails error) `shouldBe` "git clone XXXXXXXXXXXXXXXX failed with exit code 1\nStderr:\nstderr: XXXXXXXXXXXXXXXX"

    it "obfuscates github tokens in `OtherError`" $ do
      let token = "ghs_puMag5LeethueBee2oof"
          error = wrapError Error $ OtherError ("token: " <> token)
      userMessage (toErrorDetails error) `shouldBe` "token: XXXXXXXXXXXXXXXX"

  describe "buildComment" $ do
    it "converts builds to comments" $ runTestM $ do
      build <- testBuild identity
      liftIO
        $ buildComment build
        `shouldBe` cs
          [i|#{pretty (build ^. id)}_test-owner/test-repo/test-branch|]
