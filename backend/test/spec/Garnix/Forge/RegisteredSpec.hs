module Garnix.Forge.RegisteredSpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Garnix.Access (identityAdministers)
import Garnix.Forge.Registered
import Garnix.Monad (ForgeInstance (..))
import Garnix.Prelude
import Garnix.TestHelpers (testForgeInstance)
import Garnix.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "normaliseRegistrationUrl" $ do
    let normalise = normaliseRegistrationUrl False
        refusal url = either identity (const "") (normalise url)

    it "lower-cases the host and drops a trailing slash or dot" $ do
      normalise "https://Git.Example.COM/" `shouldBe` Right (RegistrationUrl "git.example.com" "https://git.example.com")
      normalise "  https://git.example.com.  " `shouldBe` Right (RegistrationUrl "git.example.com" "https://git.example.com")

    it "keeps a port in the URL but not in the slug" $ do
      normalise "https://git.example.com:3000" `shouldBe` Right (RegistrationUrl "git.example.com" "https://git.example.com:3000")

    it "refuses plain http unless allowed" $ do
      refusal "http://git.example.com" `shouldBe` "garnix only registers forges served over https"
      normaliseRegistrationUrl True "http://git.example.com" `shouldBe` Right (RegistrationUrl "git.example.com" "http://git.example.com")
      refusal "ftp://git.example.com" `shouldBe` "garnix only registers forges served over https"

    it "refuses a forge under a sub-path, pointing to the configuration" $ do
      let message = refusal "https://example.com/gitea"
      ("not under the path /gitea" `T.isInfixOf` message) `shouldBe` True
      ("services.garnixServer.forges" `T.isInfixOf` message) `shouldBe` True

    it "refuses credentials, queries, fragments and hosts that cannot be slugs" $ do
      isLeft (normalise "https://user:pass@git.example.com") `shouldBe` True
      isLeft (normalise "https://git.example.com/?a=b") `shouldBe` True
      isLeft (normalise "https://git.example.com/#x") `shouldBe` True
      isLeft (normalise "https://[::1]") `shouldBe` True
      isLeft (normalise "https://github") `shouldBe` True
      isLeft (normalise "git.example.com") `shouldBe` True

  describe "mayManageRegisteredForge" $ do
    let gitea = ForgeSlug "git.example.com"
        registrant = UserId 1
        someoneElse = UserId 2

    it "lets the account that registered it manage it, whatever its identities" $ do
      mayManageRegisteredForge gitea (Just registrant) registrant Nothing `shouldBe` True

    it "lets an administrator of that forge manage it" $ do
      mayManageRegisteredForge gitea (Just registrant) someoneElse (Just $ ForgeIdentity gitea "root" True) `shouldBe` True

    it "lets nobody else manage it" $ do
      mayManageRegisteredForge gitea (Just registrant) someoneElse (Just $ ForgeIdentity gitea "bob" False) `shouldBe` False
      mayManageRegisteredForge gitea Nothing someoneElse Nothing `shouldBe` False

    it "never counts what another forge says of an identity" $ do
      mayManageRegisteredForge gitea (Just registrant) someoneElse (Just $ ForgeIdentity githubForge "root" True) `shouldBe` False
      mayManageRegisteredForge gitea (Just registrant) someoneElse (Just $ ForgeIdentity (ForgeSlug "other.example") "root" True) `shouldBe` False

  describe "identityAdministers" $ do
    let gitea = ForgeSlug "git.example.com"
        configOf slug' = _forgeInstanceConfig (testForgeInstance slug' GiteaForgeKind)

    it "makes a registered forge's administrators admins of its repositories" $ do
      identityAdministers Registered (configOf gitea) (ForgeIdentity gitea "root" True) `shouldBe` True
      identityAdministers Registered (configOf gitea) (ForgeIdentity gitea "root" False) `shouldBe` False

    it "grants nothing on another forge" $ do
      identityAdministers Registered (configOf gitea) (ForgeIdentity (ForgeSlug "other.example") "root" True) `shouldBe` False

    it "ignores what github or a configured forge says, keeping to the configured admins" $ do
      identityAdministers Configured (configOf githubForge) (ForgeIdentity githubForge "staff" True) `shouldBe` False
      identityAdministers Configured (configOf gitea) (ForgeIdentity gitea "root" True) `shouldBe` False
      identityAdministers Configured (configOf gitea & admins .~ ["root"]) (ForgeIdentity gitea "root" False) `shouldBe` True

  describe "withLiveIdentities" $ do
    let time = UTCTime (fromGregorian 2026 1 1) 0
        github = ForgeIdentity githubForge "alice" False
        gitea = ForgeIdentity (ForgeSlug "git.example.com") "alice" False
        account = User (UserId 1) (Email "alice@example.com") time [github, gitea]

    it "keeps only the identities on live forges" $ do
      ((^. identities) <$> withLiveIdentities [github] account) `shouldBe` Just [github]

    it "leaves an account with none of them no session" $ do
      withLiveIdentities [] account `shouldBe` Nothing
