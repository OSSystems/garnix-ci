module Garnix.Forge.RegisteredSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Garnix.API.Forges (ForgeSummary (..), Management (..), StartDecision (..), SummaryStatus (..), disabledCandidates, forgeSummaries, managementRefusal, startDecision)
import Garnix.Access (identityAdministers)
import Garnix.DB.Forges (RegisteredForgeRow (..))
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

  describe "pendingLoginRefusal" $ do
    let gitea = ForgeSlug "git.example.com"

    it "lets anybody log in through an active forge" $ do
      pendingLoginRefusal gitea ForgeActive False `shouldBe` Nothing

    it "lets only the browser that registered a pending forge log in through it" $ do
      pendingLoginRefusal gitea ForgePending True `shouldBe` Nothing
      isJust (pendingLoginRefusal gitea ForgePending False) `shouldBe` True

    it "lets nobody log in through a disabled forge" $ do
      pendingLoginRefusal gitea ForgeDisabled True `shouldBe` Just NotFound

  describe "pendingLogin" $ do
    let gitea = ForgeSlug "git.example.com"

    it "completes a pending registration from the browser holding its token, and only then" $ do
      pendingLogin gitea ForgePending (Just "hash") (Just "hash") `shouldBe` Right (Just "hash")
      isLeft (pendingLogin gitea ForgePending (Just "hash") (Just "other")) `shouldBe` True
      isLeft (pendingLogin gitea ForgePending (Just "hash") Nothing) `shouldBe` True
      isLeft (pendingLogin gitea ForgePending Nothing Nothing) `shouldBe` True

    it "lets anybody through an active forge, completing nothing" $ do
      pendingLogin gitea ForgeActive Nothing Nothing `shouldBe` Right Nothing
      pendingLogin gitea ForgeActive Nothing (Just "hash") `shouldBe` Right Nothing

    it "lets nobody through a disabled forge" $ do
      pendingLogin gitea ForgeDisabled (Just "hash") (Just "hash") `shouldBe` Left NotFound

  describe "startDecision" $ do
    let configured = ForgeSlug "configured.example"
        gitea = ForgeSlug "git.example.com"

    it "logs in through a configured forge, whatever else" $ do
      startDecision (Just configured) False Nothing `shouldBe` LoginThrough configured
      startDecision (Just configured) True (Just (gitea, Just ForgeDisabled)) `shouldBe` LoginThrough configured

    it "knows no other forge while registration is off" $ do
      startDecision Nothing False Nothing `shouldBe` UnknownForge

    it "logs in through an active registered forge, and registers any other" $ do
      startDecision Nothing True (Just (gitea, Just ForgeActive)) `shouldBe` LoginThrough gitea
      startDecision Nothing True (Just (gitea, Nothing)) `shouldBe` RegisterAt gitea
      startDecision Nothing True (Just (gitea, Just ForgePending)) `shouldBe` RegisterAt gitea
      startDecision Nothing True (Just (gitea, Just ForgeDisabled)) `shouldBe` RegisterAt gitea

  describe "managementRefusal" $ do
    let gitea = ForgeSlug "git.example.com"
        time = UTCTime (fromGregorian 2026 1 1) 0
        row status' =
          RegisteredForgeRow
            { rowSlug = gitea,
              rowWebUrl = "https://git.example.com",
              rowApiUrl = "https://git.example.com/api/v1",
              rowOAuthClientId = "client",
              rowOAuthClientSecret = EncryptedText "",
              rowWebhookSecret = EncryptedText "",
              rowStatus = status',
              rowRegisteredBy = Just (UserId 1),
              rowCreatedAt = time,
              rowRegistrationTokenHash = Nothing,
              rowUpdatedAt = time
            }
        account id' identities' = User (UserId id') (Email "someone@example.com") time identities'

        refused management status' account' = isJust (managementRefusal management (row status') account')

    it "lets the registrant, or an administrator of it, manage an active forge" $ do
      forM_ [ReplaceSecret, Disable] $ \management -> do
        refused management ForgeActive (account 1 []) `shouldBe` False
        refused management ForgeActive (account 2 [ForgeIdentity gitea "root" True]) `shouldBe` False
        refused management ForgeActive (account 2 [ForgeIdentity gitea "bob" False]) `shouldBe` True
        refused management ForgeActive (account 2 [ForgeIdentity githubForge "root" True]) `shouldBe` True

    it "lets the registrant, or an administrator of it, re-enable a disabled forge with a new secret" $ do
      refused ReplaceSecret ForgeDisabled (account 1 []) `shouldBe` False
      refused ReplaceSecret ForgeDisabled (account 2 [ForgeIdentity gitea "root" True]) `shouldBe` False
      refused ReplaceSecret ForgeDisabled (account 2 [ForgeIdentity gitea "bob" False]) `shouldBe` True

    it "disables no disabled forge, and manages no pending one, whoever asks" $ do
      managementRefusal Disable (row ForgeDisabled) (account 1 []) `shouldBe` Just NotFound
      refused ReplaceSecret ForgePending (account 1 []) `shouldBe` True
      refused Disable ForgePending (account 1 []) `shouldBe` True

  describe "forgeSummaries" $ do
    let gitea = ForgeSlug "git.example.com"
        time = UTCTime (fromGregorian 2026 1 1) 0
        row status' =
          RegisteredForgeRow
            { rowSlug = gitea,
              rowWebUrl = "https://git.example.com",
              rowApiUrl = "https://git.example.com/api/v1",
              rowOAuthClientId = "client",
              rowOAuthClientSecret = EncryptedText "",
              rowWebhookSecret = EncryptedText "",
              rowStatus = status',
              rowRegisteredBy = Just (UserId 1),
              rowCreatedAt = time,
              rowRegistrationTokenHash = Nothing,
              rowUpdatedAt = time
            }
        registrant = User (UserId 1) (Email "alice@example.com") time [ForgeIdentity githubForge "alice" False]
        stranger = User (UserId 2) (Email "bob@example.com") time [ForgeIdentity gitea "bob" False]
        active = [(Configured, testForgeInstance githubForge GithubForgeKind), (Registered, testForgeInstance gitea GiteaForgeKind)]
        listed manager' active' activeRows disabledRows =
          [ (_forgeSummarySlug summary, _forgeSummaryStatus summary, _forgeSummaryCanManage summary)
          | summary <- forgeSummaries manager' active' activeRows disabledRows
          ]

    it "says which active registered forge the caller may manage" $ do
      listed (Just registrant) active [row ForgeActive] []
        `shouldBe` [(githubForge, SummaryStatus ForgeActive, False), (gitea, SummaryStatus ForgeActive, True)]
      listed (Just stranger) active [row ForgeActive] []
        `shouldBe` [(githubForge, SummaryStatus ForgeActive, False), (gitea, SummaryStatus ForgeActive, False)]

    it "lets nobody manage a configured forge, nor anything without a caller" $ do
      listed (Just registrant) [(Configured, testForgeInstance gitea GiteaForgeKind)] [row ForgeActive] []
        `shouldBe` [(gitea, SummaryStatus ForgeActive, False)]
      listed Nothing active [row ForgeActive] [row ForgeDisabled]
        `shouldBe` [(githubForge, SummaryStatus ForgeActive, False), (gitea, SummaryStatus ForgeActive, False)]

    it "lists a disabled forge only to whoever may bring it back" $ do
      listed (Just registrant) (take 1 active) [] [row ForgeDisabled]
        `shouldBe` [(githubForge, SummaryStatus ForgeActive, False), (gitea, SummaryStatus ForgeDisabled, True)]
      listed (Just stranger) (take 1 active) [] [row ForgeDisabled]
        `shouldBe` [(githubForge, SummaryStatus ForgeActive, False)]
      -- A row read as disabled that is no longer is not listed twice.
      listed (Just registrant) active [row ForgeActive] [row ForgeActive]
        `shouldBe` [(githubForge, SummaryStatus ForgeActive, False), (gitea, SummaryStatus ForgeActive, True)]

  describe "disabledCandidates" $ do
    it "looks for disabled forges only among the account's identities on no active or configured forge" $ do
      let time = UTCTime (fromGregorian 2026 1 1) 0
          gitea = ForgeSlug "git.example.com"
          configured = ForgeSlug "code.example.com"
          account =
            User
              (UserId 1)
              (Email "alice@example.com")
              time
              [ForgeIdentity githubForge "alice" False, ForgeIdentity gitea "alice" False, ForgeIdentity configured "alice" False]
      disabledCandidates (Map.fromList [(configured, testForgeInstance configured GiteaForgeKind)]) [githubForge] account
        `shouldBe` [gitea]
      disabledCandidates mempty [githubForge, gitea] account `shouldBe` [configured]

  describe "forgetOnActivation" $ do
    it "forgets everyone on the forge unless it brings back one disabled within the quarantine" $ do
      map (\lastDisabled' -> (lastDisabled', forgetOnActivation lastDisabled')) [minBound .. maxBound]
        `shouldBe` [ (NeverDisabled, True),
                     (DisabledWithinQuarantine, False),
                     (DisabledBeforeQuarantine, True)
                   ]

    it "holds a disabled forge for 30 days" $ do
      disabledForgeQuarantine `shouldBe` 30 * 24 * 60 * 60

  describe "withLiveIdentities" $ do
    let time = UTCTime (fromGregorian 2026 1 1) 0
        github = ForgeIdentity githubForge "alice" False
        gitea = ForgeIdentity (ForgeSlug "git.example.com") "alice" False
        account = User (UserId 1) (Email "alice@example.com") time [github, gitea]

    it "keeps only the identities on live forges" $ do
      ((^. identities) <$> withLiveIdentities [github] account) `shouldBe` Just [github]

    it "leaves an account with none of them no session" $ do
      withLiveIdentities [] account `shouldBe` Nothing
