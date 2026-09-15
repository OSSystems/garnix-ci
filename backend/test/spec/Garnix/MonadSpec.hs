module Garnix.MonadSpec (spec) where

import Control.Lens
import Data.Aeson (Value, eitherDecode')
import Data.Aeson.Lens (atKey, key, _String)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Yaml (decodeThrow)
import Data.Yaml.TH (yamlQQ)
import Garnix.Monad
import Garnix.Monad.Async (joinAll_, resolve, spawn)
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.Monad
import Garnix.Types hiding (pending)
import System.IO.Silently (capture_, silence)
import System.Log.FastLogger as FastLogger
import System.Random (randomRIO)
import Test.Hspec hiding (shouldThrow)
import Test.Hspec.QuickCheck

spec :: Spec
spec = around_ silence $ do
  describe "the build owner allowlist" $ do
    describe "parseAllowedOwners" $ do
      it "treats an unset, empty or separator-only variable as no allowlist" $ do
        parseAllowedOwners Nothing `shouldBe` Nothing
        parseAllowedOwners (Just "") `shouldBe` Nothing
        parseAllowedOwners (Just " , ") `shouldBe` Nothing

      it "splits on commas and trims each entry"
        $ parseAllowedOwners (Just "one, two ,three")
        `shouldBe` Just ["one", "two", "three"]

      -- GitHub logins are case-insensitive, but what an operator types into the
      -- deployment is not necessarily the casing GitHub sends on the webhook.
      it "normalizes the casing an operator happened to type"
        $ parseAllowedOwners (Just "KraDalby")
        `shouldBe` Just ["kradalby"]

    describe "isAllowedOwner" $ do
      it "allows every owner when no allowlist is configured"
        $ isAllowedOwner Nothing "anyone"
        `shouldBe` True

      it "allows a listed owner and denies an unlisted one" $ do
        isAllowedOwner (parseAllowedOwners (Just "kradalby,myorg")) "myorg" `shouldBe` True
        isAllowedOwner (parseAllowedOwners (Just "kradalby,myorg")) "someone-else" `shouldBe` False

      it "matches regardless of the casing on either side" $ do
        isAllowedOwner (parseAllowedOwners (Just "KraDalby")) "kradalby" `shouldBe` True
        isAllowedOwner (parseAllowedOwners (Just "kradalby")) "KraDalby" `shouldBe` True

  describe "<?>" $ do
    modifyMaxSuccess (const 5) $ do
      prop "doesn't change the result of a success" $ \(value :: Int) -> runTestM $ do
        pure value <?> "foo" `shouldReturnM` value
      prop "doesn't change the result of a failure" $ \(value :: Text) ->
        throw (OtherError value) <?> "foo" `shouldThrow` OtherError value
      prop "logs a success" $ \(value :: Int) -> runTestM $ do
        pure value <?> "foo" `shouldLog` ["foo", "foo - DONE"]
      prop "logs a failure" $ \(value :: Text) -> runTestM $ do
        throw (OtherError value) <?> "foo" `shouldLog` ["foo", "foo - FAILED: OtherError: " <> value]

  describe "log" $ inM $ do
    it "logs messages" $ do
      log Informational "Some log message" `shouldLog` ["Some log message"]

    it "logs information from withSpans" $ do
      [logEntry] <- captureLogLines_ $ withSpan (PackageName "foo") $ do
        log Informational "Some log message"
      logEntry
        `shouldBeM` cs [i|{"logLevel":"Informational","span_package":"foo","message":"Some log message"}|]

    it "logs build ids" $ do
      [logEntry] <- captureLogLines_ $ withSpan (BuildId ("NVBeYB1w" ^?! hashIdText)) $ do
        log Informational "Some log message"
      logEntry
        `shouldBeM` cs [i|{"logLevel":"Informational","span_buildId":"NVBeYB1w(42)","message":"Some log message"}|]

    it "logs information from withTextSpan" $ do
      [logEntry] <- captureLogLines_ $ withTextSpan ("foo", "bar") $ do
        log Informational "Some log message"
      logEntry
        `shouldBeM` cs [i|{"logLevel":"Informational","span_foo":"bar","message":"Some log message"}|]

    it "logs spans as json" $ do
      let commitInfo = CommitInfo "owner" (RepoIsPublic True) (RepoInfo undefined undefined "owner" "repo") (Just "branch") Nothing "aaaaaa"
      [logEntry] <- captureLogLines_ $ withSpan commitInfo $ do
        log Informational "Some log message"
      let expected :: Value =
            [yamlQQ|
                logLevel: Informational
                span_req_user: owner
                span_public: "True"
                span_gh_owner: owner
                span_gh_repo: repo
                span_branch: branch
                span_commit: aaaaaa
                message: "Some log message"
              |]
      eitherDecode' (cs logEntry) `shouldBeM` Right expected

    it "escapes special characters, including quotes and newlines" $ do
      [logEntry] <- captureLogLines_
        $ withTextSpan ("a", "\"contains quotes\"")
        $ withTextSpan ("b", "contains\nnewlines")
        $ do
          log Informational "Some log message"
      let expected :: Value =
            [yamlQQ|
                logLevel: Informational
                span_a: "\"contains quotes\""
                span_b: "contains\nnewlines"
                message: "Some log message"
              |]
      eitherDecode' (cs logEntry) `shouldBeM` Right expected

  describe "concurrent logging" $ do
    it "keeps long, concurrent messages intact" $ do
      replicateM_ 100 $ do
        batches :: [[Text]] <- liftIO $ replicateM 10 $ replicateM 10 $ do
          line :: [Char] <- replicateM 100 $ randomRIO ('a', 'z')
          pure $ cs line
        logs <- capture_ $ runTestM $ do
          promises <- forM batches $ \lines -> do
            spawn $ forM_ lines $ log Informational
          joinAll_ promises >>= resolve
        T.unlines (sort (T.lines $ cs logs))
          `shouldBe` T.unlines
            ( map
                (\line -> "{\"logLevel\":\"Informational\",\"message\":\"" <> line <> "\"}")
                (sort (join batches))
            )

  describe "logThrownErrors" $ inM $ do
    it "logs monadic errors from 'throw' as Error" $ do
      [logEntry] <- captureLogs_ $ void $ try $ do
        logThrownErrors $ do
          withTextSpan ("foo", "bar") $ do
            throw $ OtherError "test error"
      liftIO $ msg logEntry `shouldMatchRegexp` "^OtherError: test error"
      logEntry ^. #severity `shouldBeM` Error

    it "logs monadic errors from 'shortcut as Informational" $ do
      [logEntry] <- captureLogs_ $ void $ try $ do
        logThrownErrors $ do
          withTextSpan ("foo", "bar") $ do
            shortcut $ OtherError "test error"
      logEntry ^. #severity `shouldBeM` Informational

    it "keeps the spans context from where the monadic error was thrown" $ do
      [logEntry] <- captureLogLines_ $ void $ try $ do
        logThrownErrors $ do
          withTextSpan ("foo", "bar") $ do
            throw $ OtherError "test error"
      yaml <- decodeThrow (cs logEntry)
      (yaml & atKey "message" .~ Nothing)
        `shouldBeM` [yamlQQ|
                      logLevel: Error
                      span_foo: bar
                    |]

shouldLog :: (HasCallStack) => M a -> [Text] -> M ()
shouldLog action expectedLogs = do
  logs <- captureLogLines_ $ void $ try action
  fmap getMessage logs `shouldBeM` expectedLogs
  where
    getMessage json = json ^. key "message" . _String . to (fst . T.breakOn "\n\nCallStack ")

captureLogLines_ :: M () -> M [Text]
captureLogLines_ action = do
  logItems <- captureLogs_ action
  pure $ fmap (decodeUtf8 . FastLogger.fromLogStr . FastLogger.toLogStr) logItems
