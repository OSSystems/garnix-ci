-- | The forge instances garnix talks to besides github.com, read at startup
-- from the JSON file named by @GARNIX_FORGES_FILE@:
--
-- > {
-- >   "git.example": {
-- >     "kind": "gitea",
-- >     "webUrl": "https://git.example.com",
-- >     "apiUrl": "https://git.example.com/api/v1",
-- >     "webhookSecretFile": "/run/secrets/webhook_secret",
-- >     "oauthClientId": "…",
-- >     "oauthClientSecretFile": "/run/secrets/oauth_client_secret",
-- >     "apiTokenFile": "/run/secrets/api_token",
-- >     "admins": ["alice"]
-- >   }
-- > }
--
-- Each key is the instance's 'ForgeSlug'. Secrets live in their own files so
-- the forges file itself can sit in the Nix store.
module Garnix.Forge.Config
  ( readForgesFile,
    parseForgesFile,
    ForgesFileError (..),
    forgeInstance,
  )
where

import Control.Exception (throwIO)
import Control.Exception.Safe qualified as Safe
import Data.Aeson (Value, eitherDecodeStrict')
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Object, Parser, parseEither, withObject, (.!=), (.:), (.:?))
import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Garnix.Forge.Gitea (giteaForgeApi, instanceHost)
import Garnix.Monad (ForgeInstance (..))
import Garnix.Prelude
import Garnix.Types
import Network.URI (URIAuth (..), parseAbsoluteURI, uriAuthority, uriScheme)
import Prelude qualified

newtype ForgesFileError = ForgesFileError Text
  deriving stock (Eq)

instance Show ForgesFileError where
  show (ForgesFileError message) = cs message

instance Exception ForgesFileError

-- | Reads the forges file and the secret files it names. Throws a
-- 'ForgesFileError' naming the file and the instance at fault.
readForgesFile :: FilePath -> IO [ForgeConfig]
readForgesFile path = do
  contents <-
    BS.readFile path `Safe.catchIO` \e ->
      throwIO $ ForgesFileError $ "Cannot read the forges file " <> cs path <> ": " <> show e
  parseForgesFile readSecret contents
    `Safe.catch` \(ForgesFileError message) ->
      throwIO $ ForgesFileError $ "In the forges file " <> cs path <> ": " <> message
  where
    readSecret secretPath =
      BS.readFile secretPath `Safe.catchIO` \e ->
        throwIO $ ForgesFileError $ "cannot read " <> cs secretPath <> ": " <> show e

-- | Parses the forges file, reading secrets with the given function.
parseForgesFile :: (FilePath -> IO StrictByteString) -> StrictByteString -> IO [ForgeConfig]
parseForgesFile readSecret contents = do
  entries <- either (throwIO . ForgesFileError . cs) pure $ do
    value <- eitherDecodeStrict' contents
    parseEither (withObject "forges file" pure) (value :: Value)
  configs <- forM (KeyMap.toList entries) $ \(key, value) -> do
    let slug' = Key.toText key
        failWith :: Text -> IO a
        failWith message = throwIO $ ForgesFileError $ "forge " <> show slug' <> ": " <> message
    either failWith pure $ validateSlug slug'
    entry <- either (failWith . cs) pure $ parseEither (withObject "forge" parseEntry) value
    let secret field secretPath = do
          raw <-
            readSecret secretPath `Safe.catch` \(ForgesFileError message) ->
              failWith $ field <> ": " <> message
          let stripped = T.strip $ T.decodeUtf8Lenient raw
          when (T.null stripped) $ failWith $ field <> ": " <> cs secretPath <> " is empty"
          pure stripped
    webhookSecret' <- secret "webhookSecretFile" (entryWebhookSecretFile entry)
    oAuthClientSecret' <- secret "oauthClientSecretFile" (entryOAuthClientSecretFile entry)
    apiToken' <- secret "apiTokenFile" (entryApiTokenFile entry)
    pure
      ForgeConfig
        { _forgeConfigSlug = ForgeSlug slug',
          _forgeConfigKind = entryKind entry,
          _forgeConfigWebUrl = entryWebUrl entry,
          _forgeConfigApiUrl = entryApiUrl entry,
          _forgeConfigWebhookSecret = T.encodeUtf8 webhookSecret',
          _forgeConfigOAuthClientId = entryOAuthClientId entry,
          _forgeConfigOAuthClientSecret = oAuthClientSecret',
          _forgeConfigApiToken = Just $ GhToken apiToken',
          _forgeConfigAdmins = GhLogin <$> entryAdmins entry
        }
  -- Credentials reach nix as netrc entries, which name a host alone: two
  -- instances on one host would get one entry each for the same machine, and
  -- curl and git would use the first for both.
  -- 'httpUrl' already checked every web URL has a host.
  let slugsByHost = Map.fromListWith (flip (<>)) [(host, [config ^. slug]) | config <- configs, Just host <- [instanceHost config]]
  forM_ (Map.toList slugsByHost) $ \(host, slugs) ->
    when (length slugs > 1)
      $ throwIO
      $ ForgesFileError
      $ "forges "
      <> T.intercalate ", " (map (show . getForgeSlug) slugs)
      <> " are all on the host "
      <> host
      <> "; there can be one forge per host"
  pure configs

-- | The slug names the instance in URLs (@/api/forges/:slug/webhook@), so it
-- has to be a plain path segment. @github@ is github.com, which is configured
-- from the GitHub App settings, never from this file.
validateSlug :: Text -> Either Text ()
validateSlug slug'
  | ForgeSlug (T.toLower slug') == githubForge =
      Left "the slug \"github\" is reserved for github.com, which is not configured in this file"
  | T.null slug' = Left "the slug is empty"
  | not (T.all allowed slug') || slug' `elem` [".", ".."] =
      Left "the slug may only contain letters, digits, '.', '-' and '_'"
  | otherwise = Right ()
  where
    allowed c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ['.', '-', '_']

data Entry = Entry
  { entryKind :: ForgeKind,
    entryWebUrl :: Text,
    entryApiUrl :: Text,
    entryWebhookSecretFile :: FilePath,
    entryOAuthClientId :: Text,
    entryOAuthClientSecretFile :: FilePath,
    entryApiTokenFile :: FilePath,
    entryAdmins :: [Text]
  }

parseEntry :: Object -> Parser Entry
parseEntry o = do
  kind' <-
    o .: "kind" >>= \case
      ("gitea" :: Text) -> pure GiteaForgeKind
      other -> fail $ "unknown kind " <> Prelude.show other <> " (the only kind supported here is \"gitea\", which also covers Forgejo)"
  Entry kind'
    <$> (o .: "webUrl" >>= httpUrl "webUrl")
    <*> (o .: "apiUrl" >>= httpUrl "apiUrl")
    <*> o
    .: "webhookSecretFile"
    <*> o
    .: "oauthClientId"
    <*> o
    .: "oauthClientSecretFile"
    <*> o
    .: "apiTokenFile"
    <*> o
    .:? "admins"
    .!= []
  where
    httpUrl :: String -> Text -> Parser Text
    httpUrl field url = case parseAbsoluteURI (cs url) of
      Just uri
        | uriScheme uri `elem` ["http:", "https:"],
          Just authority <- uriAuthority uri,
          not (null $ uriRegName authority) ->
            pure $ T.dropWhileEnd (== '/') url
      _ -> fail $ field <> " is not an http(s) URL with a host: " <> cs url

-- | How garnix talks to a configured instance. github.com is not one: it is
-- configured from the GitHub App settings.
forgeInstance :: ForgeConfig -> Either ForgesFileError ForgeInstance
forgeInstance config = case config ^. kind of
  GiteaForgeKind ->
    Right
      ForgeInstance
        { _forgeInstanceConfig = config,
          _forgeInstanceForge = giteaForgeApi config
        }
  GithubForgeKind ->
    Left $ ForgesFileError $ "forge " <> show (getForgeSlug $ config ^. slug) <> ": github.com is configured from the GitHub App settings, not from the forges file"
