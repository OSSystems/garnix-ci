module Garnix.NixConfig
  ( defaultNixConfig,
    fromNetRcFile,
    getNetRcFileSetting,
    addNixConfigEnvironment,
    githubAccessTokenNixConfig,
    githubAccessToken,
    nixConfDefaults,
    NetRcEntry (..),
    renderNetRcEntries,
  )
where

import Cradle
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Garnix.Prelude
import Garnix.Types (GhToken (..), NetRcFile (..), NixConfig (..), accessTokensSetting)
import Prelude qualified

defaultNixConfig :: NixConfig
defaultNixConfig =
  NixConfig $ Map.insert "experimental-features" (unwords ["nix-command", "flakes", "pipe-operators"]) mempty

githubAccessTokenNixConfig :: GhToken -> NixConfig
githubAccessTokenNixConfig token = NixConfig $ Map.insert accessTokensSetting ("github.com=" <> cs (getGhToken token)) mempty

-- | The token nix uses for github.com, set by 'githubAccessTokenNixConfig'.
githubAccessToken :: NixConfig -> Maybe GhToken
githubAccessToken config = do
  tokens <- Map.lookup accessTokensSetting (getNixConfig config)
  listToMaybe [GhToken (cs token) | Just token <- stripPrefix "github.com=" <$> words tokens]

fromNetRcFile :: NetRcFile -> NixConfig
fromNetRcFile file = NixConfig $ Map.insert "netrc-file" (getNetRcFile file) mempty

-- | One @machine@ of a netrc file.
data NetRcEntry = NetRcEntry
  { _netRcEntryMachine :: Text,
    _netRcEntryLogin :: Text,
    _netRcEntryPassword :: Text
  }
  deriving stock (Eq)

instance Show NetRcEntry where
  show (NetRcEntry machine login _password) =
    "NetRcEntry " <> Prelude.show machine <> " " <> Prelude.show login <> " <password>"

renderNetRcEntries :: [NetRcEntry] -> Text
renderNetRcEntries = T.unlines . concatMap render
  where
    render (NetRcEntry machine login password) =
      ["machine " <> machine, "login " <> login, "password " <> password]

getNetRcFileSetting :: NixConfig -> Maybe NetRcFile
getNetRcFileSetting (NixConfig m) = NetRcFile <$> Map.lookup "netrc-file" m

formatConfig :: NixConfig -> String
formatConfig (NixConfig config) =
  intercalate "\n" $ map (\(key, value) -> key <> " = " <> value) $ Map.assocs config

addNixConfigEnvironment :: NixConfig -> ProcessConfiguration -> ProcessConfiguration
addNixConfigEnvironment config =
  modifyEnvVar
    "NIX_CONFIG"
    $ \case
      Nothing -> Just $ formatConfig config
      Just existing -> Just $ existing <> "\n" <> formatConfig config

nixConfDefaults :: ProcessConfiguration -> ProcessConfiguration
nixConfDefaults = addArgs ["--extra-experimental-features", "nix-command flakes" :: Text]
