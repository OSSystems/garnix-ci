module Garnix.NixConfig
  ( defaultNixConfig,
    fromNetRcFile,
    getNetRcFileSetting,
    addNixConfigEnvironment,
    githubAccessTokenNixConfig,
    nixConfDefaults,
  )
where

import Cradle
import Data.Map.Strict qualified as Map
import Garnix.Prelude
import Garnix.Types (GhToken (..), NetRcFile (..), NixConfig (..))

defaultNixConfig :: NixConfig
defaultNixConfig =
  NixConfig $ Map.insert "experimental-features" (unwords ["nix-command", "flakes", "pipe-operators"]) mempty

githubAccessTokenNixConfig :: GhToken -> NixConfig
githubAccessTokenNixConfig token = NixConfig $ Map.insert "access-tokens" ("github.com=" <> cs (getGhToken token)) mempty

fromNetRcFile :: NetRcFile -> NixConfig
fromNetRcFile file = NixConfig $ Map.insert "netrc-file" (getNetRcFile file) mempty

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
