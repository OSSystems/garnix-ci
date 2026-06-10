{-# LANGUAGE TemplateHaskell #-}

module Garnix.Nix.Types
  ( DrvPath (..),
    StoreHash (..),
    Plan (..),
    planDrvPaths,
    planOutputs,
    planDrvPathsAndOutputs,
    StorePath (..),
    parseStorePath,
    parseDrvPath,
    getStorePath,
    getRelativeStorePath,
    AppExecPath (..),
    BuildOutputs (..),
    getOutputByName,
    parseDerivationShow,
    ensureStorePrefix,
  )
where

import Data.Aeson (Value (Object), withObject)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Either.Extra
import Data.Hashable (Hashable)
import Data.Map qualified as Map
import Data.String.Conversions (LBS)
import Data.Text qualified as T
import Garnix.Prelude
import Prelude qualified (show)

newtype StoreHash = StoreHash {getStoreHash :: Text}
  deriving stock (Eq, Show, Generic, Ord)
  deriving newtype (PGParameter "text", PGColumn "text", Hashable)

instance ConvertibleStrings StoreHash Text where
  convertString :: StoreHash -> Text
  convertString = getStoreHash

instance ConvertibleStrings StoreHash String where
  convertString :: StoreHash -> String
  convertString = cs . getStoreHash

newtype DrvPath = DrvPath {getDrvPath :: StorePath}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)

instance Show DrvPath where
  show (DrvPath storePath) = "DrvPath " <> cs (show (getStorePath storePath))

instance ConvertibleStrings DrvPath Text where
  convertString :: DrvPath -> Text
  convertString = cs . getDrvPath

instance ConvertibleStrings DrvPath FilePath where
  convertString :: DrvPath -> FilePath
  convertString = cs . getDrvPath

data StorePath = StorePath {getHash :: StoreHash, getName :: Text}
  deriving stock (Eq, Generic, Ord)

instance Show StorePath where
  show storePath = "StorePath " <> cs (show (getStorePath storePath))

instance ConvertibleStrings StorePath Text where
  convertString :: StorePath -> Text
  convertString = getStorePath

instance ConvertibleStrings StorePath String where
  convertString :: StorePath -> String
  convertString = cs . getStorePath

instance ConvertibleStrings StorePath LBS where
  convertString :: StorePath -> LBS
  convertString = cs . getStorePath

getRelativeStorePath :: StorePath -> Text
getRelativeStorePath storePath = cs (getHash storePath) <> "-" <> getName storePath

getStorePath :: StorePath -> Text
getStorePath storePath = "/nix/store/" <> getRelativeStorePath storePath

parseStorePath :: (ConvertibleStrings a Text) => a -> Either Text StorePath
parseStorePath path = do
  let pathT = cs path
      handleInvalid = maybeToEither $ pathT <> " is not a valid nix store path"
  withoutNixStorePrefix <- handleInvalid $ T.stripPrefix "/nix/store/" pathT
  let hash = StoreHash $ T.take 32 withoutNixStorePrefix
  name <- handleInvalid (T.stripPrefix (cs hash <> "-") withoutNixStorePrefix)
  Right $ StorePath hash name

parseDrvPath :: (ConvertibleStrings a Text) => a -> Either Text DrvPath
parseDrvPath path = do
  storePath <- parseStorePath path
  when (not (".drv" `T.isSuffixOf` cs storePath)) $ do
    throwError $ "not a drv path: " <> cs path
  pure $ DrvPath storePath

newtype Plan = Plan [(DrvPath, [StorePath])]
  deriving stock (Show, Eq)

planDrvPaths :: Plan -> [DrvPath]
planDrvPaths (Plan plan) = fmap fst plan

planOutputs :: Plan -> [StorePath]
planOutputs (Plan plan) = concatMap snd plan

planDrvPathsAndOutputs :: Plan -> [StorePath]
planDrvPathsAndOutputs plan = fmap getDrvPath (planDrvPaths plan) <> planOutputs plan

newtype AppExecPath = AppExecPath {getAppExecPath :: Text}
  deriving newtype (Show)

newtype BuildOutputs = BuildOutputs {getBuildOutputs :: Map.Map Text StorePath}
  deriving stock (Show, Eq)

getOutputByName :: Text -> BuildOutputs -> Maybe StorePath
getOutputByName outputName (BuildOutputs map) = Map.lookup outputName map

parseDerivationShow :: (FromJSON inner) => Value -> Parser (Map.Map Text inner)
parseDerivationShow = withObject "nix derivation show output" $ \o ->
  case KeyMap.lookup "derivations" o of
    Just derivations -> parseJSON derivations
    Nothing -> parseJSON (Object o)

ensureStorePrefix :: Text -> Text
ensureStorePrefix path
  | "/nix/store/" `T.isPrefixOf` path = path
  | otherwise = "/nix/store/" <> path

makeFields ''Plan
