{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Garnix.Types
  ( module Garnix.Types,
    module Garnix.Types.ExternalLenses,
    module Garnix.Types.Keys,
  )
where

import Control.Lens
import Control.Lens.Regex.Text qualified as RE
import Control.Monad (mzero)
import Data.Aeson (withObject)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Encoding qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap (toMapText)
import Data.Aeson.Types qualified as JSON
import Data.ByteString.Lazy qualified as BSL
import Data.Generics.Product (HasField' (..))
import Data.List.Extra (enumerate)
import Data.Map qualified as Map
import Data.Map.Strict qualified as StrictMap
import Data.Pool (Pool)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Typed.Types (PGStringType)
import Garnix.Nix.Types qualified as Nix
import Garnix.Prelude
import Garnix.Types.ExternalLenses
import Garnix.Types.Keys
import GitHub.App.Auth (InstallationAuth)
import Network.HTTP.Types (Header, statusMessage)
import Prettyprinter qualified as Pretty
import Servant qualified
import System.Log.FastLogger as FastLogger
import Prelude qualified

-- * System

data System
  = X8664Linux
  | X8664Darwin
  | AArch64Linux
  | AArch64Darwin
  | AArmV6LLinux
  | AArmV7LLinux
  | I686Linux
  | MIPSelLinux
  | OtherSystem Text
  deriving stock (Eq, Show, Generic, Ord)

supportedSystems :: [System]
supportedSystems = [X8664Linux, AArch64Darwin, AArch64Linux, X8664Darwin, I686Linux]

systemTextIso :: Iso' System Text
systemTextIso = iso from' to'
  where
    from' x = case x of
      X8664Linux -> "x86_64-linux"
      X8664Darwin -> "x86_64-darwin"
      AArch64Linux -> "aarch64-linux"
      AArch64Darwin -> "aarch64-darwin"
      AArmV6LLinux -> "armv6l-linux"
      AArmV7LLinux -> "armv7l-linux"
      I686Linux -> "i686-linux"
      MIPSelLinux -> "mipsel-linux"
      OtherSystem s -> s
    to' x = case x of
      "x86_64-linux" -> X8664Linux
      "x86_64-darwin" -> X8664Darwin
      "aarch64-linux" -> AArch64Linux
      "aarch64-darwin" -> AArch64Darwin
      "armv6l-linux" -> AArmV6LLinux
      "armv7l-linux" -> AArmV7LLinux
      "i686-linux" -> I686Linux
      "mipsel-linux" -> MIPSelLinux
      _ -> OtherSystem x

instance Pretty System where
  pretty x = pretty $ x ^. systemTextIso

instance ToJSON System where
  toJSON x = Aeson.String $ x ^. systemTextIso

instance FromJSON System where
  parseJSON = Aeson.withText "system" $ \t -> pure $ t ^. from systemTextIso

instance ToJSONKey System where
  toJSONKey = contramap (^. systemTextIso) Aeson.toJSONKey

instance FromJSONKey System where
  fromJSONKey = fmap (^. from systemTextIso) Aeson.fromJSONKey

-- | This is a 'Maybe System', but we use a special datatype because it
-- is not encoded as NULL in the DB (since it's part of the primary key), but
-- as 'noSystem'.
--
-- It is 'noSystem' for 'buildStarting'.
data MaybeSystem = IsSystem System | NoSystem
  deriving stock (Eq, Show, Ord, Generic)

instance Pretty MaybeSystem where
  pretty = \case
    IsSystem s -> pretty s
    NoSystem -> "none"

maybeSystemIso :: Iso' MaybeSystem (Maybe System)
maybeSystemIso = iso from' to'
  where
    from' x = case x of
      NoSystem -> Nothing
      IsSystem s -> Just s
    to' x = case x of
      Nothing -> NoSystem
      Just s -> IsSystem s

instance ToJSON MaybeSystem where
  toJSON x = Aeson.toJSON $ x ^. maybeSystemIso

instance FromJSON MaybeSystem where
  parseJSON x = (^. from maybeSystemIso) <$> Aeson.parseJSON x

instance Servant.FromHttpApiData MaybeSystem where
  parseUrlPiece piece = case piece of
    "noSystem" -> pure NoSystem
    _ -> pure $ IsSystem $ piece ^. from systemTextIso

instance PGType "system" where
  type PGVal "system" = MaybeSystem

instance PGParameter "system" MaybeSystem where
  pgEncode _ msys = case msys of
    NoSystem -> "noSystem"
    IsSystem sys -> sys ^. systemTextIso . to cs

instance PGColumn "system" MaybeSystem where
  pgDecode _ msys = case msys of
    "noSystem" -> NoSystem
    sys -> IsSystem $ sys ^. to cs . from systemTextIso

instance PGType "citext" where
  type PGVal "citext" = Text

instance PGStringType "citext"

-- * Packages

data SystemPackage = SystemPackage
  { systemPackageSystem :: System,
    systemPackagePackage :: Package
  }
  deriving stock (Eq, Show, Generic)

newtype Package = Package {_packageName :: Text}
  deriving stock (Eq, Show, Generic)

instance ToJSON Package where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON Package where parseJSON = ourParseJSON

newtype PackageName = PackageName {getPackageName :: Text}
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      ToJSONKey,
      FromJSONKey,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      PGParameter "character varying",
      PGParameter "text",
      PGColumn "character varying",
      PGColumn "text",
      Pretty,
      IsString
    )

instance ConvertibleStrings Text PackageName where
  convertString = PackageName

instance ConvertibleStrings PackageName Text where
  convertString = getPackageName

instance Inject Text PackageName where inj = cs

instance Project Text PackageName where prj = cs

instance Isomorphic Text PackageName

data PackageType
  = TypePackage
  | TypeCheck
  | TypeHomeConfiguration
  | TypeDarwinConfiguration
  | TypeNixosConfiguration
  | TypeDevShell
  | TypeDefaultDevShell
  | TypeDefaultPackage
  | TypeApp
  | TypeOverall
  deriving stock (Eq, Show, Enum, Bounded, Ord, Generic)

instance PGType "package_type" where
  type PGVal "package_type" = PackageType

asPackageType :: Prism' Text PackageType
asPackageType = prism there back
  where
    there i = case lookup i asPackageTypePairs of
      Just x -> x
      Nothing -> error $ "missing pattern: " <> show i
    back i = case lookup i (swap <$> asPackageTypePairs) of
      Just x -> Right x
      Nothing -> Left i

asPackageTypePairs :: [(PackageType, Text)]
asPackageTypePairs = annotate <$> enumerate @PackageType
  where
    annotate :: PackageType -> (PackageType, Text)
    annotate = \case
      TypePackage -> (TypePackage, "package")
      TypeCheck -> (TypeCheck, "check")
      TypeHomeConfiguration -> (TypeHomeConfiguration, "homeConfiguration")
      TypeDarwinConfiguration -> (TypeDarwinConfiguration, "darwinConfiguration")
      TypeNixosConfiguration -> (TypeNixosConfiguration, "nixosConfiguration")
      TypeDevShell -> (TypeDevShell, "devShell")
      TypeDefaultDevShell -> (TypeDefaultDevShell, "defaultDevShell")
      TypeDefaultPackage -> (TypeDefaultPackage, "defaultPackage")
      TypeApp -> (TypeApp, "app")
      TypeOverall -> (TypeOverall, "overall")

instance PGParameter "package_type" PackageType where
  pgEncode _ status = cs $ review asPackageType status

instance PGColumn "package_type" PackageType where
  pgDecode _ status = case cs status ^? asPackageType of
    Just p -> p
    Nothing ->
      error
        $ "Expected one of: "
        <> show (snd <$> asPackageTypePairs)
        <> ", got: "
        <> cs status

instance Servant.FromHttpApiData PackageType where
  parseUrlPiece piece = case piece ^? asPackageType of
    Just p -> Right p
    Nothing ->
      Left
        $ "Expected one of: "
        <> show (snd <$> asPackageTypePairs)
        <> ", got: "
        <> cs piece

instance Pretty PackageType where
  pretty = pretty . review asPackageType

instance ToJSON PackageType where
  toJSON = Aeson.toJSON . review asPackageType

instance FromJSON PackageType where
  parseJSON = Aeson.withText "PackageType" $ \pt -> case pt ^? asPackageType of
    Just p -> pure p
    Nothing ->
      fail
        . cs
        $ "Expected one of: "
        <> show (snd <$> asPackageTypePairs)
        <> ", got: "
        <> cs pt

-- | A fake package for reporting basic results such as whether the flake.nix
-- can be parsed.
buildStarting :: PackageName
buildStarting = PackageName "Build starting"

newtype Packages a = Packages {getPackages :: Map.Map System (Map.Map PackageName a)}
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON)

data GhRun = GhRun
  { _ghRunName :: Text,
    _ghRunHeadSha :: CommitHash,
    _ghRunStatus :: Text,
    _ghRunOutput :: Maybe RunOutput,
    _ghRunDetailsUrl :: Maybe Text,
    _ghRunConclusion :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON GhRun where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON GhRun where parseJSON = ourParseJSON

data RunOutput = RunOutput
  { _runOutputTitle :: Text,
    _runOutputSummary :: Text,
    _runOutputText :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON RunOutput where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON RunOutput where parseJSON = ourParseJSON

-- * Repo

data Repo = Repo
  { _repoReqUser :: UserId,
    _repoRepoUser :: GhRepoOwner,
    _repoRepoName :: GhRepoName,
    _repoEnabledAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Repo where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON Repo where parseJSON = ourParseJSON

data UserOverviewRepo = UserOverviewRepo
  { _userOverviewRepoRepoUser :: GhRepoOwner,
    _userOverviewRepoRepoName :: GhRepoName,
    _userOverviewRepoEnabled :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON UserOverviewRepo where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON UserOverviewRepo where parseJSON = ourParseJSON

-- * Build

newtype BuildId = BuildId {getBuildId :: HashId}
  deriving stock (Eq, Show, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      FromHttpApiData,
      ToHttpApiData,
      Pretty,
      PGColumn "bigint",
      PGParameter "bigint"
    )

newtype RepoPublicity = RepoIsPublic {isRepoPublic :: Bool}
  deriving stock (Eq, Show, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      FromHttpApiData,
      ToHttpApiData,
      Pretty,
      PGColumn "boolean",
      PGParameter "boolean"
    )

newtype BuildStreamId = BuildStreamId {getBuildStreamId :: HashId}
  deriving stock (Eq, Show, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      FromHttpApiData,
      ToHttpApiData,
      Pretty,
      PGColumn "bigint",
      PGParameter "bigint"
    )

newtype BuildOutputsPgColumn = BuildOutputsPgColumn {buildOutputs :: Nix.BuildOutputs}
  deriving stock (Eq, Show, Generic)

instance PGColumn "json" BuildOutputsPgColumn where
  pgDecode PGTypeProxy = fromMaybe (error "Failed to deserialize PGColumn BuildOutputsPgColumn") . Aeson.decodeStrict'

instance PGParameter "json" BuildOutputsPgColumn where
  pgEncode PGTypeProxy = cs . Aeson.encode

instance FromJSON BuildOutputsPgColumn where
  parseJSON = withObject "build outputs pg column" $ \o -> do
    outputs <- forM o $ \v -> do
      text :: Text <- parseJSON v
      case Nix.parseStorePath text of
        Left err -> JSON.parseFail $ cs err
        Right path -> pure path
    pure $ BuildOutputsPgColumn $ Nix.BuildOutputs $ toMapText outputs

instance ToJSON BuildOutputsPgColumn where
  toJSON (BuildOutputsPgColumn (Nix.BuildOutputs map)) = toJSON $ Nix.getStorePath <$> map

outputsForBuild :: Build -> Maybe Nix.BuildOutputs
outputsForBuild build = buildOutputs <$> _buildOutputPaths build

data Build = Build
  { _buildId :: BuildId,
    _buildRepoUser :: GhRepoOwner,
    _buildRepoName :: GhRepoName,
    _buildPrFromFork :: Maybe PrFromFork,
    _buildBranch :: Maybe Branch,
    _buildRepoIsPublic :: RepoPublicity,
    _buildGitCommit :: CommitHash,
    _buildPackage :: PackageName,
    _buildPackageType :: PackageType,
    _buildSystem :: MaybeSystem,
    _buildReqUser :: GhLogin,
    _buildStatus :: Maybe Status,
    _buildStartTime :: UTCTime,
    _buildEndTime :: Maybe UTCTime,
    _buildDrvPath :: Maybe FilePath,
    _buildOutputPaths :: Maybe BuildOutputsPgColumn,
    _buildGithubRunId :: Maybe GhRunId,
    _buildPersistenceName :: Maybe Text,
    _buildWantsIncrementalism :: Bool,
    _buildEvalHost :: Maybe Text,
    _buildUploadedToCache :: Maybe Bool,
    _buildAlreadyBuilt :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Build where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON Build where parseJSON = ourParseJSON

instance Pretty Build where
  pretty s =
    "build:"
      <+> Pretty.line
      <+> Pretty.nest
        2
        ( vsep
            [ "id:" <> pretty (_buildId s),
              "repoUser:" <+> pretty (_buildRepoUser s),
              "repoName:" <+> pretty (_buildRepoName s),
              "prFromFork:" <+> pretty (_buildPrFromFork s),
              "repoIsPublic:" <+> pretty (_buildRepoIsPublic s),
              "gitCommit:" <+> pretty (_buildGitCommit s),
              "package:" <+> pretty (_buildPackage s),
              "packageType:" <+> pretty (_buildPackageType s),
              "system:" <+> pretty (_buildSystem s),
              "reqUser:" <+> pretty (_buildReqUser s),
              "status:" <+> pretty (_buildStatus s),
              "startTime:" <+> pretty (show $ _buildStartTime s),
              "endTime:" <+> pretty (show $ _buildEndTime s),
              "drvPath:" <+> pretty (_buildDrvPath s),
              "githubRunId:" <+> pretty (_buildGithubRunId s)
            ]
        )

buildComment :: Build -> Text
buildComment build =
  show (pretty (_buildId build))
    <> "_"
    <> show (pretty (_buildRepoUser build))
    <> "/"
    <> show (pretty (_buildRepoName build))
    <> "/"
    <> show (pretty (_buildBranch build))

newtype LockedFile = LockedFile {getLockedFile :: FilePath}

-- | Logs as nix gives us (prior to any filtering or processing)
newtype RawLogs = RawLogs {getRawLogs :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Pretty, ConvertibleStrings String)

data BuildResponse = BuildResponse
  { _buildResponseId :: BuildId,
    _buildResponseRepoUser :: GhRepoOwner,
    _buildResponseRepoName :: GhRepoName,
    _buildResponseGitCommit :: CommitHash,
    _buildResponsePackage :: PackageName,
    _buildResponsePackageType :: PackageType,
    _buildResponseSystem :: MaybeSystem,
    _buildResponseReqUser :: GhLogin,
    _buildResponseBranch :: Maybe Branch,
    _buildResponseStatus :: Maybe Status,
    _buildResponseStartTime :: UTCTime,
    _buildResponseEndTime :: Maybe UTCTime,
    _buildResponseGithubRunId :: Maybe GhRunId,
    _buildResponseOriginalBuild :: Maybe OriginalBuild,
    _buildResponseRelatedBuilds :: [OriginalBuild] -- deprecated
  }
  deriving (Eq, Show, Generic)

instance ToJSON BuildResponse where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON BuildResponse where parseJSON = ourParseJSON

data OriginalBuild = OriginalBuild
  { _originalBuildId :: BuildId,
    _originalBuildGitCommit :: CommitHash,
    _originalBuildStatus :: Maybe Status
  }
  deriving (Eq, Show, Generic)

instance ToJSON OriginalBuild where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON OriginalBuild where parseJSON = ourParseJSON

data BuildUpdate = BuildUpdate
  { _buildUpdateStatus :: Maybe Status
  }
  deriving (Eq, Show, Generic)

instance ToJSON BuildUpdate where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON BuildUpdate where parseJSON = ourParseJSON

-- * Project

data Status = Success | Failure | Timeout | Cancelled
  deriving stock (Eq, Ord, Show, Generic, Bounded, Enum)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty Status where
  pretty = \case
    Success -> "success"
    Failure -> "failure"
    Timeout -> "timeout"
    Cancelled -> "cancelled"

instance PGType "build_status" where
  type PGVal "build_status" = Status

instance PGParameter "build_status" Status where
  pgEncode _ status = case status of
    Success -> "success"
    Failure -> "failure"
    Timeout -> "timeout"
    Cancelled -> "cancelled"

instance PGColumn "build_status" Status where
  pgDecode _ status = case status of
    "success" -> Success
    "failure" -> Failure
    "timeout" -> Timeout
    "cancelled" -> Cancelled
    e -> error $ "Impossible: expected 'success', 'failure', 'timeout' or 'cancelled', got: " <> cs e

newtype Logs = Logs {getLogs :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

data SingleRunResult = SingleRunResult
  { _singleRunResultStatus :: Status,
    _singleRunResultLogs :: Maybe Logs
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SingleRunResult where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON SingleRunResult where parseJSON = ourParseJSON

data RunResult = RunResult
  { _runResultStatus :: Status,
    _runResultPackages :: Map.Map System (Map.Map PackageName (Maybe SingleRunResult))
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RunResult where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON RunResult where parseJSON = ourParseJSON

-- * Commit

data CommitStatus = Evaluating | Evaluated
  deriving stock (Eq, Show, Enum, Bounded, Ord, Generic)

instance PGType "commit_status" where
  type PGVal "commit_status" = CommitStatus

asCommitStatus :: Prism' Text CommitStatus
asCommitStatus = prism there back
  where
    there i = case lookup i asCommitStatusPairs of
      Just x -> x
      Nothing -> error $ "missing pattern: " <> show i
    back i = case lookup i (swap <$> asCommitStatusPairs) of
      Just x -> Right x
      Nothing -> Left i

asCommitStatusPairs :: [(CommitStatus, Text)]
asCommitStatusPairs =
  [ (Evaluating, "evaluating"),
    (Evaluated, "evaluated")
  ]

instance PGParameter "commit_status" CommitStatus where
  pgEncode _ status = cs $ review asCommitStatus status

instance PGColumn "commit_status" CommitStatus where
  pgDecode _ status = case cs status ^? asCommitStatus of
    Just p -> p
    Nothing ->
      error
        $ "Expected one of: "
        <> show (snd <$> asCommitStatusPairs)
        <> ", got: "
        <> cs status

instance Pretty CommitStatus where
  pretty = pretty . review asCommitStatus

data CheckStatus = CheckPending | CheckFail | CheckSuccess
  deriving stock (Eq, Show, Enum, Bounded, Ord, Generic)

instance PGType "check_status" where
  type PGVal "check_status" = CheckStatus

asCheckStatus :: Prism' Text CheckStatus
asCheckStatus = prism there back
  where
    there i = case lookup i asCheckStatusPairs of
      Just x -> x
      Nothing -> error $ "missing pattern: " <> show i
    back i = case lookup i (swap <$> asCheckStatusPairs) of
      Just x -> Right x
      Nothing -> Left i

asCheckStatusPairs :: [(CheckStatus, Text)]
asCheckStatusPairs =
  [ (CheckPending, "pending"),
    (CheckFail, "fail"),
    (CheckSuccess, "success")
  ]

instance PGParameter "check_status" CheckStatus where
  pgEncode _ status = cs $ review asCheckStatus status

instance PGColumn "check_status" CheckStatus where
  pgDecode _ status = case cs status ^? asCheckStatus of
    Just p -> p
    Nothing ->
      error
        $ "Expected one of: "
        <> show (snd <$> asCheckStatusPairs)
        <> ", got: "
        <> cs status

instance Pretty CheckStatus where
  pretty = pretty . review asCheckStatus

data Commit = Commit
  { _commitRepoOwner :: GhRepoOwner,
    _commitRepoName :: GhRepoName,
    _commitHash :: CommitHash,
    _commitStatus :: CommitStatus,
    _commitMetaCheck :: CheckStatus
  }
  deriving stock (Eq, Show, Generic)

data FullCommitState
  = CommitEvaluating
  | CommitEvaluated Commit [Build] [Run]
  deriving stock (Eq, Show, Generic)

data Run = Run
  { _runId :: RunId,
    _runName :: Text,
    _runRepoUser :: GhRepoOwner,
    _runRepoName :: GhRepoName,
    _runGitCommit :: CommitHash,
    _runBranch :: Maybe Branch,
    _runStatus :: Maybe Status,
    _runReqUser :: GhLogin,
    _runStartTime :: UTCTime,
    _runEndTime :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)

newtype RunId = RunId {getRunId :: HashId}
  deriving stock (Eq, Show, Generic)
  deriving newtype
    ( PGColumn "bigint",
      PGParameter "bigint",
      FromHttpApiData
    )

-- * User

--
newtype RequestingGhLogin = RequestingGhLogin {getRequestingGhLogin :: GhLogin}
  deriving stock (Eq, Show, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      PGParameter "character varying",
      PGParameter "text",
      PGColumn "character varying",
      PGColumn "text",
      IsString
    )

newtype GhRepoOwner = GhRepoOwner {getGhRepoOwner :: GhLogin}
  deriving stock (Eq, Show, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      ToJSONKey,
      Ord,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      PGParameter "character varying",
      PGParameter "text",
      PGColumn "character varying",
      PGColumn "text",
      Pretty,
      IsString
    )

newtype GhLogin = GhLogin {getGhLogin :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      ToJSONKey,
      Ord,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      PGParameter "character varying",
      PGParameter "text",
      PGColumn "character varying",
      PGColumn "text",
      Pretty,
      IsString
    )

newtype GhRepoName = GhRepoName {getGhRepoName :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      Ord,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      PGParameter "character varying",
      PGParameter "text",
      PGColumn "character varying",
      PGColumn "text",
      Pretty,
      IsString
    )

newtype InternalCacheToken = InternalCacheToken {getInternalCacheToken :: Text}
  deriving stock (Eq, Show, Generic)

generateInternalCacheToken :: (MonadIO m) => m InternalCacheToken
generateInternalCacheToken = InternalCacheToken <$> randomBase64 64

newtype NetRcFile = NetRcFile {getNetRcFile :: FilePath}
  deriving stock (Show)

newtype NixConfig = NixConfig {getNixConfig :: StrictMap.Map String String}

accessTokensSetting :: String
accessTokensSetting = "access-tokens"

instance Show NixConfig where
  show (NixConfig config) =
    "NixConfig {getNixConfig = " <> Prelude.show (StrictMap.mapWithKey redact config) <> "}"
    where
      redact key value
        | key == accessTokensSetting = "<redacted>"
        | otherwise = value

instance Semigroup NixConfig where
  NixConfig left <> NixConfig right = NixConfig $ left <> right

newtype PrFromFork = PrFromFork {getPrFromForkFullName :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      Ord,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      PGParameter "character varying",
      PGParameter "text",
      PGColumn "character varying",
      PGColumn "text",
      Pretty,
      IsString
    )

newtype Email = Email {getEmail :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      PGParameter "character varying",
      PGParameter "text",
      PGColumn "character varying",
      PGColumn "text",
      Pretty,
      IsString
    )

newtype CommitHash = CommitHash {getCommitHash :: Text}
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      PGParameter "character varying",
      PGParameter "text",
      PGColumn "character varying",
      PGColumn "text",
      Pretty,
      IsString
    )

instance Inject Text CommitHash where
  inj = CommitHash

instance Project Text CommitHash where
  prj = getCommitHash

instance Isomorphic Text CommitHash

newtype Branch = Branch {getBranch :: Text}
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      PGParameter "character varying",
      PGParameter "text",
      PGColumn "character varying",
      PGColumn "text",
      Pretty,
      IsString
    )

instance ConvertibleStrings Text Branch where
  convertString = Branch

instance ConvertibleStrings Branch Text where
  convertString = getBranch

instance Inject Text Branch where inj = cs

instance Project Text Branch where prj = cs

instance Isomorphic Text Branch

newtype GhPullRequestId = GhPullRequestId {getGhPullRequestId :: Int64}
  deriving stock (Eq, Show)
  deriving newtype
    ( Num,
      ToJSON,
      FromJSON,
      PGParameter "bigint",
      PGColumn "bigint"
    )

instance (Inject a b, Functor f) => Inject (f a) (f b) where
  inj = fmap inj

instance (Project a b, Functor f) => Project (f a) (f b) where
  prj = fmap prj

instance (Inject a b, Project a b, Functor f) => Isomorphic (f a) (f b)

newtype GhToken = GhToken {getGhToken :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON, Servant.FromHttpApiData, Servant.ToHttpApiData)

obfuscateGithubToken :: Text -> Text
obfuscateGithubToken =
  [RE.regex|gh[pousr]_\w{15,255}|] . RE.match .~ "XXXXXXXXXXXXXXXX"

newtype EncryptedText = EncryptedText {getEncryptedText :: StrictByteString}
  deriving stock (Eq, Show, Generic)
  deriving newtype
    ( PGColumn "bytea",
      PGParameter "bytea"
    )

data GhUserCredentials secret = GhUserCredentials
  { _ghUserCredentialsAccessToken :: secret,
    _ghUserCredentialsAccessTokenExpiresAt :: Maybe UTCTime,
    _ghUserCredentialsRefreshToken :: Maybe secret,
    _ghUserCredentialsRefreshTokenExpiresAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)

instance (ToJSON secret) => ToJSON (GhUserCredentials secret) where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance (FromJSON secret) => FromJSON (GhUserCredentials secret) where
  parseJSON = ourParseJSON

data CommitSummary = CommitSummary
  { _commitSummaryRepoOwner :: GhRepoOwner,
    _commitSummaryRepoName :: GhRepoName,
    _commitSummaryRepoIsPublic :: RepoPublicity,
    _commitSummaryGitCommit :: CommitHash,
    _commitSummaryBranch :: Maybe Branch,
    _commitSummaryReqUser :: GhLogin,
    _commitSummaryStartTime :: UTCTime,
    _commitSummarySucceeded :: Int64,
    _commitSummaryFailed :: Int64,
    _commitSummaryPending :: Int64,
    _commitSummaryCancelled :: Int64
  }
  deriving (Eq, Show, Generic)

instance ToJSON CommitSummary where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

-- * Errors

data ErrorWithContext = ErrorWithContext
  { callstack :: CallStack,
    spans :: [(Text, Text)],
    severity :: Severity,
    err :: Error
  }
  deriving stock (Show, Generic)

errLens :: Lens' ErrorWithContext Error
errLens = lens err (\error err -> error {err})

instance Eq ErrorWithContext where
  e1 == e2 = err e1 == err e2

instance ToJSON ErrorWithContext where
  toJSON = Aeson.toJSON . err

showDebug :: ErrorWithContext -> Text
showDebug e =
  message <> "\n\n" <> cs (prettyCallStack $ callstack e)
  where
    message = case err e of
      OtherError message -> "OtherError: " <> message
      inner -> showPretty inner

data SubdomainKind
  = RepoOwnerSubdomain
  | RepoNameSubdomain
  | BranchSubdomain
  | PackageNameSubdomain
  | PersistenceNameSubdomain
  deriving stock (Eq, Show, Generic)

instance Pretty SubdomainKind where
  pretty =
    \case
      RepoOwnerSubdomain -> "repo owner"
      RepoNameSubdomain -> "repo name"
      BranchSubdomain -> "branch"
      PackageNameSubdomain -> "package name"
      PersistenceNameSubdomain -> "persistence name"

data Error
  = UncaughtRuntimeException {message :: Text}
  | RunProcessError
      { command :: Text,
        arguments :: [Text],
        stdErr :: Text,
        stdOut :: Text,
        exitCode :: Int
      }
  | DecodeError {original :: Text, message :: Text}
  | NoSuchBuild {buildId :: BuildId}
  | NoSuchBuildRunId {ghRunId :: GhRunId}
  | NoSuchRun {runId :: RunId}
  | NoSuchCommit CommitHash
  | NoSuchRepo {_owner :: GhRepoOwner, _name :: GhRepoName}
  | NoSuchUser GhLogin
  | ErrorGettingBuildPlan {message :: Text}
  | ErrorGettingAttributesToBuild {message :: Text}
  | IsDeniedAccess
  | DuplicateBuild
  | UserAlreadyExists GhLogin
  | DbError {statement :: Text, message :: Text}
  | GithubDidntGiveUsAToken
  | GithubUserTokenRejected
  | GithubSessionExpired
  | RedirectFound {location :: Text}
  | BadRequest {message :: Text}
  | Unauthorized
  | UnauthorizedWithMessage {message :: Text}
  | Forbidden
  | ForbiddenWithMessage {message :: Text}
  | NotFound
  | OtherError Text
  | DecodeConfigError {message :: Text}
  | SshTimeout {command :: Text}
  | InvalidEmail
  | InvalidAccessToken
  | DevModeOnly
  | DeploymentWantsNixosConfigurationsThatDontExist [PackageName]
  | -- | The configuration, the machine size its @servers:@ entry asked for,
    -- and the largest this instance hands out.
    ServerTierExceedsInstanceCap PackageName Text Text
  | NameIsNotValidSubdomain SubdomainKind Text
  | TransactionAlreadyStarted
  | BuildAlreadyStopped {buildId :: BuildId}
  | InvalidBuildUpdate {buildUpdateBody :: BuildUpdate}
  | FailedToParseDrvFile {drvFile :: FilePath, message :: Text}
  | CachedError {inner :: ErrorWithContext}
  | GarnixAppUnauthorized GhRepoOwner GhRepoName
  | GithubRequestTimeout
  | FailedToParseCreateReportResult Aeson.Value
  | ModuleErrorFlakeExists
  | ActionExecutionTimeout
  | ActionPreconditionNixStore Text
  | ActionPreconditionFileExists {sandboxTypeStr :: Text}
  | ActionSandboxTypeNotAllowed Text
  | ActionKeyDecryptionFailure
  | ActionEvaluationFailure Text
  | -- | The provisioner refused, or could not satisfy, a request to create,
    -- expose, or destroy a guest.
    ProvisioningError {message :: Text}
  | -- | @switch-to-configuration@ failed on a guest. Carries the guest's
    -- address (rather than a @ServerInfo@, which lives in "Garnix.Hosting.Types"
    -- and would close an import cycle) so the message can tell the user where
    -- to ssh in and look.
    ActivationError {serverAddress :: Text, stdErr :: Text}
  deriving stock (Eq, Show, Generic)

instance Pretty Error where
  pretty x = case x of
    UncaughtRuntimeException {message} -> "runtime exception:" <+> pretty message
    NoSuchUser user ->
      "No user with github login"
        <+> pretty (getGhLogin user)
        <+> "could be found"
    NoSuchBuild {} ->
      "No build matching that description could be found."
        <+> "Either it doesn't exist, or you don't have access to it."
    NoSuchBuildRunId {..} ->
      "No build with github run id"
        <+> pretty ghRunId
        <+> "found."
        <+> "Either it doesn't exist, or you don't have access to it."
    NoSuchRun {} ->
      "No run matching that description could be found."
        <+> "Either it doesn't exist, or you don't have access to it."
    NoSuchCommit commit ->
      "Commit"
        <+> pretty commit
        <+> "not found."
        <+> "Either it doesn't exist, or you don't have access to it."
    NoSuchRepo {..} ->
      "The repo https://github.com/"
        <> pretty (getGhLogin (getGhRepoOwner _owner))
        <> "/"
        <> pretty (getGhRepoName _name)
        <> " doesn't exist, has not enabled garnix, or you don't have access to it."
    IsDeniedAccess ->
      "This request has been denied. Anomalous activity detected. Please contact contact@garnix.io for more information."
    UserAlreadyExists user ->
      "User with github login"
        <+> pretty (getGhLogin user)
        <+> "already exists"
    GithubDidntGiveUsAToken -> "Github didn't give us a user token"
    GithubUserTokenRejected -> "Github rejected the user's access token"
    GithubSessionExpired -> "Your github session has expired. Please log in again."
    DecodeError {..} ->
      "Error decoding ("
        <> pretty message
        <> "):"
        <+> pretty (limitMessage original)
      where
        limit = 10000
        limitMessage text =
          if T.length text > limit
            then T.take limit text <> "[...snip]"
            else text
    DuplicateBuild -> "Build already exists"
    RedirectFound {location} -> "Redirect error to " <> pretty location
    BadRequest {message} -> "Bad Request: " <> pretty message
    Unauthorized -> "You need to login to continue"
    UnauthorizedWithMessage {message} -> "Unauthorized: " <> pretty message
    Forbidden -> "You are not authorized for this resource"
    ForbiddenWithMessage {message} -> "Forbidden: " <> pretty message
    NotFound -> "Resource not found"
    DbError {} -> "Something went wrong"
    ErrorGettingBuildPlan {..} -> "Couldn't get build plan. Error was: " <> pretty message
    ErrorGettingAttributesToBuild {..} -> "Couldn't get attributes to build. Error was: " <> pretty message
    SshTimeout {command} ->
      pretty $ "ssh command timed out: " <> command
    OtherError message -> pretty message
    DecodeConfigError {..} ->
      "Error decoding garnix.yaml:"
        <+> pretty message
    RunProcessError {..} ->
      pretty
        $ obfuscateGithubToken
        $ show
          ( "Command"
              <+> Pretty.squotes (pretty (command <> " " <> T.unwords arguments))
              <+> "failed with exit code"
              <+> pretty (show exitCode)
              <> "."
              <> Pretty.softline
              <> "Standard err was:"
              <> Pretty.line
              <> pretty stdErr
          )
    InvalidEmail -> "Invalid email address"
    InvalidAccessToken -> "Invalid access token"
    DevModeOnly -> "dev mode only"
    DeploymentWantsNixosConfigurationsThatDontExist packages -> "Deployment wants package(s) that have not been built: " <> pretty packages
    ServerTierExceedsInstanceCap package' wanted cap ->
      "Deployment of"
        <+> Pretty.squotes (pretty package')
        <+> "asks for machine"
        <+> Pretty.squotes (pretty wanted)
        <> ", over this instance's cap of"
        <+> Pretty.squotes (pretty cap)
        <> "."
    NameIsNotValidSubdomain kind name -> pretty kind <+> "name" <+> Pretty.squotes (pretty name) <+> "is not a valid subdomain name."
    TransactionAlreadyStarted -> "Internal transaction error."
    BuildAlreadyStopped {buildId} -> "Build with id" <+> pretty buildId <+> "has already been stopped."
    InvalidBuildUpdate {buildUpdateBody} -> "Invalid build update:" <+> pretty (decodeUtf8 $ BSL.toStrict (encodePretty buildUpdateBody))
    FailedToParseDrvFile {drvFile, message} -> "Failed to parse " <+> pretty drvFile <+> ": " <+> pretty message
    CachedError inner -> "(cached error)" <+> pretty (err inner)
    GarnixAppUnauthorized owner repo -> Pretty.squotes (pretty owner) <+> "uninstalled or did not give enough permissions to the garnix app for" <+> Pretty.squotes (pretty repo)
    GithubRequestTimeout -> "Request timeout when talking with Github."
    FailedToParseCreateReportResult value -> "Failed to parse create report response: " <> Pretty.squotes (pretty $ show value)
    ModuleErrorFlakeExists -> "This repo already has a flake.nix file. You should be able to use garnix without using modules."
    ActionExecutionTimeout -> "The action took too long to complete and it was cancelled."
    ActionPreconditionNixStore p -> "The action's 'program' needs to be a (path) from a derivation. Program is: " <> Pretty.squotes (pretty p)
    ActionPreconditionFileExists p -> "The action's 'program' is wrong. Please make sure the path exists and that you're not trying to pass arguments via the 'program' field. Program is: " <> Pretty.squotes (pretty p)
    ActionSandboxTypeNotAllowed typ -> "You are not allowed to run actions with the '" <> pretty typ <> "'. If you want access, get in touch with us."
    ActionEvaluationFailure t -> "Evaluation for the action failed: " <> Pretty.line <> pretty t
    ActionKeyDecryptionFailure -> "Could not decrypt private key."
    ProvisioningError {message} -> "Error provisioning server:" <+> pretty message
    ActivationError {serverAddress, stdErr} ->
      pretty
        $ T.unlines
          [ "Failed to activate server.",
            "You may be able to debug this by sshing into " <> serverAddress <> ".",
            "Stderr:",
            stdErr
          ]

instance ToJSON Error where
  toJSON x =
    Aeson.object
      $ [ "status" Aeson..= ("error" :: Text),
          "message" Aeson..= msg
        ]
      ++ additionalFields
    where
      msg = showPretty x
      additionalFields = case x of
        NoSuchUser user -> ["garnixUser" Aeson..= user]
        _ -> []

servantizeError :: ErrorWithContext -> Servant.ServerError
servantizeError e =
  let details = toErrorDetails e
   in Servant.ServerError
        (statusCode details)
        (cs $ statusMessage $ toEnum $ statusCode details)
        (cs $ userMessage details)
        (headers details)

-- Unsafe constructor, use `errorDetails` instead!
data ErrorDetails = UnsafeErrorDetails
  { statusCode :: Int,
    headers :: [Network.HTTP.Types.Header],
    -- | An error message that is safe to display to the user
    userMessage :: Text
  }

errorDetails :: Int -> Text -> ErrorDetails
errorDetails statusCode userMessage = UnsafeErrorDetails statusCode [] (obfuscateGithubToken userMessage)

fromStatusCode :: Int -> ErrorDetails
fromStatusCode statusCode = errorDetails statusCode $ cs $ statusMessage $ toEnum statusCode

toErrorDetails :: ErrorWithContext -> ErrorDetails
toErrorDetails e = case err e of
  UncaughtRuntimeException {} -> errorDetails 500 "Something went wrong"
  RedirectFound {location} -> (fromStatusCode 302) {headers = [("location", cs location)]}
  BadRequest {message} -> errorDetails 400 $ "Bad Request: " <> message
  Unauthorized -> fromStatusCode 401
  UnauthorizedWithMessage {message} -> errorDetails 401 $ "Unauthorized: " <> message
  Forbidden -> fromStatusCode 403
  ForbiddenWithMessage {message} -> errorDetails 403 $ "Forbidden: " <> message
  NotFound -> fromStatusCode 404
  e'@DecodeConfigError {} -> errorDetails 400 $ cs $ Aeson.encode e'
  e'@DuplicateBuild -> errorDetails 409 $ cs $ Aeson.encode e'
  e'@NoSuchBuild {} -> errorDetails 404 $ cs $ Aeson.encode e'
  e'@NoSuchBuildRunId {} -> errorDetails 404 $ cs $ Aeson.encode e'
  e'@NoSuchRun {} -> errorDetails 404 $ cs $ Aeson.encode e'
  e'@NoSuchCommit {} -> errorDetails 404 $ cs $ Aeson.encode e'
  e'@NoSuchUser {} -> errorDetails 404 $ cs $ Aeson.encode e'
  e'@IsDeniedAccess -> errorDetails 403 $ cs $ Aeson.encode e'
  e'@UserAlreadyExists {} -> errorDetails 409 $ cs $ Aeson.encode e'
  e'@GithubDidntGiveUsAToken {} -> errorDetails 500 $ cs $ Aeson.encode e'
  GithubUserTokenRejected -> errorDetails 401 "Github rejected the user's access token"
  GithubSessionExpired -> errorDetails 401 "Your github session has expired. Please log in again."
  e'@InvalidEmail -> errorDetails 400 $ cs $ Aeson.encode e'
  e'@InvalidAccessToken -> errorDetails 401 $ cs $ Aeson.encode e'
  e'@RunProcessError {} ->
    errorDetails 400
      $ command e'
      <> " "
      <> T.unwords (arguments e')
      <> " failed with exit code "
      <> show (exitCode e')
      <> "\nStderr:\n"
      <> stdErr e'
  DecodeError _ _ ->
    errorDetails 500 "Decode error"
  DbError _ _ ->
    fromStatusCode 500
  e@(ErrorGettingBuildPlan _) ->
    errorDetails 400 $ showPretty e
  e@(ErrorGettingAttributesToBuild _) ->
    errorDetails 400 $ showPretty e
  SshTimeout {command} ->
    errorDetails 500 $ "Timeout: ssh command " <> command
  ProvisioningError {} ->
    errorDetails 500 "Error provisioning server. This could be a temporary error."
  ActivationError {serverAddress, stdErr} ->
    errorDetails 400 $ "Error activating server at " <> serverAddress <> ": " <> stdErr
  OtherError errMsg ->
    errorDetails 500 errMsg
  DevModeOnly -> fromStatusCode 404
  DeploymentWantsNixosConfigurationsThatDontExist packages ->
    errorDetails 400
      $ "NixOS configuration(s) not found: "
      <> T.intercalate ", " (map getPackageName packages)
  ServerTierExceedsInstanceCap package' wanted cap ->
    errorDetails 400
      $ "The servers: entry for '"
      <> getPackageName package'
      <> "' asks for machine '"
      <> wanted
      <> "', but this garnix instance hands out at most '"
      <> cap
      <> "'. Lower the machine: field, or ask the instance's admin to raise its limit."
  NameIsNotValidSubdomain kind name ->
    errorDetails 400 $ show kind <> " name '" <> name <> "' is not a valid subdomain name."
  TransactionAlreadyStarted ->
    errorDetails 500 "Internal transaction error"
  BuildAlreadyStopped {} ->
    errorDetails 400 "Build has already been stopped."
  InvalidBuildUpdate {} ->
    errorDetails 500 "Invalid build update."
  NoSuchRepo {..} ->
    errorDetails 404
      $ "The repo https://github.com/"
      <> getGhLogin (getGhRepoOwner _owner)
      <> "/"
      <> getGhRepoName _name
      <> " doesn't exist, has not enabled garnix, or you don't have access to it."
  FailedToParseDrvFile {drvFile, message} -> errorDetails 500 $ "Failed to parse drv file " <> cs drvFile <> ": " <> message
  CachedError inner ->
    let details = toErrorDetails inner
     in details
          { userMessage = "(cached error) " <> userMessage details
          }
  GarnixAppUnauthorized (GhRepoOwner (GhLogin owner)) (GhRepoName repo) ->
    errorDetails 403 $ "The Garnix application does not have enough rights or was removed from '" <> show owner <> "/" <> show repo <> "'"
  GithubRequestTimeout -> errorDetails 503 "Request timeout when talking with Github."
  FailedToParseCreateReportResult _ -> errorDetails 500 "Internal error when creating report."
  ModuleErrorFlakeExists -> errorDetails 400 $ show $ pretty $ err e
  ActionExecutionTimeout -> errorDetails 400 $ show $ pretty $ err e
  ActionPreconditionNixStore _ -> errorDetails 400 $ show $ pretty $ err e
  ActionPreconditionFileExists _ -> errorDetails 400 $ show $ pretty $ err e
  ActionSandboxTypeNotAllowed _ -> errorDetails 403 $ show $ pretty $ err e
  ActionEvaluationFailure _ -> errorDetails 400 $ show $ pretty $ err e
  ActionKeyDecryptionFailure -> errorDetails 400 $ show $ pretty $ err e

data SubscriptionType
  = FreeSubscription
  | Admin
  deriving stock (Eq, Show, Generic)

instance ToJSON SubscriptionType where
  toJSON = \case
    FreeSubscription -> "free"
    Admin -> "admin"

instance FromJSON SubscriptionType where
  parseJSON = Aeson.withText "subscription_type" $ \case
    "free" -> pure FreeSubscription
    "admin" -> pure Admin
    _ -> mzero

instance PGType "subscription_type" where
  type PGVal "subscription_type" = SubscriptionType

instance PGParameter "subscription_type" SubscriptionType where
  pgEncode _ status = case status of
    FreeSubscription -> "free"
    Admin -> "admin"

instance PGColumn "subscription_type" SubscriptionType where
  pgDecode _ status = case status of
    "free" -> FreeSubscription
    "admin" -> Admin
    e -> error $ "Impossible: unknown subscription type " <> cs e

newtype UserId = UserId {getUserId :: Int32}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, Ord, FromJSON, PGColumn "integer", PGParameter "integer")

data User = User
  { _userId :: UserId,
    _userGithubLogin :: GhLogin,
    _userEmail :: Email,
    _userSubscriptionType :: SubscriptionType,
    _userCreatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON User where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON User where parseJSON = ourParseJSON

data AuthJwtPayload = WebSession User | ApiSession User
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJWT, FromJWT)

instance {-# OVERLAPS #-} HasField' "user" AuthJwtPayload User where
  field' :: Lens' AuthJwtPayload User
  field' =
    lens
      ( \case
          WebSession user -> user
          ApiSession user -> user
      )
      ( \payload user -> case payload of
          WebSession _user -> WebSession user
          ApiSession _user -> ApiSession user
      )

sessionKindWeb :: Text
sessionKindWeb = "web"

sessionKindApi :: Text
sessionKindApi = "api"

instance ToJSON AuthJwtPayload where
  toJSON payload =
    JSON.Object
      $ ("id" Aeson..= _userId user)
      <> ("github_login" Aeson..= _userGithubLogin user)
      <> ("email" Aeson..= _userEmail user)
      <> ("subscription_type" Aeson..= _userSubscriptionType user)
      <> ("created_at" Aeson..= _userCreatedAt user)
      <> ("session_kind" Aeson..= kind)
    where
      (user, kind) = case payload of
        WebSession user -> (user, sessionKindWeb)
        ApiSession user -> (user, sessionKindApi)

instance FromJSON AuthJwtPayload where
  parseJSON = withObject "AuthJwtPayload" $ \obj -> do
    user <- parseJSON (Aeson.Object obj)
    kind <- obj Aeson..: "session_kind"
    if
      | kind == sessionKindWeb -> pure $ WebSession user
      | kind == sessionKindApi -> pure $ ApiSession user
      | otherwise -> fail $ "unknown session kind: " <> cs kind

data InstallationStatus
  = AppNotInstalled
  | AppInstalled
  | AppInstalledWithoutMemberAccess
  deriving stock (Eq, Show, Generic)

instance ToJSON InstallationStatus where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

data CreatingUser a = CreatingUser
  { _creatingUserExists :: Bool,
    _creatingUserGithubLogin :: GhLogin,
    _creatingUserEmail :: Email,
    _creatingUserGithubToken :: a
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving anyclass (ToJWT, FromJWT)

instance (ToJSON a) => ToJSON (CreatingUser a) where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance (FromJSON a) => FromJSON (CreatingUser a) where parseJSON = ourParseJSON

data CreateUser = CreateUser
  { _createUserEmail :: Email,
    _createUserSubscriptionType :: SubscriptionType,
    _createUserAgreeToEmails :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJWT, FromJWT)

instance ToJSON CreateUser where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON CreateUser where parseJSON = ourParseJSON

data LoginLinks = LoginLinks
  {_loginLinksGithub :: T.Text}
  deriving stock (Eq, Show, Generic)

instance ToJSON LoginLinks where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON LoginLinks where parseJSON = ourParseJSON

data SignupLinks = SignupLinks
  { _signupLinksGithub :: T.Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SignupLinks where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON SignupLinks where parseJSON = ourParseJSON

data UserOverview = UserOverview
  { _userOverviewRepos :: [UserOverviewRepo]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON UserOverview where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

instance FromJSON UserOverview where parseJSON = ourParseJSON

newtype OAuthCode = OAuthCode {getOAuthCode :: T.Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON, Servant.FromHttpApiData, Servant.ToHttpApiData)

-- | Yes this really must be at least Int64
newtype GhRunId = GhRunId {getGhRunId :: Int64}
  deriving stock (Eq, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      Num,
      Ord,
      Show,
      PGParameter "bigint",
      PGColumn "bigint",
      Pretty
    )

data DoesFileExist = FileExists | FileDoesntExist
  deriving (Eq, Show)

newtype PullRequestResult = PullRequestResult
  { _pullRequestResultUrl :: Text
  }
  deriving stock (Generic)

instance ToJSON PullRequestResult where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

-- * FrontendConfig

data FrontendConfig = FrontendConfig
  { _frontendConfigGithubAppName :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FrontendConfig where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

-- * Log

data LogItem = LogItem
  { severity :: Severity,
    context :: [(Text, Text)],
    msg :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FastLogger.ToLogStr LogItem where
  toLogStr logItem =
    toLogStr
      $ Aeson.encodingToLazyByteString
      $ Aeson.pairs
        ( "logLevel"
            Aeson..= show (logItem ^. #severity)
              <> foldMap (\(a, b) -> AesonKey.fromText a Aeson..= b) (context logItem)
              <> ("message" Aeson..= msg logItem)
        )

data Severity
  = -- | Critical conditions, alert the entire dev team. An action is likely required.
    -- | Examples: system down, database down, invariant does not hold, etc.
    Critical
  | -- | Error conditions, notify the alerts channel. Action may or may not be required.
    -- | Examples: Any sort of unexpected error case that does not fit into `Critical`.
    Error
  | -- | Expected errors, user errors, etc.
    -- | Examples: eval errors, build errors, build timeouts, etc.
    Warning
  | -- | Key events that we may want to follow for a specific build.
    -- | Examples: pr/branch webhook received, build started, etc.
    Notice
  | -- | Information that is useful to log for users/customer support.
    -- | Examples: Individual package build start/completion, detailed deployment events, debugging info, etc.
    Informational
  deriving (Eq, Enum, Bounded, Read, Show, Ord)

newtype Candidate a = Candidate {promoteCandidate :: a}

-- * opensearch

data OpenSearchId
  = FromRun RunId
  | FromBuild BuildId
  deriving stock (Eq, Show, Generic)

data OpenSearchMessage = OpenSearchMessage
  { _openSearchMessageTimestamp :: UTCTime,
    _openSearchMessagePackage :: Maybe PackageName,
    _openSearchMessagePhase :: Maybe Text,
    _openSearchMessageLogMessage :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON OpenSearchMessage where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

-- * combined data types

data CommitInfo = CommitInfo
  { _commitInfoReqUser :: GhLogin,
    _commitInfoRepoPublicity :: RepoPublicity,
    _commitInfoRepoInfo :: RepoInfo,
    _commitInfoBranch :: Maybe Branch,
    _commitInfoPrFromFork :: Maybe PrFromFork,
    _commitInfoCommit :: CommitHash
  }
  deriving stock (Show)

data RepoInfo = RepoInfo
  { _repoInfoInstallationAuth :: InstallationAuth,
    _repoInfoGhToken :: GhToken,
    _repoInfoGhRepoOwner :: GhRepoOwner,
    _repoInfoGhRepoName :: GhRepoName
  }

instance Show RepoInfo where
  show (RepoInfo _iAuth _ghToken repoOwner repoName) =
    "RepoInfo <iAuth> <ghToken>" <> unwords [Prelude.show repoOwner, Prelude.show repoName]

data PackageInfo = PackageInfo
  { _packageInfoPackageType :: PackageType,
    _packageInfoMaybeSystem :: MaybeSystem,
    _packageInfoPackageName :: PackageName
  }

data BuildKind = Webhook | ModulePreview

data RepoConfig = RepoConfig
  { _repoConfigSkipPrivateInputsCheckForCollaborators :: Bool,
    _repoConfigMaxEvalMemory :: Memory
  }
  deriving stock (Show)

defaultRepoConfig :: RepoConfig
defaultRepoConfig = RepoConfig False (fromGigabytes 8)

newtype Memory = Memory Int64
  deriving stock (Show, Eq, Generic, Ord)
  deriving newtype
    ( PGColumn "bigint",
      PGParameter "bigint"
    )

toBytes :: Memory -> Int64
toBytes (Memory bytes) = bytes

fromGigabytes :: Int64 -> Memory
fromGigabytes gbs = Memory $ gbs * 1024 * 1024 * 1024

data DatabaseConnection
  = ConnectionPool (Pool PGConnection)
  | Transaction PGConnection

makeFields ''Repo
makeFields ''Build
makeFields ''OpenSearchMessage
makeFields ''Package
makeFields ''GhRun
makeFields ''RunOutput
makeFields ''User
makeFields ''CreatingUser
makeFields ''CreateUser
makeFields ''CommitInfo
makeFields ''RepoInfo
makeFields ''PackageInfo
makeFields ''RepoConfig
makeFields ''CommitSummary
makeFields ''GhUserCredentials
makeFields ''BuildUpdate
makeFields ''Commit
makeFields ''Run

makePrisms ''User
makePrisms ''Repo
makePrisms ''Build
makePrisms ''CommitInfo
makePrisms ''PackageInfo
makePrisms ''RepoInfo
makePrisms ''CommitStatus
makePrisms ''CheckStatus
makePrisms ''Commit
makePrisms ''FullCommitState
