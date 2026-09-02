-- | Types for hosted servers, kept provider-agnostic.
module Garnix.Hosting.Types
  ( Provider (..),
    providerName,
    parseProvider,
    InstanceId (..),
    ServerId (..),
    PreprovisionedServerId (..),
    ServerTier (..),
    tierResources,
    ServerAddress (..),
    serverAddressText,
    ServerInfo (..),
    PreprovisionedServer (..),
    ExposeResult (..),
    DeploymentType (..),
    fromDeploymentType,
    ghPrDeployment,
  )
where

import Data.Text qualified as T
import Garnix.Prelude
import Garnix.Types (Branch, BuildId, GhPullRequestId)
import Prettyprinter qualified as Pretty

-- * Providers

-- | Which driver owns an instance.
data Provider
  = -- | A microVM guest on the garnix host itself, via garnix-provisionerd.
    MicroVM
  | -- | A server rented from Hetzner Cloud.
    Hetzner
  deriving stock (Eq, Show, Ord, Generic, Enum, Bounded)

providerName :: Provider -> Text
providerName = \case
  MicroVM -> "microvm"
  Hetzner -> "hetzner"

parseProvider :: Text -> Either Text Provider
parseProvider name =
  case find ((== name) . providerName) [minBound .. maxBound] of
    Just provider -> Right provider
    Nothing ->
      Left
        $ "unknown hosting provider: "
        <> name
        <> ". Known providers: "
        <> T.intercalate ", " (map providerName [minBound .. maxBound])

instance Pretty Provider where
  pretty = pretty . providerName

-- | However the owning provider names the instance. Opaque to everything
-- but the driver that issued it.
newtype InstanceId = InstanceId {getInstanceId :: Text}
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      FromHttpApiData,
      ToHttpApiData,
      Pretty,
      PGColumn "text",
      PGParameter "text"
    )

-- * Identifiers

-- | Our own id for a deployed server row.
newtype ServerId = ServerId {getServerId :: HashId}
  deriving stock (Eq, Show, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      FromHttpApiData,
      ToHttpApiData,
      PGColumn "bigint",
      PGParameter "bigint"
    )

instance Pretty ServerId where
  pretty = pretty . getHashId . getServerId

-- | Our own id for a warm, unclaimed instance in the pool.
newtype PreprovisionedServerId = PreprovisionedServerId {getPreprovisionedServerId :: Int64}
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      FromHttpApiData,
      ToHttpApiData,
      Pretty,
      PGColumn "bigint",
      PGParameter "bigint"
    )

-- | The size class a server was asked for.
newtype ServerTier = ServerTier {getServerTier :: Text}
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON,
      FromHttpApiData,
      ToHttpApiData,
      Pretty,
      PGColumn "text",
      PGParameter "text"
    )

-- | vCPU and MiB for a tier.
tierResources :: ServerTier -> (Int, Int)
tierResources tier = case getServerTier tier of
  "small" -> (1, 2048)
  "medium" -> (2, 4096)
  "large" -> (4, 8192)
  _ -> (1, 2048)

-- * Addresses

-- | A server's reachable addresses. Neither family is guaranteed.
data ServerAddress = ServerAddress
  { _serverAddressIpv4 :: Maybe Text,
    _serverAddressIpv6 :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

-- | The address to actually connect to, preferring IPv4.
serverAddressText :: ServerAddress -> Maybe Text
serverAddressText address =
  _serverAddressIpv4 address <|> _serverAddressIpv6 address

instance Pretty ServerAddress where
  pretty address =
    case serverAddressText address of
      Nothing -> "<no address>"
      Just single -> pretty single

-- * Servers

data ServerInfo = ServerInfo
  { _serverInfoId :: ServerId,
    _serverInfoProvider :: Provider,
    _serverInfoInstanceId :: Maybe InstanceId,
    _serverInfoAddress :: ServerAddress,
    _serverInfoCreatedAt :: UTCTime,
    _serverInfoEndedAt :: Maybe UTCTime,
    _serverInfoConfigurationBuildId :: BuildId,
    _serverInfoPullRequest :: Maybe GhPullRequestId,
    _serverInfoReadyAt :: Maybe UTCTime,
    _serverInfoTier :: ServerTier,
    _serverInfoIsPrimary :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Pretty ServerInfo where
  pretty s =
    "server:"
      <+> Pretty.line
      <+> Pretty.nest
        2
        ( Pretty.vsep
            [ "id:" <+> pretty (_serverInfoId s),
              "provider:" <+> pretty (_serverInfoProvider s),
              "instance:" <+> maybe "<unprovisioned>" pretty (_serverInfoInstanceId s),
              "address:" <+> pretty (_serverInfoAddress s),
              "created at:" <+> pretty (show $ _serverInfoCreatedAt s),
              "ready at:" <+> pretty (show $ _serverInfoReadyAt s),
              "ended at:" <+> pretty (show $ _serverInfoEndedAt s)
            ]
        )

-- | A warm instance sitting in the pool, not yet claimed by a deployment.
data PreprovisionedServer = PreprovisionedServer
  { _preprovisionedServerId :: PreprovisionedServerId,
    _preprovisionedServerProvider :: Provider,
    _preprovisionedServerInstanceId :: Maybe InstanceId,
    _preprovisionedServerAddress :: ServerAddress,
    _preprovisionedServerTier :: ServerTier,
    _preprovisionedServerCreatedAt :: UTCTime,
    _preprovisionedServerReadyAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)

-- * Exposure

-- | What the provisioner published for a guest: the host port forwarding to
-- its SSH, and each (guest port, host port) pair for its TCP services.
data ExposeResult = ExposeResult
  { _exposeResultSshPort :: Maybe Int,
    _exposeResultTcpPorts :: [(Int, Int)]
  }
  deriving stock (Eq, Show, Generic)

-- * Deployments

data DeploymentType
  = BranchDeployment Branch
  | GhPrDeployment GhPullRequestId
  deriving stock (Eq, Show, Generic)

instance ToJSON DeploymentType where
  toJSON = ourToJSON

fromDeploymentType :: (Branch -> a) -> (GhPullRequestId -> a) -> DeploymentType -> a
fromDeploymentType onBranch onPr = \case
  BranchDeployment branch -> onBranch branch
  GhPrDeployment prId -> onPr prId

ghPrDeployment :: DeploymentType -> Maybe GhPullRequestId
ghPrDeployment = fromDeploymentType (const Nothing) Just

