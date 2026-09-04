-- | Types for hosted servers, kept provider-agnostic.
module Garnix.Hosting.Types
  ( Provider (..),
    providerName,
    parseProvider,
    InstanceId (..),
    ServerId (..),
    PreprovisionedServerId (..),
    ServerTier (..),
    defaultServerTier,
    tierResources,
    parseServerTier,
    tierWithinCap,
    HostingBudget (..),
    branchReserveResources,
    ServerAddress (..),
    serverAddressText,
    ServerInfo (..),
    PreprovisionedServer (..),
    ExposeResult (..),
    DeploymentType (..),
    fromDeploymentType,
    ghPrDeployment,
    ServerPortType (..),
    ServerPort (..),
    ServerExtras (..),
    defaultServerExtras,
    ServerToSpinUp (..),
    DeployPlan (..),
    Host (..),
    PrHostList (..),
    hostToDomainName,
    hostToPrimaryDomainName,
    ServerStatsSample (..),
    HostStatsReport (..),
  )
where

import Data.Aeson ((.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Garnix.Prelude
import Garnix.Types
  ( Branch (..),
    Build,
    BuildId,
    GhLogin (..),
    GhPullRequestId (..),
    GhRepoName (..),
    GhRepoOwner (..),
    PackageName (..),
  )
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

-- | The size a @servers:@ entry gets when its @machine@ field is absent.
defaultServerTier :: ServerTier
defaultServerTier = ServerTier "i1x2"

-- | vCPU and MiB for a tier.
--
-- The canonical spelling is garnix's @i\<vcpu\>x\<gib\>@ form, which is what a
-- @servers:@ entry's @machine@ defaults to and what users actually write.
-- @small@\/@medium@\/@large@ are kept as aliases. Anything we cannot parse
-- falls back to the smallest tier rather than failing a deploy over a typo.
tierResources :: ServerTier -> (Int, Int)
tierResources tier = case getServerTier tier of
  "small" -> (1, 2048)
  "medium" -> (2, 4096)
  "large" -> (4, 8192)
  other -> fromMaybe (1, 2048) (parseMachineTier other)

-- | Validate a tier as written in @garnix.yaml@.
--
-- 'tierResources' is deliberately total -- a running deploy must not die over
-- a tier it cannot parse -- so the rejection of typos happens here instead,
-- at the point where the user's yaml is read and they can still see the error.
parseServerTier :: Text -> Either String ServerTier
parseServerTier raw
  | raw `elem` ["small", "medium", "large"] = Right (ServerTier raw)
  | isJust (parseMachineTier raw) = Right (ServerTier raw)
  | otherwise =
      Left
        $ cs
        $ "Wrong machine size: "
        <> raw
        <> ". Write it as i<vcpus>x<gibibytes> (e.g. i2x4), "
        <> "or use one of: small, medium, large."

-- | Parse @iNxM@ (N vCPUs, M GiB) into (vCPU, MiB).
parseMachineTier :: Text -> Maybe (Int, Int)
parseMachineTier raw = do
  rest <- T.stripPrefix "i" raw
  let (vcpuText, gibText') = T.breakOn "x" rest
  gibText <- T.stripPrefix "x" gibText'
  vcpu <- readPositive vcpuText
  gib <- readPositive gibText
  pure (vcpu, gib * 1024)
  where
    readPositive text = case T.decimal text of
      Right (value, "") | value > 0 -> Just value
      _ -> Nothing

-- | Whether a tier is within a cap. Both dimensions have to fit: @i1x16@ is
-- over an @i4x8@ cap on memory even though it is under it on vCPUs.
tierWithinCap :: Maybe ServerTier -> ServerTier -> Bool
tierWithinCap Nothing _ = True
tierWithinCap (Just cap) tier =
  let (capVcpus, capMiB) = tierResources cap
      (vcpus, miB) = tierResources tier
   in vcpus <= capVcpus && miB <= capMiB

-- | What this instance is willing to spend on hosting, and on whom.
--
-- Lives here rather than in "Garnix.Hosting.ServerPool" because
-- "Garnix.Monad" carries one in its @Env@, and ServerPool imports Monad.
data HostingBudget = HostingBudget
  { -- | Cap on the vCPUs all guests together may hold.
    _hostingBudgetVcpus :: Maybe Int,
    -- | Cap on the MiB all guests together may hold.
    _hostingBudgetMemoryMiB :: Maybe Int,
    -- | Largest tier a single @servers:@ entry may ask for. About one repo's
    -- declaration rather than the instance as a whole, so it is checked while
    -- planning, where the user can still be told which entry is at fault.
    _hostingBudgetMaxTier :: Maybe ServerTier,
    -- | Resources pull request deploys have to leave unspoken-for, so a branch
    -- deploy always has room to land.
    _hostingBudgetBranchReserve :: Maybe ServerTier
  }
  deriving stock (Eq, Show, Generic)

-- | The reserve as (vCPUs, MiB). Zero when none is configured.
branchReserveResources :: HostingBudget -> (Int, Int)
branchReserveResources =
  maybe (0, 0) tierResources . _hostingBudgetBranchReserve

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
    _serverInfoIsPrimary :: Bool,
    -- | The persistence name of the build that deployed this server (read
    -- from the joined @builds@ row, not stored on @servers@). Servers are
    -- matched to redeploy targets by it, which is what makes a persistent
    -- guest survive a push instead of being recreated.
    _serverInfoPersistenceName :: Maybe Text
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

-- * The deploy spec

-- | How a declared port is reached from outside. @http@ ports are routed by
-- the gateway under a name-prefixed subdomain; @tcp@ ports get a host port
-- DNAT'd to them by the provisioner.
data ServerPortType
  = HttpPort
  | TcpPort
  deriving stock (Eq, Show, Ord, Generic)

instance FromJSON ServerPortType where
  parseJSON = Aeson.withText "ServerPortType" $ \case
    "http" -> pure HttpPort
    "tcp" -> pure TcpPort
    other -> fail $ "unknown port type: " <> cs other

instance ToJSON ServerPortType where
  toJSON = \case
    HttpPort -> Aeson.String "http"
    TcpPort -> Aeson.String "tcp"

-- | One entry of @garnix.server.ports@.
data ServerPort = ServerPort
  { _serverPortName :: Text,
    _serverPortPort :: Int,
    _serverPortType :: ServerPortType
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ServerPort where
  parseJSON = Aeson.withObject "ServerPort" $ \object ->
    ServerPort
      <$> object
      .: "name"
      <*> object
      .: "port"
      <*> object
      .: "type"

-- | The decoded @config.garnix.server.deploySpec@ of one built
-- @nixosConfiguration@: the knobs of a server that is already being deployed.
--
-- WHETHER a configuration is deployed, from which branch, and at what size is
-- declared in @garnix.yaml@ under @servers:@ — see
-- 'Garnix.YamlConfig.ServerSection'. Nothing here decides that, deliberately:
-- reading the yaml has to tell you what a push does.
--
-- Fields of the nix option tree that this fork does not act on (@authentik@,
-- @backups@, @applicationLog@) are deliberately not decoded.
data ServerExtras = ServerExtras
  { _serverExtrasDomains :: [Text],
    _serverExtrasExposeSSH :: Bool,
    _serverExtrasAuthorizeDeployerGithubKeys :: Bool,
    _serverExtrasAuthorizedSSHKeys :: [Text],
    _serverExtrasPorts :: [ServerPort]
  }
  deriving stock (Eq, Show, Generic)

-- | What a configuration that declares no extras at all amounts to. Used for a
-- server the yaml asks for whose build carries no readable @deploySpec@.
defaultServerExtras :: ServerExtras
defaultServerExtras =
  ServerExtras
    { _serverExtrasDomains = [],
      _serverExtrasExposeSSH = False,
      _serverExtrasAuthorizeDeployerGithubKeys = False,
      _serverExtrasAuthorizedSSHKeys = [],
      _serverExtrasPorts = []
    }

instance FromJSON ServerExtras where
  parseJSON = Aeson.withObject "ServerExtras" $ \object ->
    ServerExtras
      <$> (fromMaybe [] <$> object .:? "domains")
      <*> (fromMaybe False <$> object .:? "exposeSSH")
      <*> (fromMaybe False <$> object .:? "authorizeDeployerGithubKeys")
      <*> (fromMaybe [] <$> object .:? "authorizedSSHKeys")
      <*> (fromMaybe [] <$> object .:? "ports")

-- * The deploy plan

-- | A server the current build wants running, paired with the build that
-- produced its @nixosConfiguration@.
data ServerToSpinUp = ServerToSpinUp
  { _serverToSpinUpTier :: ServerTier,
    _serverToSpinUpBuild :: Build,
    _serverToSpinUpIsPrimary :: Bool,
    -- | @garnix.server.exposeSSH@: ask the provisioner for a public SSH port.
    _serverToSpinUpExposeSSH :: Bool,
    -- | @garnix.server.authorizeDeployerGithubKeys@: authorize the pushing
    -- user's @github.com\/\<user\>.keys@ for the guest's @garnix@ account.
    _serverToSpinUpAuthorizeDeployerGithubKeys :: Bool,
    -- | @garnix.server.authorizedSSHKeys@: extra keys for the same account.
    _serverToSpinUpAuthorizedSSHKeys :: [Text],
    -- | (name, guest port) for each @http@ port; routed by the gateway.
    _serverToSpinUpHttpPorts :: [(Text, Int)],
    -- | (name, guest port) for each @tcp@ port; DNAT'd by the provisioner.
    _serverToSpinUpTcpPorts :: [(Text, Int)],
    -- | @garnix.server.domains@: extra hostnames the server answers on.
    _serverToSpinUpDomains :: [Text]
  }
  deriving stock (Eq, Show, Generic)

-- | The difference between what is running and what the current build wants.
data DeployPlan = DeployPlan
  { _deployPlanToSpinDown :: [ServerInfo],
    _deployPlanToSpinUp :: [ServerToSpinUp],
    -- | Persistent guests kept in place, paired with the full desired spec.
    -- A redeploy has to converge credentials and exposure as well as switch
    -- the closure, so the 'Build' alone would not be enough.
    _deployPlanToRedeploy :: [(ServerInfo, ServerToSpinUp)]
  }
  deriving stock (Eq, Show, Generic)

-- * Routable hosts

-- | A running server joined with the build that deployed it: everything the
-- gateway needs to route a request to it.
data Host = Host
  { _hostRepoOwner :: GhRepoOwner,
    _hostRepoName :: GhRepoName,
    _hostBranch :: Branch,
    _hostPackageName :: PackageName,
    _hostPullRequest :: Maybe GhPullRequestId,
    _hostAddress :: ServerAddress,
    _hostDrvPath :: Maybe FilePath,
    _hostPersistenceName :: Maybe Text,
    _hostServerId :: ServerId,
    _hostInstanceId :: Maybe InstanceId,
    _hostIsPrimary :: Bool,
    -- | @garnix.server.domains@, as recorded at deploy time.
    _hostDomains :: [Text],
    -- | @http@ ports, as recorded at deploy time: (name, guest port).
    _hostHttpPorts :: [(Text, Int)]
  }
  deriving stock (Eq, Show, Generic)

newtype PrHostList = PrHostList {getPrHostList :: [Host]}
  deriving stock (Eq, Show, Generic)

-- | The canonical hostname of a deployed server, relative to the hosting base
-- domain: @\<package\>.\<branch|pull-N\>.\<repo\>.\<owner\>@.
hostToDomainName :: Host -> Text
hostToDomainName host =
  T.intercalate
    "."
    [ getPackageName (_hostPackageName host),
      maybe
        (getBranch (_hostBranch host))
        (("pull-" <>) . show . getGhPullRequestId)
        (_hostPullRequest host),
      getGhRepoName (_hostRepoName host),
      getGhLogin (getGhRepoOwner (_hostRepoOwner host))
    ]

-- | The short hostname a primary deploy additionally answers on:
-- @\<repo\>.\<owner\>@. 'Nothing' for anything but a primary branch deploy.
hostToPrimaryDomainName :: Host -> Maybe Text
hostToPrimaryDomainName host
  | not (_hostIsPrimary host) = Nothing
  | isJust (_hostPullRequest host) = Nothing
  | otherwise =
      Just
        $ getGhRepoName (_hostRepoName host)
        <> "."
        <> getGhLogin (getGhRepoOwner (_hostRepoOwner host))

-- * Guest stats

-- | One resource sample from a deployed guest. CPU is a utilisation percentage
-- (0-100) from @\/proc\/stat@ deltas; memory is @MemTotal@ and
-- @MemTotal - MemAvailable@ from @\/proc\/meminfo@, in kibibytes.
data ServerStatsSample = ServerStatsSample
  { _serverStatsSampleCpuPct :: Double,
    _serverStatsSampleMemUsedKb :: Int64,
    _serverStatsSampleMemTotalKb :: Int64,
    _serverStatsSampleSampledAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ServerStatsSample where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

-- | An inbound stats push from a guest (@POST \/api\/hosts\/stats@).
--
-- The guest is unauthenticated, like the heartbeat, and identifies itself by
-- the provider-assigned instance id the backend wrote into its reporter
-- environment at deploy time. The JSON key is still @provisioner_id@, which is
-- what the guest's reporter script sends.
data HostStatsReport = HostStatsReport
  { _hostStatsReportInstanceId :: InstanceId,
    _hostStatsReportCpuPct :: Double,
    _hostStatsReportMemUsedKb :: Int64,
    _hostStatsReportMemTotalKb :: Int64
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON HostStatsReport where
  parseJSON = Aeson.withObject "HostStatsReport" $ \object ->
    HostStatsReport
      <$> object
      .: "provisioner_id"
      <*> object
      .: "cpu_pct"
      <*> object
      .: "mem_used_kb"
      <*> object
      .: "mem_total_kb"

instance ToJSON HostStatsReport where
  toJSON report =
    Aeson.object
      [ "provisioner_id" .= _hostStatsReportInstanceId report,
        "cpu_pct" .= _hostStatsReportCpuPct report,
        "mem_used_kb" .= _hostStatsReportMemUsedKb report,
        "mem_total_kb" .= _hostStatsReportMemTotalKb report
      ]
