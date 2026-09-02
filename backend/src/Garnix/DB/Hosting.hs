{-# LANGUAGE QuasiQuotes #-}

-- | Database access for hosted servers and the warm instance pool.
module Garnix.DB.Hosting
  ( createPoolServer,
    markPoolServerReady,
    deletePoolServer,
    getPoolServers,
    claimPoolServer,
    getLiveServers,
    getServer,
    endServer,
    getRunningServersOf,
    getUnreadyServers,
    updateServerPostDeploy,
    appendToServerDeployLog,
    setServerExposed,
    getServerExposures,
    setServerDomains,
    getServerDomains,
    setServerSshUsers,
    getServerSshUsers,
    getAllRunningHosts,
    getShutdownCandidates,
    upsertServerStats,
    getServerGuestIpByInstanceId,
    getServerStatsHistory,
    serverStatsWindow,
  )
where

import Control.Lens
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.DB qualified as DB
import Garnix.Hosting.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
  ( Branch (..),
    BuildId,
    Error (..),
    GhPullRequestId,
    GhRepoName,
    GhRepoOwner,
    HasGhRepoName (ghRepoName),
    HasGhRepoOwner (ghRepoOwner),
    PackageName (..),
    RepoInfo,
  )

-- | Decode the provider column, refusing a value no driver claims.
decodeProvider :: Text -> M Provider
decodeProvider raw = case parseProvider raw of
  Right provider -> pure provider
  Left problem -> throw $ OtherError problem

-- | Register a pool instance before it exists, so the row's id can be the
-- handle the provisioner names it by.
createPoolServer :: Provider -> ServerTier -> M PreprovisionedServerId
createPoolServer provider tier = do
  let providerText = providerName provider
      tierText = getServerTier tier
  res <-
    DB.pgQuery
      [pgSQL|
    INSERT INTO server_pool (provider, server_tier)
    VALUES (${providerText}, ${tierText})
    RETURNING id
  |]
  case res of
    [poolId] -> pure poolId
    _ -> throw $ OtherError "createPoolServer did not return exactly one row"

-- | Record the address the provisioner handed back and mark the instance
-- claimable.
markPoolServerReady :: PreprovisionedServer -> M ()
markPoolServerReady server = do
  let poolId = _preprovisionedServerId server
      instanceId = getInstanceId <$> _preprovisionedServerInstanceId server
      address = _preprovisionedServerAddress server
      ipv4 = _serverAddressIpv4 address
      ipv6 = _serverAddressIpv6 address
  void
    $ DB.pgExec
      [pgSQL|
    UPDATE server_pool
    SET instance_id = ${instanceId},
        ipv4 = ${ipv4}::text::inet,
        ipv6 = ${ipv6}::text::inet,
        ready_at = now()
    WHERE id = ${poolId}
  |]

deletePoolServer :: PreprovisionedServerId -> M ()
deletePoolServer poolId =
  void
    $ DB.pgExec
      [pgSQL| DELETE FROM server_pool WHERE id = ${poolId} |]

getPoolServers :: M [PreprovisionedServer]
getPoolServers = do
  rows <-
    DB.pgQuery
      [pgSQL|
    SELECT id, provider, instance_id, host(ipv4), host(ipv6),
           server_tier, created_at, ready_at
    FROM server_pool
    ORDER BY id
  |]
  forM rows $ \(poolId, provider, instanceId, ipv4, ipv6, tier, createdAt, readyAt) -> do
    decodedProvider <- decodeProvider provider
    pure
      PreprovisionedServer
        { _preprovisionedServerId = poolId,
          _preprovisionedServerProvider = decodedProvider,
          _preprovisionedServerInstanceId = InstanceId <$> instanceId,
          _preprovisionedServerAddress =
            ServerAddress {_serverAddressIpv4 = ipv4, _serverAddressIpv6 = ipv6},
          _preprovisionedServerTier = ServerTier tier,
          _preprovisionedServerCreatedAt = createdAt,
          _preprovisionedServerReadyAt = readyAt
        }

-- | Take a warm instance of @tier@ out of the pool and record it as a
-- deployed server, in one statement.
claimPoolServer ::
  Provider ->
  ServerTier ->
  BuildId ->
  Maybe GhPullRequestId ->
  Bool ->
  M (Maybe ServerId)
claimPoolServer provider tier buildId pullRequest isPrimary = do
  let providerText = providerName provider
      tierText = getServerTier tier
  res <-
    DB.pgQuery
      [pgSQL|
    WITH claimed AS (
      DELETE FROM server_pool
      WHERE id = (
        SELECT id FROM server_pool
        WHERE provider = ${providerText}
          AND server_tier = ${tierText}
          AND ready_at IS NOT NULL
        ORDER BY ready_at
        FOR UPDATE SKIP LOCKED
        LIMIT 1
      )
      RETURNING provider, instance_id, ipv4, ipv6, server_tier
    )
    INSERT INTO servers
      (configuration_build_id, provider, instance_id, ipv4, ipv6,
       server_tier, pull_request, is_primary, ready_at)
    SELECT ${buildId}, claimed.provider, claimed.instance_id,
           claimed.ipv4, claimed.ipv6, claimed.server_tier,
           ${pullRequest}, ${isPrimary}, now()
    FROM claimed
    RETURNING id
  |]
  pure $ case res of
    [serverId] -> Just serverId
    _ -> Nothing

getLiveServers :: M [ServerInfo]
getLiveServers = do
  rows <-
    DB.pgQuery
      [pgSQL|
    SELECT servers.id, servers.provider, servers.instance_id,
           host(servers.ipv4), host(servers.ipv6), servers.created_at,
           servers.ended_at, servers.configuration_build_id,
           servers.pull_request, servers.ready_at, servers.server_tier,
           servers.is_primary, builds.persistence_name
    FROM servers
    INNER JOIN builds ON servers.configuration_build_id = builds.id
    WHERE servers.ended_at IS NULL
    ORDER BY servers.id
  |]
  traverse decodeServer rows

getServer :: ServerId -> M (Maybe ServerInfo)
getServer serverId = do
  rows <-
    DB.pgQuery
      [pgSQL|
    SELECT servers.id, servers.provider, servers.instance_id,
           host(servers.ipv4), host(servers.ipv6), servers.created_at,
           servers.ended_at, servers.configuration_build_id,
           servers.pull_request, servers.ready_at, servers.server_tier,
           servers.is_primary, builds.persistence_name
    FROM servers
    INNER JOIN builds ON servers.configuration_build_id = builds.id
    WHERE servers.id = ${serverId}
  |]
  case rows of
    [] -> pure Nothing
    (row : _) -> Just <$> decodeServer row

-- | Mark a server as torn down. Idempotent.
endServer :: ServerId -> M ()
endServer serverId =
  void
    $ DB.pgExec
      [pgSQL|
    UPDATE servers SET ended_at = now()
    WHERE id = ${serverId} AND ended_at IS NULL
  |]

decodeServer ::
  ( ServerId,
    Text,
    Maybe Text,
    Maybe Text,
    Maybe Text,
    UTCTime,
    Maybe UTCTime,
    BuildId,
    Maybe GhPullRequestId,
    Maybe UTCTime,
    Text,
    Bool,
    Maybe Text
  ) ->
  M ServerInfo
decodeServer
  ( serverId,
    provider,
    instanceId,
    ipv4,
    ipv6,
    createdAt,
    endedAt,
    buildId,
    pullRequest,
    readyAt,
    tier,
    isPrimary,
    persistence
    ) = do
    decodedProvider <- decodeProvider provider
    pure
      ServerInfo
        { _serverInfoId = serverId,
          _serverInfoProvider = decodedProvider,
          _serverInfoInstanceId = InstanceId <$> instanceId,
          _serverInfoAddress =
            ServerAddress {_serverAddressIpv4 = ipv4, _serverAddressIpv6 = ipv6},
          _serverInfoCreatedAt = createdAt,
          _serverInfoEndedAt = endedAt,
          _serverInfoConfigurationBuildId = buildId,
          _serverInfoPullRequest = pullRequest,
          _serverInfoReadyAt = readyAt,
          _serverInfoTier = ServerTier tier,
          _serverInfoIsPrimary = isPrimary,
          _serverInfoPersistenceName = persistence
        }

-- * Deploy-time reads

-- | Live servers of one deployment target, so a rollout can tell what is
-- already running before it decides what to change.
getRunningServersOf :: RepoInfo -> DeploymentType -> M [ServerInfo]
getRunningServersOf repoInfo deploymentType = do
  let owner = repoInfo ^. ghRepoOwner
      repo = repoInfo ^. ghRepoName
  rows <- case deploymentType of
    BranchDeployment branch ->
      DB.pgQuery
        [pgSQL|
      SELECT servers.id, servers.provider, servers.instance_id,
             host(servers.ipv4), host(servers.ipv6), servers.created_at,
             servers.ended_at, servers.configuration_build_id,
             servers.pull_request, servers.ready_at, servers.server_tier,
             servers.is_primary, builds.persistence_name
      FROM servers
      INNER JOIN builds ON servers.configuration_build_id = builds.id
      WHERE builds.repo_user = ${owner}
        AND builds.repo_name = ${repo}
        AND builds.branch = ${branch}
        AND servers.pull_request IS NULL
        AND servers.ended_at IS NULL
      ORDER BY servers.id
    |]
    GhPrDeployment prId ->
      DB.pgQuery
        [pgSQL|
      SELECT servers.id, servers.provider, servers.instance_id,
             host(servers.ipv4), host(servers.ipv6), servers.created_at,
             servers.ended_at, servers.configuration_build_id,
             servers.pull_request, servers.ready_at, servers.server_tier,
             servers.is_primary, builds.persistence_name
      FROM servers
      INNER JOIN builds ON servers.configuration_build_id = builds.id
      WHERE builds.repo_user = ${owner}
        AND builds.repo_name = ${repo}
        AND servers.pull_request = ${prId}
        AND servers.ended_at IS NULL
      ORDER BY servers.id
    |]
  traverse decodeServer rows

-- | Servers claimed out of the pool whose deploy never reached the ready
-- checkpoint. A backend that died mid-deploy cannot know how far it got, so
-- startup tears these down and lets planning claim a fresh guest.
getUnreadyServers :: M [ServerInfo]
getUnreadyServers = do
  rows <-
    DB.pgQuery
      [pgSQL|
    SELECT servers.id, servers.provider, servers.instance_id,
           host(servers.ipv4), host(servers.ipv6), servers.created_at,
           servers.ended_at, servers.configuration_build_id,
           servers.pull_request, servers.ready_at, servers.server_tier,
           servers.is_primary, builds.persistence_name
    FROM servers
    INNER JOIN builds ON servers.configuration_build_id = builds.id
    WHERE servers.ready_at IS NULL AND servers.ended_at IS NULL
    ORDER BY servers.created_at, servers.id
  |]
  traverse decodeServer rows

-- * Deploy-time writes

-- | Point an existing (persistent) server row at the build that just
-- redeployed it, and mark it ready.
updateServerPostDeploy :: ServerInfo -> M ()
updateServerPostDeploy server = do
  let address = _serverInfoAddress server
      ipv4 = _serverAddressIpv4 address
      ipv6 = _serverAddressIpv6 address
      instanceId = getInstanceId <$> _serverInfoInstanceId server
  changed <-
    DB.pgExec
      [pgSQL|
    UPDATE servers
    SET instance_id = ${instanceId},
        configuration_build_id = ${_serverInfoConfigurationBuildId server},
        ipv4 = ${ipv4}::text::inet,
        ipv6 = ${ipv6}::text::inet,
        ended_at = ${_serverInfoEndedAt server},
        ready_at = ${_serverInfoReadyAt server},
        is_primary = ${_serverInfoIsPrimary server}
    WHERE id = ${_serverInfoId server}
  |]
  case changed of
    0 -> throw $ OtherError "updateServerPostDeploy: no such server"
    1 -> pure ()
    _ -> throw $ OtherError "updateServerPostDeploy: updated more than one row"

appendToServerDeployLog :: ServerId -> Text -> M ()
appendToServerDeployLog serverId logs =
  void
    $ DB.pgExec
      [pgSQL|
    UPDATE servers
    SET deploy_logs = deploy_logs || ${logs} || E'\n'
    WHERE id = ${serverId}
  |]

-- | Record what a guest is reachable on: the host ports the provisioner
-- DNAT'd for SSH and TCP, plus the named @http@ ports the gateway routes.
--
-- Written as one blob, in one statement, because a partial write here means a
-- guest that is half-routable.
setServerExposed :: ServerId -> ExposeResult -> [(Text, Int)] -> M ()
setServerExposed serverId result httpPorts = do
  let encoded = cs (Aeson.encode (exposedToJSON result httpPorts)) :: Text
  void
    $ DB.pgExec
      [pgSQL|
    UPDATE servers SET exposed = ${encoded}::text::json WHERE id = ${serverId}
  |]

-- | The JSON shape stored in @servers.exposed@:
--
-- > {"ssh_port": 2201 | null,
-- >  "tcp":  [{"guest": 5432, "host": 32001}],
-- >  "http": [{"name": "api", "port": 8080}]}
exposedToJSON :: ExposeResult -> [(Text, Int)] -> Aeson.Value
exposedToJSON result httpPorts =
  Aeson.object
    [ "ssh_port" Aeson..= _exposeResultSshPort result,
      "tcp"
        Aeson..= [ Aeson.object ["guest" Aeson..= guest, "host" Aeson..= hostPort]
                   | (guest, hostPort) <- _exposeResultTcpPorts result
                 ],
      "http"
        Aeson..= [ Aeson.object ["name" Aeson..= name, "port" Aeson..= port]
                   | (name, port) <- httpPorts
                 ]
    ]

-- | Exposure blobs of every live server, as an assoc list ('ServerId' has no
-- 'Ord' instance). Rows we cannot decode are dropped rather than failing the
-- whole read — one bad blob should not take the gateway config down with it.
getServerExposures :: M [(ServerId, Aeson.Value)]
getServerExposures = do
  rows <-
    DB.pgQuery
      [pgSQL|
    SELECT id, exposed::text
    FROM servers
    WHERE ended_at IS NULL AND exposed IS NOT NULL
  |]
  pure
    [ (serverId, value)
      | (serverId, blob) <- rows,
        raw <- toList (blob :: Maybe Text),
        Just value <- [Aeson.decode (cs raw)]
    ]

-- | The named @http@ ports out of one exposure blob.
httpPortsOf :: Aeson.Value -> [(Text, Int)]
httpPortsOf blob = case Aeson.fromJSON blob of
  Aeson.Success (ExposedHttp entries) -> [(name, port) | HttpPortEntry name port <- entries]
  Aeson.Error _ -> []

-- | Just enough of the exposure blob to read its @http@ list back.
newtype ExposedHttp = ExposedHttp [HttpPortEntry]

data HttpPortEntry = HttpPortEntry Text Int

instance FromJSON ExposedHttp where
  parseJSON = Aeson.withObject "ExposedHttp" $ \object ->
    ExposedHttp . fromMaybe [] <$> object Aeson..:? "http"

instance FromJSON HttpPortEntry where
  parseJSON = Aeson.withObject "HttpPortEntry" $ \object ->
    HttpPortEntry <$> object Aeson..: "name" <*> object Aeson..: "port"

-- | Converge a persistent server's declared hostnames. New servers get these
-- at claim time; a reused row has to be updated, or a domain the user removed
-- keeps routing forever.
setServerDomains :: ServerId -> [Text] -> M ()
setServerDomains serverId domains = do
  let encoded = cs (Aeson.encode domains) :: Text
  void
    $ DB.pgExec
      [pgSQL|
    UPDATE servers SET domains = ${encoded}::text::json WHERE id = ${serverId}
  |]

getServerDomains :: M [(ServerId, [Text])]
getServerDomains = do
  rows <-
    DB.pgQuery
      [pgSQL|!
    SELECT id, domains::text FROM servers WHERE ended_at IS NULL
  |]
  pure
    [ (serverId, domains)
      | (serverId, raw) <- rows,
        Just domains <- [Aeson.decode (cs (raw :: Text))]
    ]

setServerSshUsers :: ServerId -> [Text] -> M ()
setServerSshUsers serverId users = do
  let encoded = cs (Aeson.encode users) :: Text
  void
    $ DB.pgExec
      [pgSQL|
    UPDATE servers SET ssh_users = ${encoded}::text::json WHERE id = ${serverId}
  |]

getServerSshUsers :: M [(ServerId, [Text])]
getServerSshUsers = do
  rows <-
    DB.pgQuery
      [pgSQL|!
    SELECT id, ssh_users::text FROM servers WHERE ended_at IS NULL
  |]
  pure
    [ (serverId, users)
      | (serverId, raw) <- rows,
        Just users <- [Aeson.decode (cs (raw :: Text))]
    ]

-- * Routing

-- | The row shape both host queries select, in one place so the two stay in
-- step: a running server joined with the build that deployed it.
type HostRow =
  ( GhRepoOwner,
    GhRepoName,
    Maybe Text,
    Text,
    Maybe GhPullRequestId,
    Maybe Text,
    Maybe Text,
    Maybe Text,
    Maybe Text,
    ServerId,
    Maybe Text,
    Bool
  )

decodeHostRow :: HostRow -> Host
decodeHostRow
  ( owner,
    repo,
    branch,
    package,
    pullRequest,
    ipv4,
    ipv6,
    drvPath,
    persistenceName,
    serverId,
    instanceId,
    isPrimary
    ) =
    Host
      { _hostRepoOwner = owner,
        _hostRepoName = repo,
        _hostBranch = Branch (fromMaybe "" branch),
        _hostPackageName = PackageName package,
        _hostPullRequest = pullRequest,
        _hostAddress = ServerAddress {_serverAddressIpv4 = ipv4, _serverAddressIpv6 = ipv6},
        _hostDrvPath = cs <$> drvPath,
        _hostPersistenceName = persistenceName,
        _hostServerId = serverId,
        _hostInstanceId = InstanceId <$> instanceId,
        _hostIsPrimary = isPrimary,
        -- Filled in by 'decorateHosts'.
        _hostDomains = [],
        _hostHttpPorts = []
      }

-- | Every ready, live, addressable server joined with the build that deployed
-- it. The gateway's dynamic configuration is generated from this.
getAllRunningHosts :: M [Host]
getAllRunningHosts = do
  rows <-
    DB.pgQuery
      [pgSQL|
    SELECT builds.repo_user, builds.repo_name, builds.branch, builds.package,
           servers.pull_request, host(servers.ipv4), host(servers.ipv6),
           builds.drv_path, builds.persistence_name, servers.id,
           servers.instance_id, servers.is_primary
    FROM servers
    INNER JOIN builds ON servers.configuration_build_id = builds.id
    WHERE servers.ended_at IS NULL
      AND servers.ready_at IS NOT NULL
      AND servers.ipv4 IS NOT NULL
    ORDER BY servers.id
  |]
  decorateHosts (map decodeHostRow rows)

-- | Live PR deploys that have been up for over 12 hours. Candidates only —
-- the caller still checks each against recent heartbeats before tearing it
-- down.
getShutdownCandidates :: M PrHostList
getShutdownCandidates = do
  rows <-
    DB.pgQuery
      [pgSQL|
    SELECT builds.repo_user, builds.repo_name, builds.branch, builds.package,
           servers.pull_request, host(servers.ipv4), host(servers.ipv6),
           builds.drv_path, builds.persistence_name, servers.id,
           servers.instance_id, servers.is_primary
    FROM servers
    INNER JOIN builds ON servers.configuration_build_id = builds.id
    WHERE servers.ended_at IS NULL
      AND servers.ipv4 IS NOT NULL
      AND servers.pull_request IS NOT NULL
      AND servers.ready_at IS NOT NULL
      AND now() - servers.ready_at > interval '12 hours'
    ORDER BY servers.id
  |]
  PrHostList <$> decorateHosts (map decodeHostRow rows)

-- | Attach the deploy-time facts that live on the server row rather than on
-- the build: declared domains and named http ports.
decorateHosts :: [Host] -> M [Host]
decorateHosts hosts = do
  domains <- getServerDomains
  exposures <- getServerExposures
  let key = getHashId . getServerId
      domainsBy = Map.fromList [(key serverId, ds) | (serverId, ds) <- domains]
      httpBy = Map.fromList [(key serverId, httpPortsOf blob) | (serverId, blob) <- exposures]
  pure
    [ host
        { _hostDomains = Map.findWithDefault [] (key (_hostServerId host)) domainsBy,
          _hostHttpPorts = Map.findWithDefault [] (key (_hostServerId host)) httpBy
        }
      | host <- hosts
    ]

-- * Guest stats

-- | How many samples we keep per server. The guest reporter pushes roughly
-- every 20s, so this is about 20 minutes of history.
serverStatsWindow :: Int64
serverStatsWindow = 60

-- | Store a sample pushed by a guest. Returns whether it matched a live
-- server: an unmatched push is answered with 404 rather than silently
-- accepted, since it means the guest outlived its server row.
upsertServerStats :: HostStatsReport -> M Bool
upsertServerStats report = do
  let instanceId = getInstanceId (_hostStatsReportInstanceId report)
      cpuPct = _hostStatsReportCpuPct report
      memUsedKb = _hostStatsReportMemUsedKb report
      memTotalKb = _hostStatsReportMemTotalKb report
  serverIds <-
    DB.pgQuery
      [pgSQL|
    SELECT id FROM servers
    WHERE instance_id = ${instanceId} AND ended_at IS NULL
    ORDER BY created_at DESC
    LIMIT 1
  |]
  forM_ (serverIds :: [ServerId]) $ \serverId -> do
    void
      $ DB.pgExec
        [pgSQL|
      INSERT INTO server_stats (server_id, cpu_pct, mem_used_kb, mem_total_kb)
      VALUES (${serverId}, ${cpuPct}, ${memUsedKb}, ${memTotalKb})
    |]
    void
      $ DB.pgExec
        [pgSQL|
      DELETE FROM server_stats
      WHERE server_id = ${serverId}
        AND id NOT IN (
          SELECT id FROM server_stats
          WHERE server_id = ${serverId}
          ORDER BY sampled_at DESC, id DESC
          LIMIT ${serverStatsWindow}
        )
    |]
  pure $ not (null serverIds)

-- | The guest IP the backend recorded for the live server owning an instance
-- id, so a stats push can be required to come from that specific guest rather
-- than merely from somewhere on the guest bridge.
getServerGuestIpByInstanceId :: InstanceId -> M (Maybe Text)
getServerGuestIpByInstanceId instanceId = do
  rows <-
    DB.pgQuery
      [pgSQL|!
    SELECT host(ipv4) FROM servers
    WHERE instance_id = ${getInstanceId instanceId}
      AND ended_at IS NULL
      AND ipv4 IS NOT NULL
    ORDER BY created_at DESC
    LIMIT 1
  |]
  pure $ case (rows :: [Text]) of
    (ip : _) -> Just ip
    [] -> Nothing

-- | One server's rolling window of samples, oldest first.
getServerStatsHistory :: ServerId -> M [ServerStatsSample]
getServerStatsHistory serverId = do
  rows <-
    DB.pgQuery
      [pgSQL|!
    SELECT cpu_pct, mem_used_kb, mem_total_kb, sampled_at
    FROM server_stats
    WHERE server_id = ${serverId}
    ORDER BY sampled_at DESC, id DESC
    LIMIT ${serverStatsWindow}
  |]
  pure
    $ reverse
      [ ServerStatsSample cpuPct memUsedKb memTotalKb sampledAt
        | (cpuPct, memUsedKb, memTotalKb, sampledAt) <- rows
      ]
