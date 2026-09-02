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
  )
where

import Database.PostgreSQL.Typed (pgSQL)
import Garnix.DB qualified as DB
import Garnix.Hosting.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types (BuildId, Error (..), GhPullRequestId)

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
    SELECT id, provider, instance_id, ipv4::text, ipv6::text,
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
    SELECT id, provider, instance_id, ipv4::text, ipv6::text, created_at,
           ended_at, configuration_build_id, pull_request, ready_at,
           server_tier, is_primary
    FROM servers
    WHERE ended_at IS NULL
    ORDER BY id
  |]
  traverse decodeServer rows

getServer :: ServerId -> M (Maybe ServerInfo)
getServer serverId = do
  rows <-
    DB.pgQuery
      [pgSQL|
    SELECT id, provider, instance_id, ipv4::text, ipv6::text, created_at,
           ended_at, configuration_build_id, pull_request, ready_at,
           server_tier, is_primary
    FROM servers
    WHERE id = ${serverId}
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
    Bool
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
    isPrimary
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
          _serverInfoIsPrimary = isPrimary
        }
