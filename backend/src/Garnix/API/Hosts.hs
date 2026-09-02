-- | The endpoints the hosting gateway and the deployed guests talk to, plus
-- the authenticated view of a user's own servers.
--
-- Everything under @traefik@\/@heartbeat@\/@stats@\/@dns@\/@on-demand-*@ is
-- unauthenticated by design: the gateway and the guests have no session. They
-- are reachable only from inside the deployment, and the stats endpoint gates
-- on the source address.
module Garnix.API.Hosts
  ( HostsAPI (..),
    hostsAPI,
    HostList (..),
    getHostsForTraefik,
    postHostsHeartbeat,
    postHostsStats,
    postHostsStatsGuarded,
    statsSourceAllowed,
    statsClientIp,
    getHosts,
    __onDemandDomainsCache,
  )
where

import Control.Lens
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text qualified as T
import Garnix.GithubInterface.Types (organizationName)
import Garnix.DB qualified as DB
import Garnix.DB.Hosting qualified as DBHosting
import Garnix.Duration
import Garnix.ExpiringCache
import Garnix.GithubUserToken
import Garnix.Hosting.Deploy (stopServer)
import Garnix.Hosting.Helpers
import Garnix.Hosting.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
import Network.Socket (SockAddr (..), hostAddress6ToTuple, hostAddressToTuple)
import Servant.API.RemoteHost (RemoteHost)
import Servant.Auth.Server
import System.IO.Unsafe qualified

data HostsAPI route = HostsAPI
  { -- | The gateway polls this for its dynamic routing configuration.
    _hostsAPIGetHostsForTraefik :: route :- "traefik" :> Get '[JSON] HostList,
    -- | The gateway reports every hostname it served recently, which is what
    -- keeps an idle PR deploy from being torn down.
    _hostsAPIHeartbeat :: route :- "heartbeat" :> ReqBody '[JSON] [Text] :> Post '[JSON] NoContent,
    -- | Guests push their own resource samples here, identifying themselves by
    -- the instance id the backend installed on them after claim.
    _hostsAPIPostStats ::
      route
        :- "stats"
          :> RemoteHost
          :> Header "X-Forwarded-For" Text
          :> ReqBody '[JSON] HostStatsReport
          :> Post '[JSON] NoContent,
    _hostsAPIGetIPsForDns :: route :- "dns" :> Get '[JSON] DnsHosts,
    _hostsAPIGetDomainsForOnDemandResolver ::
      route :- "on-demand-resolver" :> Get '[JSON] OnDemandResolverDomainNames,
    -- | Caddy's @on_demand_tls.ask@ contract: 200 iff the queried domain is a
    -- currently-routable server domain, 404 otherwise.
    _hostsAPIOnDemandCheck ::
      route :- "on-demand-check" :> QueryParam "domain" Text :> Get '[JSON] NoContent,
    _hostsAPIGetHosts ::
      route :- Auth '[JWT, Cookie] AuthJwtPayload :> Get '[JSON] [RunningServer],
    _hostsAPIDeleteHost ::
      route
        :- Auth '[JWT, Cookie] AuthJwtPayload
          :> Capture "serverId" ServerId
          :> Delete '[JSON] ()
  }
  deriving stock (Generic)

hostsAPI :: HostsAPI (AsServerT M)
hostsAPI =
  HostsAPI
    { _hostsAPIGetHostsForTraefik = getHostsForTraefik,
      _hostsAPIHeartbeat = postHostsHeartbeat,
      _hostsAPIPostStats = postHostsStatsGuarded,
      _hostsAPIGetIPsForDns = getHostsForDns,
      _hostsAPIGetDomainsForOnDemandResolver = getDomainsForOnDemandResolver,
      _hostsAPIOnDemandCheck = onDemandCheck,
      _hostsAPIGetHosts = getHosts,
      _hostsAPIDeleteHost = deleteHost
    }

-- * The gateway's routing table

-- | Everything the gateway needs to route, in the shape its HTTP provider
-- expects.
data HostList = HostList
  { hostList :: [Host],
    hostBaseUrl :: Text,
    hostDomain :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON HostList where
  toJSON (HostList hosts baseUrl domain) =
    let -- A router for a name under the hosting domain.
        routerMapPair serviceDomain ruleDomain =
          ( ruleDomain,
            [aesonQQ| {
              service: #{serviceDomain},
              rule: #{"Host(`" <> ruleDomain <> "." <> domain <> "`)"},
              middlewares: ["heartbeatmiddleware"]
              }
            |]
          )
        -- A router for a declared hostname, matched verbatim rather than
        -- suffixed with the hosting domain. It points at the guest's own
        -- service, which the canonical router already defines.
        fqdnRouterPair serviceDomain fqdn =
          ( fqdn,
            [aesonQQ| {
              service: #{serviceDomain},
              rule: #{"Host(`" <> fqdn <> "`)"},
              middlewares: ["heartbeatmiddleware"]
              }
            |]
          )
        -- <name>.<pkg>.<branch>.<repo>.<owner> for a declared http port.
        portDomain host name = name <> "." <> hostToDomainName host
        httpRouters =
          Map.fromList
            $ concatMap
              ( \host ->
                  [routerMapPair (hostToDomainName host) (hostToDomainName host)]
                    <> [ routerMapPair (hostToDomainName host) primary
                         | primary <- toList (hostToPrimaryDomainName host)
                       ]
                    <> [ routerMapPair (portDomain host name) (portDomain host name)
                         | (name, _) <- _hostHttpPorts host
                       ]
                    <> [fqdnRouterPair (hostToDomainName host) fqdn | fqdn <- _hostDomains host]
              )
              hosts
        serviceForUrl url =
          [aesonQQ|
            { loadBalancer:
                { servers: [ { url: #{url} } ] }
            }
          |]
        httpServices =
          Map.fromList
            $ [ (hostToDomainName host, serviceForUrl ("http://" <> address))
                | host <- hosts,
                  address <- toList (serverAddressText (_hostAddress host))
              ]
            <> [ ( portDomain host name,
                   serviceForUrl ("http://" <> address <> ":" <> cs (show port))
                 )
                 | host <- hosts,
                   address <- toList (serverAddressText (_hostAddress host)),
                   (name, port) <- _hostHttpPorts host
               ]
     in [aesonQQ|
         {
          http:
             {
              routers: #{httpRouters},
              services: #{httpServices},
              middlewares: {
                heartbeatmiddleware: {
                  plugin: {
                    heartbeatmiddleware: {
                      reportEndpoint: #{baseUrl <> "/api/hosts/heartbeat"}
                    }
                  }
                }
              }
             }
         }
       |]

-- | Hosts whose every name component is a legal DNS label. One that is not
-- could not be routed to anyway, and would produce a router rule the gateway
-- rejects — taking the whole configuration down with it.
getHostsForTraefik :: M HostList
getHostsForTraefik = do
  baseUrl <- view #baseUrl
  domain <- view #hostingDomain
  hosts <- DBHosting.getAllRunningHosts <&> filter routable
  pure $ HostList hosts baseUrl domain
  where
    routable host =
      isValidSubdomainString (getGhLogin (getGhRepoOwner (_hostRepoOwner host)))
        && isValidSubdomainString (getGhRepoName (_hostRepoName host))
        && ( isValidSubdomainString (getBranch (_hostBranch host))
               || isJust (_hostPullRequest host)
           )
        && isValidSubdomainString (getPackageName (_hostPackageName host))

postHostsHeartbeat :: [Text] -> M NoContent
postHostsHeartbeat hosts = NoContent <$ DB.upsertHeartbeat hosts

-- * Guest stats

-- | Ingest a sample from a guest. An instance id with no live server is a 404
-- rather than a silent 204, so the guest's reporter surfaces the failure
-- instead of believing its pushes land.
postHostsStats :: HostStatsReport -> M NoContent
postHostsStats report = do
  matched <- DBHosting.upsertServerStats report
  unless matched $ throw NotFound
  pure NoContent

-- | Source gate for a guest's unauthenticated stats push. Two checks, both
-- required:
--
--   1. The effective client is on the guest bridge at all — either the peer
--      itself, or, for a request proxied through the loopback listener, the
--      client the proxy saw.
--   2. It is the /specific/ guest the report claims to be. Being somewhere on
--      the shared bridge is not enough: any guest can put another guest's
--      instance id in its own POST body, so without this a guest could push
--      fabricated samples for its neighbours.
--
-- An instance id with no live server passes step 2 as a no-op, so
-- 'postHostsStats' still 404s it rather than turning it into a 403.
postHostsStatsGuarded :: SockAddr -> Maybe Text -> HostStatsReport -> M NoContent
postHostsStatsGuarded peer forwardedFor report = do
  prefix <- view #guestSubnetPrefix
  unless (statsSourceAllowed prefix peer forwardedFor)
    $ throw
    $ ForbiddenWithMessage "stats: source address is not on the guest bridge"
  guestIp <- DBHosting.getServerGuestIpByInstanceId (_hostStatsReportInstanceId report)
  forM_ guestIp $ \expected ->
    unless (statsClientIp peer forwardedFor == Just expected)
      $ throw
      $ ForbiddenWithMessage "stats: source address is not the guest registered for this server"
  postHostsStats report

-- | The accept decision, split out so it can be tested without a socket.
statsSourceAllowed :: Text -> SockAddr -> Maybe Text -> Bool
statsSourceAllowed guestPrefix peer forwardedFor =
  inGuestSubnet peerIp || (isLoopback peerIp && inGuestSubnet forwardedClient)
  where
    peerIp = sockAddrIPv4 peer
    forwardedClient = trustedForwardedFor forwardedFor
    inGuestSubnet = maybe False (guestPrefix `T.isPrefixOf`)
    isLoopback = maybe False ("127." `T.isPrefixOf`)

-- | The effective client of a stats push, derived exactly as
-- 'statsSourceAllowed' derives it: the peer for a direct connection, or the
-- forwarded client when the peer is the loopback address we proxy through.
statsClientIp :: SockAddr -> Maybe Text -> Maybe Text
statsClientIp peer forwardedFor
  | maybe False ("127." `T.isPrefixOf`) peerIp = trustedForwardedFor forwardedFor
  | otherwise = peerIp
  where
    peerIp = sockAddrIPv4 peer

-- | The /last/ @X-Forwarded-For@ entry: the one appended by the proxy we
-- trust. Earlier entries are whatever the client claimed.
trustedForwardedFor :: Maybe Text -> Maybe Text
trustedForwardedFor header = do
  raw <- header
  listToMaybe (reverse (map T.strip (T.splitOn "," raw)))

-- | An IPv4 (or IPv4-mapped IPv6) socket address as dotted decimal.
sockAddrIPv4 :: SockAddr -> Maybe Text
sockAddrIPv4 = \case
  SockAddrInet _ addr ->
    let (a, b, c, d) = hostAddressToTuple addr
     in Just $ T.intercalate "." (map (cs . show) [a, b, c, d])
  SockAddrInet6 _ _ addr _ ->
    case hostAddress6ToTuple addr of
      (0, 0, 0, 0, 0, 0xffff, hi, lo) ->
        Just
          $ T.intercalate "."
          $ map (cs . show) [hi `div` 256, hi `mod` 256, lo `div` 256, lo `mod` 256]
      _ -> Nothing
  _ -> Nothing

-- * DNS and on-demand TLS

data DnsHosts = DnsHosts
  { byHash :: Map Text HostIPs,
    byName :: Map Text HostIPs
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data HostIPs = HostIPs {ipv4 :: Maybe Text, ipv6 :: Maybe Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

getHostsForDns :: M DnsHosts
getHostsForDns = do
  hosts <- DBHosting.getAllRunningHosts
  let named getName =
        Map.fromList
          $ mapMaybe
            ( \host -> do
                name <- getName host
                pure
                  ( name,
                    HostIPs
                      { ipv4 = _serverAddressIpv4 (_hostAddress host),
                        ipv6 = _serverAddressIpv6 (_hostAddress host)
                      }
                  )
            )
            hosts
  pure
    DnsHosts
      { byHash = named $ \host -> do
          drvPath <- _hostDrvPath host
          T.take 32 <$> T.stripPrefix "/nix/store/" (cs drvPath),
        byName = named (Just . hostToDomainName)
      }

newtype OnDemandResolverDomainNames = OnDemandResolverDomainNames
  { domains :: [Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | Every hostname a running server currently answers on. This is the set
-- Caddy is allowed to issue a certificate for.
getDomainsForOnDemandResolver :: M OnDemandResolverDomainNames
getDomainsForOnDemandResolver = do
  domain <- view #hostingDomain
  hosts <- DBHosting.getAllRunningHosts
  pure
    $ OnDemandResolverDomainNames
    $ concatMap
      ( \host ->
          [hostToDomainName host <> "." <> domain]
            <> [primary <> "." <> domain | primary <- toList (hostToPrimaryDomainName host)]
            <> [ name <> "." <> hostToDomainName host <> "." <> domain
                 | (name, _) <- _hostHttpPorts host
               ]
            <> _hostDomains host
      )
      hosts

-- | Memo of the routable-domain set for the @on_demand_tls.ask@ endpoint.
--
-- Every TLS handshake with an unknown SNI reaches 'onDemandCheck', so without
-- this an SNI flood amplifies into a DB query per handshake. The 10s TTL
-- mirrors the resolver sidecar's own fetch interval. Module-level
-- 'System.IO.Unsafe.unsafePerformIO' is this codebase's established cache
-- pattern; see @Garnix.API.Cache.Permissions@.
type OnDemandDomainsCache = ExpiringCache () OnDemandResolverDomainNames

{-# NOINLINE __onDemandDomainsCache #-}
__onDemandDomainsCache :: OnDemandDomainsCache
__onDemandDomainsCache =
  System.IO.Unsafe.unsafePerformIO
    $ mkCache Nothing (fromSeconds @Int 10) (fromSeconds @Int 2)

onDemandCheck :: Maybe Text -> M NoContent
onDemandCheck queried = do
  OnDemandResolverDomainNames names <-
    lookupCache __onDemandDomainsCache () getDomainsForOnDemandResolver
  case queried of
    Just domain | domain `elem` names -> pure NoContent
    _ -> throw NotFound

-- * The authenticated view

getHosts :: AuthResult AuthJwtPayload -> M [RunningServer]
getHosts (Authenticated (WebSession user)) =
  withGithubUserToken user $ getRunningAndRecentServersForOwners <=< ownersOf user
getHosts _ = throw Unauthorized

deleteHost :: AuthResult AuthJwtPayload -> ServerId -> M ()
deleteHost (Authenticated (WebSession user)) serverId = withGithubUserToken user $ \ghToken -> do
  servers <- getRunningAndRecentServersForOwners =<< ownersOf user ghToken
  -- Deleting is gated on the server appearing in the caller's own list, so a
  -- server id alone is not authorization to tear it down.
  if any ((== serverId) . _runningServerId) servers
    then stopServer serverId
    else throw NotFound
deleteHost _ _ = throw Unauthorized

-- | The owners a session may see servers for: the user, plus every org the
-- garnix app is installed on for them.
ownersOf :: User -> GhToken -> M [GhRepoOwner]
ownersOf user ghToken =
  (GhRepoOwner (user ^. githubLogin) :) . map organizationName <$> getInstalledOrgs ghToken
