-- | Outbound connections to forges registered through the UI. Anyone can
-- register one, so its host is untrusted: it must not lead garnix to its own
-- loopback, the private network it runs in, or a cloud metadata endpoint.
--
-- The check happens when each connection is opened, on the addresses that
-- very connection uses, so a name that resolved to a public address at
-- registration and to a private one later (DNS rebinding) is still refused.
module Garnix.Forge.OutboundGuard
  ( isPublicAddress,
    Resolver,
    systemResolver,
    checkedAddresses,
    ForbiddenAddress (..),
    PlainHttpRefused (..),
    GuardLimitExceeded (..),
    GuardOptions (..),
    defaultGuardOptions,
    guardedManager,
  )
where

import Control.Exception (IOException, throwIO)
import Control.Exception qualified
import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Text qualified as T
import Data.Word (Word16, Word8)
import Garnix.Prelude
import Network.Connection qualified as NC
import Network.HTTP.Client (Manager, ManagerSettings (..), defaultManagerSettings, managerSetProxy, newManager, noProxy)
import Network.HTTP.Client.Internal (makeConnection, socketConnection)
import Network.HTTP.Client.Internal qualified as Http
import Network.Socket
  ( AddrInfo (..),
    Family (AF_INET, AF_INET6, AF_UNIX),
    HostName,
    SockAddr (..),
    Socket,
    SocketType (Stream),
    close,
    connect,
    defaultHints,
    getAddrInfo,
    hostAddress6ToTuple,
    hostAddressToTuple,
    socket,
  )
import System.Timeout qualified
import Prelude qualified

-- | Whether an address is on the public internet. Everything else is
-- refused: loopback, RFC 1918, link-local (169.254/16 holds cloud metadata
-- endpoints), CGNAT, documentation and benchmarking ranges, multicast, and
-- for IPv6 anything outside global unicast, ULAs, and the transition
-- prefixes that embed an IPv4 address garnix would not check.
isPublicAddress :: SockAddr -> Bool
isPublicAddress = \case
  SockAddrInet _ addr -> publicV4 (hostAddressToTuple addr)
  SockAddrInet6 _ _ addr _ -> case hostAddress6ToTuple addr of
    -- IPv4-mapped (::ffff:a.b.c.d): the IPv4 address is what is reached.
    (0, 0, 0, 0, 0, 0xffff, hi, lo) -> publicV4 (octets hi lo)
    (a, b, _, _, _, _, _, _) ->
      a
        .&. 0xe000
          == 0x2000 -- global unicast, 2000::/3
          && not (a == 0x2001 && b == 0x0db8) -- documentation
          && not (a == 0x2001 && b == 0) -- Teredo
          && a
          /= 0x2002 -- 6to4
  _ -> False
  where
    octets :: Word16 -> Word16 -> (Word8, Word8, Word8, Word8)
    octets hi lo = (fromIntegral (hi `shiftR` 8), fromIntegral hi, fromIntegral (lo `shiftR` 8), fromIntegral lo)
    publicV4 :: (Word8, Word8, Word8, Word8) -> Bool
    publicV4 (a, b, c, _) =
      not
        $ or
          [ a == 0, -- "this network"
            a == 10, -- RFC 1918
            a == 100 && b .&. 0xc0 == 64, -- CGNAT, 100.64/10
            a == 127, -- loopback
            a == 169 && b == 254, -- link-local, cloud metadata
            a == 172 && b .&. 0xf0 == 16, -- RFC 1918
            a == 192 && b == 0 && c == 0, -- IETF protocol assignments
            a == 192 && b == 0 && c == 2, -- documentation
            a == 192 && b == 88 && c == 99, -- 6to4 relay
            a == 192 && b == 168, -- RFC 1918
            a == 198 && b .&. 0xfe == 18, -- benchmarking, 198.18/15
            a == 198 && b == 51 && c == 100, -- documentation
            a == 203 && b == 0 && c == 113, -- documentation
            a >= 224 -- multicast, reserved, broadcast
          ]

-- | The addresses a host name and port resolve to.
type Resolver = HostName -> Int -> IO [SockAddr]

systemResolver :: Resolver
systemResolver host port =
  map addrAddress <$> getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just host) (Just $ Prelude.show port)

-- | A connection refused because its host resolved to an address that is not
-- allowed.
data ForbiddenAddress = ForbiddenAddress HostName SockAddr
  deriving stock (Show)

instance Exception ForbiddenAddress

-- | A plain http connection, which registered forges never get: their URL
-- must be https, and so must anything they redirect to.
data PlainHttpRefused = PlainHttpRefused
  deriving stock (Show)

instance Exception PlainHttpRefused

-- | A connection that read more, or for longer, than 'GuardOptions' allow.
newtype GuardLimitExceeded = GuardLimitExceeded Text
  deriving stock (Show)

instance Exception GuardLimitExceeded

-- | What a connection to an untrusted host may cost. A registered forge only
-- answers small JSON documents (its version, a token, a user), so these are
-- far above what it needs and far below what would hurt garnix.
data GuardOptions = GuardOptions
  { -- | Plain http, for specs only.
    guardAllowPlainHttp :: Bool,
    -- | Bytes read on one connection, which serves one request.
    guardMaxBytes :: Int,
    -- | How long one read may wait for data.
    guardReadTimeout :: NominalDiffTime,
    -- | How long a connection may last.
    guardDeadline :: NominalDiffTime
  }

defaultGuardOptions :: GuardOptions
defaultGuardOptions =
  GuardOptions
    { guardAllowPlainHttp = False,
      guardMaxBytes = 1024 * 1024,
      guardReadTimeout = 30,
      guardDeadline = 60
    }

-- | Resolves the host and refuses it if any of its addresses is not allowed:
-- picking only the allowed ones would let a name that mixes both reach the
-- disallowed one through a later resolution.
checkedAddresses :: (SockAddr -> Bool) -> Resolver -> HostName -> Int -> IO [SockAddr]
checkedAddresses allowed resolve host port = do
  addresses <- resolve host port
  when (null addresses) $ throwIO $ userError $ "no address for " <> host
  case filter (not . allowed) addresses of
    refused : _ -> throwIO $ ForbiddenAddress host refused
    [] -> pure addresses

-- | A 'Manager' whose every connection is opened to an address
-- 'checkedAddresses' let through, with TLS verified against the host name,
-- and bounded by the 'GuardOptions'. Proxies from the environment are
-- ignored: a proxy would make the connection from somewhere garnix cannot
-- check. No connection is kept for another request, so the bounds are per
-- request.
guardedManager :: GuardOptions -> (SockAddr -> Bool) -> Resolver -> IO Manager
guardedManager options allowed resolve = do
  context <- NC.initConnectionContext
  newManager
    $ managerSetProxy noProxy
    $ defaultManagerSettings
      { managerIdleConnectionCount = 0,
        managerRawConnection = pure $ \_ host port -> do
          unless (guardAllowPlainHttp options) $ throwIO PlainHttpRefused
          bracketOnError (open host port) close $ \sock -> bounded options =<< socketConnection sock 8192,
        managerTlsConnection = pure $ \_ host port ->
          bracketOnError (open host port) close $ \sock ->
            NC.connectFromSocket
              context
              sock
              NC.ConnectionParams
                { NC.connectionHostname = stripBrackets host,
                  NC.connectionPort = fromIntegral port,
                  NC.connectionUseSecure = Just def,
                  NC.connectionUseSocks = Nothing
                }
              >>= fromTlsConnection
              >>= bounded options
      }
  where
    open :: HostName -> Int -> IO Socket
    open host port = checkedAddresses allowed resolve (stripBrackets host) port >>= connectToAny
    connectToAny :: [SockAddr] -> IO Socket
    connectToAny = \case
      [] -> throwIO $ userError "no address to connect to"
      address : rest -> do
        result <- Control.Exception.try $ bracketOnError (socket (family address) Stream 0) close $ \sock -> do
          connect sock address
          pure sock
        case result of
          Right sock -> pure sock
          Left (e :: IOException)
            | null rest -> throwIO e
            | otherwise -> connectToAny rest
    family = \case
      SockAddrInet {} -> AF_INET
      SockAddrInet6 {} -> AF_INET6
      SockAddrUnix {} -> AF_UNIX
    stripBrackets = cs . T.dropAround (`elem` ['[', ']']) . cs

-- | The connection, failing a read past 'guardMaxBytes' in total, one that
-- waits longer than 'guardReadTimeout', or any after 'guardDeadline'.
bounded :: GuardOptions -> Http.Connection -> IO Http.Connection
bounded options conn = do
  deadline <- addUTCTime (guardDeadline options) <$> getCurrentTime
  readSoFar <- newIORef (0 :: Int)
  let read' = do
        now <- getCurrentTime
        when (now > deadline) $ throwIO $ GuardLimitExceeded "it kept answering for too long"
        chunk <-
          maybe (throwIO $ GuardLimitExceeded "it stopped answering") pure
            =<< System.Timeout.timeout (ceiling (guardReadTimeout options * 1000 * 1000)) (Http.connectionRead conn)
        total <- atomicModifyIORef' readSoFar $ \n -> let n' = n + BS.length chunk in (n', n')
        when (total > guardMaxBytes options) $ throwIO $ GuardLimitExceeded "its answer is too large"
        pure chunk
  makeConnection read' (Http.connectionWrite conn) (Http.connectionClose conn)

fromTlsConnection :: NC.Connection -> IO Http.Connection
fromTlsConnection conn =
  makeConnection
    (NC.connectionGetChunk conn)
    (NC.connectionPut conn)
    (NC.connectionClose conn `Control.Exception.catch` \(_ :: IOException) -> pure ())
