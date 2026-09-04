-- | A 'Provisioner' backed by the root garnix-provisionerd daemon, which
-- creates microvm.nix guests on the garnix host itself (newline-delimited
-- JSON over a unix socket). Selected when @GARNIX_PROVISIONER_SOCKET@ is set.
module Garnix.LocalProvisioner
  ( localProvisionerInterface,
    exposeServer,
  )
where

import Control.Exception qualified as Exception
import Control.Lens hiding ((.=))
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, values, _Integer, _String)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Garnix.Duration
import Garnix.Hosting.Types
import Garnix.Monad
import Garnix.Monad.Async (timeoutThrowing)
import Garnix.Prelude
import Garnix.Types (Error (..))
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBS

localProvisionerInterface :: FilePath -> Provisioner
localProvisionerInterface socketPath =
  Provisioner
    { _provisionerProvider = MicroVM,
      _provisionerProvisionServer = provisionServer' socketPath,
      _provisionerUpdateMetadata = \_ _ _ _ -> pure (),
      _provisionerDeleteServer = deleteServer' socketPath,
      _provisionerGetServerStatus = getServerStatus' socketPath
    }

-- | Creating a guest boots it and waits for SSH, so it is minutes.
createTimeout :: Duration
createTimeout = fromMinutes @Int 35

-- | Everything else is a state lookup or a handful of iptables calls.
controlTimeout :: Duration
controlTimeout = fromMinutes @Int 2

provisionServer' :: FilePath -> PreprovisionedServerId -> ServerTier -> M PreprovisionedServer
provisionServer' socketPath poolId tier = do
  let instanceId = InstanceId $ cs (show (getPreprovisionedServerId poolId))
      (vcpu, mem) = tierResources tier
  resp <-
    provisionerRequest socketPath createTimeout
      $ object
        [ "action" .= ("create" :: Text),
          "id" .= instanceId,
          "vcpu" .= vcpu,
          "mem" .= mem
        ]
  ipv4 <- case resp ^? key "ipv4" . _String of
    Just ip -> pure ip
    Nothing -> throw $ OtherError "provisioner create response is missing ipv4"
  now <- liftIO getCurrentTime
  pure
    PreprovisionedServer
      { _preprovisionedServerId = poolId,
        _preprovisionedServerProvider = MicroVM,
        _preprovisionedServerInstanceId = Just instanceId,
        _preprovisionedServerAddress =
          ServerAddress {_serverAddressIpv4 = Just ipv4, _serverAddressIpv6 = Nothing},
        _preprovisionedServerTier = tier,
        _preprovisionedServerCreatedAt = now,
        _preprovisionedServerReadyAt = Nothing
      }

deleteServer' :: FilePath -> InstanceId -> M ()
deleteServer' socketPath instanceId =
  void
    $ provisionerRequest socketPath controlTimeout
    $ object ["action" .= ("destroy" :: Text), "id" .= instanceId]

-- | Ask the daemon to publish a guest's SSH and/or TCP ports via host-port
-- DNAT. Not part of 'Provisioner': port forwarding is specific to guests
-- behind the host.
exposeServer :: FilePath -> InstanceId -> Bool -> [Int] -> M ExposeResult
exposeServer socketPath instanceId sshExposeReq tcpGuestPorts = do
  resp <-
    provisionerRequest socketPath controlTimeout
      $ object
        [ "action" .= ("expose" :: Text),
          "id" .= instanceId,
          "ssh_expose" .= sshExposeReq,
          "tcp_ports" .= tcpGuestPorts
        ]
  pure
    ExposeResult
      { _exposeResultSshPort = fromIntegral <$> (resp ^? key "ssh_port" . _Integer),
        _exposeResultTcpPorts =
          [ (fromIntegral guestPort, fromIntegral hostPort)
          | entry <- resp ^.. key "tcp_ports" . values,
            guestPort <- toList (entry ^? key "guest" . _Integer),
            hostPort <- toList (entry ^? key "host" . _Integer)
          ]
      }

getServerStatus' :: FilePath -> InstanceId -> M Text
getServerStatus' socketPath instanceId = do
  resp <-
    provisionerRequest socketPath controlTimeout
      $ object ["action" .= ("status" :: Text), "id" .= instanceId]
  case resp ^? key "status" . _String of
    Just status -> pure status
    Nothing -> throw $ OtherError "provisioner status response is missing status"

-- | Cap on a daemon response.
maxResponseBytes :: Int
maxResponseBytes = 1024 * 1024

-- | One request/response over the daemon socket.
provisionerRequest :: FilePath -> Duration -> Aeson.Value -> M Aeson.Value
provisionerRequest socketPath timeout payload = do
  raw <-
    timeoutThrowing
      timeout
      (OtherError $ "provisioner did not answer within " <> cs (show timeout))
      $ liftIO
      $ Exception.bracket
        (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol)
        Socket.close
        ( \sock -> do
            Socket.connect sock (Socket.SockAddrUnix socketPath)
            SocketBS.sendAll sock (BSL.toStrict (Aeson.encode payload) <> "\n")
            let loop acc
                  | BSC.length acc > maxResponseBytes =
                      Exception.throwIO
                        $ userError "provisioner response exceeded the size limit"
                  | otherwise = do
                      chunk <- SocketBS.recv sock 65536
                      if BSC.null chunk || BSC.elem '\n' chunk
                        then pure (acc <> chunk)
                        else loop (acc <> chunk)
            loop ""
        )
  resp <- case Aeson.eitherDecodeStrict (BSC.takeWhile (/= '\n') raw) of
    Left decodeError -> throw $ OtherError $ "provisioner response decode: " <> cs decodeError
    Right value -> pure (value :: Aeson.Value)
  case resp ^? key "error" . _String of
    Just daemonError -> throw $ OtherError $ "provisioner: " <> daemonError
    Nothing -> pure resp
