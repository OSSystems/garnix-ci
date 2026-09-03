module Garnix.API.Cache
  ( CacheAPI (..),
    cacheAPI,

    -- * exported for tests
    XForwardedFor (..),
    isInternal,
    nixCacheInfo,
    serveNarInfo,
  )
where

import Amazonka qualified
import Amazonka.S3 qualified as Amazonka
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Text qualified as T
import Garnix.API.Cache.Auth (getStoreHashPermission)
import Garnix.API.Cache.Permissions (Permission (..))
import Garnix.API.Cache.Types
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Monad
import Garnix.Monad.Metrics (incrementEvent)
import Garnix.Nix.Types (StorePath (..))
import Garnix.Prelude
import Garnix.S3Cache (recordCacheAccess, toNarFilePath)
import Garnix.Types
import Servant

data CacheAPI route = CacheAPI
  { _cacheAPInixCacheInfo :: route :- "nix-cache-info" :> Get '[PlainText] Text,
    _cacheAPInarInfo ::
      route
        :- Header "X-Forwarded-For" XForwardedFor
          :> Capture "narinfo file" NarInfoFileName
          :> Header "Authorization" Text
          :> Get '[NarInfo] NarInfo
  }
  deriving (Generic)

newtype XForwardedFor = XForwardedFor Text
  deriving stock (Show)

instance FromHttpApiData XForwardedFor where
  parseUrlPiece :: Text -> Either Text XForwardedFor
  parseUrlPiece = Right . XForwardedFor

cacheAPI :: CacheAPI (AsServerT M)
cacheAPI =
  CacheAPI
    { _cacheAPInixCacheInfo = pure nixCacheInfo,
      _cacheAPInarInfo = serveNarInfo
    }

nixCacheInfo :: Text
nixCacheInfo =
  cs
    $ unindent
      [i|
        StoreDir: /nix/store
        WantMassQuery: 1
        Priority: 50
      |]

serveNarInfo :: Maybe XForwardedFor -> NarInfoFileName -> Maybe Text -> M NarInfo
serveNarInfo xForwardedFor (NarInfoFileName hash) authorization = do
  enabled <- view #s3CacheEnabled
  unless enabled $ shortcut NotFound
  cacheStoreHash <- DB.getS3CacheStoreHash hash
  case cacheStoreHash of
    Nothing -> shortcut NotFound
    Just
      ( DB.S3CacheStoreHash
          { hash,
            packageName,
            narHash,
            narSize,
            public,
            sig,
            references,
            fileSize,
            fileHash
          }
        ) -> do
        let storePath = StorePath hash packageName
        let compression = XZ
        url <- do
          if public
            then
              if isInternal xForwardedFor
                then
                  publicInternalS3Url storePath compression
                else
                  publicS3Url storePath compression
            else do
              permission <- getStoreHashPermission (getHash storePath) authorization
              case permission of
                Disallowed -> shortcut NotFound
                Allowed -> privateS3Url storePath compression
        recordCacheAccess hash
        incrementEvent #s3CacheNarfilesServed
        pure
          $ NarInfo
            { storePath,
              narHash,
              narSize,
              url,
              sig,
              references,
              compression,
              fileSize,
              fileHash
            }

publicS3Url :: StorePath -> Compression -> M Text
publicS3Url storePath compression = do
  publicBaseUrl <- view $ #s3CacheEnv . #publicBaseUrl
  pure $ publicBaseUrl <> toNarFilePath storePath compression

isInternal :: Maybe XForwardedFor -> Bool
isInternal = \case
  Nothing -> False
  Just (XForwardedFor header) ->
    any
      (`elem` map T.strip (T.splitOn "," header))
      internalServers
    where
      internalServers =
        [ "65.108.28.108",
          "2a01:4f9:3071:29ce::2",
          "65.108.28.106",
          "2a01:4f9:3071:29cf::2",
          "65.108.28.107",
          "2a01:4f9:3071:29cd::2",
          "88.99.75.150",
          "2a01:4f8:10a:130d::2",
          "157.90.140.190",
          "2a01:4f8:2220:37c4::2",
          "65.21.80.216",
          "2a01:4f9:3a:47cf::1",
          "23.88.85.24",
          "2a01:4f8:e0:204e::2",
          "65.109.75.126",
          "2a01:4f9:3071:3220::2",
          "91.107.205.127",
          "142.132.141.88",
          "142.132.141.89",
          "2a01:4f8:c012:7fe0::1"
        ]

publicInternalS3Url :: StorePath -> Compression -> M Text
publicInternalS3Url storePath compression = do
  log Informational "serving with publicInternalS3Url"
  publicBucket <- view $ #s3CacheEnv . #publicBucket
  let request =
        Amazonka.newGetObject
          publicBucket
          (Amazonka.ObjectKey (toNarFilePath storePath compression))
  env <- view $ #s3CacheEnv . #amazonkaEnv
  now <- liftIO getCurrentTime
  expiration <- view $ #s3CacheEnv . #expiration
  cs <$> Amazonka.presignURL env now (toAmazonkaSeconds expiration) request

privateS3Url :: StorePath -> Compression -> M Text
privateS3Url storePath compression = do
  privateBucket <- view $ #s3CacheEnv . #privateBucket
  let request =
        Amazonka.newGetObject
          privateBucket
          (Amazonka.ObjectKey (toNarFilePath storePath compression))
  env <- view $ #s3CacheEnv . #amazonkaEnv
  now <- liftIO getCurrentTime
  expiration <- view $ #s3CacheEnv . #expiration
  cs <$> Amazonka.presignURL env now (toAmazonkaSeconds expiration) request

toAmazonkaSeconds :: Duration -> Amazonka.Seconds
toAmazonkaSeconds = Amazonka.Seconds . realToFrac . toSeconds
