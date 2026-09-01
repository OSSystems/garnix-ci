module Garnix.S3Cache (upload, toNarFilePath, compress, parallelisableBlocks) where

import Amazonka qualified
import Amazonka.S3 qualified as Amazonka
import Control.Concurrent.STM (atomically, modifyTVar', readTVar, retry, writeTVar)
import Control.Lens
import Control.Retry (RetryPolicyM, fullJitterBackoff, limitRetries, limitRetriesByCumulativeDelay)
import Cradle
import Data.ByteString.Builder qualified as ByteString
import Data.Containers.ListUtils (nubOrd)
import Data.Set qualified as Set
import Data.Text qualified as T
import Garnix.API.Cache.Types
import Garnix.Build.Types (EvaluationResult)
import Garnix.BuildLogs.Types hiding (log)
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Monad
import Garnix.Monad.Memoization (memoize)
import Garnix.Monad.Metrics (incrementEvent, timingAs)
import Garnix.Monad.Pool
import Garnix.Monad.SubProcess (runSubProcess, runSubProcess_)
import Garnix.Nix.PathInfo (getPathInfo)
import Garnix.Nix.PathInfo qualified as Nix
import Garnix.Nix.StorePath qualified as Nix
import Garnix.Nix.Types
import Garnix.Nix.Types qualified as Nix
import Garnix.NixConfig (addNixConfigEnvironment)
import Garnix.Prelude
import Garnix.Request (retryingWithPolicy)
import Garnix.Types
import System.Directory (doesDirectoryExist, doesFileExist, getFileSize, listDirectory, pathIsSymbolicLink)
import System.IO (withBinaryFile)
import System.IO qualified as IO
import System.IO.Temp (withSystemTempDirectory)
import Prelude qualified

upload :: RunReporter -> GhRepoOwner -> GhRepoName -> EvaluationResult -> RepoPublicity -> M ()
upload = curry5 $ mockable #s3CacheUploadMock $ \(runReporter, repoOwner, repoName, evalResult, repoPublicity) -> do
  enabled <- view #s3CacheEnabled
  if not enabled
    then log Informational "s3 cache disabled; skipping upload"
    else withTextSpan ("phase", "s3-cache-upload") $ do
      withSpan (Garnix.S3Cache.getPackageName (evalResult ^. #derivation)) $ do
        storePathClosure <- nubOrd . mconcat . catMaybes <$> forM (evalResult ^. #toUpload) Nix.getClosure
        notInNixosCache <- filterM (fmap not . isInNixosCache) storePathClosure
        notInS3Cache <- DB.claimS3CachedStorePaths notInNixosCache
        forM_ (notInNixosCache \\ notInS3Cache) $ \storePath -> do
          DB.tagCacheUploadForS3Cache repoOwner repoName $ getHash storePath
        forConcurrently_ notInS3Cache $ \storePath -> do
          dirSize <- liftIO $ getDirSize $ cs $ getStorePath storePath
          limit <- view (#s3CacheEnv . #maxUploadSize)
          if dirSize > limit
            then do
              log Notice $ getStorePath storePath <> " too big (" <> show dirSize <> "), not uploading"
              reportLogs runReporter
                $ mkLogLine
                  ( getStorePath storePath
                      <> " is "
                      <> show dirSize
                      <> " bytes, the limit is "
                      <> show limit
                      <> ". Not uploading to the garnix binary cache."
                  )
            else do
              uploadStorePath repoOwner repoName storePath repoPublicity <?> "uploading to s3-cache"
              reportLogs runReporter $ mkLogLine ("Uploaded " <> getStorePath storePath <> " to the garnix binary cache.")

getPackageName :: DrvPath -> PackageName
getPackageName drvPath =
  case T.stripSuffix ".drv" (cs (getName $ getDrvPath drvPath)) of
    Just name -> PackageName name
    Nothing -> "<unknown>"

getDirSize :: FilePath -> IO Integer
getDirSize path = do
  isSymLink <- pathIsSymbolicLink path
  if isSymLink
    then pure 0
    else do
      isDir <- doesDirectoryExist path
      if isDir
        then do
          entries <- listDirectory path
          sum <$> mapM (getDirSize . (path </>)) entries
        else do
          isFile <- doesFileExist path
          if isFile then getFileSize path else pure 0

uploadStorePath :: GhRepoOwner -> GhRepoName -> StorePath -> RepoPublicity -> M ()
uploadStorePath repoOwner repoName storePath repoPublicity = do
  nixConfig <- view #userNixConfig
  withPoolM s3UploadPool repoOwner
    $ withBinaryFileInTempDir
    $ \(narFilePath, narFileHandle) -> do
      runSubProcess_
        $ cmd "nix"
        & addArgs ["nar", "pack", cs storePath :: Text]
        & addStdoutHandle narFileHandle
        & addNixConfigEnvironment nixConfig
      liftIO $ IO.hClose narFileHandle
      narSize <- fromIntegral <$> Amazonka.getFileSize narFilePath
      narHash <- getFileHash narFilePath
      compressedNarFilePath <- compress narFilePath
      cachePublic <- isPublicForCache repoOwner repoPublicity
      bucket <-
        if cachePublic
          then view $ #s3CacheEnv . #publicBucket
          else view $ #s3CacheEnv . #privateBucket
      body <- Amazonka.toBody <$> Amazonka.hashedFile compressedNarFilePath
      let policy :: RetryPolicyM M
          policy =
            limitRetriesByCumulativeDelay
              (toMicroseconds (fromMinutes @Int 30))
              (limitRetries 5 <> fullJitterBackoff (toMicroseconds (fromMilliSeconds @Int 100)))
      void
        $ timingAs #cachePushTime
        $ retryingWithPolicy policy
        $ sendWithLogging
        $ Amazonka.newPutObject
          bucket
          (Amazonka.ObjectKey $ toNarFilePath storePath XZ)
          body
      fileHash <- getFileHash compressedNarFilePath
      (sig, pathInfo) <- signStorePath storePath
      let references = T.unwords $ fmap Nix.getRelativeStorePath (pathInfo ^. #references)
      fileSize <- fromIntegral <$> Amazonka.getFileSize compressedNarFilePath
      DB.finalizeS3CacheUpload
        $ DB.S3CacheStoreHash
          { DB.hash = getHash storePath,
            DB.packageName = getName storePath,
            narHash,
            narSize,
            public = cachePublic,
            sig,
            references,
            fileSize,
            fileHash
          }
      DB.tagCacheUploadForS3Cache repoOwner repoName $ getHash storePath
      incrementEvent #s3CacheUploads

isPublicForCache :: GhRepoOwner -> RepoPublicity -> M Bool
isPublicForCache repoOwner repoPublicity
  | isRepoPublic repoPublicity = pure True
  | otherwise = do
      publicRepoOwners <- view $ #s3CacheEnv . #publicRepoOwners
      pure $ Set.member (T.toLower $ getGhLogin $ getGhRepoOwner repoOwner) publicRepoOwners

getFileHash :: FilePath -> M Text
getFileHash file = do
  StdoutTrimmed hash <-
    runSubProcess
      $ cmd "nix-hash"
      & addArgs
        ["--base32", "--type", "sha256", "--flat", file]
  pure hash

withBinaryFileInTempDir :: ((FilePath, Handle) -> M a) -> M a
withBinaryFileInTempDir action = do
  withSystemTempDirectory "garnix-narfile" $ \tempDir -> do
    let file = tempDir </> "file"
    liftBaseOp (withBinaryFile file IO.WriteMode) $ \handle -> do
      action (file, handle)

compress :: FilePath -> M FilePath
compress file = do
  fileSize <- liftIO $ getFileSize file
  withCompressionThreads (parallelisableBlocks fileSize) $ \threads -> do
    run_ $ cmd "xz"
      & addArgs (threadArgs threads <> [file])
      & setWorkingDir (takeDirectory file)
  pure (file <> ".xz")
  where
    threadArgs 1 = ["--threads=1"]
    threadArgs threads =
      [ "--threads=" <> Prelude.show threads,
        "--block-size=" <> Prelude.show xzBlockSize
      ]

xzBlockSize :: Integer
xzBlockSize = 24 * 1024 * 1024

parallelisableBlocks :: Integer -> Int
parallelisableBlocks fileSize =
  fromInteger $ max 1 $ (fileSize + xzBlockSize - 1) `div` xzBlockSize

withCompressionThreads :: Int -> (Int -> M a) -> M a
withCompressionThreads usableThreads action = do
  CompressionBudget {unreservedThreads, perCompressionLimit} <- view #compressionBudget
  let maxReservable = min usableThreads perCompressionLimit
      reserve = liftIO $ atomically $ do
        unreserved <- readTVar unreservedThreads
        when (unreserved < 1) retry
        let reserved = min unreserved maxReservable
        writeTVar unreservedThreads (unreserved - reserved)
        pure reserved
      release reserved = liftIO $ atomically $ modifyTVar' unreservedThreads (+ reserved)
  bracket reserve release action

nixosCacheKeyName :: Text
nixosCacheKeyName = "cache.nixos.org-1"

isInNixosCache :: StorePath -> M Bool
isInNixosCache storePath = memoize (#s3CacheEnv . #isInNixosCacheMemoTable) (getHash storePath) $ do
  pathInfo <- getPathInfo storePath
  let nixosSignatures = Nix.signaturesForCacheKey pathInfo nixosCacheKeyName
  pure $ not $ null nixosSignatures

signStorePath :: StorePath -> M (Text, Nix.PathInfo)
signStorePath storePath = do
  cachePrivKeyFile <- view $ #s3CacheEnv . #cachePrivKeyFile
  cachePrivKeyName <- view $ #s3CacheEnv . #cachePrivKeyName
  nixConfig <- view #userNixConfig
  runSubProcess_
    $ cmd "nix"
    & addArgs ["store", "sign", "-k", cs cachePrivKeyFile, cs storePath :: Text]
    & addNixConfigEnvironment nixConfig
  pathInfo <- getPathInfo storePath
  let garnixSignatures = Nix.signaturesForCacheKey pathInfo cachePrivKeyName
  case garnixSignatures of
    sig : _ -> pure (sig, pathInfo)
    _ -> throw $ OtherError "Error parsing `nix path-info` output: no signature for cachePrivKey"

toNarFilePath :: StorePath -> Compression -> Text
toNarFilePath storePath compression =
  getRelativeStorePath storePath <> ".nar." <> case compression of
    XZ -> "xz"

sendWithLogging ::
  (Amazonka.AWSRequest request, Typeable request, Typeable (Amazonka.AWSResponse request)) =>
  request ->
  M (Amazonka.AWSResponse request)
sendWithLogging request = do
  withTextSpan ("tag", "amazonka-log") $ do
    logger <- view #logger
    spans <- view #spanCtx
    s3Env <-
      view (#s3CacheEnv . #amazonkaEnv)
        <&> #logger .~ amazonkaLogger spans logger
    response <-
      liftIO
        $ runResourceT
        $ Amazonka.sendEither s3Env request
    case response of
      Left error -> throw $ OtherError $ show error
      Right response -> pure response
  where
    amazonkaLogger :: [(Text, Text)] -> (LogItem -> IO ()) -> Amazonka.Logger
    amazonkaLogger spans logger logLevel message = do
      when (Amazonka.Info >= logLevel) $ do
        logger $ LogItem (convertLogLevel logLevel) spans $ cs $ ByteString.toLazyByteString message

    convertLogLevel :: Amazonka.LogLevel -> Severity
    convertLogLevel = \case
      Amazonka.Info -> Informational
      Amazonka.Error -> Error
      Amazonka.Debug -> Informational
      Amazonka.Trace -> Informational
