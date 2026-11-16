module Garnix.BuildLogs
  ( buildInternalLogProcessor,
    mkInternalLogProcessorState,
    processLogsForGithub,
    processMessage,
    shouldKeepLn,
  )
where

import Control.Concurrent
import Data.Aeson hiding (Error)
import Data.ByteString.Lazy qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.AnsiEscapeCodes.Strip.Text (stripAnsiEscapeCodes)
import Data.Text qualified as T
import Garnix.BuildLogs.Internal qualified as LogsInternal
import Garnix.BuildLogs.Types (LogLine (LogLine), mkLogLine)
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types

-- * Common

processMessage :: Text -> Text
processMessage =
  T.unlines
    . filter shouldKeepLn
    . T.lines
    . obfuscateGithubToken
    . stripAnsiEscapeCodes

shouldKeepLn :: Text -> Bool
shouldKeepLn ln =
  not ("error (ignored): error: SQLite database" `T.isPrefixOf` ln)
    && not ("garnixMacBuilder" `T.isInfixOf` ln)
    && not ("macMini" `T.isInfixOf` ln)
    && not ("/run/secrets/garnix_server_ssh" `T.isInfixOf` ln)

-- * Parsing internal nix logs for opensearch

data InternalLogProcessorState = InternalLogProcessorState (MVar (Map LogsInternal.InternalId Text)) (MVar (Map LogsInternal.InternalId PackageName))

mkInternalLogProcessorState :: M InternalLogProcessorState
mkInternalLogProcessorState = liftIO $ InternalLogProcessorState <$> newMVar mempty <*> newMVar mempty

-- | Also tells @started@ and @stopped@ which derivations nix is building.
buildInternalLogProcessor ::
  (LogLine -> M ()) ->
  (LogsInternal.InternalId -> Text -> M ()) ->
  (LogsInternal.InternalId -> M ()) ->
  InternalLogProcessorState ->
  (String -> M ())
buildInternalLogProcessor consumer started stopped (InternalLogProcessorState phaseById pkgsById) internalLogLine = do
  let jsonStr = BS.stripPrefix "@nix" (cs internalLogLine)
      parsedLine = case jsonStr of
        Just line -> eitherDecode' line
        Nothing -> Left $ "Expected internal log line from nix but got: " <> internalLogLine
  case parsedLine of
    Right (LogsInternal.Start id _ (LogsInternal.Build drvPath _)) -> do
      setPackage id $ drvPathToPkgName drvPath
      started id drvPath
    Right (LogsInternal.Stop id) -> stopped id
    Right (LogsInternal.Result id (LogsInternal.SetPhase phase)) -> setPhase id phase
    Right (LogsInternal.Result id (LogsInternal.LogLine log)) -> LogLine <$> getPkgForId id <*> getPhaseForId id <*> pure log >>= consumer
    Right (LogsInternal.Msg log) -> consumer $ mkLogLine log
    Right _ -> pure ()
    Left err -> log Warning $ cs err
  where
    setPackage :: LogsInternal.InternalId -> PackageName -> M ()
    setPackage id pkgName = liftIO $ modifyMVar_ pkgsById $ pure . Map.insert id pkgName

    getPkgForId :: LogsInternal.InternalId -> M (Maybe PackageName)
    getPkgForId id = liftIO $ Map.lookup id <$> readMVar pkgsById

    setPhase :: LogsInternal.InternalId -> Text -> M ()
    setPhase id phase = liftIO $ modifyMVar_ phaseById $ pure . Map.insert id phase

    getPhaseForId :: LogsInternal.InternalId -> M (Maybe Text)
    getPhaseForId id = liftIO $ Map.lookup id <$> readMVar phaseById

drvPathToPkgName :: Text -> PackageName
drvPathToPkgName = PackageName . T.dropEnd suffixLength . T.drop prefixLength
  where
    prefixLength = T.length "/nix/store/this-is-a-fictional-hash-.......-"
    suffixLength = T.length ".drv"

-- * Github-specific

processLogsForGithub :: RawLogs -> Text
processLogsForGithub (RawLogs l) =
  "Last 100 lines of logs:\n"
    <> "```\n"
    <> T.unlines (reverse $ take 100 $ reverse $ fmap limitLine $ T.lines $ processMessage l)
    <> "```\n"
  where
    limitLine ln
      | T.length ln > 650 = T.take 649 ln <> "…"
      | otherwise = ln
