module Garnix.UserLogs
  ( testImplementation,
    getLogLines,
    getRunLogLines,
    storeRunLogLine,
    storeBuildLogLine,
  )
where

import Control.Concurrent
import Control.Lens
import Data.Aeson hiding (Error)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy (ByteString)
import Data.Maybe
import Data.Text qualified as T
import Data.Time
import Data.Time.Format.ISO8601
import Garnix.BuildLogs.Types (LogLine (LogLine))
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types hiding (branch, commit, repoName, repoOwner, statusCode)
import Garnix.Types qualified
import Network.HTTP.Types (Status (statusCode), statusIsSuccessful)
import Network.Wreq qualified as Wreq

storeRunLogLine :: Run -> LogLine -> M ()
storeRunLogLine run = storeLogLine (fromRun run) (FromRun $ run ^. id)

storeBuildLogLine :: Build -> LogLine -> M ()
storeBuildLogLine build logLine = do
  let metaData =
        AdditionalMetadata
          { repoOwner = build ^. Garnix.Types.repoUser,
            repoName = build ^. Garnix.Types.repoName,
            branch = build ^. Garnix.Types.branch,
            commit = build ^. Garnix.Types.gitCommit,
            requestingUser = build ^. reqUser
          }
  storeLogLine metaData (FromBuild $ build ^. id) logLine

storeLogLine :: AdditionalMetadata -> OpenSearchId -> LogLine -> M ()
storeLogLine metadata = curry $ mockable #storeLogLineMock $ \(openSearchId, logLine) -> do
  reportingPort <- view #buildLogsReportingPort
  forM_ reportingPort $ \reportingPort ->
    postToCollector reportingPort (OpenSearchSerializedMessage openSearchId metadata logLine)
      `catchAny` (reportDroppedLogLine . cs . show)

postToCollector :: Int -> OpenSearchSerializedMessage -> M ()
postToCollector reportingPort message = do
  response <- withWreqOptions $ \options ->
    Wreq.postWith
      options
      ("http://localhost:" <> (cs . show $ reportingPort))
      (toJSON message)
  unless (statusIsSuccessful $ response ^. Wreq.responseStatus)
    $ reportDroppedLogLine
    $ T.intercalate
      " "
      [ "received response with status code",
        response ^. Wreq.responseStatus & show . statusCode,
        "and body:",
        response ^. Wreq.responseBody & cs
      ]

reportDroppedLogLine :: Text -> M ()
reportDroppedLogLine reason =
  log Error
    $ T.intercalate
      " "
      [ "[fluent-bit writer thread]",
        "Dropping user log line, local collector failed:",
        reason
      ]

toOpenSearchKey :: OpenSearchId -> String
toOpenSearchKey (FromRun _) = "runId"
toOpenSearchKey (FromBuild _) = "buildId"

toHashId :: OpenSearchId -> HashId
toHashId (FromRun id) = getRunId id
toHashId (FromBuild id) = getBuildId id

data AdditionalMetadata = AdditionalMetadata
  { repoOwner :: GhRepoOwner,
    repoName :: GhRepoName,
    branch :: Maybe Branch,
    commit :: CommitHash,
    requestingUser :: GhLogin
  }
  deriving stock (Eq, Show, Generic)

fromRun :: Run -> AdditionalMetadata
fromRun run =
  AdditionalMetadata
    { repoOwner = run ^. Garnix.Types.repoUser,
      repoName = run ^. Garnix.Types.repoName,
      branch = run ^. Garnix.Types.branch,
      commit = run ^. Garnix.Types.gitCommit,
      requestingUser = run ^. reqUser
    }

-- OpenSearchSerializedMessage is used only for serialization since there are a
-- few extra fields on it that aren't needed when querying, but are useful when
-- viewing log lines on opensearch
data OpenSearchSerializedMessage = OpenSearchSerializedMessage OpenSearchId AdditionalMetadata LogLine
  deriving stock (Eq, Show, Generic)

instance ToJSON OpenSearchSerializedMessage where
  toJSON (OpenSearchSerializedMessage openSearchId metadata logLine) =
    Aeson.object
      [ fromString (toOpenSearchKey openSearchId) Aeson..= toHashId openSearchId,
        "repoOwner" Aeson..= repoOwner metadata,
        "repoName" Aeson..= repoName metadata,
        "branch" Aeson..= branch metadata,
        "commit" Aeson..= commit metadata,
        "requestingUser" Aeson..= requestingUser metadata,
        "package" Aeson..= (logLine ^. #package),
        "phase" Aeson..= (logLine ^. #phase),
        "message" Aeson..= (logLine ^. #log)
      ]

getLogLines :: Build -> Int -> Maybe UTCTime -> M [OpenSearchMessage]
getLogLines build maxResults mAfter = do
  now <- liftIO getCurrentTime
  queryOpenSearch
    (FromBuild $ build ^. id)
    [(build ^. startTime . to utctDay) .. (utctDay $ fromMaybe now $ build ^. endTime)]
    mAfter
    maxResults

getRunLogLines :: Run -> Int -> Maybe UTCTime -> M [OpenSearchMessage]
getRunLogLines run maxResults mAfter = do
  now <- liftIO getCurrentTime
  queryOpenSearch
    (FromRun $ run ^. id)
    [(run ^. startTime . to utctDay) .. (utctDay $ fromMaybe now $ run ^. endTime)]
    mAfter
    maxResults

makeOpenSearchMsearchRequest :: Value -> Value -> M ByteString
makeOpenSearchMsearchRequest = curry $ mockable #makeOpenSearchMsearchRequestMock $ \(metadata, query) -> do
  opensearchUrl <- view #opensearchQueryUrl
  opensearchPassword <- view #opensearchPassword
  res <- withWreqOptions $ \opts ->
    Wreq.postWith
      ( opts
          & Wreq.auth ?~ Wreq.basicAuth "garnix" opensearchPassword
          & Wreq.header "Content-Type" .~ ["application/json"]
      )
      opensearchUrl
      $ encode metadata
      <> "\n"
      <> encode query
      <> "\n"
  pure $ res ^. Wreq.responseBody

queryOpenSearch ::
  (HasCallStack) =>
  OpenSearchId ->
  [Day] ->
  Maybe UTCTime ->
  Int ->
  M [OpenSearchMessage]
queryOpenSearch = curry4 $ mockable #queryOpenSearchMock $ \(openSearchId, indexDaysToSearch, mAfter, maxResults) -> do
  let queryTermKey = toOpenSearchKey openSearchId <> ".keyword"
      metadata = [aesonQQ| { index: #{indexForDay <$> indexDaysToSearch} } |]
      query =
        [aesonQQ|
          {
            query: { bool: { filter: [
              { term: { $queryTermKey: { value: #{toHashId openSearchId} } } },
              { range: { "@timestamp": { gt: #{mAfter} } } }
            ] } },
            sort: [{ "@timestamp": { order: "asc" } }],
            size: #{maxResults}
          }
        |]
  res <- makeOpenSearchMsearchRequest metadata query
  pure
    $ mapMaybe fromEntry
    $ res
    ^.. key "responses"
      . _Array
      . traverse
      . key "hits"
      . key "hits"
      . _Array
      . traverse
  where
    fromEntry :: Value -> Maybe OpenSearchMessage
    fromEntry record = do
      timestamp <- record ^? key "_source" . key "@timestamp" . _String . to cs >>= iso8601ParseM
      message <- record ^? key "_source" . key "message" . _String
      let package = fmap PackageName $ record ^? key "_source" . key "package" . _String
          phase = record ^? key "_source" . key "phase" . _String
      pure $ OpenSearchMessage timestamp package phase message
    indexForDay :: Day -> Text
    indexForDay day = "garnix-build-logs-" <> T.replace "-" "." (cs $ iso8601Show day)

testImplementation ::
  IO
    ( Mock (OpenSearchId, LogLine) (),
      Mock (OpenSearchId, [Day], Maybe UTCTime, Int) [OpenSearchMessage]
    )
testImplementation = do
  logs :: MVar [(OpenSearchId, UTCTime, Maybe PackageName, Maybe Text, Text)] <- newMVar []
  storeLogLine <- newMock $ \(id, LogLine pkgName phase text) -> do
    now <- liftIO getCurrentTime
    liftIO $ modifyMVar_ logs $ \cur -> pure (cur <> [(id, now, pkgName, phase, text)])
  queryOpenSearch <- newMock $ \(id, indexes, after, limit) -> do
    logs <- liftIO $ readMVar logs
    pure
      $ take limit
      $ mapMaybe
        ( \(buildId', time, pkgName, phase, message) ->
            if id
              == buildId'
              && utctDay time
              `elem` indexes
              && maybe True (< time) after
              then Just $ OpenSearchMessage time pkgName phase message
              else Nothing
        )
        logs
  pure (storeLogLine, queryOpenSearch)
