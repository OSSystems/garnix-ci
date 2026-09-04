{-# LANGUAGE QuasiQuotes #-}

-- | The user-facing view of a deployed server: what the Servers list shows.
module Garnix.Hosting.Helpers
  ( RunningServer (..),
    ServerStatus (..),
    getRunningAndRecentServersForOwners,
  )
where

import Control.Lens
import Control.Monad (mfilter)
import Data.Aeson qualified as Aeson
import Data.Maybe (mapMaybe)
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.DB qualified as DB
import Garnix.DB.Hosting qualified as DBHosting
import Garnix.Hosting.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types

-- | Where a server is in its lifecycle, as far as a user is concerned.
data ServerStatus
  = -- | Claimed, but its first activation has not committed yet.
    Booting
  | Online
  | Ended
  deriving stock (Eq, Show, Generic)

instance ToJSON ServerStatus where
  toJSON = ourToJSON

data RunningServer = RunningServer
  { _runningServerId :: ServerId,
    _runningServerType :: DeploymentType,
    _runningServerStatus :: ServerStatus,
    _runningServerRepoOwner :: GhRepoOwner,
    _runningServerRepoName :: GhRepoName,
    _runningServerPackageName :: PackageName,
    _runningServerCreatedAt :: Maybe UTCTime,
    -- | When the most recent successful activation finished. Re-stamped in
    -- place on every redeploy of a persistent server, so this is "last
    -- deployed", not "first created". 'Nothing' until it first comes online.
    _runningServerReadyAt :: Maybe UTCTime,
    _runningServerConfigurationBuildId :: BuildId,
    _runningServerCommit :: CommitHash,
    _runningServerIpv4 :: Maybe Text,
    _runningServerDeployLogs :: Text,
    -- | Where the server answers once it is online.
    _runningServerUrl :: Text,
    -- | The raw @servers.exposed@ blob, when the server exposes anything, so
    -- the caller can build ssh commands and port links from it.
    _runningServerExposed :: Maybe Aeson.Value,
    -- | The declared extra hostnames, when it declares any.
    _runningServerDomains :: Maybe [Text],
    -- | The guest's real login accounts, when we managed to read them.
    _runningServerSshUsers :: Maybe [Text]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RunningServer where
  toJSON = ourToJSON

-- | Every server of these owners that is running, plus those that ended in the
-- last day so a user can still see why a deploy went away.
getRunningAndRecentServersForOwners :: [GhRepoOwner] -> M [RunningServer]
getRunningAndRecentServersForOwners owners = do
  domain <- view #hostingDomain
  exposures <- DBHosting.getServerExposures
  domainsByServer <- DBHosting.getServerDomains
  sshUsersByServer <- DBHosting.getServerSshUsers
  rows <-
    DB.pgQuery
      [pgSQL|
    SELECT servers.id, servers.pull_request, builds.branch, servers.ready_at,
           servers.ended_at, builds.repo_user, builds.repo_name, builds.package,
           servers.created_at, servers.configuration_build_id, builds.git_commit,
           host(servers.ipv4), servers.deploy_logs
    FROM servers
    INNER JOIN builds ON servers.configuration_build_id = builds.id
    WHERE builds.repo_user = ANY(${owners})
      AND (servers.ended_at IS NULL OR servers.ended_at > now() - interval '24 hours')
    ORDER BY servers.created_at DESC
  |]
  pure $ mapMaybe (toRunningServer domain exposures domainsByServer sshUsersByServer) rows
  where
    toRunningServer ::
      Text ->
      [(ServerId, Aeson.Value)] ->
      [(ServerId, [Text])] ->
      [(ServerId, [Text])] ->
      ServerRow ->
      Maybe RunningServer
    toRunningServer
      domain
      exposures
      domainsByServer
      sshUsersByServer
      ( serverId,
        pullRequest,
        branch,
        readyAt,
        endedAt,
        owner,
        repo,
        package,
        createdAt,
        buildId,
        commit,
        ipv4,
        logs
        ) = do
        deploymentType <- case (pullRequest, branch) of
          (Just prId, _) -> Just (GhPrDeployment prId)
          (Nothing, Just branch') -> Just (BranchDeployment branch')
          (Nothing, Nothing) -> Nothing
        -- A server that ended without ever being ready never came up at all; it
        -- is a failed claim, not a server the user had.
        status <- case (readyAt, endedAt) of
          (Nothing, Nothing) -> Just Booting
          (Just _, Nothing) -> Just Online
          (Just _, Just _) -> Just Ended
          (Nothing, Just _) -> Nothing
        let url =
              "https://"
                <> getPackageName (PackageName package)
                <> "."
                <> fromDeploymentType
                  getBranch
                  (("pull-" <>) . show . getGhPullRequestId)
                  deploymentType
                <> "."
                <> getGhRepoName repo
                <> "."
                <> getGhLogin (getGhRepoOwner owner)
                <> "."
                <> domain
        pure
          RunningServer
            { _runningServerId = serverId,
              _runningServerType = deploymentType,
              _runningServerStatus = status,
              _runningServerRepoOwner = owner,
              _runningServerRepoName = repo,
              _runningServerPackageName = PackageName package,
              _runningServerCreatedAt = Just createdAt,
              _runningServerReadyAt = readyAt,
              _runningServerConfigurationBuildId = buildId,
              _runningServerCommit = commit,
              _runningServerIpv4 = ipv4,
              _runningServerDeployLogs = logs,
              _runningServerUrl = url,
              _runningServerExposed = lookupById serverId exposures,
              _runningServerDomains = mfilter (not . null) (lookupById serverId domainsByServer),
              _runningServerSshUsers = mfilter (not . null) (lookupById serverId sshUsersByServer)
            }

    -- ServerId has no Ord instance, so these arrive as assoc lists; key them
    -- by the underlying hash id to look one up.
    lookupById :: ServerId -> [(ServerId, a)] -> Maybe a
    lookupById serverId assoc =
      lookup
        (getHashId (getServerId serverId))
        [(getHashId (getServerId key), value) | (key, value) <- assoc]

-- | The row shape of the query above, spelled out so the column types are
-- inferable at the destructuring site.
type ServerRow =
  ( ServerId,
    Maybe GhPullRequestId,
    Maybe Branch,
    Maybe UTCTime,
    Maybe UTCTime,
    GhRepoOwner,
    GhRepoName,
    Text,
    UTCTime,
    BuildId,
    CommitHash,
    Maybe Text,
    Text
  )
