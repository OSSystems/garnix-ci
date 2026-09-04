-- | Turning a finished build into running servers, and tearing down the ones
-- that are no longer wanted.
--
-- What gets deployed is declared in @garnix.yaml@ under @servers:@, one entry
-- per @nixosConfiguration@ plus the branch (or pull request) that triggers it.
-- The configuration's own nix code only supplies the extras of a server that
-- is already being deployed — ports, domains, ssh access — which we read off
-- the build afterwards via 'Garnix.Build.Package.discoverDeploySpec'.
module Garnix.Hosting.Deploy
  ( rolloutNewServerVersion,
    assembleDeployPlan,
    matchesDeployment,
    startServer,
    redeployServer,
    stopServer,
    stopUnusedServers,
    cleanupUnreadyServers,
    checkDeployPlan,
    checkTiersWithinCap,
    statsEnvContents,
    parseLoginUsers,
    failedUnitsFromActivation,
    publicHostFor,
  )
where

import Control.Concurrent.Async.Lifted qualified as Async
import Control.Lens
import Cradle
import Data.Containers.ListUtils (nubOrd)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text qualified as T
import Garnix.API.Keys (getRepoKeys)
import Garnix.Build.Package qualified as Package
import Garnix.Build.PrComment qualified as PrComment
import Garnix.BuildLogs.Types (mkLogLine)
import Garnix.DB qualified as DB
import Garnix.DB.Hosting qualified as DBHosting
import Garnix.Duration
import Garnix.Hosting.ServerPool qualified as ServerPool
import Garnix.Hosting.Types
import Garnix.LocalProvisioner (exposeServer)
import Garnix.Monad
import Garnix.Monad.KeyedMutex (withKeyedMutex)
import Garnix.Monad.Polling (PollingConfig (PollingConfig), withPolling)
import Garnix.Monad.SubProcess (runSubProcess)
import Garnix.Nix.StorePath (withStorePath)
import Garnix.Nix.Types (StorePath)
import Garnix.Prelude
import Garnix.Reporters.Utils (withRunReporter)
import Garnix.Request (retryingFor)
import Garnix.Types
import Garnix.YamlConfig
  ( DeploySection (OnBranch, OnPullRequest),
    ServerSection (..),
    flakeDir,
    getConfig,
    serverSection,
  )
import Network.Wreq qualified as Wreq
import System.Process qualified as Proc

-- | Deploy the servers this commit wants and stop the ones it no longer does.
--
-- Serialized per @(owner, repo)@ via 'deployMutex'. The lock covers planning
-- as well as execution, deliberately: 'getDeployPlan' derives its plan from
-- the current DB state, so two overlapping rollouts for the same repo would
-- otherwise both plan against the same stale snapshot of what is running and
-- then fight over the same guest.
rolloutNewServerVersion ::
  Reporter ->
  CommitInfo ->
  DeploymentType ->
  M [ServerInfo]
rolloutNewServerVersion reporter commitInfo deploymentType = do
  mutex <- view #deployMutex
  let repoKey =
        ( commitInfo ^. repoInfo . ghRepoOwner,
          commitInfo ^. repoInfo . ghRepoName
        )
  withKeyedMutex mutex repoKey
    $ withTextSpan
      ("deployment_type", fromDeploymentType (const "branch") (const "pr") deploymentType)
    $ (<?> "Rolling out new servers")
    $ reportingToPullRequest commitInfo deploymentType
    $ do
      plan <- getDeployPlan reporter commitInfo deploymentType
      servers <- executeDeployPlan reporter commitInfo plan deploymentType
      commentDeployedUrls commitInfo deploymentType plan
      pure servers

-- | Report a failed pull request rollout as a comment on it, then re-raise.
--
-- Both channels are caught: a user who only hears about the deploy through
-- this comment must not miss a rollout that died of an IO exception.
reportingToPullRequest :: CommitInfo -> DeploymentType -> M a -> M a
reportingToPullRequest commitInfo deploymentType action =
  case ghPrDeployment deploymentType of
    Nothing -> action
    Just prId -> do
      -- This runs for every pull request of every repo, most of which never
      -- asked for a deploy. A yaml we cannot even read declares nothing.
      declared <- catchEither declaresServers (const (pure False))
      if not declared
        then action
        else
          action `whenErrorEither` \problem ->
            PrComment.commentDeployFailed commitInfo prId (problemMessage problem)
  where
    declaresServers = do
      config <- getConfig
      pure
        $ any
          (isJust . matchesDeployment deploymentType . _serverSectionDeploySection)
          (config ^. serverSection)
    problemMessage =
      either
        (const "Something went wrong.")
        (userMessage . toErrorDetails)

-- | Tell the pull request the addresses its servers landed on.
--
-- Read off the plan rather than the returned 'ServerInfo's: the hostname comes
-- from the package name, which a bare 'ServerInfo' does not carry.
commentDeployedUrls :: CommitInfo -> DeploymentType -> DeployPlan -> M ()
commentDeployedUrls commitInfo deploymentType plan =
  case ghPrDeployment deploymentType of
    Nothing -> pure ()
    Just prId -> do
      domain <- view #hostingDomain
      let deployed =
            [ ( getPackageName (build ^. package),
                publicHostFor domain commitInfo deploymentType build
              )
              | build <-
                  map _serverToSpinUpBuild (_deployPlanToSpinUp plan)
                    <> map (_serverToSpinUpBuild . snd) (_deployPlanToRedeploy plan)
            ]
      PrComment.commentDeployedUrls commitInfo prId (nubOrd deployed)

-- * Planning

getDeployPlan :: Reporter -> CommitInfo -> DeploymentType -> M DeployPlan
getDeployPlan reporter commitInfo deploymentType =
  withErrorReporter reporter commitInfo $ do
    config <- getConfig
    existing <- DBHosting.getRunningServersOf (commitInfo ^. repoInfo) deploymentType

    -- What the yaml asks for on this push. A repo that declares nothing here
    -- has no builds to wait for: the only thing left to plan is tearing down
    -- whatever is still running from an earlier declaration.
    let wanted =
          [ section
            | section <- config ^. serverSection,
              isJust (matchesDeployment deploymentType (_serverSectionDeploySection section))
          ]

    checkTiersWithinCap deploymentType wanted

    -- Deploying means activating a closure, so every nixosConfiguration build
    -- of the commit has to have finished and uploaded first. We wait on all of
    -- them rather than only the declared ones: a repo whose other
    -- configurations are still building is mid-CI, not ready to deploy.
    nixosBuilds <-
      if null wanted
        then pure []
        else withPolling (PollingConfig (fromSeconds @Int 2) (fromHours @Int 2)) $ do
          builds <- DB.getLatestBuildsMatching (commitInfo ^. repoInfo) (commitInfo ^. commit)
          let overallFinished = case find (\b -> b ^. packageType == TypeOverall) builds of
                Nothing -> False
                Just b -> isJust (b ^. status)
              nixosConfigBuilds = filter (\b -> b ^. packageType == TypeNixosConfiguration) builds
              -- The TypeOverall row alone is NOT enough: Build/Flake.hs marks
              -- it Success at registration time, before any individual build
              -- has run. Every nixosConfiguration row must reach a terminal
              -- status of its own, or 'checkAllBuildsSucceeded' below sees a
              -- still-running build as "no status" and reports it as a
              -- failure.
              allFinished = all (\b -> isJust (b ^. status)) nixosConfigBuilds
              -- Only successful builds ever upload, so a failed one cannot
              -- gate readiness.
              allUploaded =
                all
                  (\b -> b ^. status /= Just Success || b ^. uploadedToCache == Just True)
                  nixosConfigBuilds
          pure
            $ if overallFinished && allFinished && allUploaded
              then Just nixosConfigBuilds
              else Nothing

    -- A `servers:` entry naming an attribute this commit does not build is a
    -- mistake worth telling the user about, rather than a server that silently
    -- never appears.
    let missing =
          nubOrd
            [ packageName
              | section <- wanted,
                let packageName = _serverSectionConfiguration section,
                packageName `notElem` map (view package) nixosBuilds
            ]
    unless (null missing)
      $ throw (DeploymentWantsNixosConfigurationsThatDontExist missing)

    -- The extras are read even off a build whose status is Failure/Timeout:
    -- nix eval does not need the derivation realized, and a server whose build
    -- failed should surface as an error from 'checkAllBuildsSucceeded' rather
    -- than silently vanish from the plan.
    discovered <-
      forM nixosBuilds $ \build ->
        (build,) <$> Package.discoverDeploySpec (config ^. flakeDir) build

    let plan = assembleDeployPlan deploymentType (config ^. serverSection) existing discovered

    -- A repo with no servers at all must not be failed for, say, having a
    -- branch name that is not a legal DNS label.
    unless (null (_deployPlanToSpinUp plan) && null (_deployPlanToRedeploy plan))
      $ checkDeployPlan (commitInfo ^. repoInfo) deploymentType plan
    pure plan

-- | The difference between what is running and what this commit wants.
--
-- Split out of 'getDeployPlan' as a pure function so the decision itself --
-- which guests to keep, replace, or tear down -- can be tested without a
-- database, a nix evaluation, or a provisioner.
assembleDeployPlan ::
  DeploymentType ->
  -- | The @servers:@ list of @garnix.yaml@: the whole of the declaration.
  [ServerSection] ->
  -- | Servers currently running for this repo and this deployment.
  [ServerInfo] ->
  -- | Every @nixosConfiguration@ build of the commit, paired with the extras
  -- its nix code declared, when we could read them.
  [(Build, Maybe ServerExtras)] ->
  DeployPlan
assembleDeployPlan deploymentType declared existing discovered =
  DeployPlan toSpinDown toSpinUp toRedeploy
  where
    -- Keyed by package name, so the same package built for two systems asks
    -- for one guest rather than two fighting over the same hostname.
    buildsByPackage :: Map PackageName (Build, ServerExtras)
    buildsByPackage =
      Map.fromList
        [ (build ^. package, (build, fromMaybe defaultServerExtras extras))
          | (build, extras) <- discovered
        ]
    -- A configuration the yaml never names is not a server, however much
    -- `garnix.server` it sets. An entry naming a configuration this commit did
    -- not build is dropped here and reported by 'getDeployPlan' instead.
    wantedPackages :: Map PackageName (ServerTier, Bool, ServerExtras, Build)
    wantedPackages =
      Map.fromList
        [ (packageName, (tier, isPrimary', extras, build))
          | section <- declared,
            let packageName = _serverSectionConfiguration section,
            Just (tier, isPrimary') <-
              [matchesDeployment deploymentType (_serverSectionDeploySection section)],
            Just (build, extras) <- [Map.lookup packageName buildsByPackage]
        ]
    wantedServers = map toSpinUpSpec (Map.elems wantedPackages)
    -- A running server is redeployed in place, rather than replaced, when it
    -- and the wanted server share a persistence name. That is what makes a
    -- persistent guest keep its disk across a push.
    toRedeploy =
      [ (server, wanted)
        | server <- existing,
          wanted <- wantedServers,
          let persistence = _serverToSpinUpBuild wanted ^. persistenceName,
          isJust persistence,
          persistence == _serverInfoPersistenceName server
      ]
    redeployedBuilds = map (_serverToSpinUpBuild . snd) toRedeploy
    toSpinDown = filter (`notElem` map fst toRedeploy) existing
    toSpinUp =
      filter ((`notElem` redeployedBuilds) . _serverToSpinUpBuild) wantedServers

-- | Does this yaml entry's declared deployment match what we are rolling out,
-- and if so at what machine size and primacy?
--
-- A pull request deploy sizes itself the same way a branch deploy does, and is
-- held to the same instance cap. It is never primary: the short hostname
-- belongs to the branch deploy, and the yaml has no field to ask for it.
matchesDeployment :: DeploymentType -> DeploySection -> Maybe (ServerTier, Bool)
matchesDeployment deploymentType = \case
  OnBranch wantedBranch tier isPrimary'
    | BranchDeployment thisBranch <- deploymentType,
      wantedBranch == thisBranch ->
        Just (tier, isPrimary')
  OnPullRequest tier
    | GhPrDeployment _ <- deploymentType -> Just (tier, False)
  _ -> Nothing

toSpinUpSpec :: (ServerTier, Bool, ServerExtras, Build) -> ServerToSpinUp
toSpinUpSpec (tier, isPrimary', extras, build) =
  ServerToSpinUp
    { _serverToSpinUpTier = tier,
      _serverToSpinUpBuild = build,
      _serverToSpinUpIsPrimary = isPrimary',
      _serverToSpinUpExposeSSH = _serverExtrasExposeSSH extras,
      _serverToSpinUpAuthorizeDeployerGithubKeys =
        _serverExtrasAuthorizeDeployerGithubKeys extras,
      _serverToSpinUpAuthorizedSSHKeys = _serverExtrasAuthorizedSSHKeys extras,
      _serverToSpinUpHttpPorts = portsOfType HttpPort,
      _serverToSpinUpTcpPorts = portsOfType TcpPort,
      _serverToSpinUpDomains = _serverExtrasDomains extras
    }
  where
    portsOfType wanted =
      [ (_serverPortName port, _serverPortPort port)
        | port <- _serverExtrasPorts extras,
          _serverPortType port == wanted
      ]

-- | Report a planning failure to the user as its own run, then re-raise.
withErrorReporter :: Reporter -> CommitInfo -> M a -> M a
withErrorReporter reporter commitInfo action =
  try action >>= \case
    Right value -> pure value
    Left problem -> do
      run <- DB.newRun "deployment plan" commitInfo
      withRunReporter reporter (ReportRun run) $ \runReporter -> do
        reportLogs runReporter (mkLogLine $ userMessage $ toErrorDetails problem)
        reportComplete runReporter RunReportStatusFailure
      rethrow problem

-- | Refuse a @servers:@ entry asking for a larger machine than this instance
-- hands out, before anything is waited on.
--
-- Refused here rather than at acquisition time, where it would surface as "no
-- capacity" and blame the instance for what the repo asked for. Applies to
-- branch and pull request deploys alike: the limit is the instance's.
checkTiersWithinCap :: DeploymentType -> [ServerSection] -> M ()
checkTiersWithinCap deploymentType sections = do
  maxTier <- _hostingBudgetMaxTier <$> view #hostingBudget
  forM_ maxTier $ \cap ->
    forM_ sections $ \section ->
      forM_ (matchesDeployment deploymentType (_serverSectionDeploySection section)) $ \(tier, _) ->
        unless (tierWithinCap (Just cap) tier)
          $ throw
          $ ServerTierExceedsInstanceCap
            (_serverSectionConfiguration section)
            (getServerTier tier)
            (getServerTier cap)

checkDeployPlan :: RepoInfo -> DeploymentType -> DeployPlan -> M ()
checkDeployPlan repo deploymentType plan = do
  let builds =
        map _serverToSpinUpBuild (_deployPlanToSpinUp plan)
          <> map (_serverToSpinUpBuild . snd) (_deployPlanToRedeploy plan)
  checkSubdomainValidity repo deploymentType builds
  checkAllBuildsSucceeded builds

-- | Every part of a server's hostname has to be a legal DNS label, or the
-- server would be deployed to an address nobody can reach.
checkSubdomainValidity :: RepoInfo -> DeploymentType -> [Build] -> M ()
checkSubdomainValidity repo deploymentType builds = do
  let GhRepoOwner (GhLogin owner) = repo ^. ghRepoOwner
      GhRepoName name = repo ^. ghRepoName
  unless (isValidSubdomainString owner)
    $ throw
    $ NameIsNotValidSubdomain RepoOwnerSubdomain owner
  unless (isValidSubdomainString name)
    $ throw
    $ NameIsNotValidSubdomain RepoNameSubdomain name
  case deploymentType of
    BranchDeployment (Branch branch') ->
      unless (isValidSubdomainString branch')
        $ throw
        $ NameIsNotValidSubdomain BranchSubdomain branch'
    GhPrDeployment _ -> pure ()
  forM_ builds $ \build -> do
    let packageName = getPackageName (build ^. package)
    unless (isValidSubdomainString packageName)
      $ throw
      $ NameIsNotValidSubdomain PackageNameSubdomain packageName
    forM_ (build ^. persistenceName) $ \persistence ->
      unless (isValidSubdomainString persistence)
        $ throw
        $ NameIsNotValidSubdomain PersistenceNameSubdomain persistence

checkAllBuildsSucceeded :: [Build] -> M ()
checkAllBuildsSucceeded builds =
  forM_ builds $ \build -> do
    let packageName = getPackageName (build ^. package)
    case build ^. status of
      Just Success -> pure ()
      Just Failure -> throw $ OtherError $ packageName <> " failed"
      Just Timeout -> throw $ OtherError $ packageName <> " timed out"
      Just Cancelled -> throw $ OtherError $ packageName <> " cancelled"
      -- Unreachable: planning waits for every build to reach a status.
      Nothing -> throw $ OtherError $ packageName <> " has no status"

-- * Execution

executeDeployPlan ::
  Reporter ->
  CommitInfo ->
  DeployPlan ->
  DeploymentType ->
  M [ServerInfo]
executeDeployPlan reporter commitInfo plan deploymentType = do
  -- New guests are not committed ready until every concurrent start has
  -- succeeded. If any fails, compensate all unready claims at once; the old
  -- generation is left untouched and still serving.
  started <-
    Async.mapConcurrently (startServer reporter commitInfo deploymentType) (_deployPlanToSpinUp plan)
      `whenErrorEither` const (void cleanupUnreadyServers)
  redeployed <-
    Async.mapConcurrently
      (uncurry (redeployServer reporter commitInfo deploymentType))
      (_deployPlanToRedeploy plan)
  deployed <-
    toggleServerFlags started (_deployPlanToSpinDown plan)
      <?> "Marking servers ready and old ones ended"
  Async.mapConcurrently_ (stopServer . _serverInfoId) (_deployPlanToSpinDown plan)
  pure (deployed <> redeployed)

-- | Mark the new servers ready and the replaced ones ended, in one
-- transaction: a crash between the two would leave the repo serving from both
-- generations at once.
toggleServerFlags :: [ServerInfo] -> [ServerInfo] -> M [ServerInfo]
toggleServerFlags wanted current = do
  now <- liftIO getCurrentTime
  let wanted' = [server {_serverInfoReadyAt = Just now} | server <- wanted]
      current' = [server {_serverInfoEndedAt = Just now} | server <- current]
  DB.pgTransaction $ traverse_ DBHosting.updateServerPostDeploy (wanted' <> current')
  pure wanted'

stopServer :: ServerId -> M ()
stopServer = ServerPool.releaseServer

-- | Compensating transaction for a backend that died after claiming a pool
-- guest but before the deploy committed @ready_at@. We cannot know how far
-- the guest was mutated, so it is destroyed and a later rollout claims a
-- clean one; keeping it would leak both the VM and any credentials already
-- installed on it.
cleanupUnreadyServers :: M Int
cleanupUnreadyServers = do
  unready <- DBHosting.getUnreadyServers
  cleaned <- forM unready $ \server ->
    catchEither
      (stopServer (_serverInfoId server) $> 1)
      ( \problem -> do
          log Error
            $ "cleanupUnreadyServers: could not remove unready server "
            <> showPretty (_serverInfoId server)
            <> ": "
            <> either show showDebug problem
          pure 0
      )
  pure (sum cleaned)

-- | Tear down PR deploys that have not been reached in a while. The gateway
-- reports every hostname it serves; a candidate absent from that report for
-- the whole heartbeat window is idle.
stopUnusedServers :: M ()
stopUnusedServers = do
  domain <- view #hostingDomain
  PrHostList candidates <- DBHosting.getShutdownCandidates
  unless (null candidates) $ do
    heartbeats <- DB.getRecentHeartbeats
    -- No heartbeats at all means nobody is reporting — a gateway that is down
    -- or not configured — not that every server is idle. Tearing down live
    -- deploys because the reporter is broken is far worse than leaving an
    -- idle one running until it comes back.
    if null heartbeats
      then
        log Warning
          $ "stopUnusedServers: "
          <> show (length candidates)
          <> " server(s) are old enough to reap, but no heartbeats have been"
          <> " reported at all. Leaving them alone; is the gateway running?"
      else do
        let idle host = (hostToDomainName host <> "." <> domain) `notElem` heartbeats
        traverse_ (stopServer . _hostServerId) (filter idle candidates)

-- * Bringing one server up

newtype SshUser = SshUser Text
  deriving stock (Eq, Show)

startServer ::
  Reporter ->
  CommitInfo ->
  DeploymentType ->
  ServerToSpinUp ->
  M ServerInfo
startServer reporter commitInfo deploymentType wanted = do
  let build = _serverToSpinUpBuild wanted
  run <- DB.newRun ("deployment " <> getPackageName (build ^. package)) commitInfo
  withRunReporter reporter (ReportRun run) $ \runReporter -> do
    domain <- view #hostingDomain
    budget <- view #hostingBudget
    let publicHost = publicHostFor domain commitInfo deploymentType build
        -- Surface progress on the run so the UI shows what it is waiting on
        -- instead of a bare Pending; the first line also flips the run from
        -- pending to running.
        reportProgress message = reportLogs runReporter (mkLogLine message)
        tier = _serverToSpinUpTier wanted
    reportProgress
      $ "Provisioning "
      <> getPackageName (build ^. package)
      <> " on a "
      <> getServerTier tier
      <> " guest…"
    serverId <-
      ServerPool.acquireServer
        budget
        tier
        (build ^. id)
        (ghPrDeployment deploymentType)
        (_serverToSpinUpIsPrimary wanted)
    serverInfo <-
      DBHosting.getServer serverId >>= \case
        Just info -> pure info
        Nothing -> throw $ ProvisioningError "the server row vanished right after it was claimed"
    reportProgress
      $ "Guest "
      <> maybe "?" identity (serverAddressText (_serverInfoAddress serverInfo))
      <> " ready — activating configuration…"
    DBHosting.setServerDomains serverId (_serverToSpinUpDomains wanted)
    configureServer (SshUser "root") commitInfo serverInfo wanted
    (_, stderr) <-
      setupServer (commitInfo ^. repoInfo) build serverInfo `whenError` \problem -> do
        DBHosting.appendToServerDeployLog serverId (showPretty (err problem))
        -- The guest booted but failed to activate, so it is still reachable
        -- (teardown happens later). Capture why while we can.
        case err problem of
          ActivationError {stdErr} -> captureGuestFailureDiagnostics serverInfo stdErr
          _ -> pure ()
    let logs = deployLogs "deployed" publicHost serverInfo stderr
    DBHosting.appendToServerDeployLog serverId logs
    reportLogs runReporter (mkLogLine logs)
    reportComplete runReporter RunReportStatusSuccess
    captureAndStoreSshUsers serverInfo
    pure serverInfo

redeployServer ::
  Reporter ->
  CommitInfo ->
  DeploymentType ->
  ServerInfo ->
  ServerToSpinUp ->
  M ServerInfo
redeployServer reporter commitInfo deploymentType serverInfo wanted = do
  let build = _serverToSpinUpBuild wanted
      serverId = _serverInfoId serverInfo
  withStorePath build "out" $ \case
    Nothing -> throw $ OtherError "Store path is missing"
    Just storePath -> do
      run <- DB.newRun ("redeployment " <> getPackageName (build ^. package)) commitInfo
      withRunReporter reporter (ReportRun run) $ \runReporter -> do
        sshUser <- chooseRedeploySshUser serverInfo
        domain <- view #hostingDomain
        let publicHost = publicHostFor domain commitInfo deploymentType build
        -- /var/garnix/keys is a tmpfs on the guest, so every runtime file has
        -- to be re-converged before activation, not just copied once at
        -- creation.
        copyKeys sshUser (commitInfo ^. repoInfo) serverInfo <?> "Copying repo key"
        copyStatsEnv sshUser serverInfo <?> "Copying guest stats configuration"
        configureServer sshUser commitInfo serverInfo wanted
        DBHosting.setServerDomains serverId (_serverToSpinUpDomains wanted)
        copyClosure sshUser serverInfo storePath <?> "Copying closure for redeployment"
        deployStderr <-
          (switchToConfiguration sshUser serverInfo storePath <?> "Switching to redeployment configuration")
            `whenError` \problem -> case err problem of
              ActivationError {stdErr} -> captureGuestFailureDiagnostics serverInfo stdErr
              _ -> pure ()
        now <- liftIO getCurrentTime
        let updated =
              serverInfo
                { _serverInfoConfigurationBuildId = build ^. id,
                  _serverInfoReadyAt = Just now,
                  _serverInfoIsPrimary = _serverToSpinUpIsPrimary wanted
                }
        DBHosting.updateServerPostDeploy updated <?> "Recording the redeploy"
        captureAndStoreSshUsers updated
        let logs = deployLogs "redeployed" publicHost serverInfo deployStderr
        DBHosting.appendToServerDeployLog serverId logs
        reportLogs runReporter (mkLogLine logs)
        reportComplete runReporter RunReportStatusSuccess
        pure updated

-- | The per-deploy convergence both spin-up and redeploy share: authorized
-- keys and port exposure. Convergence, not an additive copy — revoking a key
-- or a port in the config has to take effect on the next deploy.
configureServer :: SshUser -> CommitInfo -> ServerInfo -> ServerToSpinUp -> M ()
configureServer sshUser commitInfo serverInfo wanted = do
  copyAuthorizedKeys
    sshUser
    serverInfo
    (if _serverToSpinUpAuthorizeDeployerGithubKeys wanted then Just (commitInfo ^. reqUser) else Nothing)
    (_serverToSpinUpAuthorizedSSHKeys wanted)
    <?> "Synchronizing SSH keys"
  exposeResult <- exposeServerPorts serverInfo wanted
  forM_ exposeResult $ \result ->
    DBHosting.setServerExposed
      (_serverInfoId serverInfo)
      result
      (_serverToSpinUpHttpPorts wanted)

-- | Deploy a NixOS configuration onto a freshly provisioned guest. Does not
-- touch the previous generation.
setupServer :: RepoInfo -> Build -> ServerInfo -> M (ServerInfo, Text)
setupServer repo build serverInfo =
  withStorePath build "out" $ \case
    Nothing -> throw $ OtherError "Store path is missing"
    Just storePath -> do
      copyKeys (SshUser "root") repo serverInfo
      copyStatsEnv (SshUser "root") serverInfo <?> "Copying guest stats configuration"
      copyClosure (SshUser "root") serverInfo storePath <?> "Copying closure"
      stderr <-
        switchToConfiguration (SshUser "root") serverInfo storePath
          <?> "Switching to configuration"
      pure (serverInfo, stderr)

deployLogs :: Text -> Text -> ServerInfo -> Text -> Text
deployLogs verb publicHost serverInfo stderr =
  T.unlines
    [ "Server has been successfully " <> verb <> " to: https://" <> publicHost,
      "address: " <> fromMaybe "<none>" (serverAddressText (_serverInfoAddress serverInfo)),
      "",
      "logs:" <> stderr
    ]

-- | The public hostname a deploy answers on.
publicHostFor :: Text -> CommitInfo -> DeploymentType -> Build -> Text
publicHostFor domain commitInfo deploymentType build =
  T.intercalate
    "."
    [ getPackageName (build ^. package),
      fromDeploymentType getBranch (("pull-" <>) . show . getGhPullRequestId) deploymentType,
      getGhRepoName (commitInfo ^. repoInfo . ghRepoName),
      getGhLogin (getGhRepoOwner (commitInfo ^. repoInfo . ghRepoOwner)),
      domain
    ]

-- * Talking to a guest

-- | Prefer the least-privileged account, but keep the root path as a bridge
-- for guests created before the @garnix@ account was part of the base image.
chooseRedeploySshUser :: ServerInfo -> M SshUser
chooseRedeploySshUser server = do
  garnixWorks <- canConnect "garnix"
  if garnixWorks
    then pure (SshUser "garnix")
    else do
      rootWorks <- canConnect "root"
      if rootWorks
        then pure (SshUser "root")
        else
          throw
            $ ProvisioningError
              "Neither garnix nor root hosting-key SSH access works for this persistent guest"
  where
    canConnect :: Text -> M Bool
    canConnect user = do
      (ip, sshArgs) <- ServerPool.sshArgsFor server
      (exitCode, _, _) <-
        liftIO
          $ Proc.readProcessWithExitCode
            "ssh"
            ((cs <$> sshArgs) <> [cs user <> "@" <> cs ip, "true"])
            ""
      pure (exitCode == ExitSuccess)

copyClosure :: SshUser -> ServerInfo -> StorePath -> M ()
copyClosure (SshUser user) server storePath = do
  (ip, sshArgs) <- ServerPool.sshArgsFor server
  retryingFor (fromMinutes @Int 1) $ do
    runSubProcess
      $ cmd "nix-copy-closure"
      & addArgs ["--to", user <> "@" <> ip, cs storePath]
      & modifyEnvVar "NIX_SSHOPTS" (const $ Just $ cs $ T.intercalate " " sshArgs)

copyKeys :: SshUser -> RepoInfo -> ServerInfo -> M ()
copyKeys (SshUser user) repo server = do
  (ip, sshArgs) <- ServerPool.sshArgsFor server
  let keyLocation = "/var/garnix/keys/repo-key"
      sudoArgs = if user == "root" then [] else ["sudo", "-n"]
      doRemotely args =
        runSubProcess
          $ cmd "ssh"
          & addArgs (sshArgs <> [user <> "@" <> ip] <> sudoArgs <> args)
  doRemotely ["mkdir", "-p", cs (takeDirectory keyLocation)]
  (_, privKey) <-
    getRepoKeys (repo ^. ghRepoOwner) (repo ^. ghRepoName) <?> "Get private keys"
  repoSecretsKey <- view #repoSecretsEncryptionKeyPath
  exportResult <-
    liftIO
      ( exportKeys
          ExportKeysOpts
            { privateKey = privKey,
              ipAddr = ip,
              targetPath = keyLocation,
              sshArgs,
              sshUser = user,
              sshSudo = user /= "root"
            }
          repoSecretsKey
      )
      <?> "Export private keys to server"
  whenIs _Left exportResult $ throw . ProvisioningError
  doRemotely ["chmod", "400", cs keyLocation]

-- | Install the non-secret stats endpoint and instance id. Written on every
-- activation, so changing the endpoint does not require recreating guests.
copyStatsEnv :: SshUser -> ServerInfo -> M ()
copyStatsEnv (SshUser user) server = do
  endpoint <- view #statsReportUrl
  case (endpoint, _serverInfoInstanceId server) of
    (Just url, Just instanceId) -> do
      (ip, sshArgs) <- ServerPool.sshArgsFor server
      result <-
        liftIO
          $ installPublicFile
          $ InstallPublicFileOpts
            { installPublicFileContents = statsEnvContents url instanceId,
              installPublicFileIpAddr = ip,
              installPublicFileTargetPath = "/var/lib/garnix/stats.env",
              installPublicFileSshOptions = sshArgs,
              installPublicFileSshUser = user,
              installPublicFileSshSudo = user /= "root"
            }
      either (throw . ProvisioningError) pure result
    -- No endpoint configured, or no instance to attribute samples to: the
    -- guest's reporter is gated on this file existing, so leaving it absent
    -- simply turns reporting off.
    _ -> pure ()

statsEnvContents :: Text -> InstanceId -> Text
statsEnvContents endpoint instanceId =
  T.unlines
    [ "GARNIX_STATS_URL=" <> endpoint,
      "GARNIX_PROVISIONER_ID=" <> getInstanceId instanceId
    ]

-- | Authorize login as the guest's @garnix@ account by writing the
-- authorized_keys file its profile reads. With no keys the file is removed,
-- so revoking access in the config actually revokes it.
copyAuthorizedKeys :: SshUser -> ServerInfo -> Maybe GhLogin -> [Text] -> M ()
copyAuthorizedKeys (SshUser user) server deployer extraKeys = do
  deployerKeys <- maybe (pure []) fetchDeployerKeys deployer
  let keys = filter (not . T.null . T.strip) (deployerKeys <> extraKeys)
      keyFile = "/var/garnix/keys/authorized_keys" :: Text
  if null keys
    then removeRuntimeFile (SshUser user) server keyFile
    else do
      (ip, sshArgs) <- ServerPool.sshArgsFor server
      (exitCode, _, _) <-
        liftIO
          $ Proc.readProcessWithExitCode
            "ssh"
            ( (cs <$> sshArgs)
                <> [ cs user <> "@" <> cs ip,
                     remoteAsRoot user
                       -- Written via a temporary file and renamed: sshd may be
                       -- reading authorized_keys while we replace it.
                       $ "mkdir -p /var/garnix/keys && cat > "
                       <> keyFile
                       <> ".tmp && chmod 444 "
                       <> keyFile
                       <> ".tmp && mv -f "
                       <> keyFile
                       <> ".tmp "
                       <> keyFile
                   ]
            )
            (cs (T.unlines keys))
      case exitCode of
        ExitSuccess -> pure ()
        ExitFailure _ -> throw $ OtherError "Writing authorized_keys to the server failed"

removeRuntimeFile :: SshUser -> ServerInfo -> Text -> M ()
removeRuntimeFile (SshUser user) server path = do
  (ip, sshArgs) <- ServerPool.sshArgsFor server
  (exitCode, _, _) <-
    liftIO
      $ Proc.readProcessWithExitCode
        "ssh"
        ((cs <$> sshArgs) <> [cs user <> "@" <> cs ip, remoteAsRoot user ("rm -f " <> path)])
        ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> throw $ OtherError "Removing a stale runtime credential from the server failed"

remoteAsRoot :: Text -> Text -> String
remoteAsRoot user command
  | user == "root" = cs command
  | otherwise = cs $ "sudo -n sh -c '" <> command <> "'"

-- | The deployer's public keys from @github.com\/\<login\>.keys@. Best-effort:
-- any failure yields no keys rather than failing the deploy.
fetchDeployerKeys :: GhLogin -> M [Text]
fetchDeployerKeys login =
  ( do
      response <-
        withWreqOptions $ \opts ->
          Wreq.getWith opts (cs ("https://github.com/" <> getGhLogin login <> ".keys"))
      pure $ filter (not . T.null . T.strip) $ T.lines $ cs (response ^. Wreq.responseBody)
  )
    `catchAny` const (pure [])

-- | Converge the guest's SSH and TCP exposure. Sending an empty request is
-- deliberate: it is what removes stale DNAT rules when a persistent server's
-- config drops its last exposed port. 'Nothing' only when no local
-- provisioner is configured.
exposeServerPorts :: ServerInfo -> ServerToSpinUp -> M (Maybe ExposeResult)
exposeServerPorts server wanted = do
  socket <- view #provisionerSocket
  case (socket, _serverInfoInstanceId server) of
    (Just sock, Just instanceId) ->
      Just
        <$> exposeServer
          sock
          instanceId
          (_serverToSpinUpExposeSSH wanted)
          (map snd (_serverToSpinUpTcpPorts wanted))
    _ -> pure Nothing

switchToConfiguration :: SshUser -> ServerInfo -> StorePath -> M Text
switchToConfiguration (SshUser user) server storePath = do
  (ip, sshArgs) <- ServerPool.sshArgsFor server
  StderrRaw stderr <-
    withError (errLens %~ toActivationError ip)
      $ runSubProcess
      $ cmd "ssh"
      & addArgs
        ( sshArgs
            <> [ user <> "@" <> ip,
                 "sudo",
                 cs $ cs storePath </> "bin/switch-to-configuration",
                 "switch"
               ]
        )
      & silenceStdout
  pure $ cs stderr
  where
    toActivationError ip = \case
      RunProcessError {stdErr} -> ActivationError ip stdErr
      other -> other

-- * Best-effort guest introspection

-- | Read the guest's real login accounts so a user can be told which account
-- to ssh in as. Never fails the deploy.
--
-- Uses 'catchEither' rather than 'catchAny' because 'runSubProcess' signals a
-- non-zero exit through 'MonadError', which an exception handler alone would
-- not see.
captureAndStoreSshUsers :: ServerInfo -> M ()
captureAndStoreSshUsers server =
  capture `catchEither` \problem ->
    log Informational
      $ "captureAndStoreSshUsers: could not read guest login users for server "
      <> showPretty (_serverInfoId server)
      <> ": "
      <> either show showDebug problem
  where
    capture = do
      (ip, sshArgs) <- ServerPool.sshArgsFor server
      StdoutUntrimmed output <-
        runSubProcess
          $ cmd "ssh"
          & addArgs (sshArgs <> ["garnix@" <> ip, "getent passwd"])
      DBHosting.setServerSshUsers (_serverInfoId server) (parseLoginUsers output)

-- | When activation fails the guest is still up (teardown happens later), so
-- pull its failed units and journal into the deploy log.
-- @switch-to-configuration@ reports only "the following units failed", never
-- why. Never fails the deploy.
captureGuestFailureDiagnostics :: ServerInfo -> Text -> M ()
captureGuestFailureDiagnostics server activationStderr =
  capture `catchEither` \problem ->
    log Informational
      $ "captureGuestFailureDiagnostics: could not collect diagnostics for server "
      <> showPretty (_serverInfoId server)
      <> ": "
      <> either show showDebug problem
  where
    capture = do
      (ip, sshArgs) <- ServerPool.sshArgsFor server
      StdoutUntrimmed output <-
        runSubProcess $ cmd "ssh" & addArgs (sshArgs <> ["root@" <> ip, diagCmd])
      DBHosting.appendToServerDeployLog (_serverInfoId server)
        $ "\n=== guest diagnostics ("
        <> ip
        <> ") ===\n"
        <> cs output

    -- The units activation named, unioned on the guest with whatever
    -- `systemctl --failed` reports. Both are needed: a unit that failed during
    -- activation may already have been reset by the time we look, and a unit
    -- that failed for an unrelated reason will not be in the stderr.
    diagCmd =
      "reported=\""
        <> T.intercalate " " (failedUnitsFromActivation activationStderr)
        <> "\"; "
        <> "echo '--- failed units ---'; "
        <> "systemctl --failed --no-legend --plain 2>/dev/null; "
        <> "units=$(printf '%s\\n' $reported $(systemctl --failed --no-legend --plain 2>/dev/null | awk '{print $1}') | sort -u); "
        <> "echo; echo '--- per-unit status and logs ---'; "
        <> "for u in $units; do "
        <> "echo \"=== $u ===\"; "
        <> "systemctl status \"$u\" --no-pager --full 2>&1 | head -40; "
        -- Deliberately no -p filter: systemd records a daemon's stdout at info
        -- priority, so a warning-and-above filter throws away exactly the
        -- fatal message we are looking for.
        <> "echo \"--- journal: $u ---\"; "
        <> "journalctl -b -u \"$u\" --no-pager -o short-precise 2>/dev/null | tail -200; "
        <> "echo; "
        <> "done; "
        <> "echo '--- journal since boot, tail ---'; "
        <> "journalctl -b --no-pager -o short-precise 2>/dev/null | tail -200"

-- | The units @switch-to-configuration@ reports as failed, out of its stderr
-- (@warning: the following units failed: a.service, b.service@).
--
-- Only that exact phrasing counts: activation also prints "NOT restarting the
-- following changed units:" on every healthy deploy, which must not be read as
-- a failure. Deduplicated and capped, since the guest controls this text.
failedUnitsFromActivation :: Text -> [Text]
failedUnitsFromActivation raw =
  take maxReportedUnits $ nubOrd $ concatMap unitsOnLine (T.lines raw)
  where
    maxReportedUnits = 25 :: Int
    marker = "the following units failed:"
    unitsOnLine line = case T.breakOn marker line of
      (_, rest)
        | T.null rest -> []
        | otherwise ->
            filter (not . T.null)
              $ map T.strip
              $ T.splitOn ","
              $ T.drop (T.length marker) rest

-- | Parse @getent passwd@ output into login usernames, dropping system
-- accounts whose shell ends in @nologin@ or @false@. Deduplicated,
-- first-occurrence order, and capped so a pathological guest cannot blow up
-- the row. @garnix@ is always kept when present regardless of its shell,
-- since it is the deploy account and always a valid login.
parseLoginUsers :: Text -> [Text]
parseLoginUsers raw =
  take maxCapturedSshUsers $ nubOrd (shellUsers <> garnixIfPresent)
  where
    maxCapturedSshUsers = 50 :: Int
    entries = map (T.splitOn ":") (T.lines raw)
    shellUsers = mapMaybe loginUser entries
    garnixIfPresent = ["garnix" | any ((== Just "garnix") . listToMaybe) entries]
    loginUser = \case
      (name : _ : _ : _ : _ : _ : shell : _)
        | name /= "root" && not (hasNologinShell shell) -> Just name
      _ -> Nothing
    hasNologinShell shell =
      "nologin" `T.isSuffixOf` shell || "false" `T.isSuffixOf` shell
