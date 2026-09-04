module Garnix.Hosting.DeploySpec (spec) where

import Control.Arrow ((&&&))
import Control.Lens
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Garnix.Build.Package (decodeDeploySpec)
import Garnix.Hosting.Deploy
import Garnix.Hosting.Types
import Garnix.Monad (Env (..))
import Garnix.Prelude
import Garnix.TestHelpers (defaultCommitInfo, runTestM)
import Garnix.Types
import Garnix.YamlConfig qualified as Yaml
import Test.Hspec

spec :: Spec
spec = do
  describe "failedUnitsFromActivation" $ do
    it "reads the units activation reports as failed" $ do
      failedUnitsFromActivation
        "warning: the following units failed: app.service, db.service\n"
        `shouldBe` ["app.service", "db.service"]

    it "ignores the line a healthy deploy also prints" $ do
      -- Activation prints this on every successful switch that changed a unit.
      -- Reading it as a failure would make every deploy look broken.
      failedUnitsFromActivation
        "NOT restarting the following changed units: dbus.service\n"
        `shouldBe` []

    it "finds nothing in output that mentions no failure" $ do
      failedUnitsFromActivation "activating the configuration...\nsetting up /etc...\n"
        `shouldBe` []

    it "deduplicates units named on more than one line" $ do
      failedUnitsFromActivation
        "warning: the following units failed: app.service\n\
        \warning: the following units failed: app.service, db.service\n"
        `shouldBe` ["app.service", "db.service"]

    it "caps how many units a guest can make us report" $ do
      -- The guest controls this text, so it must not be able to make the
      -- diagnostics command unboundedly long.
      let many' = T.intercalate ", " [cs (show n) <> ".service" | n <- [1 :: Int .. 100]]
      length (failedUnitsFromActivation ("the following units failed: " <> many'))
        `shouldBe` 25

  describe "parseLoginUsers" $ do
    it "keeps accounts with a real shell" $ do
      parseLoginUsers
        "alice:x:1000:1000::/home/alice:/run/current-system/sw/bin/bash\n\
        \bob:x:1001:1001::/home/bob:/bin/zsh\n"
        `shouldBe` ["alice", "bob"]

    it "drops system accounts that cannot log in" $ do
      parseLoginUsers
        "nginx:x:60:60::/var/empty:/run/current-system/sw/bin/nologin\n\
        \sshd:x:61:61::/var/empty:/bin/false\n\
        \alice:x:1000:1000::/home/alice:/bin/bash\n"
        `shouldBe` ["alice"]

    it "drops root, which is never the account to suggest" $ do
      parseLoginUsers "root:x:0:0::/root:/bin/bash\n" `shouldBe` []

    it "keeps garnix even though its shell is nologin" $ do
      -- garnix is the deploy account: it is always a valid login regardless
      -- of what its shell says.
      parseLoginUsers "garnix:x:999:999::/var/garnix:/run/current-system/sw/bin/nologin\n"
        `shouldBe` ["garnix"]

    it "survives a line that is not a passwd entry at all" $ do
      parseLoginUsers "garbage\n\nalice:x:1000:1000::/home/alice:/bin/bash\n"
        `shouldBe` ["alice"]

    it "caps how many accounts a guest can make us store" $ do
      let entries =
            foldMap
              (\n -> "u" <> cs (show n) <> ":x:1:1::/home:/bin/bash\n")
              [1 :: Int .. 100]
      length (parseLoginUsers entries) `shouldBe` 50

  describe "statsEnvContents" $ do
    it "gives the guest reporter its endpoint and its own id" $ do
      statsEnvContents "https://garnix.example/api/hosts/stats" (InstanceId "guest-7")
        `shouldBe` "GARNIX_STATS_URL=https://garnix.example/api/hosts/stats\n\
                   \GARNIX_PROVISIONER_ID=guest-7\n"

  describe "publicHostFor" $ do
    it "builds a branch deploy's hostname" $ do
      publicHostFor
        "hosting.example"
        (commitInfoFor "acme" "widgets")
        (BranchDeployment (Branch "main"))
        (buildFor "web")
        `shouldBe` "web.main.widgets.acme.hosting.example"

    it "builds a PR deploy's hostname from the PR number" $ do
      publicHostFor
        "hosting.example"
        (commitInfoFor "acme" "widgets")
        (GhPrDeployment (GhPullRequestId 42))
        (buildFor "web")
        `shouldBe` "web.pull-42.widgets.acme.hosting.example"

  describe "decodeDeploySpec" $ do
    it "reads the extras a configuration declares" $ do
      let spec' =
            decodeDeploySpec
              $ cs
              $ Aeson.encode
              $ deploySpecJson
                [ ("domains", Aeson.toJSON ["app.example" :: Text]),
                  ("exposeSSH", Aeson.Bool True),
                  ("authorizeDeployerGithubKeys", Aeson.Bool True),
                  ("authorizedSSHKeys", Aeson.toJSON ["ssh-ed25519 AAAA alice" :: Text])
                ]
      case spec' of
        Left problem -> expectationFailure (cs problem)
        Right extras -> do
          _serverExtrasDomains extras `shouldBe` ["app.example"]
          _serverExtrasExposeSSH extras `shouldBe` True
          _serverExtrasAuthorizeDeployerGithubKeys extras `shouldBe` True
          _serverExtrasAuthorizedSSHKeys extras `shouldBe` ["ssh-ed25519 AAAA alice"]

    it "reads a configuration that asks for nothing in particular" $ do
      -- Whether this configuration is deployed at all is garnix.yaml's
      -- business. A spec that sets no extras is the ordinary case, not an
      -- error.
      case decodeDeploySpec (cs (Aeson.encode (deploySpecJson []))) of
        Left problem -> expectationFailure (cs problem)
        Right extras -> extras `shouldBe` defaultServerExtras

    it "splits declared ports by how they are reached" $ do
      let json =
            deploySpecJson
              [ ( "ports",
                  Aeson.toJSON
                    [ Aeson.object
                        [ "name" Aeson..= ("api" :: Text),
                          "port" Aeson..= (8080 :: Int),
                          "type" Aeson..= ("http" :: Text)
                        ],
                      Aeson.object
                        [ "name" Aeson..= ("db" :: Text),
                          "port" Aeson..= (5432 :: Int),
                          "type" Aeson..= ("tcp" :: Text)
                        ]
                    ]
                )
              ]
      case decodeDeploySpec (cs (Aeson.encode json)) of
        Left problem -> expectationFailure (cs problem)
        Right extras ->
          map (_serverPortName &&& _serverPortType) (_serverExtrasPorts extras)
            `shouldBe` [("api", HttpPort), ("db", TcpPort)]

    it "refuses a spec it does not understand rather than guessing" $ do
      let json =
            deploySpecJson
              [ ( "ports",
                  Aeson.toJSON
                    [ Aeson.object
                        [ "name" Aeson..= ("api" :: Text),
                          "port" Aeson..= (8080 :: Int),
                          "type" Aeson..= ("carrier-pigeon" :: Text)
                        ]
                    ]
                )
              ]
      decodeDeploySpec (cs (Aeson.encode json)) `shouldSatisfy` isLeft

  describe "assembleDeployPlan" $ do
    it "wants the configuration the yaml names for this branch" $ do
      let plan = assembleDeployPlan mainDeploy [declOnBranch "web" "main"] [] [built "web"]
      packagesToSpinUp plan `shouldBe` ["web"]
      serverIdsToSpinDown plan `shouldBe` []
      redeployPairs plan `shouldBe` []

    it "leaves alone a nixosConfiguration the yaml never names" $ do
      -- The ordinary case: most repos build NixOS configurations they have no
      -- intention of hosting. Importing the guest profile does not make one a
      -- server; being listed under `servers:` does.
      let plan = assembleDeployPlan mainDeploy [] [] [built "laptop"]
      packagesToSpinUp plan `shouldBe` []

    it "deploys a declared server whose extras could not be read" $ do
      -- A configuration that sets no `garnix.server` at all still gets
      -- deployed if the yaml asks for it; it just gets no ports or domains.
      let plan =
            assembleDeployPlan mainDeploy [declOnBranch "web" "main"] [] [(buildFor "web", Nothing)]
      packagesToSpinUp plan `shouldBe` ["web"]
      map _serverToSpinUpDomains (_deployPlanToSpinUp plan) `shouldBe` [[]]

    it "ignores a declaration pinned to a different branch" $ do
      let plan = assembleDeployPlan mainDeploy [declOnBranch "web" "staging"] [] [built "web"]
      packagesToSpinUp plan `shouldBe` []

    it "does not deploy a branch server for a pull request" $ do
      let plan = assembleDeployPlan prDeploy [declOnBranch "web" "main"] [] [built "web"]
      packagesToSpinUp plan `shouldBe` []

    it "does not deploy a pull-request server for a branch push" $ do
      let plan = assembleDeployPlan mainDeploy [declOnPullRequest "web"] [] [built "web"]
      packagesToSpinUp plan `shouldBe` []

    it "deploys a pull-request server whatever branch the PR is on" $ do
      let plan = assembleDeployPlan prDeploy [declOnPullRequest "web"] [] [built "web"]
      packagesToSpinUp plan `shouldBe` ["web"]

    it "gives a pull-request server the machine size its entry asked for" $ do
      let plan = assembleDeployPlan prDeploy [declOnPullRequestTier "web" "i2x4"] [] [built "web"]
      map _serverToSpinUpTier (_deployPlanToSpinUp plan) `shouldBe` [ServerTier "i2x4"]

    it "never treats a pull-request deploy as primary" $ do
      -- A PR deploy answering on <repo>.<owner> would take the production
      -- hostname away from the branch deploy. The yaml cannot ask for it: a
      -- pull-request declaration carries no isPrimary at all.
      let plan = assembleDeployPlan prDeploy [declOnPullRequest "web"] [] [built "web"]
      map _serverToSpinUpIsPrimary (_deployPlanToSpinUp plan) `shouldBe` [False]

    it "asks for one guest when the same package was built for two systems" $ do
      -- Both builds carry the same package name; provisioning both would put
      -- two guests behind one hostname.
      let plan =
            assembleDeployPlan
              mainDeploy
              [declOnBranch "web" "main"]
              []
              [built "web", built "web"]
      packagesToSpinUp plan `shouldBe` ["web"]

    it "ignores a declaration whose configuration this commit did not build" $ do
      -- getDeployPlan reports this to the user separately; the plan itself
      -- must not invent a server out of a build it does not have.
      let plan = assembleDeployPlan mainDeploy [declOnBranch "web" "main"] [] []
      packagesToSpinUp plan `shouldBe` []

    it "tears down a running server the commit no longer wants" $ do
      let plan = assembleDeployPlan mainDeploy [] [runningServer 1 Nothing] []
      serverIdsToSpinDown plan `shouldBe` [serverIdOf 1]
      packagesToSpinUp plan `shouldBe` []

    it "replaces a disposable server rather than redeploying onto it" $ do
      -- With no persistence name the guest is throwaway: the new generation
      -- comes up first, and the old one is torn down after it is ready.
      let plan =
            assembleDeployPlan
              mainDeploy
              [declOnBranch "web" "main"]
              [runningServer 1 Nothing]
              [built "web"]
      packagesToSpinUp plan `shouldBe` ["web"]
      serverIdsToSpinDown plan `shouldBe` [serverIdOf 1]
      redeployPairs plan `shouldBe` []

    it "redeploys onto the running guest that shares its persistence name" $ do
      -- This is the whole point of persistence: the guest keeps its disk
      -- across a push instead of being recreated empty.
      let plan =
            assembleDeployPlan
              mainDeploy
              [declOnBranch "web" "main"]
              [runningServer 1 (Just "db")]
              [builtPersistent "web" (Just "db")]
      redeployPairs plan `shouldBe` [(serverIdOf 1, "web")]
      packagesToSpinUp plan `shouldBe` []
      serverIdsToSpinDown plan `shouldBe` []

    it "replaces a persistent guest when the persistence name changes" $ do
      let plan =
            assembleDeployPlan
              mainDeploy
              [declOnBranch "web" "main"]
              [runningServer 1 (Just "old")]
              [builtPersistent "web" (Just "new")]
      packagesToSpinUp plan `shouldBe` ["web"]
      serverIdsToSpinDown plan `shouldBe` [serverIdOf 1]
      redeployPairs plan `shouldBe` []

    it "does not redeploy a persistent build onto a disposable guest" $ do
      -- The running guest has no disk to keep, so reusing it would silently
      -- give the repo a "persistent" server that is not.
      let plan =
            assembleDeployPlan
              mainDeploy
              [declOnBranch "web" "main"]
              [runningServer 1 Nothing]
              [builtPersistent "web" (Just "db")]
      redeployPairs plan `shouldBe` []
      packagesToSpinUp plan `shouldBe` ["web"]
      serverIdsToSpinDown plan `shouldBe` [serverIdOf 1]

    it "keeps one guest and tears down another in the same rollout" $ do
      let plan =
            assembleDeployPlan
              mainDeploy
              [declOnBranch "db" "main", declOnBranch "web" "main"]
              [runningServer 1 (Just "db"), runningServer 2 Nothing]
              [builtPersistent "db" (Just "db"), built "web"]
      redeployPairs plan `shouldBe` [(serverIdOf 1, "db")]
      packagesToSpinUp plan `shouldBe` ["web"]
      serverIdsToSpinDown plan `shouldBe` [serverIdOf 2]

    it "takes machine size and primacy from the yaml, and the rest from nix" $ do
      -- The split is the whole point of the contract: reading garnix.yaml
      -- tells you what a push deploys and how big it is; reading the nix
      -- tells you what the server exposes once it is up.
      let declaration =
            Yaml.ServerSection
              { Yaml._serverSectionConfiguration = PackageName "web",
                Yaml._serverSectionDeploySection =
                  Yaml.OnBranch (Branch "main") (ServerTier "i4x8") True
              }
          extras =
            defaultServerExtras
              { _serverExtrasExposeSSH = True,
                _serverExtrasDomains = ["app.example"],
                _serverExtrasPorts =
                  [ ServerPort "api" 8080 HttpPort,
                    ServerPort "db" 5432 TcpPort
                  ]
              }
          plan = assembleDeployPlan mainDeploy [declaration] [] [(buildFor "web", Just extras)]
      case _deployPlanToSpinUp plan of
        [wanted] -> do
          _serverToSpinUpTier wanted `shouldBe` ServerTier "i4x8"
          _serverToSpinUpIsPrimary wanted `shouldBe` True
          _serverToSpinUpExposeSSH wanted `shouldBe` True
          _serverToSpinUpDomains wanted `shouldBe` ["app.example"]
          -- http ports are routed by the gateway, tcp ports DNAT'd by the
          -- provisioner, so the split has to survive planning.
          _serverToSpinUpHttpPorts wanted `shouldBe` [("api", 8080)]
          _serverToSpinUpTcpPorts wanted `shouldBe` [("db", 5432)]
        other ->
          expectationFailure $ cs $ "expected exactly one server, got " <> show (length other)

  describe "matchesDeployment" $ do
    it "matches a branch declaration only against its own branch" $ do
      matchesDeployment mainDeploy (Yaml.OnBranch (Branch "main") (ServerTier "i2x4") True)
        `shouldBe` Just (ServerTier "i2x4", True)
      matchesDeployment mainDeploy (Yaml.OnBranch (Branch "staging") (ServerTier "i2x4") True)
        `shouldBe` Nothing

    it "gives a pull-request deploy the size it asked for, and no primacy" $ do
      matchesDeployment prDeploy (Yaml.OnPullRequest (ServerTier "i2x4"))
        `shouldBe` Just (ServerTier "i2x4", False)

  describe "checkTiersWithinCap" $ do
    it "accepts anything when the instance sets no cap" $ do
      capError Nothing mainDeploy [declOnBranchTier "web" "main" "i64x256"]
        `shouldReturn` Nothing

    it "refuses a branch deploy asking for more than the cap" $ do
      capError (Just "i4x8") mainDeploy [declOnBranchTier "web" "main" "i8x16"]
        `shouldReturn` Just (ServerTierExceedsInstanceCap (PackageName "web") "i8x16" "i4x8")

    it "names the entry at fault, not the first one it looked at" $ do
      capError
        (Just "i4x8")
        mainDeploy
        [declOnBranch "web" "main", declOnBranchTier "worker" "main" "i8x16"]
        `shouldReturn` Just (ServerTierExceedsInstanceCap (PackageName "worker") "i8x16" "i4x8")

    it "ignores an entry this deployment does not match" $ do
      capError (Just "i4x8") mainDeploy [declOnBranchTier "web" "staging" "i8x16"]
        `shouldReturn` Nothing

    it "lets a pull request deploy through when it stays under the cap" $ do
      capError (Just "i4x8") prDeploy [declOnPullRequest "web"]
        `shouldReturn` Nothing

    it "refuses a pull request deploy asking for more than the cap" $ do
      capError (Just "i4x8") prDeploy [declOnPullRequestTier "web" "i8x16"]
        `shouldReturn` Just (ServerTierExceedsInstanceCap (PackageName "web") "i8x16" "i4x8")

    it "accepts a tier that meets the cap exactly" $ do
      capError (Just "i4x8") mainDeploy [declOnBranchTier "web" "main" "i4x8"]
        `shouldReturn` Nothing

  describe "checkDeployPlan" $ do
    it "accepts a plan whose every name is a legal DNS label" $ do
      planError mainDeploy (planFor mainDeploy [declOnBranch "web" "main"] [built "web"])
        `shouldReturn` Nothing

    it "refuses a package name that cannot be a subdomain" $ do
      planError mainDeploy (planFor mainDeploy [declOnBranch "my_server" "main"] [built "my_server"])
        `shouldReturn` Just (NameIsNotValidSubdomain PackageNameSubdomain "my_server")

    it "refuses a branch name that cannot be a subdomain" $ do
      let slashy = BranchDeployment (Branch "feature/x")
      planError slashy (planFor slashy [declOnBranch "web" "feature/x"] [built "web"])
        `shouldReturn` Just (NameIsNotValidSubdomain BranchSubdomain "feature/x")

    it "refuses a persistence name that cannot be a subdomain" $ do
      planError
        mainDeploy
        (planFor mainDeploy [declOnBranch "web" "main"] [builtPersistent "web" (Just "my_db")])
        `shouldReturn` Just (NameIsNotValidSubdomain PersistenceNameSubdomain "my_db")

    it "refuses a repo owner that cannot be a subdomain" $ do
      planErrorFor "bad_owner" "widgets" mainDeploy (planFor mainDeploy [declOnBranch "web" "main"] [built "web"])
        `shouldReturn` Just (NameIsNotValidSubdomain RepoOwnerSubdomain "bad_owner")

    it "refuses a repo name that cannot be a subdomain" $ do
      planErrorFor "acme" "my_widgets" mainDeploy (planFor mainDeploy [declOnBranch "web" "main"] [built "web"])
        `shouldReturn` Just (NameIsNotValidSubdomain RepoNameSubdomain "my_widgets")

    it "does not check the branch of a pull-request deploy" $ do
      -- A PR deploy is addressed as pull-<n>, so a branch name that is not a
      -- legal label never reaches a hostname.
      planError prDeploy (planFor prDeploy [declOnPullRequest "web"] [built "web"])
        `shouldReturn` Nothing

    it "reports a failed build instead of deploying it" $ do
      let failed = (buildFor "web") {_buildStatus = Just Failure}
      planError mainDeploy (planFor mainDeploy [declOnBranch "web" "main"] [(failed, Nothing)])
        `shouldReturn` Just (OtherError "web failed")

    it "reports a build that timed out" $ do
      let timedOut = (buildFor "web") {_buildStatus = Just Timeout}
      planError mainDeploy (planFor mainDeploy [declOnBranch "web" "main"] [(timedOut, Nothing)])
        `shouldReturn` Just (OtherError "web timed out")

    it "checks the builds it is redeploying, not only the new ones" $ do
      -- A redeploy switches a running guest to a new closure; a failed build
      -- must not be activated onto it either.
      let failed = (persistentBuildFor "web" (Just "db")) {_buildStatus = Just Failure}
          plan =
            assembleDeployPlan
              mainDeploy
              [declOnBranch "web" "main"]
              [runningServer 1 (Just "db")]
              [(failed, Nothing)]
      redeployPairs plan `shouldBe` [(serverIdOf 1, "web")]
      planError mainDeploy plan `shouldReturn` Just (OtherError "web failed")

    it "says nothing about a repo that deploys no servers at all" $ do
      -- getDeployPlan skips the check entirely for an empty plan; this pins
      -- down that an empty plan is not itself an error.
      planError mainDeploy (planFor mainDeploy [] []) `shouldReturn` Nothing

-- | A commit on @\<owner\>\/\<repo\>@; only the repo identity matters here.
commitInfoFor :: GhLogin -> Text -> CommitInfo
commitInfoFor owner repo =
  defaultCommitInfo
    & repoInfo . ghRepoOwner .~ GhRepoOwner owner
    & repoInfo . ghRepoName .~ GhRepoName repo

-- | A successful @nixosConfiguration@ build of one package, with an optional
-- persistence name. Every field is concrete rather than a placeholder:
-- 'assembleDeployPlan' compares whole builds for equality, so an undefined
-- field would be forced.
buildFor :: Text -> Build
buildFor package' = persistentBuildFor package' Nothing

persistentBuildFor :: Text -> Maybe Text -> Build
persistentBuildFor package' persistence =
  Build
    { _buildId = BuildId (review hashIdInt 1),
      _buildRepoUser = "owner",
      _buildRepoName = "repo",
      _buildPrFromFork = Nothing,
      _buildBranch = Just "main",
      _buildRepoIsPublic = RepoIsPublic False,
      _buildGitCommit = CommitHash "aaaaaaaa",
      _buildPackage = PackageName package',
      _buildPackageType = TypeNixosConfiguration,
      _buildSystem = NoSystem,
      _buildReqUser = "owner",
      _buildStatus = Just Success,
      _buildStartTime = testTime,
      _buildEndTime = Just testTime,
      _buildDrvPath = Just "/nix/store/whatever.drv",
      _buildOutputPaths = Nothing,
      _buildGithubRunId = Nothing,
      _buildPersistenceName = persistence,
      _buildWantsIncrementalism = False,
      _buildEvalHost = Nothing,
      _buildUploadedToCache = Just True,
      _buildAlreadyBuilt = Just False
    }

testTime :: UTCTime
testTime = UTCTime (fromGregorian 2026 1 1) 0

-- | A guest already running for this repo, with an optional persistence name.
runningServer :: Int -> Maybe Text -> ServerInfo
runningServer n persistence =
  ServerInfo
    { _serverInfoId = ServerId (review hashIdInt n),
      _serverInfoProvider = MicroVM,
      _serverInfoInstanceId = Just (InstanceId ("guest-" <> cs (show n))),
      _serverInfoAddress = ServerAddress (Just "10.111.0.7") Nothing,
      _serverInfoCreatedAt = testTime,
      _serverInfoEndedAt = Nothing,
      _serverInfoConfigurationBuildId = BuildId (review hashIdInt n),
      _serverInfoPullRequest = Nothing,
      _serverInfoReadyAt = Just testTime,
      _serverInfoTier = ServerTier "i1x2",
      _serverInfoIsPrimary = False,
      _serverInfoPersistenceName = persistence
    }

-- | A @servers:@ entry deploying one configuration from one branch.
declOnBranch :: Text -> Text -> Yaml.ServerSection
declOnBranch package' branch' =
  Yaml.ServerSection
    { Yaml._serverSectionConfiguration = PackageName package',
      Yaml._serverSectionDeploySection =
        Yaml.OnBranch (Branch branch') (ServerTier "i1x2") False
    }

-- | A @servers:@ entry deploying one configuration per open pull request.
declOnPullRequest :: Text -> Yaml.ServerSection
declOnPullRequest package' = declOnPullRequestTier package' "i1x2"

-- | The same, asking for a machine size of its own.
declOnPullRequestTier :: Text -> Text -> Yaml.ServerSection
declOnPullRequestTier package' tier =
  Yaml.ServerSection
    { Yaml._serverSectionConfiguration = PackageName package',
      Yaml._serverSectionDeploySection = Yaml.OnPullRequest (ServerTier tier)
    }

-- | A finished build of one configuration, declaring no extras of its own.
built :: Text -> (Build, Maybe ServerExtras)
built package' = (buildFor package', Just defaultServerExtras)

builtPersistent :: Text -> Maybe Text -> (Build, Maybe ServerExtras)
builtPersistent package' persistence =
  (persistentBuildFor package' persistence, Just defaultServerExtras)

packagesToSpinUp :: DeployPlan -> [Text]
packagesToSpinUp plan =
  [ getPackageName (_serverToSpinUpBuild wanted ^. package)
    | wanted <- _deployPlanToSpinUp plan
  ]

serverIdsToSpinDown :: DeployPlan -> [ServerId]
serverIdsToSpinDown = map _serverInfoId . _deployPlanToSpinDown

-- | (server kept, package redeployed onto it)
redeployPairs :: DeployPlan -> [(ServerId, Text)]
redeployPairs plan =
  [ (_serverInfoId server, getPackageName (_serverToSpinUpBuild wanted ^. package))
    | (server, wanted) <- _deployPlanToRedeploy plan
  ]

-- | Run 'checkTiersWithinCap' under an instance cap, and report the error it
-- raised, if any.
capError :: Maybe Text -> DeploymentType -> [Yaml.ServerSection] -> IO (Maybe Error)
capError cap deploymentType sections =
  runTestM
    $ local (\env -> env {hostingBudget = budget})
    $ either (Just . err) (const Nothing)
    <$> try (checkTiersWithinCap deploymentType sections)
  where
    budget = HostingBudget Nothing Nothing (ServerTier <$> cap) Nothing

declOnBranchTier :: Text -> Text -> Text -> Yaml.ServerSection
declOnBranchTier package' branch' tier =
  Yaml.ServerSection
    { Yaml._serverSectionConfiguration = PackageName package',
      Yaml._serverSectionDeploySection =
        Yaml.OnBranch (Branch branch') (ServerTier tier) False
    }

-- | Run 'checkDeployPlan' and report the error it raised, if any.
planError :: DeploymentType -> DeployPlan -> IO (Maybe Error)
planError = planErrorFor "acme" "widgets"

planErrorFor :: GhLogin -> Text -> DeploymentType -> DeployPlan -> IO (Maybe Error)
planErrorFor owner repo deploymentType plan =
  runTestM
    $ either (Just . err) (const Nothing)
    <$> try (checkDeployPlan (commitInfoFor owner repo ^. repoInfo) deploymentType plan)

-- | The two deployments every planning test is phrased against.
mainDeploy :: DeploymentType
mainDeploy = BranchDeployment (Branch "main")

prDeploy :: DeploymentType
prDeploy = GhPrDeployment (GhPullRequestId 42)

serverIdOf :: Int -> ServerId
serverIdOf = ServerId . review hashIdInt

-- | A plan for a repo with nothing running yet.
planFor :: DeploymentType -> [Yaml.ServerSection] -> [(Build, Maybe ServerExtras)] -> DeployPlan
planFor deploymentType declared = assembleDeployPlan deploymentType declared []

-- | The shape guest-profile.nix's @deploySpec@ renders, with the given fields
-- overridden. Written out rather than reusing production code so that a change
-- to either side has to be reconciled deliberately.
deploySpecJson :: [(Text, Aeson.Value)] -> Aeson.Value
deploySpecJson overrides =
  Aeson.object
    $ [ (Aeson.Key.fromText name, value)
        | (name, value) <-
            [ ("domains", Aeson.toJSON ([] :: [Text])),
              ("exposeSSH", Aeson.Bool False),
              ("authorizeDeployerGithubKeys", Aeson.Bool False),
              ("authorizedSSHKeys", Aeson.toJSON ([] :: [Text])),
              ("authentikDefault", Aeson.Bool False),
              ("ports", Aeson.toJSON ([] :: [Aeson.Value])),
              ("applicationLog", Aeson.Null),
              ("backups", Aeson.Null),
              ("persistence", Aeson.object ["enable" Aeson..= False, "name" Aeson..= Aeson.Null])
            ],
          name `notElem` map fst overrides
      ]
    <> [(Aeson.Key.fromText name, value) | (name, value) <- overrides]
