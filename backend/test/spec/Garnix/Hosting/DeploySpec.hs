module Garnix.Hosting.DeploySpec (spec) where

import Control.Arrow ((&&&))
import Control.Lens
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Text qualified as T
import Garnix.Build.Package (decodeDeploySpec)
import Garnix.Hosting.Deploy
import Garnix.Hosting.Types
import Garnix.Prelude
import Garnix.TestHelpers (defaultCommitInfo)
import Garnix.Types
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

-- | A commit on @\<owner\>\/\<repo\>@; only the repo identity matters here.
commitInfoFor :: GhLogin -> Text -> CommitInfo
commitInfoFor owner repo =
  defaultCommitInfo
    & repoInfo . ghRepoOwner .~ GhRepoOwner owner
    & repoInfo . ghRepoName .~ GhRepoName repo

-- | A build of one package. Only the package name is read here; the rest are
-- left undefined so a test that starts depending on one fails loudly rather
-- than quietly asserting against a placeholder.
buildFor :: Text -> Build
buildFor package' =
  Build
    { _buildId = undefined,
      _buildRepoUser = undefined,
      _buildRepoName = undefined,
      _buildPrFromFork = undefined,
      _buildBranch = undefined,
      _buildRepoIsPublic = undefined,
      _buildGitCommit = undefined,
      _buildPackage = PackageName package',
      _buildPackageType = TypeNixosConfiguration,
      _buildSystem = undefined,
      _buildReqUser = undefined,
      _buildStatus = Just Success,
      _buildStartTime = undefined,
      _buildEndTime = undefined,
      _buildDrvPath = undefined,
      _buildOutputPaths = undefined,
      _buildGithubRunId = undefined,
      _buildPersistenceName = Nothing,
      _buildWantsIncrementalism = undefined,
      _buildEvalHost = undefined,
      _buildUploadedToCache = undefined,
      _buildAlreadyBuilt = undefined
    }

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
