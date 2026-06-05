module Garnix.YamlConfigSpec (spec) where

import Data.String.Interpolate
import Garnix.Build.Checkout (remoteWithConfig, runWithCheckout)
import Garnix.Prelude
import Garnix.TestHelpers (defaultCommitInfo, fromSingleton)
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.YamlConfig
import Test.Hspec

spec :: Spec
spec = do
  describe "the config" $ do
    let defaultConfig =
          cs
            [i|
              builds:
                - include:
                    - "*.x86_64-linux.*"
                    - "defaultPackage.x86_64-linux"
                    - "devShell.x86_64-linux"
                    - "homeConfigurations.*"
                    - "darwinConfigurations.*"
                    - "nixosConfigurations.*"
                  exclude: []

              incrementalizeBuilds: false

              fodChecks: false
            |]
    it "parses the empty config to the default config"
      $ decodeConfig ""
      `shouldBe` decodeConfig defaultConfig

    it "parses the empty object to the default config"
      $ decodeConfig "{}"
      `shouldBe` decodeConfig defaultConfig

    describe "build section" $ do
      let simpleConfig =
            cs
              [i|
                builds:
                  include:
                    - "*.*.*"
                    - "*.*"
                  exclude:
                    - "*.x86_64-linux.*"
              |]
      it "parses the excludes section" $ do
        let actual =
              (^. buildSections . to fromSingleton . excludeSection)
                <$> decodeConfig simpleConfig
        actual `shouldBe` Right [AttributeMatcher "*" "x86_64-linux" (Just "*")]

      it "parses the includes section" $ do
        let actual =
              (^. buildSections . to fromSingleton . includeSection)
                <$> decodeConfig simpleConfig
        actual
          `shouldBe` Right
            [ AttributeMatcher "*" "*" (Just "*"),
              AttributeMatcher "*" "*" Nothing
            ]

      it "parses home-, darwin- and nixosConfigurations" $ do
        let config =
              cs
                [i|
                  builds:
                    include:
                      - homeConfigurations.*
                      - darwinConfigurations.foo
                    exclude:
                      - nixosConfigurations.*
                |]
            Right actual =
              (^. buildSections . to fromSingleton)
                <$> decodeConfig config
        (actual ^. includeSection)
          `shouldBe` [ AttributeMatcher "homeConfigurations" "*" Nothing,
                       AttributeMatcher "darwinConfigurations" "foo" Nothing
                     ]
        (actual ^. excludeSection)
          `shouldBe` [AttributeMatcher "nixosConfigurations" "*" Nothing]

      it "parses a missing exclude section to an empty list" $ do
        let config =
              cs
                [i|
                  builds:
                    include: ["*.86_64-linux.*"]
                |]
            actual =
              (^. buildSections . to fromSingleton . excludeSection)
                <$> decodeConfig config
        actual `shouldBe` Right []

      it "parses a missing include section to the default list" $ do
        let config =
              cs
                [i|
                  builds:
                    exclude:
                      - "*.x86_64-linux.*"
                |]
            actual =
              (^. buildSections . to fromSingleton . includeSection)
                <$> decodeConfig config
            defaultInclude =
              (^. buildSections . to fromSingleton . includeSection)
                <$> decodeConfig ""
        actual `shouldBe` defaultInclude

      it "parses configs with multiple 'builds' sections" $ do
        let config =
              cs
                [i|
                  builds:
                    - include:
                        - "packages.*.*"
                      exclude:
                        - "packages.x86_64-linux.*"
                      branch: feature1
                    - include:
                        - "checks.*.*"
                      exclude:
                        - "checks.aarch64-linux.*"
                      branch: feature2
                |]
            actual = (^. buildSections) <$> decodeConfig config
        actual
          `shouldBe` Right
            [ BuildSection
                { _buildSectionIncludeSection = [AttributeMatcher "packages" "*" (Just "*")],
                  _buildSectionExcludeSection = [AttributeMatcher "packages" "x86_64-linux" (Just "*")],
                  _buildSectionBranchSection = Just "feature1"
                },
              BuildSection
                { _buildSectionIncludeSection = [AttributeMatcher "checks" "*" (Just "*")],
                  _buildSectionExcludeSection = [AttributeMatcher "checks" "aarch64-linux" (Just "*")],
                  _buildSectionBranchSection = Just "feature2"
                }
            ]

    describe "incrementalizeBuilds section" $ do
      it "parses the boolean values" $ do
        let config1 =
              cs
                [i|
                  incrementalizeBuilds: true
                |]
        let config2 =
              cs
                [i|
                  incrementalizeBuilds: false
                |]
        let actual1 = (^. incrementalizeBuildsSection) <$> decodeConfig config1
        let actual2 = (^. incrementalizeBuildsSection) <$> decodeConfig config2
        actual1 `shouldBe` Right (IncrementalizeBuilds True)
        actual2 `shouldBe` Right (IncrementalizeBuilds False)

      it "parses the section" $ do
        let config =
              cs
                [i|
                  incrementalizeBuilds:
                    excludeBranches:
                      - main
                |]
        let actual = (^. incrementalizeBuildsSection) <$> decodeConfig config
        actual `shouldBe` Right (IncrementalBuildsExcludeBranches (ExcludeBranches ["main"]))

    context "actions section" $ do
      it "allows empty action sections" $ do
        let config = "actions: []"
        decodeConfig config `shouldBe` Right def

      it "parses single action" $ do
        let config =
              cs
                [i|
                  actions:
                    - on: push
                      run: free
                |]
        (_garnixConfigActions <$> decodeConfig config)
          `shouldBe` Right [Action "free" ActionTriggerPush FastStartup False]

      it "parses multiple actions" $ do
        let config =
              cs
                [i|
                  actions:
                    - on: push
                      run: free
                      sandboxType: fast-startup
                    - on: push
                      run: wild
                      sandboxType: shared-resources
                      withRepoContents: true
                |]
        (_garnixConfigActions <$> decodeConfig config)
          `shouldBe` Right
            [ Action "free" ActionTriggerPush FastStartup False,
              Action "wild" ActionTriggerPush SharedResources True
            ]

    inM . aroundM_ suppressLogsWhenPassing . context "parsing from flake.nix" $ do
      it "uses default config when there's no yaml file and no config section in flake" $ GH.withFakeGithubInterface $ \ghState -> do
        let emptyFlake =
              cs
                [i|
                  {
                    outputs = _: {};
                  }
                |]
        config <- GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup emptyFlake) $ \commitInfo ->
          runWithCheckout remoteWithConfig commitInfo pure
        config `shouldBeM` def

    context "modules section" $ do
      it "sets the publish field for the default section to false" $ do
        let config = ""
        let (Right actual) = decodeConfig config
        actual ^. moduleSection `shouldBe` ModuleSection False

      it "sets the publish field for an empty section to false" $ do
        let config = "modules: {}"
        decodeConfig config `shouldBe` Right def

      it "correctly parses when publish is set to true" $ do
        let config = "modules:\n  publish: true"
        let (Right actual) = decodeConfig config
        actual ^. moduleSection `shouldBe` ModuleSection True

      inM . aroundM_ suppressLogsWhenPassing . context "parsing from flake.nix" $ do
        it "reads module section from garnix.config" $ GH.withFakeGithubInterface $ \ghState -> do
          let flake =
                cs
                  [i|
                    {
                      outputs = _: {
                        garnix.config = {
                          modules = {
                            publish = true;
                          };
                        };
                      };
                    }
                  |]
          config <- GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo ->
            runWithCheckout remoteWithConfig commitInfo pure
          (config ^. moduleSection) `shouldBeM` ModuleSection True

    describe "fodChecks section" $ do
      it "allows enabling FOD checks" $ do
        let config =
              cs
                [i|
                  fodChecks: true
                |]
        let actual = (^. fodChecks) <$> decodeConfig config
        actual `shouldBe` Right True
