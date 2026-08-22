module Garnix.ModulesSpec where

import Control.Lens
import Data.Row ((.+), (.-), (.==))
import Data.String.Interpolate (i)
import Garnix.DB.ModuleValues qualified as DB
import Garnix.Modules qualified as Modules
import Garnix.Modules.Schema qualified as Schema
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.Reporter
import Garnix.Types hiding (context)
import Garnix.YamlConfig
import System.IO.Temp
import Test.Hspec

spec :: Spec
spec = inM . beforeM_ truncateDBM . aroundM_ suppressLogsWhenPassing $ describe "ModulesSpec" $ do
  it "skips publishing if the setting is not enabled" $ do
    Modules.publish mempty def testCommitInfo
    modules <- DB.getAvailableModules
    modules `shouldBeM` []

  it "skips publishing if the org is not garnix" $ do
    Modules.publish mempty def defaultCommitInfo
    modules <- DB.getAvailableModules
    modules `shouldBeM` []

  it "creates a failed check if the org is not garnix" $ do
    let wrongOrgCommitInfo = testCommitInfo & repoInfo . repoId . repoUser .~ "not-garnix-io"
    result <- withTestReporter_ (\reporter -> void $ try $ Modules.publish reporter enabled wrongOrgCommitInfo)
    let (Just testReport) = result ^? ix "Garnix module publish"
    testReport ^. #success `shouldBeM` Just False
    testReport ^. #logs `shouldBeM` "Publishing modules is not enabled for not-garnix-io."

  it "skips publishing if the branch is not the default branch" $ do
    let wrongBranchCommitInfo = testCommitInfo & branch ?~ "not-main"
    Modules.publish mempty enabled wrongBranchCommitInfo
    modules <- DB.getAvailableModules
    modules `shouldBeM` []

  it "inserts a single module" $ withFakeWorkingDir $ do
    Modules.publish mempty enabled testCommitInfo
    modules <- DB.getAvailableModules
    ((.- #schema) <$> modules)
      `shouldBeM` [ (#description .== Just "Test description")
                      .+ (#git_commit .== CommitHash "aaaaaaaa")
                      .+ (#name .== "Test")
                      .+ (#repo_name .== "test-module")
                      .+ (#repo_user .== "garnix-io")
                  ]

  it "inserts a newer version of a module" $ withFakeWorkingDir $ do
    Modules.publish mempty enabled testCommitInfo
    let secondCommitInfo = testCommitInfo & commit .~ CommitHash "bbbbbbbb"
    Modules.publish mempty enabled secondCommitInfo
    modules <- DB.getAvailableModules
    ((.- #schema) <$> modules)
      `shouldBeM` [ (#description .== Just "Test description")
                      .+ (#git_commit .== CommitHash "bbbbbbbb")
                      .+ (#name .== "Test")
                      .+ (#repo_name .== "test-module")
                      .+ (#repo_user .== "garnix-io")
                  ]

  it "publishes even if the name has different casing" $ withFakeWorkingDir $ do
    Modules.publish mempty enabled testCommitInfo
    let secondCommitInfo =
          testCommitInfo
            & commit .~ CommitHash "bbbbbbbb"
            & repoInfo . repoId . repoName .~ "tEsT-module"
    Modules.publish mempty enabled secondCommitInfo
    modules <- DB.getAvailableModules
    ((.- #schema) <$> modules)
      `shouldBeM` [ (#description .== Just "Test description")
                      .+ (#git_commit .== CommitHash "bbbbbbbb")
                      .+ (#name .== "TEsT")
                      .+ (#repo_name .== "tEsT-module")
                      .+ (#repo_user .== "garnix-io")
                  ]
  context "module names are stored with pascal casing" $ do
    let tests =
          [ ("kebab-case", "some-name-here", "SomeNameHere"),
            ("camelCase", "someNameHere", "SomeNameHere"),
            ("snake_Case", "some_Name_Here", "SomeNameHere"),
            ("quiet_snake_case", "some_name_here", "SomeNameHere"),
            ("SCREAMING_CASE", "SOME_NAME_HERE", "SomENamEHere"),
            ("mixed-case_versionSample", "mixed-case_versionSample", "MixedCaseVersionSample"),
            -- Our own modules (some with special cases):
            ("haskell module", "haskell-module", "Haskell"),
            ("nodejs-module", "nodejs-module", "NodeJS"),
            ("postgresql-module", "postgresql-module", "PostgreSQL"),
            ("rust-module", "rust-module", "Rust"),
            ("user-module", "user-module", "User"),
            ("rss-bridge-module", "rss-bridge-module", "RSS-Bridge")
          ]

    forM_ tests $ \(name, repoName, expectation) -> do
      it ("correctly converts " <> name) $ withFakeWorkingDir $ do
        let commitInfo = testCommitInfo & repoInfo . repoId . Garnix.Types.repoName .~ repoName
        Modules.publish mempty enabled commitInfo
        modules <- DB.getAvailableModules
        ((.- #schema) <$> modules)
          `shouldBeM` [ (#description .== Just "Test description")
                          .+ (#git_commit .== CommitHash "aaaaaaaa")
                          .+ (#name .== expectation)
                          .+ (#repo_name .== repoName)
                          .+ (#repo_user .== "garnix-io")
                      ]
        truncateDBM

testCommitInfo :: CommitInfo
testCommitInfo =
  defaultCommitInfo
    & repoInfo . repoId . repoUser .~ "garnix-io"
    & repoInfo . repoId . repoName .~ "test-module"
    & Garnix.Types.reqUser .~ ForgeLogin githubForge "garnix-io"
    & branch ?~ "main"

enabled :: GarnixConfig
enabled = def & moduleSection .~ ModuleSection True

withFakeWorkingDir :: M a -> M a
withFakeWorkingDir action = do
  liftBaseOp (withSystemTempDirectory "garnix-test") $ \dir -> do
    liftIO
      $ writeFile
        (dir </> "flake.nix")
        [i|
          {
            description = "Test description";
            outputs = { ... } :
              let
                lib = #{Schema.nixpkgsLib};
              in
              {
                garnixModules.default = {
                  options.foo = lib.mkOption { type = lib.types.str; };
                };
              };
          }
        |]
    local (#workingDir .~ dir) action
