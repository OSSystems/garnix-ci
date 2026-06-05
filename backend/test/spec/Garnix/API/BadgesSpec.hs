module Garnix.API.BadgesSpec (spec) where

import Control.Lens
import Data.String.Interpolate
import Garnix.API.Badges
import Garnix.Monad
import Garnix.Monad.Async
import Garnix.Orchestrator
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.Deprecated qualified as Deprecated
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad (aroundM_, beforeM_, inM, shouldBeM, suppressLogsWhenPassing)
import Garnix.Types hiding (context)
import Test.Hspec

spec :: Spec
spec = do
  around_ Deprecated.addTestSecrets $ inM $ beforeM_ truncateDBM $ aroundM_ suppressLogsWhenPassing $ do
    describe "getBadgeStatus" $ do
      it "should say 'build status unknown' when there are no builds" $ do
        badge <- badgesAPI repositoryLogin repositoryName repositoryBranch
        badgeMessage badge `shouldBeM` "build status unknown"

      it "should say number of build succeeded if everything succeeds" $ do
        badge <- do
          let commitInfo = defaultCommitInfo & repoPublicity .~ RepoIsPublic True
          build successFlake commitInfo
          badgesAPI repositoryLogin repositoryName repositoryBranch
        badgeMessage badge `shouldBeM` "2 builds succeeded"

      it "should say 'all builds failed' if all builds fail" $ do
        badge <- do
          let commitInfo = defaultCommitInfo & repoPublicity .~ RepoIsPublic True
          build emptyFlake commitInfo
          badgesAPI repositoryLogin repositoryName repositoryBranch
        badgeMessage badge `shouldBeM` "all builds failed"

      it "should say '<n> builds succeeded out of <total>' if some fail" $ do
        badge <- do
          let commitInfo = defaultCommitInfo & repoPublicity .~ RepoIsPublic True
          build failureFlake commitInfo
          badgesAPI repositoryLogin repositoryName repositoryBranch
        badgeMessage badge `shouldBeM` "1 build succeeded out of 2"

      it "should show the most recent build results" $ do
        let commitInfo1 =
              defaultCommitInfo
                & repoPublicity .~ RepoIsPublic True
                & commit .~ "3"
        build successFlake commitInfo1
        badge1 <- badgesAPI repositoryLogin repositoryName repositoryBranch
        badgeMessage badge1 `shouldBeM` "2 builds succeeded"
        let commitInfo2 = commitInfo1 & commit .~ "1"
        build failureFlake commitInfo2
        badge2 <- badgesAPI repositoryLogin repositoryName repositoryBranch
        badgeMessage badge2 `shouldBeM` "1 build succeeded out of 2"
        let commitInfo3 = commitInfo2 & commit .~ "2"
        build successFlake commitInfo3
        badge3 <- badgesAPI repositoryLogin repositoryName repositoryBranch
        badgeMessage badge3 `shouldBeM` "2 builds succeeded"

      it "should say 'build status unknown' on private repos" $ do
        badge <- do
          let commitInfo = defaultCommitInfo & repoPublicity .~ RepoIsPublic False
          build successFlake commitInfo
          badgesAPI repositoryLogin repositoryName repositoryBranch
        badgeMessage badge `shouldBeM` "build status unknown"

      it "should say 'build status unknown' when there are no commits in the given branch" $ do
        badge <- do
          let commitInfo = defaultCommitInfo & repoPublicity .~ RepoIsPublic True
          build successFlake commitInfo
          badgesAPI repositoryLogin repositoryName Nothing
        badgeMessage badge `shouldBeM` "build status unknown"

      it "ignores later commits on other branches" $ do
        badge <- do
          let commitInfoA =
                defaultCommitInfo
                  & repoPublicity .~ RepoIsPublic True
                  & branch ?~ "a"
          build successFlake commitInfoA
          let commitInfoB =
                defaultCommitInfo
                  & repoPublicity .~ RepoIsPublic True
                  & branch ?~ "b"
          build emptyFlake commitInfoB
          badgesAPI repositoryLogin repositoryName (Just "a")
        badgeMessage badge `shouldBeM` "2 builds succeeded"

      context "pending" $ do
        let withBuild f = testBuild $ \build ->
              f $ build
                & repoUser .~ repositoryLogin
                & repoName .~ repositoryName
                & branch .~ repositoryBranch
                & package .~ "Build starting"
                & status .~ Nothing
        it "should say builds in progress if no builds are complete" $ do
          void $ withBuild identity
          badge <- badgesAPI repositoryLogin repositoryName repositoryBranch
          badgeMessage badge `shouldBeM` "build in progress"

        it "should report 1 build completed and 1 in progress" $ do
          void $ withBuild identity
          void $ withBuild $ \b -> b & status ?~ Success
          badge <- badgesAPI repositoryLogin repositoryName repositoryBranch
          badgeMessage badge `shouldBeM` "1 build succeeded, 1 build in progress"

        it "should report 1 build completed and multiple in progress" $ do
          void $ withBuild identity
          void $ withBuild identity
          void $ withBuild $ \b -> b & status ?~ Success
          badge <- badgesAPI repositoryLogin repositoryName repositoryBranch
          badgeMessage badge `shouldBeM` "1 build succeeded, 2 builds in progress"

        it "should report multiple completed and one in progress" $ do
          void $ withBuild identity
          void $ withBuild $ \b -> b & status ?~ Success
          void $ withBuild $ \b -> b & status ?~ Success
          badge <- badgesAPI repositoryLogin repositoryName repositoryBranch
          badgeMessage badge `shouldBeM` "2 builds succeeded, 1 build in progress"

        it "should report multiple completed and multiple in progress" $ do
          void $ withBuild identity
          void $ withBuild identity
          void $ withBuild $ \b -> b & status ?~ Success
          void $ withBuild $ \b -> b & status ?~ Success
          badge <- badgesAPI repositoryLogin repositoryName repositoryBranch
          badgeMessage badge `shouldBeM` "2 builds succeeded, 2 builds in progress"

        it "should report success, failure, and pending" $ do
          void $ withBuild identity
          void $ withBuild identity
          void $ withBuild $ \b -> b & status ?~ Success
          void $ withBuild $ \b -> b & status ?~ Success
          void $ withBuild $ \b -> b & status ?~ Failure
          void $ withBuild $ \b -> b & status ?~ Failure
          badge <- badgesAPI repositoryLogin repositoryName repositoryBranch
          badgeMessage badge `shouldBeM` "2 succeeded, 2 failed, and 2 in progress"

repositoryLogin :: GhRepoOwner
repositoryLogin = GhRepoOwner (GhLogin "owner")

repositoryName :: GhRepoName
repositoryName = GhRepoName "repo"

repositoryBranch :: Maybe Branch
repositoryBranch = Just (Branch "branch")

emptyFlake :: Text
emptyFlake = "{}"

successFlake :: Text
successFlake =
  cs
    [i|{ outputs = {self}:
      { packages.x86_64-linux.foo = derivation
        { name = "foo"
        ; builder = "/bin/sh"
        ; args = ["-c" "echo hi > $out"]
        ; system = "x86_64-linux"
        ;
        };
      };
    }|]

failureFlake :: Text
failureFlake =
  cs
    [i|{ outputs = {self}:
      { packages.x86_64-linux.foo = derivation
        { name = "foo"
        ; builder = "/bin/sh"
        ; args = ["-c" "fail"]
        ; system = "x86_64-linux"
        ;
        };
      };
    }|]

build :: Text -> CommitInfo -> M ()
build flake commitInfo = do
  GH.withFakeGithubInterface $ \ghState -> do
    GH.withLocalRepo ghState "owner" "repo" identity commitInfo (GH.simpleSetup flake) $ \commitInfo -> do
      void $ try $ resolve =<< handleCommit mempty True commitInfo
