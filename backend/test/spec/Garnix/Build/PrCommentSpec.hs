module Garnix.Build.PrCommentSpec (spec) where

import Control.Lens
import Data.String.Interpolate (i)
import Garnix.Monad
import Garnix.Monad.Async (resolve)
import Garnix.Orchestrator (handleCommit)
import Garnix.Prelude
import Garnix.Reporters.GithubReporter (mkGithubReporter)
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.Types
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)
import Test.Hspec

spec :: Spec
spec = inM $ aroundM_ suppressLogsWhenPassing $ beforeM_ truncateDBM $ do
  describe "commentOnFailure" $ do
    it "comments on the pull request when a build fails and it is enabled" $ GH.withFakeGithubInterface $ \ghState -> do
      withPrRepo ghState (Just "commentOnFailure: true") failingFlake $ \commitInfo -> do
        testHandleCommit commitInfo
      comments <- GH.getPrComments ghState
      case comments of
        [(_repoInfo, prId, body)] -> do
          prId `shouldBeM` GhPullRequestId 1
          cs @Text @String body `shouldContainM` "garnix checks failed"
          cs @Text @String body `shouldContainM` "package failing"
          cs @Text @String body `shouldNotContainM` "package succeeding"
        other -> liftIO $ expectationFailure $ "expected exactly one comment, got: " <> cs @Text @String (show other)

    it "does not comment when it is not enabled" $ GH.withFakeGithubInterface $ \ghState -> do
      withPrRepo ghState Nothing failingFlake $ \commitInfo -> do
        testHandleCommit commitInfo
      (length <$> GH.getPrComments ghState) `shouldReturnM` 0

    it "does not comment when everything succeeds" $ GH.withFakeGithubInterface $ \ghState -> do
      withPrRepo ghState (Just "commentOnFailure: true") succeedingFlake $ \commitInfo -> do
        testHandleCommit commitInfo
      (length <$> GH.getPrComments ghState) `shouldReturnM` 0

    it "does not comment when the commit is not in a pull request" $ GH.withFakeGithubInterface $ \ghState -> do
      GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.setupWithConfig failingFlake (Just "commentOnFailure: true")) $ \commitInfo -> do
        testHandleCommit commitInfo
      (length <$> GH.getPrComments ghState) `shouldReturnM` 0

    it "comments only once when the same commit is handled twice" $ GH.withFakeGithubInterface $ \ghState -> do
      withPrRepo ghState (Just "commentOnFailure: true") failingFlake $ \commitInfo -> do
        testHandleCommit commitInfo
        testHandleCommit commitInfo
      comments <- GH.getPrComments ghState
      length comments `shouldBeM` 1

-- | A repo that the fake github interface reports as having an open pull
-- request containing every commit.
withPrRepo :: GH.GithubFakeState -> Maybe Text -> Text -> (CommitInfo -> M a) -> M a
withPrRepo ghState mConfig flake =
  GH.withLocalRepo
    ghState
    "owner"
    "repo"
    (#pullRequestBranch ?~ Branch "some-feature")
    defaultCommitInfo
    (GH.setupWithConfig flake mConfig)

testHandleCommit :: CommitInfo -> M ()
testHandleCommit commitInfo = do
  let reporter = mkGithubReporter (commitInfo ^. repoInfo) (commitInfo ^. commit)
  resolve =<< handleCommit reporter True commitInfo

random :: Int
random = unsafePerformIO randomIO
{-# NOINLINE random #-}

succeedingFlake :: Text
succeedingFlake =
  cs
    [i|
      { outputs = { self }: {
          packages.x86_64-linux.succeeding = derivation {
            name = "succeeding";
            builder = "/bin/sh";
            system = "x86_64-linux";
            args = [ "-c" ''
              echo "succeeding #{random}" > $out
            ''];
          };
        };
      }
    |]

failingFlake :: Text
failingFlake =
  cs
    [i|
      { outputs = { self }: {
          packages.x86_64-linux.succeeding = derivation {
            name = "succeeding";
            builder = "/bin/sh";
            system = "x86_64-linux";
            args = [ "-c" ''
              echo "succeeding #{random}" > $out
            ''];
          };
          packages.x86_64-linux.failing = derivation {
            name = "failing";
            builder = "/bin/sh";
            system = "x86_64-linux";
            args = [ "-c" ''
              echo "failing #{random}"
            ''];
          };
        };
      }
    |]
