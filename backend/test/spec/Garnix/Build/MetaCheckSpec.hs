module Garnix.Build.MetaCheckSpec (spec) where

import Control.Lens
import Data.Maybe (mapMaybe)
import Garnix.Build.MetaCheck qualified as MetaCheck
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.Reporters.GithubReporter (mkGithubReporter)
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.Types
import Test.Hspec

spec :: Spec
spec = inM $ aroundM_ suppressLogsWhenPassing $ beforeM_ truncateDBM $ do
  describe "two overlapping runs of the same commit" $ do
    it "takes the verdict of the newer run when the older one fails first" $ GH.withFakeGithubInterface $ \ghState -> do
      GH.mkRepo ghState "owner" "repo" (#pullRequestBranch ?~ Branch "some-feature")
      older <- MetaCheck.newReport reporter defaultCommitInfo
      newer <- MetaCheck.newReport reporter defaultCommitInfo
      MetaCheck.updateFail MetaCheck.CommentOnFailure defaultCommitInfo older Nothing
      MetaCheck.updateSuccess defaultCommitInfo newer

      metaCheckVerdict `shouldReturnM` Just CheckSuccess
      metaCheckRunStatuses ghState
        `shouldReturnM` [RunReportStatusCancelled, RunReportStatusSuccess]
      (length <$> GH.getPrComments ghState) `shouldReturnM` 0

    it "closes the older run with the newer run's verdict when the newer one finishes first" $ GH.withFakeGithubInterface $ \ghState -> do
      GH.mkRepo ghState "owner" "repo" identity
      older <- MetaCheck.newReport reporter defaultCommitInfo
      newer <- MetaCheck.newReport reporter defaultCommitInfo
      MetaCheck.updateSuccess defaultCommitInfo newer
      MetaCheck.updateFail MetaCheck.NoComment defaultCommitInfo older Nothing

      metaCheckVerdict `shouldReturnM` Just CheckSuccess
      metaCheckRunStatuses ghState
        `shouldReturnM` [RunReportStatusSuccess, RunReportStatusSuccess]

    it "still lets a single run fail the meta check" $ GH.withFakeGithubInterface $ \ghState -> do
      GH.mkRepo ghState "owner" "repo" identity
      run <- MetaCheck.newReport reporter defaultCommitInfo
      MetaCheck.updateFail MetaCheck.NoComment defaultCommitInfo run Nothing

      metaCheckVerdict `shouldReturnM` Just CheckFail
      metaCheckRunStatuses ghState `shouldReturnM` [RunReportStatusFailure]
  where
    reporter :: Reporter
    reporter = mkGithubReporter (defaultCommitInfo ^. repoInfo) (defaultCommitInfo ^. commit)

    metaCheckVerdict :: M (Maybe CheckStatus)
    metaCheckVerdict =
      fmap (^. metaCheck)
        <$> DB.getCommit (defaultCommitInfo ^. repoInfo . repoId) (defaultCommitInfo ^. commit)

    metaCheckRunStatuses :: GH.GithubFakeState -> M [RunReportStatus]
    metaCheckRunStatuses ghState =
      mapMaybe (fmap (^. status) . (^? _last) . filter ((== "All Garnix checks") . (^. name)) . map snd)
        <$> GH.getReports ghState
