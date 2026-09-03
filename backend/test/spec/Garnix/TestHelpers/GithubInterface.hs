{-# LANGUAGE OverloadedRecordDot #-}

module Garnix.TestHelpers.GithubInterface
  ( withFakeGithubInterface,
    mkRepo,
    addOrgMembers,
    lookupRepo,
    getReports,
    getPrComments,
    getAllReportLogs,
    getFinalLogs,
    withLocalRepo,
    GithubFakeState,
    TestRepo (..),

    -- * Helpers for withLocalRepo
    simpleSetup,
    setupWithNoFlake,
    setupWithConfig,
    setupWithGarnixConfig,

    -- * Test reports and statuses
    SimpleReports,
    getSimpleReports,
    filterByName,
    reportsShouldBe,
    reposAndReportsShouldBe,
    assertSingleRunForReport,
    assertReportDoesNotExist,
  )
where

import Control.Lens
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Yaml (encode)
import Garnix.GithubInterface.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers.GithubInterface.Internal
import Garnix.TestInstances ()
import Garnix.Types
import Garnix.YamlConfig
import System.Directory (copyFile)
import System.Random (randomIO)
import Test.HUnit (assertFailure)
import Test.Hspec (shouldBe)

defaultFakeRepos :: [(GhRepoOwner, GhRepoName)]
defaultFakeRepos =
  [ ("NixOS", "nixpkgs"),
    ("garnix-io", "incrementalize")
  ]

withFakeGithubInterface :: (GithubFakeState -> M a) -> M a
withFakeGithubInterface action = do
  (ghState, ghInterface) <- mkFakeGithubInterface
  forM_ defaultFakeRepos $ \(owner, repo) -> mkRepo ghState owner repo identity
  local (#githubInterface .~ ghInterface) $ action ghState

mkRepo :: GithubFakeState -> GhRepoOwner -> GhRepoName -> (TestRepo -> TestRepo) -> M ()
mkRepo ghState = setRepoImpl ghState.repoCollection

addOrgMembers :: GithubFakeState -> [GhUserOrgMembership] -> M ()
addOrgMembers ghState = addOrgMembersImpl ghState.orgMembersCollection

lookupRepo :: GithubFakeState -> GhRepoOwner -> GhRepoName -> M (Maybe TestRepo)
lookupRepo ghState = lookupRepoImpl ghState.repoCollection

getReports :: GithubFakeState -> M [[(RepoInfo, GhRunReport)]]
getReports ghState = getReportsImpl ghState.reportCollection

getPrComments :: GithubFakeState -> M [(RepoInfo, GhPullRequestId, Text)]
getPrComments ghState = getCommentsImpl ghState.commentCollection

getAllReportLogs :: GithubFakeState -> M [Text]
getAllReportLogs ghState = fmap (getRawLogs . (^. logs) . snd) . join <$> getReports ghState

withLocalRepo :: GithubFakeState -> GhRepoOwner -> GhRepoName -> (TestRepo -> TestRepo) -> CommitInfo -> (FilePath -> M ()) -> (CommitInfo -> M a) -> M a
withLocalRepo ghState owner name modify commitInfo setup action = do
  setRepoImpl ghState.repoCollection owner name modify
  withLocalRepoImpl ghState.repoCollection owner name commitInfo setup action

setupWithNoFlake :: FilePath -> M ()
setupWithNoFlake repoPath = do
  randomness :: Int <- randomIO
  liftIO $ T.writeFile (repoPath </> "randomness-file") (show randomness)

simpleSetup :: Text -> FilePath -> M ()
simpleSetup flake repoPath = setupWithConfig flake Nothing repoPath

setupWithConfig :: Text -> Maybe Text -> FilePath -> M ()
setupWithConfig flake mConfig repoPath = do
  liftIO @M $ T.writeFile (repoPath </> "flake.nix") flake
  forM_ mConfig $ \config -> do
    liftIO $ T.writeFile (repoPath </> "garnix.yaml") config
  liftIO $ copyFile "../flake.lock" (repoPath </> "flake.lock")
  randomness :: Int <- randomIO
  liftIO $ T.writeFile (repoPath </> "randomness-file") (show randomness)

setupWithGarnixConfig :: GarnixConfig -> Text -> FilePath -> M ()
setupWithGarnixConfig config flake repoPath = do
  liftIO $ T.writeFile (repoPath </> "garnix.yaml") (cs $ encode config)
  simpleSetup flake repoPath

type SimpleReports = Map.Map CommitHash (Map.Map Text (RunReportStatus, Text))

getSimpleReports :: GithubFakeState -> M SimpleReports
getSimpleReports ghState = do
  reports <- map snd . mconcat <$> getReports ghState
  pure $ foldl' insertIntoCommitMap mempty reports
  where
    insertIntoCommitMap acc ghRunReport = do
      Map.alter (Just . insertIntoRunMap ghRunReport . fromMaybe mempty) (ghRunReport ^. commit) acc
    insertIntoRunMap ghRunReport =
      Map.insert
        (ghRunReport ^. name)
        (ghRunReport ^. status, ghRunReport ^. logs . to getRawLogs)

getFinalLogs :: GithubFakeState -> CommitHash -> Text -> M Text
getFinalLogs ghState hash name = do
  logs <- getSimpleReports ghState
  case Map.lookup hash logs of
    Nothing ->
      error
        $ "cannot find build logs for: "
        <> getCommitHash hash
        <> ", available commit hashes: "
        <> T.intercalate ", " (getCommitHash <$> Map.keys logs)
    Just logs -> case Map.lookup name logs of
      Nothing ->
        error
          $ "cannot find build logs for: "
          <> name
          <> ", available packages: "
          <> T.intercalate ", " (show <$> Map.keys logs)
      Just (_status, logs) -> pure logs

filterByName :: Text -> SimpleReports -> SimpleReports
filterByName name m = flip fmap m $ flip Map.restrictKeys (Set.singleton name)

reposAndReportsShouldBe :: (HasCallStack) => [(RepoInfo, GhRunReport)] -> [(Text, RunReportStatus)] -> M ()
reposAndReportsShouldBe actual expected = liftIO $ (preprocess . snd <$> actual) `shouldBe` expected
  where
    preprocess :: GhRunReport -> (Text, RunReportStatus)
    preprocess GhRunReport {..} = (_ghRunReportName, _ghRunReportStatus)

reportsShouldBe :: (HasCallStack) => [(RepoInfo, GhRunReport)] -> [RunReportStatus] -> M ()
reportsShouldBe actual expected = liftIO $ (_ghRunReportStatus . snd <$> actual) `shouldBe` expected

assertSingleRunForReport :: Text -> [[(RepoInfo, GhRunReport)]] -> M [(RepoInfo, GhRunReport)]
assertSingleRunForReport name reports =
  go reports >>= \case
    [] -> liftIO $ assertFailure $ "could not find expected report '" <> cs name <> "'. Found reports:\n" <> reportBulletList reports
    result -> pure result
  where
    matchesName :: Text -> GhRunReport -> Bool
    matchesName name GhRunReport {..} = _ghRunReportName == name

    reportBulletList :: [[(RepoInfo, GhRunReport)]] -> String
    reportBulletList = unlines . fmap (("  - " <>) . intercalate ", " . fmap (cs . _ghRunReportName . snd))

    go :: [[(RepoInfo, GhRunReport)]] -> M [(RepoInfo, GhRunReport)]
    go =
      \case
        [] -> pure []
        (r : rs) -> do
          if all (matchesName name . snd) r
            then do
              duplicates <- go rs
              when (not $ null duplicates)
                $ liftIO
                $ assertFailure ("report '" <> cs name <> "' found with multiple runs")
              pure r
            else go rs

assertReportDoesNotExist :: Text -> [[(RepoInfo, GhRunReport)]] -> M ()
assertReportDoesNotExist name reports =
  when (elem name $ _ghRunReportName . snd <$> join reports)
    $ liftIO
    $ assertFailure ("unexpected report '" <> cs name <> "' found")
