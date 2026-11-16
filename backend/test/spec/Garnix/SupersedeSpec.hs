{-# LANGUAGE OverloadedRecordDot #-}

module Garnix.SupersedeSpec (spec) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, tryReadMVar)
import Control.Concurrent.Async.Lifted (async, wait)
import Control.Concurrent.STM (readTVarIO)
import Control.Lens ((<&>))
import Cradle
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Garnix.Async (Promise (..), PromiseResult (..), timeout)
import Garnix.BuildLogs.Internal (InternalId (..))
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Monad
import Garnix.Monad.Async (emptyPromise, resolve)
import Garnix.Monad.Pool (newPool, withPool)
import Garnix.Nix.Types qualified as Nix
import Garnix.NixConfig (nixConfDefaults)
import Garnix.Orchestrator (handleCommit, handlePullRequest)
import Garnix.Prelude
import Garnix.Reporters.GithubReporter (mkGithubReporter)
import Garnix.Reporters.OpenSearchReporter (openSearchReporter)
import Garnix.Supersede (buildActivityStarted, cancelsOnLine, newSupersession, onLine)
import Garnix.Supersede.Types
import Garnix.TestHelpers
import Garnix.TestHelpers.Common (commitAll)
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.GithubInterface.Internal (GithubFakeState (..), updateRepo)
import Garnix.TestHelpers.Monad
import Garnix.Types hiding (context, head)
import Garnix.YamlConfig (CancelSuperseded (..), ExcludeBranches (..))
import System.Metrics.Prometheus.Metric.Gauge qualified as Gauge
import System.Random (randomIO)
import Test.HUnit (assertFailure)
import Test.Hspec hiding (shouldReturn)

spec :: Spec
spec = inM . aroundM_ suppressLogsWhenPassing . beforeM_ truncateDBM $ do
  describe "a commit superseded on its branch" $ do
    it "is cancelled before it has any builds, and built once the branch is back on it" $ GH.withFakeGithubInterface $ \ghState -> do
      user <- newOwner
      randomness <- liftIO randomIO
      GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (writeFlake randomness "a" Nothing) $ \commitA -> do
        onFeatureBranch ghState
        promiseA <- push commitA
        -- Pushed while the run of A is still checking out, long before it
        -- gets any builds.
        commitB <- nextCommit ghState commitA (writeFlake randomness "b" Nothing)
        promiseB <- push commitB
        within $ resolve promiseA >> resolve promiseB

        buildsA <- buildsOf user commitA
        map (\b -> (b ^. packageType, b ^. status)) buildsA `shouldBeM` [(TypeOverall, Just Cancelled)]
        reports <- GH.getSimpleReports ghState
        finalStatus reports commitA "Evaluate flake.nix" `shouldBeM` Just RunReportStatusCancelled
        finalStatus reports commitA "All Garnix checks" `shouldBeM` Just RunReportStatusCancelled
        finalStatus reports commitB pkgCheck `shouldBeM` Just RunReportStatusSuccess
        finalStatus reports commitB "All Garnix checks" `shouldBeM` Just RunReportStatusSuccess

        -- A late duplicate of the event that pushed it.
        within $ push commitA >>= resolve
        pkgBuildOf user commitA `shouldReturnM` Nothing

        resetBranchTo ghState commitA
        within $ push commitA >>= resolve
        (fmap (^. status) <$> pkgBuildOf user commitA) `shouldReturnM` Just (Just Success)

    it "is superseded from the start when its webhook arrives after the newer commit's" $ GH.withFakeGithubInterface $ \ghState -> do
      user <- newOwner
      randomness <- liftIO randomIO
      GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (writeFlake randomness "a" Nothing) $ \commitA -> do
        onFeatureBranch ghState
        commitB <- nextCommit ghState commitA (writeFlake randomness "b" Nothing)
        builds <- blockingBuilds commitA (Just . cs)
        withMock #nixBuildMock builds.mock $ do
          promiseB <- push commitB
          within $ liftIO $ takeMVar builds.bRunning
          within $ push commitA >>= resolve
          liftIO $ putMVar builds.releaseB ()
          within $ resolve promiseB

        buildsA <- buildsOf user commitA
        map (\b -> (b ^. packageType, b ^. status)) buildsA `shouldBeM` [(TypeOverall, Just Cancelled)]
        (fmap (^. status) <$> pkgBuildOf user commitB) `shouldReturnM` Just (Just Success)
        reports <- GH.getSimpleReports ghState
        finalStatus reports commitA "All Garnix checks" `shouldBeM` Just RunReportStatusCancelled
        finalStatus reports commitB "All Garnix checks" `shouldBeM` Just RunReportStatusSuccess

    describe "under the batch strategy" $ aroundM_ (local (#evalStrategy .~ Batch)) $ do
      it "stops its nix-eval-jobs run and cancels the builds waiting on it" $ GH.withFakeGithubInterface $ \ghState -> do
        user <- newOwner
        randomness <- liftIO randomIO
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (writeFlake randomness "a" Nothing) $ \commitA -> do
          onFeatureBranch ghState
          evaluating <- liftIO newEmptyMVar
          interrupted <- liftIO newEmptyMVar
          let evalJobs expr
                | getCommitHash (commitA ^. commit) `T.isInfixOf` expr = do
                    liftIO $ putMVar evaluating ()
                    forever (threadDelay (fromSeconds @Int 1)) `onException` liftIO (putMVar interrupted ())
                -- Nothing, so that the newer commit evaluates per attribute.
                | otherwise = pure ""
          commitB <- withMock #evalJobsMock evalJobs $ do
            promiseA <- push commitA
            within $ liftIO $ takeMVar evaluating
            commitB <- nextCommit ghState commitA (writeFlake randomness "b" Nothing)
            promiseB <- push commitB
            within $ liftIO $ takeMVar interrupted
            within $ resolve promiseA >> resolve promiseB
            pure commitB

          (fmap (^. status) <$> pkgBuildOf user commitA) `shouldReturnM` Just (Just Cancelled)
          reports <- GH.getSimpleReports ghState
          finalStatus reports commitA pkgCheck `shouldBeM` Just RunReportStatusCancelled
          finalLogs reports commitA pkgCheck `shouldSatisfyM` T.isInfixOf ("superseded by " <> getCommitHash (commitB ^. commit))
          finalStatus reports commitA "All Garnix checks" `shouldBeM` Just RunReportStatusCancelled
          finalStatus reports commitB pkgCheck `shouldBeM` Just RunReportStatusSuccess

    it "cancels a build waiting for a build slot, which leaves the queue" $ GH.withFakeGithubInterface $ \ghState -> do
      user <- newOwner
      randomness <- liftIO randomIO
      metrics <- view #metrics
      pool <- newPool 1 metrics #nixBuildQueueWaitTime #nixBuildQueueLen
      let queued = liftIO (Gauge.sample (metrics ^. #nixBuildQueueLen)) <&> \(Gauge.GaugeSample n) -> n
      local (#nixBuildPool .~ pool) $ withMock #nixBuildMock (\_ -> pure Success) $ do
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (writeFlake randomness "a" Nothing) $ \commitA -> do
          onFeatureBranch ghState
          release <- liftIO newEmptyMVar
          holding <- liftIO newEmptyMVar
          holder <- async $ withPool pool (GhRepoOwner "someone-else") $ liftIO $ putMVar holding () >> takeMVar release
          within $ liftIO $ takeMVar holding
          promiseA <- push commitA
          within $ waitFor (fromSeconds @Int 120) $ queued `shouldReturnM` 1
          commitB <- nextCommit ghState commitA (writeFlake randomness "b" Nothing)
          promiseB <- push commitB
          within $ waitFor (fromSeconds @Int 120) $ (fmap (^. status) <$> pkgBuildOf user commitA) `shouldReturnM` Just (Just Cancelled)
          liftIO $ putMVar release ()
          within $ wait holder >> resolve promiseA >> resolve promiseB

          built <- getMockCalls #nixBuildMock
          map ((^. gitCommit) . fst) built `shouldBeM` [commitB ^. commit]
          (fmap (^. status) <$> pkgBuildOf user commitB) `shouldReturnM` Just (Just Success)
          within $ withPool pool (GhRepoOwner "someone-else") (pure ())

    it "keeps a running build of a derivation the newer commit needs" $ GH.withFakeGithubInterface $ \ghState -> do
      user <- newOwner
      randomness <- liftIO randomIO
      shared <- sharedDepDrv randomness
      GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (writeFlake randomness "a" Nothing) $ \commitA -> do
        onFeatureBranch ghState
        builds <- blockingBuilds commitA (const (Just shared))
        commitB <- withMock #nixBuildMock builds.mock $ do
          promiseA <- push commitA
          within $ liftIO $ takeMVar builds.aRunning
          commitB <- nextCommit ghState commitA (writeFlake randomness "b" Nothing)
          promiseB <- push commitB
          within $ liftIO $ takeMVar builds.bRunning
          -- The newer commit has evaluated, so the decision is due.
          threadDelay (fromSeconds @Int 3)
          liftIO (tryReadMVar builds.aKilled) `shouldReturnM` Nothing
          liftIO $ putMVar builds.releaseA ()
          within $ resolve promiseA
          liftIO $ putMVar builds.releaseB ()
          within $ resolve promiseB
          pure commitB

        (fmap (^. status) <$> pkgBuildOf user commitA) `shouldReturnM` Just (Just Success)
        reports <- GH.getSimpleReports ghState
        finalStatus reports commitA "All Garnix checks" `shouldBeM` Just RunReportStatusSuccess
        finalStatus reports commitB pkgCheck `shouldBeM` Just RunReportStatusSuccess

    it "keeps a running build while it builds no derivation" $ GH.withFakeGithubInterface $ \ghState -> do
      user <- newOwner
      randomness <- liftIO randomIO
      GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (writeFlake randomness "a" Nothing) $ \commitA -> do
        onFeatureBranch ghState
        builds <- blockingBuilds commitA (const Nothing)
        withMock #nixBuildMock builds.mock $ do
          promiseA <- push commitA
          within $ liftIO $ takeMVar builds.aRunning
          commitB <- nextCommit ghState commitA (writeFlake randomness "b" Nothing)
          promiseB <- push commitB
          within $ liftIO $ takeMVar builds.bRunning
          threadDelay (fromSeconds @Int 3)
          liftIO (tryReadMVar builds.aKilled) `shouldReturnM` Nothing
          liftIO $ putMVar builds.releaseA ()
          liftIO $ putMVar builds.releaseB ()
          within $ resolve promiseA >> resolve promiseB

        (fmap (^. status) <$> pkgBuildOf user commitA) `shouldReturnM` Just (Just Success)

    it "kills a running build of nothing the newer commit needs" $ GH.withFakeGithubInterface $ \ghState -> do
      user <- newOwner
      randomness <- liftIO randomIO
      GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (writeFlake randomness "a" Nothing) $ \commitA -> do
        onFeatureBranch ghState
        builds <- blockingBuilds commitA (Just . cs)
        withMock #nixBuildMock builds.mock $ do
          promiseA <- push commitA
          within $ liftIO $ takeMVar builds.aRunning
          commitB <- nextCommit ghState commitA (writeFlake randomness "b" Nothing)
          promiseB <- push commitB
          within $ liftIO $ takeMVar builds.aKilled
          within $ resolve promiseA
          liftIO $ putMVar builds.releaseB ()
          within $ resolve promiseB

          (fmap (^. status) <$> pkgBuildOf user commitA) `shouldReturnM` Just (Just Cancelled)
          reports <- GH.getSimpleReports ghState
          finalStatus reports commitA pkgCheck `shouldBeM` Just RunReportStatusCancelled
          finalStatus reports commitA "All Garnix checks" `shouldBeM` Just RunReportStatusCancelled
          finalStatus reports commitB pkgCheck `shouldBeM` Just RunReportStatusSuccess

    forM_
      [ ("with cancelSuperseded: false", Just "cancelSuperseded: false\n", True),
        ("on the default branch when cancelSuperseded is unset", Nothing, False)
      ]
      $ \(title, garnixYaml, onFeature) ->
        it ("is built to the end " <> title) $ GH.withFakeGithubInterface $ \ghState -> do
          user <- newOwner
          randomness <- liftIO randomIO
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (writeFlake randomness "a" garnixYaml) $ \commitA -> do
            when onFeature $ onFeatureBranch ghState
            builds <- blockingBuilds commitA (Just . cs)
            withMock #nixBuildMock builds.mock $ do
              promiseA <- push commitA
              within $ liftIO $ takeMVar builds.aRunning
              commitB <- nextCommit ghState commitA (writeFlake randomness "b" garnixYaml)
              promiseB <- push commitB
              within $ liftIO $ takeMVar builds.bRunning
              threadDelay (fromSeconds @Int 3)
              liftIO (tryReadMVar builds.aKilled) `shouldReturnM` Nothing
              liftIO $ putMVar builds.releaseA ()
              liftIO $ putMVar builds.releaseB ()
              within $ resolve promiseA >> resolve promiseB

            (fmap (^. status) <$> pkgBuildOf user commitA) `shouldReturnM` Just (Just Success)
            reports <- GH.getSimpleReports ghState
            finalStatus reports commitA "All Garnix checks" `shouldBeM` Just RunReportStatusSuccess

  describe "a commit superseded on a pull request from a fork" $ do
    it "kills a running build of nothing the newer commit needs" $ GH.withFakeGithubInterface $ \ghState -> do
      user <- newOwner
      randomness <- liftIO randomIO
      GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (writeFlake randomness "a" Nothing) $ \onBranch -> do
        let fromFork ci = ci & branch .~ Nothing & prFromFork ?~ PrFromFork "forker/repo"
            commitA = fromFork onBranch
            pushToPr ci = handlePullRequest (reporterFor ci) ci (GhPullRequestId 7)
        builds <- blockingBuilds commitA (Just . cs)
        withMock #nixBuildMock builds.mock $ do
          promiseA <- pushToPr commitA
          within $ liftIO $ takeMVar builds.aRunning
          commitB <- fromFork <$> nextCommit ghState onBranch (writeFlake randomness "b" Nothing)
          promiseB <- pushToPr commitB
          within $ liftIO $ takeMVar builds.aKilled
          within $ resolve promiseA
          liftIO $ putMVar builds.releaseB ()
          within $ resolve promiseB

        (fmap (^. status) <$> pkgBuildOf user commitA) `shouldReturnM` Just (Just Cancelled)
        reports <- GH.getSimpleReports ghState
        finalStatus reports commitA "All Garnix checks" `shouldBeM` Just RunReportStatusCancelled

  describe "cancelsOnLine" $ do
    it "follows cancelSuperseded, and spares the default branch when it is unset" $ GH.withFakeGithubInterface $ \ghState -> do
      GH.mkRepo ghState "owner" "repo" (#defaultBranch ?~ "main")
      let repo = RepoId githubForge "owner" "repo"
          onBranch = BranchLine repo
          pr = PullRequestLine repo 1
          except = Just . CancelSupersededExceptOn . ExcludeBranches
          decide policy line = cancelsOnLine defaultCommitInfo policy line
      decide Nothing (onBranch "main") `shouldReturnM` False
      decide Nothing (onBranch "feature") `shouldReturnM` True
      decide Nothing pr `shouldReturnM` True
      decide (Just (CancelSuperseded True)) (onBranch "main") `shouldReturnM` True
      decide (Just (CancelSuperseded False)) (onBranch "feature") `shouldReturnM` False
      decide (Just (CancelSuperseded False)) pr `shouldReturnM` False
      decide (except ["release"]) (onBranch "release") `shouldReturnM` False
      decide (except ["release"]) (onBranch "main") `shouldReturnM` True
      decide (except ["release"]) pr `shouldReturnM` True

    it "keeps building when the default branch is not known" $ GH.withFakeGithubInterface $ \ghState -> do
      GH.mkRepo ghState "owner" "repo" identity
      cancelsOnLine defaultCommitInfo Nothing (BranchLine (RepoId githubForge "owner" "repo") "feature") `shouldReturnM` False

  describe "onLine" $ do
    let supersededAfterNewerPush :: Bool -> M (Maybe CommitHash)
        supersededAfterNewerPush enabled = do
          supersession <- newSupersession enabled
          local (#supersession .~ supersession) $ do
            let line = BranchLine (RepoId githubForge "owner" "repo") "feature"
            pending <- liftIO newEmptyMVar
            runA <- liftIO newEmptyMVar
            _ <- onLine line (CommitHash "aaaa") NewHead $ do
              view #lineRun >>= liftIO . putMVar runA
              pure $ Promise pending
            _ <- onLine line (CommitHash "bbbb") NewHead emptyPromise
            by <-
              liftIO (takeMVar runA) >>= \case
                Just run -> liftIO $ readTVarIO (lineRunSupersededBy run)
                Nothing -> liftIO $ assertFailure "the run is not on a line"
            liftIO $ putMVar pending (PromiseSucceeded ())
            pure by

    it "supersedes the line's runs of other commits" $ do
      supersededAfterNewerPush True `shouldReturnM` Just (CommitHash "bbbb")

    it "supersedes nothing when the server turns cancelling off" $ do
      supersededAfterNewerPush False `shouldReturnM` Nothing

pkgCheck :: Text
pkgCheck = "package pkg [x86_64-linux]"

newOwner :: M User
newOwner = DB.newUser (ForgeLogin githubForge (GhLogin "owner")) "owner@owner.com"

reporterFor :: CommitInfo -> Reporter
reporterFor ci = mkGithubReporter (ci ^. repoInfo) (ci ^. commit) <> openSearchReporter

push :: CommitInfo -> M (Promise ())
push ci = handleCommit (reporterFor ci) False ci

-- | The fake forge's default branch is the one tests push to, unless this
-- moves it away.
onFeatureBranch :: GH.GithubFakeState -> M ()
onFeatureBranch ghState = updateRepo ghState.repoCollection "owner" "repo" (#defaultBranch ?~ "main")

-- | Two derivations: @pkg@, which differs with @marker@, and the dependency
-- it shares with every other marker.
writeFlake :: Int -> Text -> Maybe Text -> FilePath -> M ()
writeFlake randomness marker garnixYaml repoPath = liftIO $ do
  T.writeFile (repoPath </> "flake.nix")
    $ cs
      [i|
        { outputs = {self}: let
            dep = #{sharedDep randomness};
          in {
            packages.x86_64-linux.pkg = derivation {
              name = "pkg";
              builder = "/bin/sh";
              system = "x86_64-linux";
              args = [ "-c" "echo ${dep} #{marker} > $out" ];
            };
          };
        }
      |]
  for_ garnixYaml $ T.writeFile (repoPath </> "garnix.yaml")

sharedDep :: Int -> Text
sharedDep randomness =
  cs
    [i|
      derivation {
        name = "shared-dep";
        builder = "/bin/sh";
        system = "x86_64-linux";
        args = [ "-c" "echo #{randomness} > $out" ];
      }
    |]

sharedDepDrv :: Int -> M Text
sharedDepDrv randomness = do
  StdoutTrimmed drv <- liftIO $ run $ cmd "nix" & nixConfDefaults & addArgs ["eval", "--raw", "--expr", "(" <> sharedDep randomness <> ").drvPath"]
  pure drv

nextCommit :: GH.GithubFakeState -> CommitInfo -> (FilePath -> M ()) -> M CommitInfo
nextCommit ghState ci setup = do
  path <- localRepoPath ghState
  setup path
  hash <- commitAll path
  pure $ ci & commit .~ hash

localRepoPath :: GH.GithubFakeState -> M FilePath
localRepoPath ghState =
  GH.lookupRepo ghState "owner" "repo" >>= \case
    Just repo | Just path <- repo.localPath -> pure path
    _ -> liftIO $ assertFailure "the repository has no local path"

-- | Like a force-push of the commit to the branch.
resetBranchTo :: GH.GithubFakeState -> CommitInfo -> M ()
resetBranchTo ghState ci = do
  path <- localRepoPath ghState
  liftIO $ run_ $ cmd "git" & setWorkingDir path & addArgs ["reset", "--hard", getCommitHash (ci ^. commit)] & silenceStdout

buildsOf :: User -> CommitInfo -> M [Build]
buildsOf user ci = filter ((== ci ^. commit) . (^. gitCommit)) <$> DB.getBuilds user

pkgBuildOf :: User -> CommitInfo -> M (Maybe Build)
pkgBuildOf user ci = find ((== TypePackage) . (^. packageType)) <$> buildsOf user ci

finalStatus :: GH.SimpleReports -> CommitInfo -> Text -> Maybe RunReportStatus
finalStatus reports ci name = fst <$> (Map.lookup (ci ^. commit) reports >>= Map.lookup name)

finalLogs :: GH.SimpleReports -> CommitInfo -> Text -> Text
finalLogs reports ci name = maybe "" snd (Map.lookup (ci ^. commit) reports >>= Map.lookup name)

within :: M a -> M a
within action =
  timeout (fromSeconds @Int 180) action >>= \case
    Just a -> pure a
    Nothing -> liftIO $ assertFailure "timed out"

-- | A @nix build@ mock under which the older commit's build runs, reporting
-- the derivation @building@ picks from its drvPath, if any, as the one it
-- builds, until it is killed or released, and the newer commit's build until
-- it is released.
data BlockingBuilds = BlockingBuilds
  { mock :: (Build, Nix.DrvPath) -> M Status,
    aRunning :: MVar (),
    aKilled :: MVar (),
    releaseA :: MVar (),
    bRunning :: MVar (),
    releaseB :: MVar ()
  }

blockingBuilds :: CommitInfo -> (Nix.DrvPath -> Maybe Text) -> M BlockingBuilds
blockingBuilds older building = do
  aRunning <- liftIO newEmptyMVar
  aKilled <- liftIO newEmptyMVar
  releaseA <- liftIO newEmptyMVar
  bRunning <- liftIO newEmptyMVar
  releaseB <- liftIO newEmptyMVar
  let mock (build, drv)
        | build ^. gitCommit == older ^. commit = do
            for_ (building drv) $ buildActivityStarted (InternalId 1)
            liftIO $ putMVar aRunning ()
            (liftIO (takeMVar releaseA) $> Success) `onException` liftIO (putMVar aKilled ())
        | otherwise = do
            liftIO $ putMVar bRunning ()
            liftIO (takeMVar releaseB) $> Success
  pure BlockingBuilds {..}
