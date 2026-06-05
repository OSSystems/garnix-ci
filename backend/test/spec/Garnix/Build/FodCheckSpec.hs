module Garnix.Build.FodCheckSpec (spec) where

import Control.Lens
import Control.Monad.Trans.Control (liftBaseOp_)
import Cradle
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Lens
import Data.Containers.ListUtils (nubOrd)
import Data.Map ((!))
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.Build.Checkout (withCheckout)
import Garnix.Build.FodCheck
import Garnix.DB qualified as DB
import Garnix.DB.FeatureFlags.Types
import Garnix.Monad
import Garnix.Monad.Async
import Garnix.Monad.SubProcess (runSubProcess_)
import Garnix.Nix.Types qualified as Nix
import Garnix.NixConfig (addNixConfigEnvironment, nixConfDefaults)
import Garnix.Orchestrator qualified as Orchestrator
import Garnix.Prelude
import Garnix.Reporters.GithubReporter (mkGithubReporter)
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.Reporter (TestReport (..), withTestReporter_)
import Garnix.Types hiding (context)
import System.Environment (setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import System.Random (randomIO)
import Test.Hspec
import Test.Mockery.Directory (inTempDirectory)

spec :: Spec
spec = inM $ aroundM_ (withUnmock #fodCheckMock . setUpXdgCacheDir . suppressLogsWhenPassing) $ beforeM_ truncateDBM $ do
  describe "withFodChecker" $ do
    it "disables the FOD check by default" $ do
      GH.withFakeGithubInterface $ \ghState -> do
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup "") $ \commitInfo -> do
          withCheckout commitInfo $ do
            withFodChecker mempty commitInfo $ \fodChecker -> do
              void fodChecker `shouldSatisfyM` isNothing

    it "enables the FOD check in the yaml config" $ do
      GH.withFakeGithubInterface $ \ghState -> do
        let garnixConfig = "fodChecks: true"
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.setupWithConfig "" (Just garnixConfig)) $ \commitInfo -> do
          withCheckout commitInfo $ do
            withFodChecker mempty commitInfo $ \fodChecker -> do
              void fodChecker `shouldSatisfyM` isJust

    it "disables the FOD check when GARNIX_FOD_CHECK is off" $ do
      GH.withFakeGithubInterface $ \ghState -> do
        let garnixConfig = "fodChecks: true"
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.setupWithConfig "" (Just garnixConfig)) $ \commitInfo -> do
          withCheckout commitInfo $ do
            liftIO $ setEnv "GARNIX_FOD_CHECK" "false"
            withFodChecker mempty commitInfo $ \fodChecker -> do
              liftIO $ unsetEnv "GARNIX_FOD_CHECK"
              void fodChecker `shouldSatisfyM` isNothing

  describe "__pickRemoteBuilderUrlFromMachinesFile" $ do
    let realMachinesFile =
          [i|
            ssh-ng://nix-ssh@macMini1 aarch64-darwin,x86_64-darwin /run/secrets/garnix_server_remote_builder_ssh 4 1 big-parallel,recursive-nix - -
            ssh-ng://nix-ssh@macMini2 aarch64-darwin,x86_64-darwin /run/secrets/garnix_server_remote_builder_ssh 4 1 big-parallel,recursive-nix - -
            ssh-ng://nix-ssh@garnix5 x86_64-linux,i686-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
            ssh-ng://nix-ssh@garnix6 x86_64-linux,i686-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
            ssh-ng://nix-ssh@garnix7 x86_64-linux,i686-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
            ssh-ng://nix-ssh@garnix8 x86_64-linux,i686-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
            ssh-ng://nix-ssh@garnix9 x86_64-linux,i686-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
            ssh-ng://nix-ssh@arm-server-0 aarch64-linux /run/secrets/garnix_server_remote_builder_ssh 60 4 nixos-test,kvm,big-parallel,recursive-nix - -
            ssh-ng://nix-ssh@arm-server-1 aarch64-linux /run/secrets/garnix_server_remote_builder_ssh 8 1 nixos-test,kvm,big-parallel,recursive-nix - -
          |]
    let cases =
          [ ( realMachinesFile,
              AArch64Darwin,
              [ "macMini1",
                "macMini2"
              ]
            ),
            ( realMachinesFile,
              X8664Darwin,
              [ "macMini1",
                "macMini2"
              ]
            ),
            ( realMachinesFile,
              X8664Linux,
              [ "garnix5",
                "garnix6",
                "garnix7",
                "garnix8",
                "garnix9"
              ]
            ),
            ( realMachinesFile,
              I686Linux,
              [ "garnix5",
                "garnix6",
                "garnix7",
                "garnix8",
                "garnix9"
              ]
            ),
            ( realMachinesFile,
              OtherSystem "builtin",
              [ "garnix5",
                "garnix6",
                "garnix7",
                "garnix8",
                "garnix9"
              ]
            ),
            ( realMachinesFile,
              AArch64Linux,
              [ "arm-server-0",
                "arm-server-1"
              ]
            )
          ]
    forM_ cases $ \(machinesFile, system, builders) -> do
      it ("picks out a remote builder fairly for " <> cs (system ^. systemTextIso)) $ do
        pickedBuilderUrl <- (sort . nubOrd <$>) $ replicateM 100 $ do
          __pickRemoteBuilderUrlFromMachinesFile system (cs $ unindent machinesFile)
        pickedBuilderUrl `shouldBeM` map (\builder -> "ssh-ng://nix-ssh@" <> builder) builders

  describe "__parseMachinesFile" $ do
    let cases =
          [ [i|
              ssh-ng://nix-ssh@foo x86_64-linux,i686-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
              ssh-ng://nix-ssh@bar x86_64-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
            |],
            [i|
              ssh-ng://nix-ssh@foo x86_64-linux,i686-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
              ssh-ng://nix-ssh@bar x86_64-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -

            |],
            [i|
              ssh-ng://nix-ssh@foo x86_64-linux,i686-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
              ssh-ng://nix-ssh@bar x86_64-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -|],
            [i|

              ssh-ng://nix-ssh@foo x86_64-linux,i686-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
              ssh-ng://nix-ssh@bar x86_64-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
            |],
            [i|
              ssh-ng://nix-ssh@foo x86_64-linux,i686-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -

              ssh-ng://nix-ssh@bar x86_64-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
            |],
            [i|
              ssh-ng://nix-ssh@foo x86_64-linux,i686-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
              ssh-ng://nix-ssh@bar    x86_64-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
            |],
            [i|
              ssh-ng://nix-ssh@foo x86_64-linux,i686-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
              ssh-ng://nix-ssh@bar x86_64-linux    /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
            |],
            [i|
              ssh-ng://nix-ssh@foo x86_64-linux,i686-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
               ssh-ng://nix-ssh@bar x86_64-linux /run/secrets/garnix_server_remote_builder_ssh 28 4 nixos-test,kvm,big-parallel,recursive-nix - -
            |]
          ]
    forM_ (zip [0 :: Int ..] cases) $ \(i, machinesFile) -> do
      it ("parses differently formatted machines files (" <> cs (show i) <> ")") $ do
        __parseMachinesFile (cs $ unindent machinesFile)
          `shouldBeM` Right (X8664Linux ~> ["ssh-ng://nix-ssh@bar", "ssh-ng://nix-ssh@foo"] <> I686Linux ~> ["ssh-ng://nix-ssh@foo"])

  describe "__findAllFodsRecursively" $ do
    it "doesn't crash on a real derivation from nixpkgs" $ do
      nixpkgsCommitSha <- getNixpkgsCommitSha
      StdoutRaw output <-
        Cradle.run $ Cradle.cmd "nix"
          & nixConfDefaults
          & Cradle.addArgs
            [ "derivation",
              "show",
              "github:nixos/nixpkgs/"
                <> nixpkgsCommitSha
                <> "#qemu"
            ]
      let drvHash = Aeson.toText $ fromSingleton $ Aeson.keys $ output ^. key "derivations" . _Object
      let drvPath = fromRight $ Nix.parseDrvPath ("/nix/store/" <> drvHash)
      void $ __findAllFodsRecursively drvPath

  let rebuildFodTestImpl :: (System, Nix.DrvPath) -> M (Either Text Text)
      rebuildFodTestImpl (_system, drvPath) = do
        nixConfig <- view #userNixConfig
        run_
          $ cmd "nix"
          & addArgs ["build", cs drvPath <> "^*", "--no-link" :: Text]
          & addNixConfigEnvironment nixConfig
          & silenceStderr
        (exitCode, StdoutRaw stdout, StderrRaw stderr) <-
          run
            $ cmd "nix"
            & addArgs ["build", cs drvPath <> "^*", "--no-link", "--json", "--rebuild" :: Text]
            & addNixConfigEnvironment nixConfig
            & silenceStderr
        pure $ case exitCode of
          ExitFailure _ -> Left $ cs stderr
          ExitSuccess -> Right $ cs stdout
  describe "fodCheck" $ aroundM_ (withMock #rebuildFodMock rebuildFodTestImpl) $ do
    let test :: Nix.DrvPath -> M TestReport
        test drvPath = do
          let garnixConfig = "fodChecks: true"
          dir <- view #workingDir
          liftIO $ T.writeFile (dir </> "garnix.yaml") garnixConfig
          result <- withTestReporter_ $ \reporter -> do
            withFodChecker reporter defaultCommitInfo $ \fodChecker -> do
              void fodChecker `shouldSatisfyM` isJust
              fodCheck fodChecker drvPath
          pure $ result ! "FOD checks"

    it "does not fail for benign fods" $ do
      flake <- mkFodFlake Nothing =<< mkRandomOutput
      drvPath <- fst <$> testDerivation flake "default"
      buildDrvPath drvPath
      report <- test drvPath
      fodDrvPath <- fst <$> testDerivation flake "fod"
      (report ^. #success) `shouldBeM` Just True
      report ^. #logs `shouldBeM` "Checking fixed output derivations...\n1 FOD was verified."
      verifiedFodsShouldBe [fodDrvPath]

    it "fails for lying fods" $ do
      output <- mkRandomOutput
      validButMalicious <- mkFodFlake Nothing output
      buildDrvPath . fst =<< testDerivation validButMalicious "default"
      lying <- mkFodFlake Nothing (output & #output .~ "bar")
      lyingDrvPath <- fst <$> testDerivation lying "default"
      report <- test lyingDrvPath
      (report ^. #success) `shouldBeM` Just False
      (report ^. #logs) `shouldSatisfyM` T.isInfixOf "hash mismatch in fixed-output derivation"

    it "works for benign fods that weren't built yet" $ do
      flake <- mkFodFlake Nothing =<< mkRandomOutput
      drvPath <- fst <$> testDerivation flake "default"
      report <- test drvPath
      (report ^. #success) `shouldBeM` Just True
      fodDrvPath <- fst <$> testDerivation flake "fod"
      report ^. #logs `shouldBeM` "Checking fixed output derivations...\n1 FOD was verified."
      verifiedFodsShouldBe [fodDrvPath]

    it "doesn't fail for fods that have different build scripts, but produce the same result" $ do
      output <- mkRandomOutput
      fodA <- mkFodFlake (Just "echo a") output
      drvPathA <- fst <$> testDerivation fodA "default"
      buildDrvPath drvPathA
      report <- test drvPathA
      (report ^. #success) `shouldBeM` Just True
      fodB <- mkFodFlake (Just "echo b") output
      drvPathB <- fst <$> testDerivation fodB "default"
      buildDrvPath drvPathB
      report <- test drvPathB
      (report ^. #success) `shouldBeM` Just True

    it "fails for lying fods after verifying other fods" $ do
      output <- mkRandomOutput
      fod <- mkFodFlake Nothing output
      drvPath <- fst <$> testDerivation fod "default"
      buildDrvPath drvPath
      report <- test drvPath
      (report ^. #success) `shouldBeM` Just True
      validButMalicious <- mkFodFlake Nothing output
      buildDrvPath . fst =<< testDerivation validButMalicious "default"
      lying <- mkFodFlake Nothing (output & #output .~ "bar")
      lyingDrvPath <- fst <$> testDerivation lying "default"
      report <- test lyingDrvPath
      (report ^. #success) `shouldBeM` Just False
      (report ^. #logs) `shouldSatisfyM` T.isInfixOf "hash mismatch in fixed-output derivation"

    it "adds verified fods into the db" $ do
      fod <- mkFodFlake Nothing =<< mkRandomOutput
      (drvPath, storePath) <- testDerivation fod "fod"
      buildDrvPath drvPath
      buildDrvPath . fst =<< testDerivation fod "default"
      report <- test drvPath
      (report ^. #success) `shouldBeM` Just True
      getVerifiedFods
        `shouldReturnM` [ ( Nix.getStoreHash $ Nix.getHash $ Nix.getDrvPath drvPath,
                            Nix.getStoreHash $ Nix.getHash storePath
                          )
                        ]

    it "doesn't add lying fods into the db" $ do
      output <- mkRandomOutput
      validButMalicious <- mkFodFlake Nothing output
      buildDrvPath . fst =<< testDerivation validButMalicious "default"
      lying <- mkFodFlake Nothing (output & #output .~ "bar")
      lyingDrvPath <- fst <$> testDerivation lying "default"
      report <- test lyingDrvPath
      (report ^. #success) `shouldBeM` Just False
      getVerifiedFods `shouldReturnM` []

    it "doesn't recheck already verified fods" $ do
      output <- mkRandomOutput
      validButMalicious <- mkFodFlake Nothing output
      buildDrvPath . fst =<< testDerivation validButMalicious "default"
      lying <- mkFodFlake Nothing (output & #output .~ "bar")
      (lyingDrvPath, lyingStorePath) <- testDerivation lying "fod"
      DB.addVerifiedFod lyingDrvPath lyingStorePath
      report <- test lyingDrvPath
      (report ^. #success) `shouldBeM` Just True

    it "does not run the fod check if it is not enabled" $ do
      result <- withTestReporter_ $ \reporter -> do
        let commitInfo =
              defaultCommitInfo
                & repoInfo . ghRepoOwner .~ "owner"
                & repoInfo . ghRepoName .~ "repo"
                & reqUser .~ "owner"
        output <- mkRandomOutput
        validButMalicious <- mkFodFlake Nothing output
        buildDrvPath . fst =<< testDerivation validButMalicious "default"
        lying <- mkFodFlake Nothing (output & #output .~ "bar")
        lyingDrvPath <- fst <$> testDerivation lying "default"
        withFodChecker reporter commitInfo $ \fodChecker -> do
          fodCheck fodChecker lyingDrvPath
      result `shouldBeM` mempty

    it "reports the correct final status" $ do
      lyingDrvPath <- do
        -- fast bad FOD
        output <- mkRandomOutput
        validButMaliciousFlake <- mkFodFlake Nothing output
        buildDrvPath . fst =<< testDerivation validButMaliciousFlake "default"
        lying <- mkFodFlake Nothing (output & #output .~ "bar")
        fst <$> testDerivation lying "fod"
      successDrvPath <- do
        -- slow good FOD
        output <- mkRandomOutput
        successFlake <- mkFodFlake (Just "sleep 5") output
        buildDrvPath . fst =<< testDerivation successFlake "default"
        fst <$> testDerivation successFlake "fod"
      reports <- do
        let garnixConfig = "fodChecks: true"
        dir <- view #workingDir
        liftIO $ T.writeFile (dir </> "garnix.yaml") garnixConfig
        withTestReporter_ $ \reporter -> do
          withFodChecker reporter defaultCommitInfo $ \fodChecker -> do
            fodCheck fodChecker lyingDrvPath
            fodCheck fodChecker successDrvPath
      (reports ! "FOD checks") ^. #success `shouldBeM` Just False
      let logs = cs $ (reports ! "FOD checks") ^. #logs
      logs `shouldContainM` "1 FOD was verified."
      logs `shouldContainM` ("Failure when checking FOD '" <> cs lyingDrvPath <> "':\n")
      verifiedFodsShouldBe [successDrvPath]

    it "deduplicates fod checks running within the same `FodChecker`" $ do
      drvPath <- do
        output <- mkRandomOutput
        flake <- mkFodFlake Nothing output
        buildDrvPath . fst =<< testDerivation flake "default"
        fst <$> testDerivation flake "fod"
      reports <- do
        let garnixConfig = "fodChecks: true"
        dir <- view #workingDir
        liftIO $ T.writeFile (dir </> "garnix.yaml") garnixConfig
        withTestReporter_ $ \reporter -> do
          withFodChecker reporter defaultCommitInfo $ \fodChecker -> do
            fodCheck fodChecker drvPath
            fodCheck fodChecker drvPath
      (reports ! "FOD checks") ^. #success `shouldBeM` Just True
      let logs = (reports ! "FOD checks") ^. #logs
      logs `shouldBeM` "Checking fixed output derivations...\n1 FOD was verified."
      verifiedFodsShouldBe [drvPath]

    context "testing through `handleCommit`" $ do
      aroundM_ (withFeatureFlags (FeatureFlagConfigDbo (Map.toList (FodChecks ~> Percentage 100)))) $ do
        let config =
              cs
                $ unindent
                  [i|
                    builds:
                      include:
                        - "*.x86_64-linux.default"
                  |]
        it "logs a Error error on lying fods" $ GH.withFakeGithubInterface $ \ghState -> do
          output <- mkRandomOutput
          validButMalicious <- mkFodFlake Nothing output
          buildDrvPath . fst =<< testDerivation validButMalicious "default"
          lying <- mkFodFlake Nothing (output & #output .~ "bar")
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.setupWithConfig lying (Just config)) $ \commitInfo -> do
            logs <- captureLogs_ $ resolve =<< Orchestrator.handleCommit mempty True commitInfo
            let errorLogs = filter (\logItem -> (logItem ^. #severity) == Error) logs
            let log = cs $ msg $ fromSingleton errorLogs
            lyingDerivation <- fst <$> testDerivation lying "fod"
            log `shouldContainM` ("hash mismatch in fixed-output derivation '" <> cs lyingDerivation <> "'")
            log `shouldContainM` ("specified: " <> cs (output ^. #outputHash) <> "\n")
            barOutputHash <- getOutputHash "bar"
            log `shouldContainM` ("got:    " <> cs barOutputHash <> "\n")

        it "doesn't interfere with new (unbuilt) benign fods" $ GH.withFakeGithubInterface $ \ghState -> do
          output <- mkRandomOutput
          flake <- mkFodFlake Nothing output
          void $ testDerivation flake "default"
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.setupWithConfig flake (Just config)) $ \commitInfo -> do
            criticalLog <-
              captureLogs_ (resolve =<< Orchestrator.handleCommit mempty True commitInfo)
                <&> filter (\logItem -> (logItem ^. #severity) == Error)
            criticalLog `shouldBeM` []

      it "creates a CI check when enabled" $ GH.withFakeGithubInterface $ \ghState -> do
        output <- mkRandomOutput
        flake <- mkFodFlake Nothing output
        (drvPath, _) <- testDerivation flake "default"
        buildDrvPath drvPath
        let garnixConfig = "fodChecks: true"
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.setupWithConfig flake (Just garnixConfig)) $ \commitInfo -> do
          let reporter = mkGithubReporter (commitInfo ^. repoInfo) (commitInfo ^. commit)
          resolve =<< Orchestrator.handleCommit reporter True commitInfo
          (fodDrv, _) <- testDerivation flake "fod"
          verifiedFodsShouldBe [fodDrv]

setUpXdgCacheDir :: M a -> M a
setUpXdgCacheDir action = withSystemTempDirectory "garnix-xdg-cache-dir" $ \dir ->
  local (#nixXdgCacheDir ?~ dir) action

data TestOutput = TestOutput
  { output :: Text,
    outputHash :: Text
  }
  deriving (Generic, Show)

mkRandomOutput :: (MonadIO m) => m TestOutput
mkRandomOutput = do
  n :: Int <- randomIO
  let output = show n
  hash <- getOutputHash output
  pure $ TestOutput output hash

getOutputHash :: (MonadIO m) => Text -> m Text
getOutputHash output =
  liftIO $ inTempDirectory $ do
    T.writeFile "file" (output <> "\n")
    StdoutTrimmed (hash :: Text) <-
      run
        $ cmd "nix"
        & addArgs (T.words "hash path file --base64")
        & nixConfDefaults
    pure ("sha256-" <> hash)

mkFodFlake :: Maybe Text -> TestOutput -> M Text
mkFodFlake fodCommand testOutput = do
  let hash = testOutput ^. #outputHash
  let out = testOutput ^. #output
  pure
    $ cs
    $ unindent
      [i|
        {
          outputs = {...} : {
            packages.x86_64-linux = rec {
              fod = derivation {
                name = "fod";
                system = "x86_64-linux";
                outputHashMode = "recursive";
                outputHashAlgo = "sha256";
                outputHash = "#{hash}";
                builder = "/bin/sh";
                args = ["-c" "#{fromMaybe "" fodCommand}\necho #{out} > $out"];
              };
              default = derivation {
                name = "depends-on-fod-at-build-time";
                system = "x86_64-linux";
                builder = "/bin/sh";
                args = ["-c" "# ${fod}\necho > $out"];
              };
            };
          };
        }
      |]

testDerivation :: Text -> Text -> M (Nix.DrvPath, Nix.StorePath)
testDerivation flake packageName = do
  liftBaseOp_ inTempDirectory $ do
    liftIO $ T.writeFile "flake.nix" flake
    StdoutTrimmed output <-
      run
        $ cmd "nix"
        & addArgs
          [ "eval",
            ".#" <> packageName,
            "--apply",
            "x : {storePath = builtins.toString x; drvPath = x.drvPath; }",
            "--json" :: Text
          ]
        & nixConfDefaults
    let drvPath = Nix.DrvPath $ fromRight $ Nix.parseStorePath (output ^. key "drvPath" . _String)
    let storePath = fromRight $ Nix.parseStorePath (output ^. key "storePath" . _String)
    pure (drvPath, storePath)

buildDrvPath :: Nix.DrvPath -> M ()
buildDrvPath drvPath = do
  runSubProcess_
    $ cmd "nix"
    & addArgs ["build", (cs drvPath <> "^*") :: Text]
    & nixConfDefaults

getVerifiedFods :: M [(Text, Text)]
getVerifiedFods =
  DB.pgQuery
    [pgSQL| SELECT drv_hash, store_path_hash FROM verified_fods; |]

verifiedFodsShouldBe :: [Nix.DrvPath] -> M ()
verifiedFodsShouldBe drvPaths =
  (map fst <$> getVerifiedFods) `shouldReturnM` fmap (cs . Nix.getHash . Nix.getDrvPath) drvPaths
