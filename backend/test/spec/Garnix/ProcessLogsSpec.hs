module Garnix.ProcessLogsSpec (spec) where

import Control.Concurrent
import Cradle
import Data.Map qualified as Map
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text qualified as T
import Data.Time
import Garnix.BuildLogs
import Garnix.BuildLogs.Types (LogLine (LogLine))
import Garnix.Monad
import Garnix.Monad.SubProcess
import Garnix.Prelude
import Garnix.TestHelpers hiding (shouldReturn)
import Garnix.Types
import System.Directory (copyFile)
import Test.Hspec

spec :: Spec
spec = describe "ProcessLogs" $ do
  around_ (addNixExperimentalFeatures ["nix-command", "flakes"]) $ do
    describe "processInternalJsonBuildLogs" $ do
      let mkCollector :: M (a -> M (), M [a])
          mkCollector = do
            elems <- liftIO $ newMVar []
            let push elem = liftIO $ putMVar elems . (elem :) =<< takeMVar elems
                getAll = reverse <$> liftIO (takeMVar elems)
            pure (push, getAll)
          buildFlake flake processor = do
            mockRemote <- view #workingDir
            liftIO $ writeFile (mockRemote </> "flake.nix") flake
            liftIO $ copyFile "../flake.lock" (mockRemote </> "flake.lock")
            withUtf8LinesStream processor $ \logHandle -> do
              runSubProcess_
                $ cmd "nix"
                & addArgs
                  [ "build",
                    mockRemote <> "#packages.x86_64-linux.test-pkg",
                    "--log-lines",
                    "0",
                    "--print-build-logs",
                    "--log-format",
                    "internal-json" :: String
                  ]
                & addStderrHandle logHandle
          getLogsForFlake flake = runTestM $ do
            (collector, getLogs) <- mkCollector
            processor <- buildInternalLogProcessor collector <$> mkInternalLogProcessorState
            buildFlake flake processor
            getLogs
          mkPackageName baseName = do
            bustCache <- diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
            pure $ PackageName $ baseName <> "-" <> show bustCache

      it "transforms logs for flakes with no phases" $ do
        pkgName <- mkPackageName "test-pkg"
        let flake =
              [i|
              {
                outputs = { self }: {
                  packages.x86_64-linux.test-pkg = derivation {
                    name = "#{getPackageName pkgName}";
                    builder = "/bin/sh";
                    args = ["-c" "echo no phase && echo > $out"];
                    system = "x86_64-linux";
                  };
                };
              }
            |]
        filter (isJust . (^. #package)) <$> getLogsForFlake flake
          `shouldReturn` [LogLine (Just pkgName) Nothing "no phase"]

      it "handles flakes with multiple phases" $ do
        pkgName <- mkPackageName "test-pkg"
        let flake =
              [i|
              {
                inputs.nixpkgs.url = "#{testNixpkgsUrl}";
                outputs = { self, nixpkgs }: let
                  inherit (import nixpkgs { system = "x86_64-linux"; }) stdenv;
                in {
                  packages.x86_64-linux.test-pkg = stdenv.mkDerivation {
                    name = "#{getPackageName pkgName}";
                    unpackPhase = "echo unpacking";
                    patchPhase = "echo patching";
                    configurePhase = "echo configuring";
                    buildPhase = "echo building";
                    checkPhase = "echo checking";
                    installPhase = "echo installing";
                    fixupPhase = "echo fixingup && touch $out";
                  };
                };
              }
            |]
        filter (\p -> isJust (p ^. #package)) <$> getLogsForFlake flake
          `shouldReturn` [ LogLine (Just pkgName) Nothing "Running phase: unpackPhase",
                           LogLine (Just pkgName) (Just "unpackPhase") "unpacking",
                           LogLine (Just pkgName) (Just "unpackPhase") "Running phase: patchPhase",
                           LogLine (Just pkgName) (Just "patchPhase") "patching",
                           LogLine (Just pkgName) (Just "patchPhase") "Running phase: updateAutotoolsGnuConfigScriptsPhase",
                           LogLine (Just pkgName) (Just "updateAutotoolsGnuConfigScriptsPhase") "Running phase: configurePhase",
                           LogLine (Just pkgName) (Just "configurePhase") "configuring",
                           LogLine (Just pkgName) (Just "configurePhase") "Running phase: buildPhase",
                           LogLine (Just pkgName) (Just "buildPhase") "building",
                           LogLine (Just pkgName) (Just "buildPhase") "Running phase: installPhase",
                           LogLine (Just pkgName) (Just "installPhase") "installing",
                           LogLine (Just pkgName) (Just "installPhase") "Running phase: fixupPhase",
                           LogLine (Just pkgName) (Just "fixupPhase") "fixingup"
                         ]

      it "handles derivations with dependencies" $ do
        pkgAName <- mkPackageName "pkg-a"
        pkgBName <- mkPackageName "pkg-b"
        pkgCName <- mkPackageName "pkg-c"
        pkgName <- mkPackageName "test-pkg"
        let flake =
              [i|
              {
                outputs = { self }: let
                  pkg-a = derivation {
                    name = "#{getPackageName pkgAName}";
                    builder = "/bin/sh";
                    args = ["-c" "echo building-a && echo output-a > $out"];
                    system = "x86_64-linux";
                  };
                  \# pkg-b depends on pkg-a
                  pkg-b = derivation {
                    name = "#{getPackageName pkgBName}";
                    builder = "/bin/sh";
                    args = ["-c" "echo building-b && read a < ${pkg-a} && echo $a && echo output-b > $out"];
                    system = "x86_64-linux";
                  };
                  pkg-c = derivation {
                    name = "#{getPackageName pkgCName}";
                    builder = "/bin/sh";
                    args = ["-c" "echo building-c && echo output-c > $out"];
                    system = "x86_64-linux";
                  };
                in {
                  \# test-pkg depends on pkg-b & pkg-c
                  packages.x86_64-linux.test-pkg = derivation {
                    name = "#{getPackageName pkgName}";
                    builder = "/bin/sh";
                    args = ["-c" "read b < ${pkg-b} && read c < ${pkg-c} && echo $b && echo $c && echo > $out"];
                    system = "x86_64-linux";
                  };
                };
              }
            |]
        filter (isJust . (^. #package)) <$> getLogsForFlake flake
          `shouldReturn` [ LogLine (Just pkgCName) Nothing "building-c",
                           LogLine (Just pkgAName) Nothing "building-a",
                           LogLine (Just pkgBName) Nothing "building-b",
                           LogLine (Just pkgBName) Nothing "output-a",
                           LogLine (Just pkgName) Nothing "output-b",
                           LogLine (Just pkgName) Nothing "output-c"
                         ]

      it "handles phases in dependencies" $ do
        let shouldContainJust :: (HasCallStack, Show a, Eq a) => Maybe [a] -> Maybe [a] -> IO ()
            shouldContainJust actual expected =
              case (actual, expected) of
                (Just actual, Just expected) -> actual `shouldContain` expected
                _ -> actual `shouldBe` expected
        pkgAName <- mkPackageName "pkg-a"
        pkgBName <- mkPackageName "pkg-b"
        pkgName <- mkPackageName "test-pkg"
        let flake =
              [i|
              {
                inputs.nixpkgs.url = "#{testNixpkgsUrl}";
                outputs = { self, nixpkgs }: let
                  pkgs = import nixpkgs { system = "x86_64-linux"; };
                  mkDerivationWithPhases = name: text: pkgs.stdenv.mkDerivation {
                    inherit name;
                    unpackPhase = "echo unpacking ${text}";
                    patchPhase = "echo patching ${text}";
                    configurePhase = "echo configuring ${text}";
                    buildPhase = "echo building ${text}";
                    checkPhase = "echo checking ${text}";
                    installPhase = "echo installing ${text}";
                    fixupPhase = "echo fixingup ${text} && echo output-${text} > $out";
                  };
                in {
                  packages.x86_64-linux.test-pkg = pkgs.runCommand "#{getPackageName pkgName}" {} ''
                    echo building main pkg
                    cat ${mkDerivationWithPhases "#{getPackageName pkgAName}" "pkga"}
                    cat ${mkDerivationWithPhases "#{getPackageName pkgBName}" "pkgb"}
                    echo done
                    touch $out
                  '';
                };
              }
            |]
        logs <- getLogsForFlake flake
        let byPackageName =
              Map.fromListWith (flip mappend)
                $ fmap (\(LogLine pkgName phase logLine) -> (pkgName, [(phase, logLine)])) logs
        Map.lookup (Just pkgAName) byPackageName
          `shouldContainJust` Just
            [ (Nothing, "Running phase: unpackPhase"),
              (Just "unpackPhase", "unpacking pkga"),
              (Just "unpackPhase", "Running phase: patchPhase"),
              (Just "patchPhase", "patching pkga"),
              (Just "patchPhase", "Running phase: updateAutotoolsGnuConfigScriptsPhase"),
              (Just "updateAutotoolsGnuConfigScriptsPhase", "Running phase: configurePhase"),
              (Just "configurePhase", "configuring pkga"),
              (Just "configurePhase", "Running phase: buildPhase"),
              (Just "buildPhase", "building pkga"),
              (Just "buildPhase", "Running phase: installPhase"),
              (Just "installPhase", "installing pkga"),
              (Just "installPhase", "Running phase: fixupPhase"),
              (Just "fixupPhase", "fixingup pkga")
            ]
        Map.lookup (Just pkgBName) byPackageName
          `shouldContainJust` Just
            [ (Nothing, "Running phase: unpackPhase"),
              (Just "unpackPhase", "unpacking pkgb"),
              (Just "unpackPhase", "Running phase: patchPhase"),
              (Just "patchPhase", "patching pkgb"),
              (Just "patchPhase", "Running phase: updateAutotoolsGnuConfigScriptsPhase"),
              (Just "updateAutotoolsGnuConfigScriptsPhase", "Running phase: configurePhase"),
              (Just "configurePhase", "configuring pkgb"),
              (Just "configurePhase", "Running phase: buildPhase"),
              (Just "buildPhase", "building pkgb"),
              (Just "buildPhase", "Running phase: installPhase"),
              (Just "installPhase", "installing pkgb"),
              (Just "installPhase", "Running phase: fixupPhase"),
              (Just "fixupPhase", "fixingup pkgb")
            ]
        Map.lookup (Just pkgName) byPackageName
          `shouldContainJust` Just
            [ (Nothing, "building main pkg"),
              (Nothing, "output-pkga"),
              (Nothing, "output-pkgb"),
              (Nothing, "done")
            ]

  describe "processMessage" $ do
    it "removes ANSI escape characters" $ do
      processMessage "a\x1b[93;41mc" `shouldBe` "ac\n"

    it "does not remove spaces" $ do
      processMessage "  a c  " `shouldBe` "  a c  \n"
      processMessage "  \na c  " `shouldBe` "  \na c  \n"

    it "never includes any Github token" $ do
      let logs =
            cs
              . unindent
              $ [i|
                foo
                ghs_puMag5LeethueBee2oof
                bar
                git clone https://x-access-token:ghs_puMag5LeethueBee2oof@github.com/foo/bar.git
              |]
          expected =
            cs
              . unindent
              $ [i|
                foo
                XXXXXXXXXXXXXXXX
                bar
                git clone https://x-access-token:XXXXXXXXXXXXXXXX@github.com/foo/bar.git
              |]
      processMessage logs `shouldBe` expected

    it "isn't too strict about matching on github tokens" $ do
      map processMessage ["ghc_options\n", "ghr_foo\n", "ghc_somelongstringhere\n"]
        `shouldBe` ["ghc_options\n", "ghr_foo\n", "ghc_somelongstringhere\n"]

  describe "processLogsForGithub" $ do
    it "never exceeds 65535 characters" $ do
      let log = T.replicate 1000 (T.replicate 1000 "x" <> "\n")
      let len = T.length (processLogsForGithub (RawLogs log))
      len `shouldSatisfy` (< 65535)
      len `shouldSatisfy` (>= 65000)
