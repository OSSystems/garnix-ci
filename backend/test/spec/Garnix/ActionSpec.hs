module Garnix.ActionSpec (spec) where

import Control.Exception.Safe qualified as Safe
import Control.Lens
import Cradle qualified
import Data.ByteString qualified as ByteString
import Data.Map.Strict ((!))
import Data.Maybe (fromJust)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Development.Shake qualified as Shake
import Garnix.API.Keys (getActionPublicKey)
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Monad
import Garnix.Monad.Async (resolve)
import Garnix.Orchestrator qualified as Orchestrator
import Garnix.Prelude
import Garnix.Reporters.GithubReporter (mkGithubReporter)
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.NixosVmScripts (getActionRunnerVmScript)
import Garnix.Types hiding (context, head)
import Garnix.YamlConfig (ActionSandboxType (..))
import System.Directory (makeAbsolute)
import System.Exit (ExitCode (ExitSuccess))
import System.IO qualified as IO
import System.IO.Temp
import Test.Hspec

spec :: Spec
spec = do
  aroundAll withActionRunner $ inMWith $ aroundM_ suppressLogsWhenPassing $ beforeM_ truncateDBM $ do
    context "actions @slow @skip-ci" $ do
      let testHandleCommitWith sandboxType ghState flake =
            let sandboxStr :: String
                sandboxStr = case sandboxType of
                  FastStartup -> "fast-startup"
                  SharedResources -> "shared-resources"
                commitInfo =
                  defaultCommitInfo
                    & repoInfo . ghRepoOwner .~ "garnix-io"
                    & repoInfo . ghRepoName .~ "repo"
                    & reqUser .~ "garnix-io"
                yaml =
                  cs
                    [i|
                      actions:
                        - on: push
                          run: test-action
                          sandboxType: #{sandboxStr}
                    |]
             in GH.withLocalRepo ghState "garnix-io" "repo" identity commitInfo (GH.setupWithConfig flake $ Just yaml) $ \commitInfo -> do
                  let reporter = mkGithubReporter (commitInfo ^. repoInfo) (commitInfo ^. commit)
                  void $ try $ resolve =<< Orchestrator.handleCommit reporter True commitInfo
                  pure commitInfo
      let testHandleCommit = testHandleCommitWith FastStartup
      let flakeFromProgramPath program =
            cs
              [i|
                {
                  # If you update this, update also places where it matches.
                  # Search for INNER_NIXPKGS_MATCHES
                  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11-small";
                  outputs = { self, nixpkgs }: {
                    apps.x86_64-linux.test-action = {
                      type = "app";
                      program =
                        let
                          pkgs = import nixpkgs { system = "x86_64-linux"; };
                        in
                          #{program};
                    };
                  };
                }
              |]
      let flakeFromScript :: String -> Text
          flakeFromScript script =
            flakeFromProgramPath
              [i|
                builtins.toString (
                  pkgs.writeScript "script.sh"
                    ''#!${pkgs.bash}/bin/bash
                      #{script}
                    ''
                )
              |]

      context "building actions" $ do
        let flake = flakeFromScript "echo thing-that-should-be-in-logs"
            failingFlake =
              cs
                [i|
                  {
                    outputs = {self}: {
                      packages.x86_64-linux.test-action = derivation {};
                    };
                  }
                |]
        it "builds apps that are defined as actions" $ GH.withFakeGithubInterface $ \ghState -> do
          commitInfo <- testHandleCommit ghState flake
          builds <- DB.getLatestBuildsMatching (commitInfo ^. repoInfo) (commitInfo ^. commit)
          packageAndStatus builds
            `shouldBeM` [ ("Build starting", Success),
                          ("test-action", Success)
                        ]

        it "registers a failed build for failing apps" $ GH.withFakeGithubInterface $ \ghState -> do
          commitInfo <- testHandleCommit ghState failingFlake
          builds <- DB.getLatestBuildsMatching (commitInfo ^. repoInfo) (commitInfo ^. commit)
          packageAndStatus builds
            `shouldBeM` [ ("Build starting", Failure),
                          ("test-action", Failure),
                          ("test-action", Failure)
                        ]

        it "creates a report for building the app" $ GH.withFakeGithubInterface $ \ghState -> do
          void $ testHandleCommit ghState flake
          report <- GH.getReports ghState >>= GH.assertSingleRunForReport "app test-action"
          uniq (fmap (^. _2 . status) report) `shouldBeM` [RunReportStatusInProgress, RunReportStatusSuccess]

        it "send github a failure report if the prerequisites fail to build" $ GH.withFakeGithubInterface $ \ghState -> do
          let testDebounceDuration = fromMilliSeconds @Int 100
          local (#githubLogDebounceDuration .~ testDebounceDuration) $ do
            void $ testHandleCommit ghState failingFlake
            threadDelay (testDebounceDuration `addDuration` testDebounceDuration)
            buildReports <- GH.getReports ghState >>= GH.assertSingleRunForReport "app test-action"
            buildReports `GH.reportsShouldBe` [RunReportStatusInProgress, RunReportStatusFailure]

      context "running actions" $ do
        it "does not run SharedResources action for orgs that are not garnix-io" $ GH.withFakeGithubInterface $ \ghState -> do
          let commitInfo =
                defaultCommitInfo
                  & repoInfo . ghRepoOwner .~ "some-other-org"
                  & repoInfo . ghRepoName .~ "repo"
                  & reqUser .~ "some-other-org"
              yaml =
                cs
                  [i|
                    actions:
                      - on: push
                        run: test-action
                        sandboxType: shared-resources
                  |]
              flake = flakeFromScript "echo test-message"
          GH.withLocalRepo ghState "some-other-org" "repo" identity commitInfo (GH.setupWithConfig flake $ Just yaml) $ \commitInfo -> do
            let reporter = mkGithubReporter (commitInfo ^. repoInfo) (commitInfo ^. commit)
            void $ try $ resolve =<< Orchestrator.handleCommit reporter True commitInfo
            report <- GH.getReports ghState >>= GH.assertSingleRunForReport "action test-action"
            let finalReport = report ^?! _last . _2
            (finalReport ^. status, finalReport ^. logs)
              `shouldBeM` (RunReportStatusFailure, "You are not allowed to run actions with the 'shared-resources'. If you want access, get in touch with us.\n")

        it "runs the action on the server" $ GH.withFakeGithubInterface $ \ghState -> do
          void
            $ testHandleCommit ghState
            $ flakeFromScript "echo test-message"
          report <- GH.getReports ghState >>= GH.assertSingleRunForReport "action test-action"
          let finalReport = report ^?! _last . _2
          (finalReport ^. status, finalReport ^. logs) `shouldBeM` (RunReportStatusSuccess, "test-message\n")

        it "does not run the action and reports an error if the program does not start with /nix/store/*" $ GH.withFakeGithubInterface $ \ghState -> do
          void
            $ testHandleCommit ghState
            $ flakeFromProgramPath
              [i|
                let
                  file = pkgs.writeText "file" "aaaaaaaaaaaaaaaa";
                in "cat ${file}"
              |]
          report <- GH.getReports ghState >>= GH.assertSingleRunForReport "action test-action"
          let finalReport = report ^?! _last . _2
          (finalReport ^. status) `shouldBeM` RunReportStatusFailure
          (finalReport ^. logs . to getRawLogs) `shouldMatchRegexp` "The action's 'program' needs to be a \\(path\\) from a derivation. Program is: 'cat \\/nix\\/store\\/.*-file'"

        it "does not run the action and reports an error if the program is passed arguments" $ GH.withFakeGithubInterface $ \ghState -> do
          void
            $ testHandleCommit ghState
            $ flakeFromProgramPath
              [i|
                let
                  script = pkgs.writeScript "script.sh"
                    ''
                      #!${pkgs.bash}/bin/bash
                      echo test-message
                      exit 1
                    '';
                in "${script} with some args"
              |]
          report <- GH.getReports ghState >>= GH.assertSingleRunForReport "action test-action"
          let finalReport = report ^?! _last . _2
          (finalReport ^. status) `shouldBeM` RunReportStatusFailure
          (finalReport ^. logs . to getRawLogs) `shouldMatchRegexp` "The action's 'program' is wrong. Please make sure the path exists and that you're not trying to pass arguments via the 'program' field. Program is: '\\/nix\\/store\\/.*-script.sh with some args'"

        it "reports an error to the user if the path is incorrect" $ GH.withFakeGithubInterface $ \ghState -> do
          void
            $ testHandleCommit ghState
            $ cs
              [i|
                {
                  # If you update this, update also places where it matches.
                  # Search for INNER_NIXPKGS_MATCHES
                  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11-small";
                  outputs = { self, nixpkgs }: {
                    apps.x86_64-linux.test-action = {
                      type = "app";
                      program =
                        let
                          pkgs = import nixpkgs { system = "x86_64-linux"; };
                          script = pkgs.writeScript "script.sh"
                            ''
                              #!${pkgs.bash}/bin/bash
                              echo test-message
                              exit 1
                            '';
                        in "${script}/bad/path";
                    };
                  };
                }
                |]
          report <- GH.getReports ghState >>= GH.assertSingleRunForReport "action test-action"
          let finalReport = report ^?! _last . _2
          (finalReport ^. status) `shouldBeM` RunReportStatusFailure
          (finalReport ^. logs . to getRawLogs) `shouldMatchRegexp` "The action's 'program' is wrong. Please make sure the path exists and that you're not trying to pass arguments via the 'program' field. Program is: '\\/nix\\/store\\/.*-script.sh\\/bad\\/path'"

        it "reports failing actions" $ GH.withFakeGithubInterface $ \ghState -> do
          void
            $ testHandleCommit ghState
            $ flakeFromScript
              [i|
                echo test-message
                exit 1
              |]
          report <- GH.getReports ghState >>= GH.assertSingleRunForReport "action test-action"
          let finalReport = report ^?! _last . _2
          (finalReport ^. status) `shouldBeM` RunReportStatusFailure
          getRawLogs (finalReport ^. logs) `shouldBeM` "test-message\n"

        it "includes stderr in logs" $ GH.withFakeGithubInterface $ \ghState -> do
          void
            $ testHandleCommit ghState
            $ flakeFromScript "echo test-message >&2"
          report <- GH.getReports ghState >>= GH.assertSingleRunForReport "action test-action"
          let finalReport = report ^?! _last . _2
          getRawLogs (finalReport ^. logs) `shouldBeM` "test-message\n"

        it "runs actions in a writeable empty directory" $ GH.withFakeGithubInterface $ \ghState -> do
          commitInfo <-
            testHandleCommit ghState
              $ flakeFromScript
                [i|
                  echo test-message > test-file
                  cat test-file
                |]
          reports <-
            GH.getSimpleReports ghState
              <&> GH.filterByName "action test-action"
          reports
            `shouldBeM` ( (commitInfo ^. commit)
                            ~> ("action test-action" ~> (RunReportStatusSuccess, "test-message\n"))
                        )

        it "runs actions in a fresh temporary directory" $ GH.withFakeGithubInterface $ \ghState -> do
          void
            $ testHandleCommit ghState
            $ flakeFromScript "echo test-message > test-file"
          commitInfo <- testHandleCommit ghState $ flakeFromScript "ls -a"
          reports <-
            GH.getSimpleReports ghState
              <&> GH.filterByName "action test-action"
          reports
            ! (commitInfo ^. commit)
            `shouldBeM` ("action test-action" ~> (RunReportStatusSuccess, ".\n..\n"))

        it "allows network access" $ GH.withFakeGithubInterface $ \ghState -> do
          commitInfo <-
            testHandleCommit ghState
              $ flakeFromScript "curl -sv garnix.io 2>&1 | grep -i location"
          reports <-
            GH.getSimpleReports ghState
              <&> GH.filterByName "action test-action"
          reports
            ! (commitInfo ^. commit)
            `shouldBeM` ("action test-action" ~> (RunReportStatusSuccess, "< Location: https://garnix.io/\n"))

        it "allows process substitution and redirection" $ GH.withFakeGithubInterface $ \ghState -> do
          void
            $ testHandleCommit ghState
            $ flakeFromScript "read line < <(echo worked) && echo $line"
          report <- GH.getReports ghState >>= GH.assertSingleRunForReport "action test-action"
          let finalReport = report ^?! _last . _2
          getRawLogs (finalReport ^. logs) `shouldBeM` "worked\n"
          finalReport ^. status `shouldBeM` RunReportStatusSuccess

        it "provides `/bin/sh` and `/usr/bin/env`" $ GH.withFakeGithubInterface $ \ghState -> do
          commitInfo <-
            testHandleCommit ghState
              $ flakeFromScript "/usr/bin/env foo=bar /bin/sh -c 'echo $foo'"
          reports <-
            GH.getSimpleReports ghState
              <&> GH.filterByName "action test-action"
          reports
            ! (commitInfo ^. commit)
            `shouldBeM` ("action test-action" ~> (RunReportStatusSuccess, "bar\n"))

        it "times out the action" $ shouldTerminate (fromSeconds @Int 30) $ GH.withFakeGithubInterface $ \ghState -> do
          local (#action . #timeoutDuration .~ fromSeconds @Int 1) $ do
            commitInfo <-
              testHandleCommit ghState
                $ flakeFromScript "sleep inf"
            waitFor (fromSeconds @Int 1) $ do
              reports <-
                GH.getSimpleReports ghState
                  <&> GH.filterByName "action test-action"
              reports
                ! (commitInfo ^. commit)
                `shouldBeM` ("action test-action" ~> (RunReportStatusFailure, "The action took too long to complete and it was cancelled.\n"))

        it "injects GARNIX_CI, GARNIX_BRANCH, and GARNIX_COMMIT_SHA" $ GH.withFakeGithubInterface $ \ghState -> do
          commitInfo <-
            testHandleCommit ghState
              $ flakeFromScript
              $ unlines
              $ (\e -> "echo \"" <> e <> " = $" <> e <> "\"")
              <$> [ "GARNIX_CI",
                    "GARNIX_BRANCH",
                    "GARNIX_COMMIT_SHA"
                  ]
          reports <-
            GH.getSimpleReports ghState
              <&> GH.filterByName "action test-action"
          reports
            ! (commitInfo ^. commit)
            `shouldBeM` ( "action test-action"
                            ~> ( RunReportStatusSuccess,
                                 T.unlines
                                   [ "GARNIX_CI = true",
                                     "GARNIX_BRANCH = " <> commitInfo ^. branch . to fromJust . to getBranch,
                                     "GARNIX_COMMIT_SHA = " <> commitInfo ^. commit . to getCommitHash
                                   ]
                               )
                        )

        context "shared-resources sandbox" $ do
          let testHandleCommit = testHandleCommitWith SharedResources
          it "allows accessing /dev/kvm" $ GH.withFakeGithubInterface $ \ghState -> do
            commitInfo <-
              testHandleCommit ghState
                $ flakeFromScript "ls /dev/kvm"
            reports <-
              GH.getSimpleReports ghState
                <&> GH.filterByName "action test-action"
            reports
              ! (commitInfo ^. commit)
              `shouldBeM` ("action test-action" ~> (RunReportStatusSuccess, "/dev/kvm\n"))
          it "allows running initdb (postgres)" $ GH.withFakeGithubInterface $ \ghState -> do
            void
              $ testHandleCommit ghState
              $ flakeFromScript "PGDATA=$(mktemp -d) ${pkgs.postgresql}/bin/pg_ctl initdb"
            report <- GH.getReports ghState >>= GH.assertSingleRunForReport "action test-action"
            let finalReport = report ^?! _last . _2
            finalReport ^. status `shouldBeM` RunReportStatusSuccess

          it "has a separate network" $ GH.withFakeGithubInterface $ \ghState -> do
            void
              $ testHandleCommit ghState
              $ flakeFromScript "ip addr | grep tap0 && echo found"
            report <- GH.getReports ghState >>= GH.assertSingleRunForReport "action test-action"
            let finalReport = report ^?! _last . _2
            finalReport ^. status `shouldBeM` RunReportStatusSuccess
            (last . lines . cs . getRawLogs $ finalReport ^. logs) `shouldBeM` "found"

          it "has timezone files" $ GH.withFakeGithubInterface $ \ghState -> do
            void
              $ testHandleCommit ghState
              $ flakeFromScript "([[ -d \"$TZDIR\" ]] || exit 1)"
            report <- GH.getReports ghState >>= GH.assertSingleRunForReport "action test-action"
            let finalReport = report ^?! _last . _2
            finalReport ^. status `shouldBeM` RunReportStatusSuccess

          it "works with perl's DateTime" $ GH.withFakeGithubInterface $ \ghState -> do
            void
              $ testHandleCommit ghState
              $ flakeFromScript "${pkgs.perl.withPackages (p: with p; [ DateTime ])}/bin/perl -MDateTime::TimeZone::Local -e 'DateTime::TimeZone::Local->TimeZone'"
            report <- GH.getReports ghState >>= GH.assertSingleRunForReport "action test-action"
            let finalReport = report ^?! _last . _2
            finalReport ^. status `shouldBeM` RunReportStatusSuccess

      context "secrets" $ do
        it "gives access to the secret key for that action"
          $ GH.withFakeGithubInterface
          $ \ghState -> do
            commitInfo <-
              testHandleCommit ghState
                $ flakeFromScript "cat $GARNIX_ACTION_PRIVATE_KEY_FILE"
            reports <- GH.getSimpleReports ghState
            let Just privateKey =
                  reports
                    ^? at (commitInfo ^. commit)
                      . _Just
                      . at "action test-action"
                      . _Just
                      . _2
            PublicKey actionKey <- getActionPublicKey "garnix-io" "repo" "test-action"
            let message = "hi there!"

            (Shake.Exit ExitSuccess, Shake.Stdout encrypted) <-
              liftIO
                $ Shake.cmd
                  ("age" :: String)
                  ["--recipient" :: String, cs actionKey]
                  (Shake.StdinBS message)
            withSystemTempFile "garnix-test-secret" $ \f h -> do
              liftIO $ ByteString.hPut h (cs privateKey)
              liftIO $ IO.hClose h
              Shake.Stdout decrypted <-
                liftIO
                  $ Shake.cmd
                    ("age" :: String)
                    ["--decrypt" :: String, "-i", f]
                    (Shake.StdinBS encrypted)
              decrypted `shouldBeM` message

        it "generates different keys for each action" $ do
          let count = 20
          keys <-
            forConcurrently [1 :: Int .. 20]
              $ \c -> getActionPublicKey "garnix-io" "repo" (PackageName $ "action" <> show c)
          length (nub keys) `shouldBeM` count

        it "adds the repo contents when withRepoContents is true" $ GH.withFakeGithubInterface $ \ghState -> do
          let yaml =
                cs
                  [i|
                    actions:
                      - on: push
                        run: test-action
                        withRepoContents: true
                  |]
              flake = flakeFromScript "ls -a"
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.setupWithConfig flake $ Just yaml) $ \commitInfo -> do
            let reporter = mkGithubReporter (commitInfo ^. repoInfo) (commitInfo ^. commit)
            void $ try $ resolve =<< Orchestrator.handleCommit reporter True commitInfo
            report <- GH.getReports ghState >>= GH.assertSingleRunForReport "action test-action"
            let finalReport = report ^?! _last . _2
            (finalReport ^. status, finalReport ^. logs)
              `shouldBeM` (RunReportStatusSuccess, ".\n..\n.git\nflake.lock\nflake.nix\ngarnix.yaml\nrandomness-file\n")

        it "does not give access to the key in PRs from forks" $ do
          GH.withFakeGithubInterface $ \ghState -> do
            let commitInfo =
                  defaultCommitInfo
                    & repoInfo . ghRepoOwner .~ "garnix-io"
                    & repoInfo . ghRepoName .~ "repo"
                    & prFromFork ?~ "somefork"
                yaml =
                  cs
                    [i|
                      actions:
                        - on: push
                          run: test-action
                    |]
                flake = flakeFromScript "cat $GARNIX_ACTION_PRIVATE_KEY_FILE"
            commitInfo <- GH.withLocalRepo ghState "garnix-io" "repo" identity commitInfo (GH.setupWithConfig flake $ Just yaml) $ \commitInfo -> do
              let reporter = mkGithubReporter (commitInfo ^. repoInfo) (commitInfo ^. commit)
              void $ try $ resolve =<< Orchestrator.handleCommit reporter True commitInfo
              pure commitInfo
            reports <- GH.getSimpleReports ghState
            let Just privateKey =
                  reports
                    ^? at (commitInfo ^. commit)
                      . _Just
                      . at "action test-action"
                      . _Just
                      . _2
            privateKey `shouldBeM` "none\n"

packageAndStatus :: [Build] -> [(PackageName, Status)]
packageAndStatus = catMaybes . fmap go
  where
    go :: Build -> Maybe (PackageName, Status)
    go b = (b ^. package,) <$> b ^. status

withActionRunner :: ((Env -> Env) -> IO ()) -> IO ()
withActionRunner inner = do
  withSystemTempDirectory "garnix-qemu" $ \qemuImageDir -> do
    Safe.bracket (startVm qemuImageDir) (liftIO . killThread) $ const $ do
      void $ waitFor (fromMinutes @Int 5) $ do
        threadDelay $ fromMilliSeconds @Int 200
        liftIO
          $ Cradle.run_
          $ Cradle.cmd "ssh"
          & Cradle.addArgs @Text
            [ "-oConnectTimeout=1",
              "-oStrictHostKeyChecking=no",
              "action-runner@localhost",
              "-i",
              "./dev-action-runner-ssh-key",
              "-p",
              "2299",
              "curl",
              "--fail",
              "google.com"
            ]
          & Cradle.silenceStdout
          & Cradle.silenceStderr
      actionRunnerKey <- liftIO $ makeAbsolute "./dev-action-runner-ssh-key"
      inner ((#action . #runnerHost .~ "localhost:2299") . (#action . #runnerSshKey .~ cs actionRunnerKey))
  where
    startVm :: FilePath -> IO ThreadId
    startVm qemuImageDir = do
      vmScript <- getActionRunnerVmScript
      fork $ do
        (_ :: Cradle.ExitCode, Cradle.StdoutRaw stdout, Cradle.StderrRaw stderr) <-
          Cradle.run
            $ Cradle.cmd vmScript
            & Cradle.modifyEnvVar "NIX_DISK_IMAGE" (const $ Just $ qemuImageDir </> "img.qcow2")
            & Cradle.modifyEnvVar "TMPDIR" (const $ Just qemuImageDir)
            & Cradle.modifyEnvVar "QEMU_NET_OPTS" (const $ Just "hostfwd=tcp::2299-:22")
            & Cradle.silenceStdout
            & Cradle.silenceStderr
        ByteString.putStr stdout
        ByteString.hPutStr IO.stderr stderr
        error "action-runner mock vm crashed unexpectedly"
