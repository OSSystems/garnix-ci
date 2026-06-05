module Garnix.Build.Package where

import Control.Concurrent.Async.Lifted
import Control.Lens
import Cradle
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UUID (UUID)
import Garnix.Async
import Garnix.Attribute
import Garnix.Build.Evaluation
import Garnix.Build.FodCheck qualified as FodCheck
import Garnix.Build.Reporting
import Garnix.BuildLogs
import Garnix.BuildLogs.Types (LogLine (LogLine), mkLogLine)
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Limits qualified as Limits
import Garnix.Monad
import Garnix.Monad.Concurrency
import Garnix.Monad.Metrics
import Garnix.Monad.SubProcess
import Garnix.Nix.Types (DrvPath)
import Garnix.NixConfig (addNixConfigEnvironment)
import Garnix.Prelude
import Garnix.S3Cache qualified as S3Cache
import Garnix.Sandbox
import Garnix.Types as Types
import System.Random (randomIO)

doBuild :: Maybe FodChecker -> RunReporter -> BuildKind -> FlakeDir -> RepoConfig -> Build -> M Build
doBuild fodChecker runReporter buildKind flakeDir repoConfig initialBuild = do
  attr <- localAttr flakeDir (attribute initialBuild)
  withMessage ("Running build for " <> attr) $ do
    withSpan (initialBuild ^. id, initialBuild ^. packageType, initialBuild ^. system, initialBuild ^. package) $ do
      -- Catch both IO exceptions and M errors
      ( runBuild
          `catchError` \_ -> do
            returnFailedBuild
        )
        `catch` \e -> do
          log Warning $ "An exception occurred while building the package: " <> show (e :: SomeException)
          returnFailedBuild
  where
    returnFailedBuild :: M Build
    returnFailedBuild = do
      let build = initialBuild & status ?~ Failure
      reportBuildResult runReporter build
      pure build

    runBuild :: M Build
    runBuild = do
      build <- buildPkg fodChecker runReporter buildKind flakeDir repoConfig initialBuild <?> "Starting build"
      persistence <- getPersistenceName flakeDir build
      let updatedBuild = build & persistenceName .~ persistence
      reportBuildResult runReporter updatedBuild <?> "Reporting final build result"
      pure updatedBuild

getPersistenceName :: FlakeDir -> Build -> M (Maybe Text)
getPersistenceName flakeDir b = do
  let isNixos = b ^. packageType == TypeNixosConfiguration
      succeeded = b ^. status == Just Success
  workingDir <- view #workingDir
  cacheDir <- getNixXdgCacheDir
  nixConfig <- view #userNixConfig
  flakeDir' <- safeGetAbsoluteFlakeDir flakeDir
  if isNixos && succeeded
    then do
      (exit, StdoutRaw s) <-
        (>>= run)
          $ cmd "nix"
          & addArgs
            [ "eval",
              cs flakeDir' <> "#nixosConfigurations." <> cs (b ^. package),
              "--apply",
              "c : if c.config.garnix.server.persistence.enable then c.config.garnix.server.persistence.name else null",
              "--json" :: Text
            ]
          & addNixConfigEnvironment nixConfig
          & setWorkingDir workingDir
          & silenceStderr
          & pure
          & inNixSandbox [] (Just cacheDir)

      pure $ case exit of
        ExitFailure _ -> Nothing
        ExitSuccess -> case Aeson.decodeStrict @Text s of
          Just "" -> Nothing
          other -> other
    else pure Nothing

buildPkg ::
  (HasCallStack) =>
  Maybe FodChecker ->
  RunReporter ->
  BuildKind ->
  FlakeDir ->
  RepoConfig ->
  Build ->
  M Build
buildPkg = curry6
  $ mockable #buildPkgMock
  $ \(fodChecker, runReporter, buildKind, flakeDir, repoConfig, build) -> do
    incrementEvent #packageBuildsAttempted
    cacheDir <- getNixXdgCacheDir
    attr <- localAttr flakeDir . addNixosExtension . attribute $ build
    workingDir <- view #workingDir
    evalRes <- evaluateAttribute repoConfig cacheDir workingDir build attr
    case evalRes of
      Right evaluationResult -> do
        let drvPath' = evaluationResult ^. #derivation
        FodCheck.fodCheck fodChecker drvPath'
        build <-
          if null $ evaluationResult ^. #toUpload
            then do
              log Informational $ "No derivations to upload for " <> cs drvPath'
              DB.setBuildUploaded (build ^. id)
              pure $ build
                & status ?~ Success
                & alreadyBuilt ?~ True
            else do
              let builder = runNixBuild runReporter cacheDir workingDir build drvPath'
              status' <- withAsync builder $ \q -> do
                abortOnCancellation build q
              log Informational "buildPkg: build finished, checking status"
              forkM $ do
                S3Cache.upload runReporter (build ^. repoUser) (build ^. repoName) evaluationResult (build ^. repoIsPublic)
                DB.setBuildUploaded (build ^. id)
              case status' of
                Failure -> log Warning "build failed"
                Cancelled -> log Notice "build cancelled"
                _ -> pure ()
              pure $ build
                & status ?~ status'
                & alreadyBuilt ?~ False
        buildEnd <- liftIO getCurrentTime
        pure $ build
          & drvPath ?~ cs (evaluationResult ^. #derivation)
          & outputPaths ?~ BuildOutputsPgColumn (evaluationResult ^. #outputs)
          & endTime ?~ buildEnd
      Left err -> do
        build <- do
          evalEnd <- liftIO getCurrentTime
          pure $ build
            & status ?~ Failure
            & endTime ?~ evalEnd
            & alreadyBuilt ?~ False
        case err of
          (AttributeIsSourceOutput src) -> do
            log Warning $ "found src derivation: " <> cs src
            reportLogs runReporter $ mkLogLine $ "failed output is source path: not supported (" <> cs src <> ")"
            pure build
          TimeoutReached -> do
            let message = "Timed out during nix evaluation of: " <> attr
            reportLogs runReporter $ mkLogLine message
            pure $ build
              & status ?~ Timeout
              & endTime .~ Nothing
          (NixEvaluationError (Stderr errorMessage) (RanCommand command)) -> do
            log Warning $ "package evaluation failed: " <> cs errorMessage
            case buildKind of
              Webhook -> do
                reportLogs runReporter $ LogLine (Just $ build ^. package) Nothing $ "failed running package evaluation. If you have `nix` installed, you can reproduce the error locally by running: " <> cs command
                reportLogs runReporter $ LogLine (Just $ build ^. package) Nothing $ cs errorMessage
              ModulePreview -> do
                log Critical
                  $ "Module evaluation error for https://github.com/"
                  <> getGhLogin (getGhRepoOwner $ build ^. repoUser)
                  <> "/"
                  <> getGhRepoName (build ^. repoName)
                  <> "/commit/"
                  <> getCommitHash (build ^. gitCommit)
                  <> " error: "
                  <> cs errorMessage
                liftIO (T.readFile (workingDir </> "flake.nix")) >>= log Informational

                reportLogs runReporter
                  $ LogLine (Just $ build ^. package) Nothing
                  $ T.unlines
                    [ "Package evaluation failed. The error message is:",
                      "",
                      cs errorMessage,
                      "",
                      "(This may be caused by a misconfiguration on your part. Go to https://garnix.io/modules/configure to correct this. This could also be a bug in the module. Consider opening an issue on https://github.com/garnix-io/issues/issues."
                    ]
            pure build
          (AppEvalParseError (Stdout stdout) (Stderr err)) -> do
            log Critical
              $ "parsing nix eval for apps failed "
              <> cs err
              <> " ("
              <> cs stdout
              <> ")"
            reportLogs runReporter $ mkLogLine "failed parsing nix outputs for app: unexpected format"
            pure build
          (ParseError (ParsingError err) (Stdout json) _stderr) -> do
            log Critical
              $ "parsing nix build dry-run failed "
              <> cs err
              <> " ("
              <> cs json
              <> ")"
            reportLogs runReporter $ mkLogLine "failed parsing nix outputs: unexpected format"
            pure build
          (UnexpectedNumberOfParsedResults (NumberOfParsedResults 0) _stdout) -> do
            log Critical "parsing nix build dry-run failed: empty result"
            reportLogs runReporter $ mkLogLine "failed parsing nix outputs: empty result"
            pure build
          (UnexpectedNumberOfParsedResults (NumberOfParsedResults num) (Stdout stdout)) -> do
            log Critical $ "parsing nix build dry-run failed: got " <> show num <> " results" <> " (" <> cs stdout <> ")"
            reportLogs runReporter $ mkLogLine "failed parsing nix outputs: too many results"
            pure build

abortOnCancellation :: Build -> Async (Either ErrorWithContext Status) -> M Status
abortOnCancellation build builder = do
  let go = do
        b <- DB.getBuild $ build ^. id
        case b ^. status of
          Just Cancelled -> pure ()
          _ -> threadDelay (fromSeconds @Int 30) >> go
  withAsync go $ \isCancelled -> do
    result <- waitEither builder isCancelled
    pure $ case result of
      Left status -> status
      Right _ -> Cancelled

runNixBuild :: RunReporter -> String -> FilePath -> Build -> DrvPath -> M Status
runNixBuild runReporter cacheDir workingDir build drvPath = do
  nixConfig <- view #userNixConfig
  log Informational $ "runNixBuild: using nixConfig '" <> show nixConfig <> "'"
  processor <- buildInternalLogProcessor (reportLogs runReporter) <$> mkInternalLogProcessorState <?> "runNixBuild: buildInternalLogProcessor"
  -- This is a unique ID for the outlink (which is stored in the working dir) so
  -- that nothing gets garbage collected until the working dir is.
  uuid :: UUID <- randomIO
  buildTimeoutDuration <- liftIO Limits.buildTimeout
  mExitCode <-
    withTextSpan ("phase", "build") $ do
      withUtf8LinesStream processor $ \logHandle -> do
        timeout buildTimeoutDuration
          $ (>>= run)
          $ cmd "comment"
          & addArgs
            [ buildComment build,
              "--",
              "nix",
              "build",
              T.append (cs drvPath) "^*",
              "--log-lines",
              "0",
              "--print-build-logs",
              "--log-format",
              "internal-json",
              "--out-link",
              show uuid
            ]
          & addNixConfigEnvironment nixConfig
          & addStdoutHandle logHandle
          & addStderrHandle logHandle
          & setWorkingDir workingDir
          & pure
          & inNixSandbox [] (Just cacheDir)
  log Informational $ "runNixBuild: exit code is " <> show mExitCode
  pure $ case mExitCode of
    Nothing -> Timeout
    Just (ExitFailure _) -> Failure
    Just ExitSuccess -> Success
