{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Garnix.Build.Evaluation
  ( evaluateAttribute,
    Stdout (..),
    Stderr (..),
    RanCommand (..),
    ParsingError (..),
    NumberOfParsedResults (..),
    EvaluationResult (..),
    EvaluateError (..),
    _TimeoutReached,
    _NixEvaluationError,
    _ParseError,
    _UnexpectedNumberOfParsedResults,
  )
where

import Cradle (ExitCode)
import Cradle hiding (ExitCode)
import Data.Aeson ((.:))
import Data.Aeson qualified as JSON
import Data.Aeson.Types qualified as JSON
import Data.ByteString (ByteString)
import Data.Either.Extra (mapLeft)
import Garnix.Async
import Garnix.Build.Types (EvaluationResult (..))
import Garnix.Incremental
import Garnix.Limits qualified as Limits
import Garnix.Monad
import Garnix.Monad.Bubbling
import Garnix.Monad.Metrics
import Garnix.Monad.Pool (withPoolM)
import Garnix.Nix.Plan qualified as Nix
import Garnix.Nix.Types qualified as Nix
import Garnix.NixConfig (addNixConfigEnvironment)
import Garnix.Prelude
import Garnix.Sandbox
import Garnix.Types as Types

newtype Stdout = Stdout ByteString
  deriving newtype (Show)

newtype Stderr = Stderr ByteString
  deriving newtype (Show)

newtype ParsingError = ParsingError String
  deriving newtype (Show)

newtype NumberOfParsedResults = NumberOfParsedResults Int
  deriving newtype (Show)

newtype RanCommand = RanCommand String
  deriving newtype (Show)

data EvaluateError
  = TimeoutReached
  | NixEvaluationError Stderr RanCommand
  | ParseError ParsingError Stdout Stderr
  | AppEvalParseError Stdout Stderr
  | UnexpectedNumberOfParsedResults NumberOfParsedResults Stdout
  | AttributeIsSourceOutput Text
  deriving stock (Show)

makePrisms ''EvaluateError

data NixBuildPackage
  = SourceOutput Text
  | Derivation Nix.DrvPath Nix.BuildOutputs

instance FromJSON NixBuildPackage where
  parseJSON =
    \case
      JSON.String s -> pure $ SourceOutput $ cs s
      json -> flip (JSON.withObject "nix evaluate attribute") json $ \o -> do
        drvPath <- o .: "drvPath" >>= parseStorePath
        outputs <- o .: "outputs" >>= mapM parseStorePath
        pure $ Derivation (Nix.DrvPath drvPath) (Nix.BuildOutputs outputs)
    where
      parseStorePath :: JSON.Value -> JSON.Parser Nix.StorePath
      parseStorePath v = do
        text :: Text <- parseJSON v
        case Nix.parseStorePath text of
          Left err -> JSON.parseFail $ cs err
          Right path -> pure path

evaluateAttribute ::
  RepoConfig ->
  String ->
  FilePath ->
  Build ->
  Text ->
  M (Either EvaluateError EvaluationResult)
evaluateAttribute repoConfig cacheDir workingDir build attr = withBubbling $ \bubble -> do
  evalTimeoutDuration <- liftIO Limits.evalTimeout
  evalRes <-
    timingAs #evalDrvPathTime
      $ withPoolM nixEvalPool (build ^. repoUser)
      $ logDuration "nix eval for drvPath"
      $ withTextSpan ("phase", "eval")
      $ timeout evalTimeoutDuration
      $ do
        mDrvPath <-
          if build ^. packageType == TypeApp
            then bubble =<< mapLeft (uncurry AppEvalParseError) <$> getAppDrvPath repoConfig build workingDir cacheDir attr
            else pure Nothing
        result <- runNixEval (maybe attr cs mDrvPath)
        case result of
          (ExitFailure _, _, StderrRaw stderr) ->
            pure
              $ Left
              $ NixEvaluationError
                (Stderr stderr)
                (RanCommand $ cs $ "prlimit --as=" <> show (toBytes maxEvalMem) <> " nix build " <> attr <> " --dry-run --json")
          (ExitSuccess, StdoutRaw stdout, StderrRaw stderr) ->
            case mDrvPath of
              Nothing ->
                case JSON.eitherDecodeStrict' @[NixBuildPackage] stdout of
                  Left err -> pure $ Left $ ParseError (ParsingError err) (Stdout stdout) (Stderr stderr)
                  Right [result] -> case result of
                    SourceOutput src -> pure $ Left $ AttributeIsSourceOutput src
                    Derivation drvPath outputs -> do
                      plan <- Nix.getPlanOf stderr
                      pure $ Right $ EvaluationResult drvPath (Nix.planOutputs plan) outputs
                  Right xs -> pure $ Left $ UnexpectedNumberOfParsedResults (NumberOfParsedResults $ length xs) (Stdout stdout)
              Just drvPath -> do
                plan <- Nix.getPlanOf stderr
                pure $ Right $ EvaluationResult drvPath (Nix.planOutputs plan) (Nix.BuildOutputs mempty)
  bubble $ fromMaybe (Left TimeoutReached) evalRes
  where
    maxEvalMem :: Memory
    maxEvalMem = repoConfig ^. maxEvalMemory

    runNixEval :: Text -> M (ExitCode, StdoutRaw, StderrRaw)
    runNixEval attr' = do
      let withIncrementalExtraArgs action
            | build ^. wantsIncrementalism = withIntermediatesFlake build $ \case
                Nothing -> action [] []
                Just intermediates ->
                  action
                    [ "--override-input",
                      "garnix-incrementalize",
                      "path:" <> cs intermediates
                    ]
                    [(intermediates, ReadOnly)]
            | otherwise = action [] []
      nixConfig <- view #userNixConfig
      withIncrementalExtraArgs $ \incArgs extraSandboxPaths ->
        (>>= run)
          $ cmd "comment"
          & addArgs
            ( [ buildComment build,
                "--",
                "prlimit",
                "--as=" <> show (toBytes maxEvalMem),
                "nix",
                "build",
                attr' <> "^*",
                "--dry-run",
                "--extra-experimental-features",
                "fetch-closure",
                "--json"
              ]
                <> incArgs
            )
          & addNixConfigEnvironment nixConfig
          & setWorkingDir workingDir
          & pure
          & inNixSandbox extraSandboxPaths (Just cacheDir)

getAppDrvPath :: RepoConfig -> Build -> FilePath -> FilePath -> Text -> M (Either (Stdout, Stderr) (Maybe Nix.DrvPath))
getAppDrvPath repoConfig build workingDir cacheDir attr = do
  let appDerivation = "a: [{ context = builtins.attrNames (builtins.getContext a.program); }]"
  nixConfig <- view #userNixConfig
  (exitCode, StdoutRaw stdout, StderrRaw stderr) <-
    (>>= run)
      $ cmd "comment"
      & addArgs
        [ buildComment build,
          "--",
          "prlimit",
          "--as=" <> show (toBytes $ repoConfig ^. maxEvalMemory),
          "nix",
          "eval",
          attr,
          "--apply",
          appDerivation,
          "--json"
        ]
      & addNixConfigEnvironment nixConfig
      & setWorkingDir workingDir
      & pure
      & inNixSandbox [] (Just cacheDir)
  pure $ case exitCode of
    ExitFailure _ -> Left (Stdout stdout, Stderr stderr)
    ExitSuccess -> Right $ parseNixEvalOutput stdout
  where
    parseNixEvalOutput :: ByteString -> Maybe Nix.DrvPath
    parseNixEvalOutput stdout =
      JSON.decode (cs stdout) >>= singletonList >>= JSON.parseMaybe go
      where
        singletonList :: JSON.Value -> Maybe JSON.Value
        singletonList =
          \case
            JSON.Array [single] -> Just single
            _ -> Nothing
        go :: JSON.Value -> JSON.Parser Nix.DrvPath
        go json = flip (JSON.withObject "app") json $ \o -> do
          textDrvPath :: [Text] <- o .: "context"
          path <- case textDrvPath of
            [singlePath] -> pure singlePath
            _ -> JSON.parseFail "AppPackage: expecting single context derivation"
          case Nix.parseStorePath path of
            Left err -> JSON.parseFail $ cs err
            Right path -> pure $ Nix.DrvPath path
