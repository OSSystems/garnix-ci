module Garnix.Nix.StorePath
  ( withStorePath,
    getClosure,

    -- * exported for tests
    _getOutputs,
    _withGcRoot,
  )
where

import Cradle
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Row (Rec, type (.==))
import Data.Text qualified as T
import Garnix.Monad
import Garnix.Monad.SubProcess (runSubProcess, runSubProcess_)
import Garnix.Nix.Types qualified as Nix
import Garnix.NixConfig (addNixConfigEnvironment)
import Garnix.Prelude
import Garnix.Types
import System.Directory (removeFile)
import System.IO.Temp (withSystemTempDirectory)

type NixDerivationShowOutput =
  Map Text (Rec ("outputs" .== Map Text (Rec ("path" .== Text))))

_getOutputs :: Nix.DrvPath -> M (Map Text Nix.StorePath)
_getOutputs drvPath = do
  nixConfig <- view #userNixConfig
  (StdoutTrimmed output, StderrRaw _, exit) <-
    run
      $ cmd "nix"
      & addArgs ["derivation", "show", cs drvPath :: Text]
      & addNixConfigEnvironment nixConfig
  case exit of
    ExitSuccess -> do
      parsed :: NixDerivationShowOutput <- aesonDecode "nix derivation show output" Nix.parseDerivationShow output
      let raw :: Map Text Text =
            fmap (Nix.ensureStorePrefix . (^. #path))
              $ Map.fromList
              $ mconcat
              $ map (\x -> Map.toList (x ^. #outputs))
              $ Map.elems parsed
      forM raw $ \storePath -> do
        case Nix.parseStorePath storePath of
          Right storePath -> pure storePath
          Left error -> throw $ OtherError error
    ExitFailure _ -> do
      throw $ OtherError "Could not get outputs from derivation"

_withGcRoot :: Nix.StorePath -> (FilePath -> M a) -> M a
_withGcRoot storePath action =
  withSystemTempDirectory "garnix-gc" $ \tempDir -> do
    let rootPath = tempDir </> cs (Nix.getHash storePath)
    bracket (acquire rootPath) release action
  where
    acquire rootPath = do
      workingDir <- view #workingDir
      nixConfig <- view #userNixConfig
      runSubProcess_
        $ cmd "nix-store"
        & addArgs ["--add-root", cs rootPath, "--indirect", "--realize", cs storePath :: Text]
        & setWorkingDir workingDir
        & addNixConfigEnvironment nixConfig
        & silenceStdout
      pure rootPath
    release = liftIO . removeFile . cs

withStorePath :: Build -> Text -> (Maybe Nix.StorePath -> M a) -> M a
withStorePath build output action = case outputsForBuild build >>= Nix.getOutputByName output of
  Just storePath -> _withGcRoot storePath $ const $ action $ Just storePath
  Nothing -> do
    case (build ^. drvPath :: Maybe FilePath) of
      Nothing -> action Nothing
      Just drvPath -> do
        log Warning $ "Build missing store path for " <> output <> ", but did have drvPath. Falling back to old logic"
        drvStorePath <- case Nix.parseStorePath drvPath of
          Left error -> do
            log Critical error
            throw $ OtherError error
          Right path -> pure path
        _withGcRoot drvStorePath $ \_ -> do
          storePathsMap <- _getOutputs $ Nix.DrvPath drvStorePath
          case Map.lookup output storePathsMap of
            Nothing -> action Nothing
            Just storePath -> _withGcRoot storePath $ const $ action $ Just storePath

-- | Returns nothing if the passed store path is not valid (for example if passed a store path of a failed build)
getClosure :: Nix.StorePath -> M (Maybe [Nix.StorePath])
getClosure drvPath = do
  result <-
    try
      $ runSubProcess
      $ cmd "nix-store"
      & addArgs ["--query", "--requisites", cs drvPath :: Text]
  case result of
    Right (StdoutRaw output) -> do
      paths <- forM (T.lines $ cs output) $ \line -> case Nix.parseStorePath line of
        Left _ -> throw $ OtherError $ "cannot parse 'nix-store --query' output: " <> cs output
        Right storePath -> pure storePath
      pure $ Just paths
    Left (ErrorWithContext {err = RunProcessError {stdErr}})
      | "error: path '" `T.isPrefixOf` stdErr && "' is not valid\n" `T.isSuffixOf` stdErr -> pure Nothing
    Left e -> throwError e
