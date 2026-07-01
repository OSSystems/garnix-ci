-- | Batched flake evaluation via @nix-eval-jobs@.
--
-- The per-attribute path ('Garnix.Build.Evaluation.evaluateAttribute') runs one
-- cold @nix build --dry-run@ per attribute, each re-evaluating the flake and
-- everything it imports. This module evaluates all of a commit's batchable
-- attributes in one @nix-eval-jobs@ run instead, so that shared work is done
-- once per worker rather than once per attribute.
--
-- The run starts when the commit's builds are set up, runs beside them and is
-- cancelled with them ('withBatchEval'). Under 'Batch', a build takes its
-- drvPath from the run ('batchedDrvPath') and only dry-runs the @.drv@ for
-- @toUpload@ and the outputs.
module Garnix.Build.BatchEval
  ( withBatchEval,
    batchedDrvPath,
    -- exported for testing
    Batched (..),
    lookupBatched,
    buildJobsetExpr,
    parseEvalJobsOutput,
  )
where

import Control.Concurrent.Async.Lifted (waitCatch, withAsync)
import Cradle hiding (ExitCode)
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as JSON
import Data.Aeson.Types qualified as JSON
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Map qualified as Map
import Data.Text qualified as T
import Garnix.Async
import Garnix.Attribute
import Garnix.Limits qualified as Limits
import Garnix.Monad
import Garnix.Monad.Metrics
import Garnix.Monad.Pool (poolSize, withPoolSlotsM)
import Garnix.Nix.Types qualified as Nix
import Garnix.NixConfig (addNixConfigEnvironment)
import Garnix.Prelude
import Garnix.Sandbox
import Garnix.Types
import System.IO.Temp (withSystemTempDirectory)

-- | Whether a build can take its drvPath from a batch. Incremental builds
-- evaluate against an overridden input and apps go through their @program@,
-- so both keep the per-attribute path.
isBatchable :: Build -> Bool
isBatchable build =
  not (build ^. wantsIncrementalism) && build ^. packageType /= TypeApp

-- | The name an attribute has in the jobset and in nix-eval-jobs' output.
batchKey :: Attribute -> Text
batchKey = review asAttribute

-- | Run nix-eval-jobs over the batchable builds beside @action@, which sees
-- the run through 'evalBatch'. Does nothing under 'PerAttr'. The run is
-- cancelled with @action@, and its @.drv@s stay rooted until then: builds
-- dry-run and build them after the run has exited, possibly minutes later.
withBatchEval :: RepoConfig -> CommitInfo -> FlakeDir -> [Build] -> M a -> M a
withBatchEval repoConfig commitInfo flakeDir builds action = do
  strategy <- view #evalStrategy
  case map attribute (filter isBatchable builds) of
    attributes
      | strategy == Batch,
        not (null attributes) ->
          withSystemTempDirectory "garnix-eval-jobs-roots" $ \gcRootsDir ->
            withAsync (evaluateFlakeBatch repoConfig (commitInfo ^. repoInfo . repoId . repoUser) flakeDir gcRootsDir attributes)
              $ \batch -> local (#evalBatch ?~ batch) action
    _ -> action

-- | What the commit's nix-eval-jobs run has for one build.
data Batched
  = -- | No run covers this build: 'PerAttr', or not 'isBatchable'.
    NotBatched
  | -- | The run did not finish: it failed, timed out or was cancelled.
    BatchFailed
  | -- | The run finished without it, e.g. because it is not a derivation.
    BatchMissed
  | -- | The run's drvPath, or its eval error.
    BatchEvaluated (Either Text Nix.DrvPath)
  deriving stock (Eq, Show)

-- | Waits for the commit's run to finish.
lookupBatched :: Build -> M Batched
lookupBatched build =
  view #evalBatch >>= \case
    Just batch | isBatchable build -> do
      result <- liftIO $ waitCatch batch
      pure $ case result of
        Right (Right (Just drvs)) ->
          maybe BatchMissed BatchEvaluated $ Map.lookup (batchKey (attribute build)) drvs
        _ -> BatchFailed
    _ -> pure NotBatched

-- | The drvPath the commit's run produced for this build. An eval error
-- gives 'Nothing' as well: a worker carries heap over from earlier
-- attributes, so an attribute that fails in the batch may still fit alone.
batchedDrvPath :: Build -> M (Maybe Nix.DrvPath)
batchedDrvPath build =
  lookupBatched build >>= \case
    BatchEvaluated (Right drv) -> pure $ Just drv
    _ -> pure Nothing

-- | 'Nothing' if the run could not finish.
evaluateFlakeBatch :: RepoConfig -> GhRepoOwner -> FlakeDir -> FilePath -> [Attribute] -> M (Maybe BatchResult)
evaluateFlakeBatch repoConfig repoOwner flakeDir gcRootsDir attributes = do
  absFlakeDir <- safeGetAbsoluteFlakeDir flakeDir
  -- One attribute's limit for the whole run: past it, falling back to
  -- per-attribute evals costs less than waiting longer.
  evalTimeoutDuration <- liftIO Limits.evalTimeout
  -- Each worker is an evaluator, so it takes a slot of the per-attribute pool.
  evaluators <- views #nixEvalPool poolSize
  let workers = minimum [Limits.evalJobsWorkers, length attributes, evaluators]
  result <-
    timingAs #evalFlakeBatchTime
      $ withPoolSlotsM nixEvalPool workers repoOwner
      $ timeout evalTimeoutDuration
      $ logDuration "nix-eval-jobs"
      $ withTextSpan ("phase", "eval-batch")
      $ lockedFlakeUrl absFlakeDir
      >>= traverse (runEvalJobs repoConfig gcRootsDir workers . flip buildJobsetExpr attributes)
  when (isNothing result) $ log Warning "nix-eval-jobs timed out; evaluating per attribute instead"
  pure $ parseEvalJobsOutput <$> join result

-- | The locked URL nix resolves @\<dir\>@ to, e.g.
-- @git+file:\/\/\<repo\>?dir=sub&rev=...@, so that the run can be pure like the
-- per-attribute eval. @builtins.getFlake@ on the bare path would load it as a
-- @path:@ flake instead: no @self.rev@, a different @self.outPath@, and so
-- different drvPaths than @\<dir\>#attr@ gets. 'Nothing' for a dirty tree,
-- which only has an unlocked URL: e.g. after a stale @flake.lock@ was
-- rewritten.
lockedFlakeUrl :: FilePath -> M (Maybe Text)
lockedFlakeUrl flakeDir = do
  cacheDir <- getNixXdgCacheDir
  nixConfig <- view #userNixConfig
  workingDir <- view #workingDir
  (exitCode, StdoutRaw stdout, StderrRaw stderr) <-
    (>>= run)
      $ cmd "nix"
      & addArgs ["flake", "metadata", "--json", flakeDir]
      & addNixConfigEnvironment nixConfig
      & setWorkingDir workingDir
      & pure
      & inNixSandbox [] (Just cacheDir)
  let parseLockedUrl = JSON.withObject "flake metadata" $ \o -> do
        _ :: Text <- o .: "revision"
        o .: "url"
  case (exitCode, JSON.decodeStrict stdout >>= JSON.parseMaybe parseLockedUrl) of
    (ExitSuccess, Just url) -> pure $ Just url
    (ExitSuccess, Nothing) -> do
      log Notice "flake is not a clean git tree; evaluating per attribute instead"
      pure Nothing
    (ExitFailure _, _) -> do
      log Warning $ "could not resolve the flake for nix-eval-jobs: " <> cs stderr
      pure Nothing

-- | A flat attrset from each 'batchKey' to the derivation garnix builds for
-- it, after the same projection as 'addNixosExtension', from the flake at a
-- 'lockedFlakeUrl'.
buildJobsetExpr :: Text -> [Attribute] -> Text
buildJobsetExpr flakeUrl attributes =
  T.unlines
    $ [ "let",
        "  flake = builtins.getFlake " <> nixString flakeUrl <> ";",
        "  getPath = builtins.foldl' (acc: k: acc.${k}) flake;",
        "in {"
      ]
    <> map entry attributes
    <> ["}"]
  where
    entry :: Attribute -> Text
    entry attr =
      let segments = T.splitOn "." $ review asAttribute (addNixosExtension attr)
       in "  " <> nixString (batchKey attr) <> " = getPath [ " <> T.unwords (map nixString segments) <> " ];"

    -- Attribute names come from the flake, so they may hold anything.
    nixString :: Text -> Text
    nixString t = "\"" <> T.replace "${" "\\${" (T.replace "\"" "\\\"" (T.replace "\\" "\\\\" t)) <> "\""

-- | One line of nix-eval-jobs' JSONL output.
data EvalJobLine = EvalJobLine
  { lineAttrPath :: [Text],
    lineDrvPath :: Maybe Text,
    lineError :: Maybe Text
  }

instance JSON.FromJSON EvalJobLine where
  parseJSON = JSON.withObject "nix-eval-jobs line" $ \o ->
    EvalJobLine
      <$> o
      .: "attrPath"
      <*> o
      .:? "drvPath"
      <*> o
      .:? "error"

-- | Keyed by the unescaped attribute path: nix-eval-jobs quotes names that
-- contain dots in @attr@, which every 'batchKey' does.
parseEvalJobsOutput :: ByteString -> BatchResult
parseEvalJobsOutput stdout =
  Map.fromList
    [ (T.intercalate "." (lineAttrPath line), toResult line)
    | Just line <- JSON.decodeStrict <$> BSC.lines stdout
    ]
  where
    toResult :: EvalJobLine -> Either Text Nix.DrvPath
    toResult line = case (lineError line, lineDrvPath line) of
      (Just err, _) -> Left err
      (Nothing, Just drv) -> either (Left . ("unparseable drvPath: " <>)) Right $ Nix.parseDrvPath drv
      (Nothing, Nothing) -> Left "nix-eval-jobs reported neither a drvPath nor an error"

-- | Runs under the same sandbox and @prlimit --as@ as the per-attribute eval.
-- The limit is per process, so it binds each worker as it binds each
-- per-attribute @nix@; workers restart at half of it so that heap they keep
-- between attributes does not run into it.
runEvalJobs :: RepoConfig -> FilePath -> Int -> Text -> M ByteString
runEvalJobs repoConfig gcRootsDir workers = mockable #evalJobsMock $ \expr -> do
  cacheDir <- getNixXdgCacheDir
  nixConfig <- view #userNixConfig
  workingDir <- view #workingDir
  let maxEvalMemory' = toBytes (repoConfig ^. maxEvalMemory)
  (exitCode, StdoutRaw stdout, StderrRaw stderr) <-
    (>>= run)
      $ cmd "prlimit"
      & addArgs
        [ "--as=" <> show maxEvalMemory',
          "nix-eval-jobs",
          -- nix-eval-jobs only evaluates --flake purely. Impure, the flake
          -- could read the access tokens in NIX_CONFIG with getEnv.
          "--option",
          "pure-eval",
          "true",
          "--gc-roots-dir",
          cs gcRootsDir,
          "--workers",
          show workers,
          "--max-memory-size",
          show (maxEvalMemory' `div` (2 * 1024 * 1024)),
          "--expr",
          cs expr
        ]
      & addNixConfigEnvironment nixConfig
      & setWorkingDir workingDir
      & pure
      & inNixSandbox [(gcRootsDir, ReadWrite)] (Just cacheDir)
  case exitCode of
    ExitSuccess -> pure ()
    -- Per-attribute errors come back as JSON lines with exit 0, so this is the
    -- run itself failing. Lines it printed before that are still good.
    ExitFailure code -> log Warning $ "nix-eval-jobs exited with " <> show code <> ": " <> cs stderr
  pure stdout
