-- | Cancelling the work of commits that a newer commit superseded.
--
-- A run of a commit is registered on its 'Line' (a branch, or a pull request
-- from a fork) as soon as its webhook is handled, before the checkout. A run
-- of a newly pushed commit supersedes the line's runs of other commits right
-- then, whatever they are doing, so that a run still checking out cannot
-- miss it. Webhooks can arrive late, so when the line has runs of other
-- commits the forge is asked which commit is its head ('arrivalOn'): a late
-- one is superseded by the head instead of superseding it.
--
-- Whether a superseded run gives up its work is up to its own garnix.yaml
-- ('Garnix.YamlConfig.CancelSuperseded'), which it only knows after the
-- checkout. Once it does ('watchSupersession'), a run that gives up its work:
--
--   * stops evaluating, which is what costs the most: attribute discovery,
--     the nix-eval-jobs run and the per-attribute dry-runs;
--   * cancels each build that has not started its @nix build@ yet, including
--     those waiting for a build slot;
--   * lets a running @nix build@ carry on for as long as one of the
--     derivations it is building at that moment is in the closure of what a
--     live run needs, since killing it would only make that run build it
--     again from scratch instead of waiting for it. While the runs on its
--     own line have not all evaluated, that is not known yet, and it carries
--     on as well, as it does while it builds no derivation at all (it
--     starts, substitutes, or waits for a lock).
--
-- The state is per server process: a restart loses the runs anyway.
module Garnix.Supersede
  ( newSupersession,
    arrivalOn,
    onLine,
    watchSupersession,
    cancellableUntil,
    unlessCancelled,
    cancelledBy,
    supersededMessage,
    knowBuilds,
    cancellableBuild,
    buildEvaluated,
    startedBuilding,
    buildActivityStarted,
    buildActivityStopped,
    cancelsOnLine,
  )
where

import Control.Concurrent (newMVar)
import Control.Concurrent.Async.Lifted (race, withAsync)
import Control.Concurrent.Lifted (modifyMVar)
import Control.Concurrent.STM (STM, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Lens ((<&>))
import Cradle
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Garnix.Async (Promise)
import Garnix.BuildLogs.Internal (InternalId)
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Monad.Async (resolve, spawn)
import Garnix.Nix.Types qualified as Nix
import Garnix.NixConfig (addNixConfigEnvironment)
import Garnix.Prelude
import Garnix.Sandbox
import Garnix.Supersede.Types
import Garnix.Types as Types
import Garnix.YamlConfig (CancelSuperseded (..), ExcludeBranches (..))

newSupersession :: (MonadIO m) => Bool -> m Supersession
newSupersession enabled =
  liftIO
    $ Supersession enabled
    <$> newTVarIO mempty
    <*> newTVarIO 0
    <*> newMVar (NeededClosure mempty mempty)

-- | How a newly pushed commit comes onto the line. Only when the line has
-- runs of other commits, which it would supersede, does it ask @lineHead@
-- for the line's head. If that is not known, the commit is taken as the
-- head.
arrivalOn :: Line -> CommitHash -> M (Maybe CommitHash) -> M Arrival
arrivalOn line commit' lineHead = do
  supersession <- view #supersession
  contended <-
    liftIO
      $ atomically
      $ any ((/= commit') . lineRunCommit)
      . Map.findWithDefault [] line
      <$> readTVar (supersessionLines supersession)
  if supersessionEnabled supersession && contended
    then
      lineHead <&> \case
        Just head' | head' /= commit' -> Behind head'
        _ -> NewHead
    else pure NewHead

-- | Starts a run of the commit on the line with @start@, which sees the run
-- in 'lineRun', and keeps the run registered until the promise @start@ gives
-- resolves.
onLine :: Line -> CommitHash -> Arrival -> M (Promise a) -> M (Promise a)
onLine line commit' arrival start = do
  supersession <- view #supersession
  (run, superseded) <- liftIO $ register supersession line commit' arrival
  unless (null superseded)
    $ log Notice
    $ getCommitHash commit'
    <> " supersedes "
    <> T.intercalate ", " (map (getCommitHash . lineRunCommit) superseded)
    <> " on "
    <> show line
  case arrival of
    Behind head' | supersessionEnabled supersession -> log Notice $ getCommitHash commit' <> " arrived behind the head of " <> show line <> ", " <> getCommitHash head'
    _ -> pure ()
  let unregister = liftIO $ atomically $ modifyTVar' (supersessionLines supersession) (Map.update (without run) line)
  promise <- local (#lineRun ?~ run) start `onError` unregister
  spawn $ resolve promise `finally` unregister
  where
    without run runs = case filter ((/= lineRunId run) . lineRunId) runs of
      [] -> Nothing
      rest -> Just rest

register :: Supersession -> Line -> CommitHash -> Arrival -> IO (LineRun, [LineRun])
register supersession line commit' arrival = do
  let enabled = supersessionEnabled supersession
  run <-
    LineRun
      <$> atomically (readTVar (supersessionNextRunId supersession) <* modifyTVar' (supersessionNextRunId supersession) (+ 1))
      <*> pure line
      <*> pure commit'
      <*> newTVarIO (case arrival of Behind head' | enabled -> Just head'; _ -> Nothing)
      <*> newTVarIO Nothing
      <*> newTVarIO Nothing
      <*> newTVarIO mempty
  atomically $ do
    others <- Map.findWithDefault [] line <$> readTVar (supersessionLines supersession)
    superseded <-
      if arrival == NewHead && enabled
        then flip filterM others $ \other ->
          if lineRunCommit other == commit'
            then pure False
            else
              readTVar (lineRunSupersededBy other) >>= \case
                Just _ -> pure False
                Nothing -> writeTVar (lineRunSupersededBy other) (Just commit') $> True
        else pure []
    modifyTVar' (supersessionLines supersession) (Map.insert line (run : others))
    pure (run, superseded)

-- | Decides, once the current run is superseded, whether it gives up its work,
-- for as long as @action@ runs.
watchSupersession :: CommitInfo -> Maybe CancelSuperseded -> M a -> M a
watchSupersession commitInfo policy action =
  withLineRun action $ \run -> withAsync (decide run) (const action)
  where
    decide run = do
      by <- liftIO $ atomically $ readTVar (lineRunSupersededBy run) >>= maybe retry pure
      cancels <- cancelsOnLine commitInfo policy (lineRunLine run)
      liftIO $ atomically $ writeTVar (lineRunCancelling run) (Just cancels)
      if cancels
        then do
          log Notice $ "Superseded by " <> getCommitHash by <> "; cancelling the work left"
          case lineRunLine run of
            -- So that pushing the commit again builds it again.
            BranchLine repo branch' -> DB.markPushSuperseded repo (commitInfo ^. commit) branch'
            PullRequestLine _ _ -> pure ()
        else log Informational $ "Superseded by " <> getCommitHash by <> ", but cancelSuperseded keeps building it"

-- | Whether @cancelSuperseded@ cancels superseded runs on the line. When
-- unset, it does on every line but the repository's default branch. If
-- that is not known, the run is built to the end.
cancelsOnLine :: CommitInfo -> Maybe CancelSuperseded -> Line -> M Bool
cancelsOnLine commitInfo policy line = case (policy, line) of
  (Just (CancelSuperseded cancels), _) -> pure cancels
  (Just (CancelSupersededExceptOn (ExcludeBranches branches)), BranchLine _ branch') -> pure (branch' `notElem` branches)
  (Just (CancelSupersededExceptOn _), PullRequestLine _ _) -> pure True
  (Nothing, PullRequestLine _ _) -> pure True
  (Nothing, BranchLine repo branch') ->
    ( maybe False (/= branch')
        <$> getDefaultBranch (Just $ commitInfo ^. repoInfo . credentials) repo
    )
      `catchError` (\e -> logError e $> False)
      `catchAny` (\e -> logSomeException e $> False)

-- | Blocks until the run gives up its work, and gives the commit that
-- superseded it.
awaitCancelling :: LineRun -> STM CommitHash
awaitCancelling run =
  readTVar (lineRunCancelling run) >>= \case
    Just True -> readTVar (lineRunSupersededBy run) >>= maybe retry pure
    _ -> retry

-- | Whether the run gives up its work. While it is superseded but has not
-- decided yet, waits for it to.
settledCancelling :: LineRun -> STM Bool
settledCancelling run =
  readTVar (lineRunSupersededBy run) >>= \case
    Nothing -> pure False
    Just _ -> readTVar (lineRunCancelling run) >>= maybe retry pure

-- | The commit the current run gives up its work to, if it does. Only
-- within 'watchSupersession', which decides it.
cancelledBy :: M (Maybe CommitHash)
cancelledBy =
  withLineRun (pure Nothing) $ \run -> liftIO $ atomically $ do
    cancels <- settledCancelling run
    if cancels then readTVar (lineRunSupersededBy run) else pure Nothing

-- | Runs @action@, unless the current run gives up its work first. Then
-- 'Left' with the commit that superseded it.
unlessCancelled :: M a -> M (Either CommitHash a)
unlessCancelled action = cancellableUntil (const action)

-- | Like 'unlessCancelled', but only until @action@ calls the function it is
-- passed. That call blocks while the run is about to give up its work, so
-- the run is cancelled there rather than after it.
cancellableUntil :: (M () -> M a) -> M (Either CommitHash a)
cancellableUntil action =
  withLineRun (Right <$> action (pure ())) $ \run -> do
    committed <- liftIO $ newTVarIO False
    let commitTo = liftIO $ atomically $ do
          cancels <- settledCancelling run
          when cancels retry
          writeTVar committed True
        cancelled = liftIO $ atomically $ do
          by <- awaitCancelling run
          readTVar committed >>= \c -> when c retry
          pure by
    race cancelled (action commitTo)

supersededMessage :: CommitHash -> Text
supersededMessage by =
  "Cancelled: superseded by "
    <> getCommitHash by
    <> ". Set cancelSuperseded in garnix.yaml to build every commit."

-- | Tells the current run how many builds it has, all of which must call
-- 'buildEvaluated'.
knowBuilds :: Int -> M ()
knowBuilds count =
  withLineRun (pure ()) $ \run -> liftIO $ atomically $ writeTVar (lineRunUnevaluated run) (Just count)

-- | Runs a build of the current run, cancelled when the run gives up its
-- work: at once while it evaluates or waits for a build slot, and once its
-- @nix build@ builds derivations, none of which a live run needs, after that.
cancellableBuild :: M a -> M (Either CommitHash a)
cancellableBuild action = do
  progress <- liftIO $ BuildProgress <$> newTVarIO False <*> newTVarIO Nothing
  let action' = local (#buildProgress ?~ progress) action
  withLineRun (Right <$> action') $ \run -> do
    supersession <- view #supersession
    race (cancelWhenUnneeded supersession run progress) action'
      `finally` liftIO (atomically $ markEvaluated run progress Nothing)

cancelWhenUnneeded :: Supersession -> LineRun -> BuildProgress -> M CommitHash
cancelWhenUnneeded supersession run progress = do
  by <- liftIO $ atomically $ awaitCancelling run
  building <- liftIO $ readTVarIO (buildProgressBuilding progress)
  case building of
    Nothing -> pure by
    Just _ -> do
      log Informational "superseded while its nix build runs; waiting until it builds only what no live run needs"
      waitUntilUnneeded
      log Informational "superseded, and its nix build builds only what no live run needs"
      pure by
  where
    snapshot :: STM (Set Nix.DrvPath, Set Text)
    snapshot = do
      settled <- lineSettled supersession (lineRunLine run)
      unless settled retry
      (,) <$> liveDrvs supersession <*> (maybe mempty (Set.fromList . Map.elems) <$> readTVar (buildProgressBuilding progress))

    waitUntilUnneeded = do
      seen@(drvs, active) <- liftIO $ atomically snapshot
      needed <- neededClosure supersession drvs
      case needed of
        Just paths | not (null active), all (`Set.notMember` paths) active -> pure ()
        _ -> do
          liftIO $ atomically $ snapshot >>= \now -> when (now == seen) retry
          waitUntilUnneeded

-- | Whether every run on the line that is not giving up its work knows the
-- drvPath of each of its builds.
lineSettled :: Supersession -> Line -> STM Bool
lineSettled supersession line = do
  runs <- Map.findWithDefault [] line <$> readTVar (supersessionLines supersession)
  and <$> forM runs (\run -> isLive run >>= \live -> if live then (== Just 0) <$> readTVar (lineRunUnevaluated run) else pure True)

isLive :: LineRun -> STM Bool
isLive run = (/= Just True) <$> readTVar (lineRunCancelling run)

-- | The drvPaths of the builds of all runs that are not giving up their work.
liveDrvs :: Supersession -> STM (Set Nix.DrvPath)
liveDrvs supersession = do
  runs <- concat . Map.elems <$> readTVar (supersessionLines supersession)
  fmap mconcat $ forM runs $ \run -> isLive run >>= \live -> if live then readTVar (lineRunDrvs run) else pure mempty

-- | The closure of the drvPaths. 'Nothing' if nix could not tell.
neededClosure :: Supersession -> Set Nix.DrvPath -> M (Maybe (Set Text))
neededClosure supersession drvs =
  modifyMVar (supersessionNeededClosure supersession) $ \cached ->
    if neededFor cached == drvs
      then pure (cached, Just (neededPaths cached))
      else do
        let (known, missing) =
              if neededFor cached `Set.isSubsetOf` drvs
                then (neededPaths cached, drvs `Set.difference` neededFor cached)
                else (mempty, drvs)
        requisites (Set.toList missing) <&> \case
          Nothing -> (cached, Nothing)
          Just paths -> let new = NeededClosure drvs (known <> paths) in (new, Just (neededPaths new))

requisites :: [Nix.DrvPath] -> M (Maybe (Set Text))
requisites [] = pure $ Just mempty
requisites drvs = do
  cacheDir <- getNixXdgCacheDir
  nixConfig <- view #userNixConfig
  workingDir <- view #workingDir
  (exitCode, StdoutRaw stdout, StderrRaw stderr) <-
    (>>= run)
      $ cmd "nix-store"
      & addArgs (["--query", "--requisites"] <> map (cs @_ @Text) drvs)
      & addNixConfigEnvironment nixConfig
      & setWorkingDir workingDir
      & pure
      & inNixSandbox [] (Just cacheDir)
  case exitCode of
    ExitSuccess -> pure $ Just $ Set.fromList $ T.lines $ cs stdout
    ExitFailure _ -> do
      log Warning $ "could not get the closure of the derivations live runs need: " <> cs stderr
      pure Nothing

-- | Tells the current run that this build has its drvPath, or will not get one.
buildEvaluated :: Maybe Nix.DrvPath -> M ()
buildEvaluated drv = do
  run <- view #lineRun
  progress <- view #buildProgress
  for_ ((,) <$> run <*> progress) $ \(run', progress') ->
    liftIO $ atomically $ markEvaluated run' progress' drv

markEvaluated :: LineRun -> BuildProgress -> Maybe Nix.DrvPath -> STM ()
markEvaluated run progress drv = do
  done <- readTVar (buildProgressEvaluated progress)
  unless done $ do
    writeTVar (buildProgressEvaluated progress) True
    modifyTVar' (lineRunUnevaluated run) (fmap (subtract 1))
    for_ drv $ modifyTVar' (lineRunDrvs run) . Set.insert

-- | Tells the current build that its @nix build@ holds a build slot.
startedBuilding :: M ()
startedBuilding = withProgress $ \progress -> writeTVar (buildProgressBuilding progress) (Just mempty)

buildActivityStarted :: InternalId -> Text -> M ()
buildActivityStarted activity drv = withProgress $ \progress ->
  modifyTVar' (buildProgressBuilding progress) (fmap (Map.insert activity drv))

buildActivityStopped :: InternalId -> M ()
buildActivityStopped activity = withProgress $ \progress ->
  modifyTVar' (buildProgressBuilding progress) (fmap (Map.delete activity))

withLineRun :: M a -> (LineRun -> M a) -> M a
withLineRun outside inside = view #lineRun >>= maybe outside inside

withProgress :: (BuildProgress -> STM ()) -> M ()
withProgress f = view #buildProgress >>= traverse_ (liftIO . atomically . f)
