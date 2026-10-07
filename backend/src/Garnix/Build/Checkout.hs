module Garnix.Build.Checkout
  ( withCheckout,
    remoteWithConfig,
    Remote,
    runWithCheckout,
    withAuthorization,
    withBeforeAction,
    cleanRemote,
  )
where

import Control.Lens qualified as Lens
import Control.Lens.Regex.Text qualified as Regex
import Cradle
import Data.Aeson qualified as Aeson
import Garnix.Build.Helpers qualified as Internal
import Garnix.FlakeInputAuthorization (InputAuthorization (..), checkAuthorization)
import Garnix.Monad
import Garnix.Monad.ForkT (safeSystemTempDirectory)
import Garnix.Monad.Metrics
import Garnix.Monad.SubProcess.Deprecated qualified as Deprecated
import Garnix.NixConfig (addNixConfigEnvironment)
import Garnix.Prelude
import Garnix.Sandbox (inNixSandboxWithHomeNetrc)
import Garnix.Types as Types
import Garnix.YamlConfig (GarnixConfig, getConfig)

withCheckout :: CommitInfo -> M a -> M a
withCheckout commitInfo action = do
  remote <- getRemote commitInfo
  inRunsDirectory . Internal.withPrivateNixXdgCache $ do
    timingAs #gitCloneTime $ void $ Deprecated.runProc "git" ["clone", "--filter=tree:0", realRemoteUrl remote, "."] []
    void $ Deprecated.runProc "git" ["checkout", getCommitHash (commitInfo ^. commit)] []
    cleanRemote remote
    action
  where
    inRunsDirectory :: (HasCallStack) => M a -> M a
    inRunsDirectory action = do
      tmp <- safeSystemTempDirectory "garnix-runs"
      local (#workingDir .~ tmp) action

-- Done so the token never leaks
cleanRemote :: RemoteUrl -> M ()
cleanRemote remote = do
  let cleanRemote = realRemoteUrl remote & [Regex.regex|x-access-token:.*@|] . Regex.match .~ ""
  void $ Deprecated.runProc "git" ["remote", "set-url", "origin", cleanRemote] []

newtype Remote = Remote
  { runWithCheckout :: forall a. CommitInfo -> (GarnixConfig -> M a) -> M a
  }

withBeforeAction :: M () -> Remote -> Remote
withBeforeAction before remote = Remote $ \commitInfo action -> do
  runWithCheckout remote commitInfo $ \garnixConfig -> do
    before
    action garnixConfig

remoteWithConfig :: (HasCallStack) => Remote
remoteWithConfig = Remote $ \commitInfo action -> do
  withCheckout commitInfo $ do
    config <- getConfig
    action config

withAuthorization :: FlakeDir -> RepoConfig -> CommitInfo -> M a -> M a
withAuthorization flakeDir repoConfig commitInfo action = do
  authorization <- checkAuthorization flakeDir repoConfig commitInfo
  unless (null $ authorizationPrefetch authorization)
    $ Internal.withNetRcEntries (authorizationNetRc authorization)
    $ mapM_ prefetchInput (authorizationPrefetch authorization)
  Lens.locally #userNixConfig (authorizationNixConfig authorization <>) action

-- | Fetches a flake input, as its lock file pins it, into the store, where
-- evaluation then finds it by its hash without fetching it again.
prefetchInput :: (HasCallStack) => Aeson.Value -> M ()
prefetchInput locked = do
  cacheDir <- getNixXdgCacheDir
  nixConfig <- view #userNixConfig
  curDir <- view #workingDir
  let expression = "(builtins.fetchTree (builtins.fromJSON (builtins.getEnv \"GARNIX_LOCKED_INPUT\"))).outPath" :: Text
      arguments = ["eval", "--impure", "--raw", "--expr", expression]
  (exitCode, StdoutTrimmed stdout, StderrRaw stderr) <-
    (>>= run)
      $ cmd "nix"
      & addArgs arguments
      & modifyEnvVar "GARNIX_LOCKED_INPUT" (const $ Just $ cs $ Aeson.encode locked)
      & addNixConfigEnvironment nixConfig
      & setWorkingDir curDir
      & pure
      & inNixSandboxWithHomeNetrc [] (Just cacheDir)
  case exitCode of
    ExitSuccess -> log Informational $ "prefetched a private flake input into " <> stdout
    ExitFailure e ->
      throw
        $ RunProcessError
          { command = "nix",
            arguments = arguments,
            stdErr = cs stderr,
            stdOut = stdout,
            exitCode = e
          }
