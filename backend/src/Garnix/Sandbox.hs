module Garnix.Sandbox (inNixSandbox, inNixSandboxWithHomeNetrc, SandboxAccessType (..)) where

import Cradle
import Cradle.ProcessConfiguration
import Data.List
import Garnix.Monad hiding (workingDir)
import Garnix.NixConfig (getNetRcFileSetting)
import Garnix.Prelude
import Garnix.Types (NetRcFile (..))
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (getEnv)

-- A sandbox appropriate for nix commands.
inNixSandbox :: [(FilePath, SandboxAccessType)] -> Maybe FilePath -> M ProcessConfiguration -> M ProcessConfiguration
inNixSandbox = nixSandbox False

-- | 'inNixSandbox', also offering nix's netrc file as @~/.netrc@. Nix fetches
-- @git@ inputs by running git, which ignores nix's netrc-file but reads
-- @~/.netrc@. Only for fetching private inputs before evaluation: the netrc
-- file's credentials must not reach a git that @flake.nix@ can point anywhere.
inNixSandboxWithHomeNetrc :: [(FilePath, SandboxAccessType)] -> Maybe FilePath -> M ProcessConfiguration -> M ProcessConfiguration
inNixSandboxWithHomeNetrc = nixSandbox True

nixSandbox :: Bool -> [(FilePath, SandboxAccessType)] -> Maybe FilePath -> M ProcessConfiguration -> M ProcessConfiguration
nixSandbox withHomeNetrc extraSandboxPaths xdgCacheHome procConfigM = do
  procConfig <- procConfigM
  path <- liftIO $ getEnv "PATH"
  let env = fromMaybe identity (environmentModification procConfig) [("PATH", path)]
  netrcFile <- do
    userNixConfig <- view #userNixConfig
    pure $ case getNetRcFileSetting userNixConfig of
      (Just (NetRcFile netrcFile)) -> [(netrcFile, ReadOnly)]
      Nothing -> []
  dir <- case workingDir procConfig of
    Nothing -> liftIO getCurrentDirectory
    Just d -> pure d
  args <- argsForNixSandbox xdgCacheHome dir (extraSandboxPaths ++ netrcFile) env
  let homeNetrc =
        if withHomeNetrc
          then concatMap (\(file, _) -> ["--ro-bind", file, sandboxHome </> ".netrc"]) netrcFile
          else []
  let oldCommand = executable procConfig : arguments procConfig
  pure
    $ procConfig
      { executable = "bwrap",
        arguments = args <> homeNetrc <> oldCommand
      }

-- | The home directory of the sandboxed process.
sandboxHome :: FilePath
sandboxHome = "/home/nix-runner"

data SandboxAccessType = TryReadOnly | ReadOnly | ReadWrite | LockFile
  deriving (Eq)

argsForNixSandbox :: (MonadIO m) => Maybe FilePath -> FilePath -> [(FilePath, SandboxAccessType)] -> [(String, String)] -> m [String]
argsForNixSandbox xdgCacheHome workingDir extraPaths env = do
  let extraPathOptions = flip concatMap extraPaths $ \case
        (path, TryReadOnly) -> ["--ro-bind-try", path, path]
        (path, ReadOnly) -> ["--ro-bind", path, path]
        (path, ReadWrite) -> ["--bind", path, path]
        (path, LockFile) -> ["--bind", path, path, "--lock-file", path]
  -- We use `-try` versions often since many of these dirs don't always exist.
  let baseArgs =
        ["--proc", "/proc"]
          <> ["--dev", "/dev"]
          <> ["--tmpfs", "/tmp"]
          <> ["--die-with-parent"]
          <> ["--unshare-pid"]
          <> ["--clearenv"]
          <> ["--ro-bind-try", "/bin/sh", "/bin/sh"]
  -- necessary for running tests on some non nixos systems
  let nonNixosSharedLibArgs =
        ["--ro-bind-try", "/lib", "/lib"]
          <> ["--ro-bind-try", "/lib64", "/lib64"]
  let nixArgs =
        ["--ro-bind-try", "/nix/", "/nix/"]
          <> ["--bind-try", "/nix/var/log/", "/nix/var/log/"]
          <> ["--ro-bind-try", "/etc/nix/machines", "/etc/nix/machines"]
          <> ["--ro-bind-try", "/etc/static/nix/machines", "/etc/static/nix/machines"]
          <> ["--ro-bind-try", "/etc/nix/nix.conf", "/etc/nix/nix.conf"]
          <> ["--ro-bind-try", "/etc/static/nix/nix.conf", "/etc/static/nix/nix.conf"]
  let networkingArgs =
        ["--ro-bind-try", "/etc/localtime", "/etc/localtime"]
          <> ["--ro-bind-try", "/etc/ssl", "/etc/ssl"]
          <> ["--ro-bind-try", "/etc/static/ssl", "/etc/static/ssl"]
          <> ["--ro-bind-try", "/etc/zoneinfo", "/etc/zoneinfo"]
          <> ["--ro-bind-try", "/etc/static/zoneinfo", "/etc/static/zoneinfo"]
          <> ["--ro-bind-try", "/etc/resolv.conf", "/etc/resolv.conf"]
          <> ["--ro-bind-try", "/etc/nsswitch.conf", "/etc/nsswitch.conf"]
          <> ["--ro-bind-try", "/etc/hostname", "/etc/hostname"]
          <> ["--ro-bind-try", "/etc/static/hostname", "/etc/static/hostname"]
  let systemProgramArgs = ["--ro-bind-try", "/run/current-system/sw", "/run/current-system/sw"]
  for_ xdgCacheHome $ \cache ->
    liftIO $ createDirectoryIfMissing True (cache </> "nix/gitv3")
  let homeArgs =
        ["--tmpfs", sandboxHome, "--setenv", "HOME", sandboxHome]
          <> ["--setenv", "XDG_CACHE_HOME", sandboxHome </> ".cache"]
          <> maybe
            ["--tmpfs", sandboxHome </> ".cache", "--tmpfs", sandboxHome </> ".cache/nix/gitv3"]
            (\dir -> ["--bind", dir, sandboxHome </> ".cache"])
            xdgCacheHome
  nixConfigForTests <- do
    hostHome <- liftIO $ getEnv "HOME"
    let nixConfigPath home = home </> ".config/nix/nix.conf"
    pure ["--ro-bind-try", nixConfigPath hostHome, nixConfigPath sandboxHome]
  let workingDirArgs = ["--bind", workingDir, workingDir]
  let setEnvs = concatMap (\(name, value) -> ["--setenv", name, value]) env
  pure
    $ baseArgs
    <> nonNixosSharedLibArgs
    <> nixArgs
    <> networkingArgs
    <> systemProgramArgs
    <> homeArgs
    <> nixConfigForTests
    <> workingDirArgs
    <> extraPathOptions
    <> setEnvs
