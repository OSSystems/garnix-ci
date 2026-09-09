module Garnix.Build.Module
  ( generateFlakeNix,
    getCommitInfo,
    remoteWithFlake,
    -- exported for testing:
    _moduleConfig,
  )
where

import Control.Lens
import Cradle qualified
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Text qualified as T
import Data.Text.IO qualified as Text
import Garnix.Build.Checkout qualified as Checkout
import Garnix.DB.ModuleValues qualified as ModuleValues
import Garnix.Monad
import Garnix.Monad.SubProcess qualified as SubProcess
import Garnix.NixConfig qualified as NixConfig
import Garnix.Prelude
import Garnix.Sandbox
import Garnix.Types (Branch (..), CommitInfo (..), Error (..), GhLogin, RepoInfo (..), getCommitHash, getGhLogin, getGhRepoName, getGhRepoOwner)
import GitHub.Data.Id (Id (Id))

getCommitInfo :: GhLogin -> ModuleValues.GetRepoAndModuleValues -> M CommitInfo
getCommitInfo reqUser modules = do
  case (modules ^. #repo_user, modules ^. #repo_name) of
    (Just user, Just repo) -> do
      installationId <- getGarnixInstallationId user repo
      iAuth <- case installationId of
        Nothing -> throw $ NoSuchRepo {_owner = user, _name = repo}
        Just id -> getInstallation (Id $ fromInteger id)
      repoPublicity <- getRepoPublicity iAuth user repo
      getDefaultBranch (Just iAuth) user repo >>= \case
        Nothing -> throw $ NoSuchRepo {_owner = user, _name = repo}
        Just branch -> do
          token <- getAccessToken iAuth
          commit <- getHeadCommit token user repo branch
          pure
            $ CommitInfo
              { _commitInfoReqUser = reqUser,
                _commitInfoRepoPublicity = repoPublicity,
                _commitInfoRepoInfo = RepoInfo iAuth token user repo,
                _commitInfoBranch = Just branch,
                _commitInfoPrFromFork = Nothing,
                _commitInfoCommit = commit
              }
    _ -> throw $ OtherError "cannot run build without a repository"

remoteWithFlake :: Branch -> ModuleValues.GetRepoAndModuleValues -> Checkout.Remote -> Checkout.Remote
remoteWithFlake branch values = Checkout.withBeforeAction $ do
  flakeContents <- generateFlakeNix branch values
  dir <- view #workingDir
  liftIO $ Text.writeFile (dir </> "flake.nix") flakeContents
  SubProcess.runGitProcess ["add", "flake.nix"]
  generateNixFlakeLockFile
  SubProcess.runGitProcess ["add", "flake.lock"]
  where
    generateNixFlakeLockFile :: M ()
    generateNixFlakeLockFile = do
      let modules = values ^. #modules
      let garnixLibInput = ("garnix-lib", "github:garnix-io/garnix-lib?rev=d3f3a98a0baddb3bdc6e0d028d1b58251a1d86f5")
      let inputs =
            garnixLibInput
              : ( ( \m ->
                      ( m ^. #name,
                        "github:"
                          <> getGhLogin (getGhRepoOwner $ m ^. #repo_user)
                          <> "/"
                          <> getGhRepoName (m ^. #repo_name)
                          <> "?rev="
                          <> getCommitHash (m ^. #git_commit)
                      )
                  )
                    <$> modules
                )
          args = (\(input, hash) -> ["--override-input", input, hash]) <$> inputs
      runNixCommand $ ["flake", "lock"] <> join args

    runNixCommand :: [Text] -> M ()
    runNixCommand args = do
      cacheDir <- getNixXdgCacheDir
      nixConfig <- view #userNixConfig
      dir <- view #workingDir
      result <-
        (>>= Cradle.run)
          $ Cradle.cmd "nix"
          & Cradle.addArgs args
          & NixConfig.addNixConfigEnvironment nixConfig
          & Cradle.setWorkingDir dir
          & Cradle.silenceStderr
          & pure
          & inNixSandbox [] (Just cacheDir)
      case result of
        (Cradle.ExitFailure code, Cradle.StdoutRaw out, Cradle.StderrRaw err) -> do
          log Warning
            $ "runNixCommand error "
            <> T.unwords args
            <> ": stdout ("
            <> cs out
            <> ") stderr ("
            <> cs err
            <> ")"
          throw RunProcessError {command = "nix", arguments = args, stdErr = cs err, stdOut = cs out, exitCode = code}
        _ -> pure ()

generateFlakeNix :: Branch -> ModuleValues.GetRepoAndModuleValues -> M Text
generateFlakeNix branch values = do
  let modules = Map.fromList . map (\m -> (m ^. #name, m)) $ values ^. #modules
  moduleConfigs <- mapM (getModuleConfig modules) (values ^. #user_config)
  toFlakeFile branch moduleConfigs
  where
    getModuleConfig :: Map Text ModuleValues.Module -> ModuleValues.ModuleValue -> M (ModuleValues.Module, ModuleValues.ModuleConfig)
    getModuleConfig modules value = case Map.lookup (value ^. #module_name) modules of
      Just m -> pure (m, value ^. #values)
      Nothing -> throw $ OtherError $ "module not found: " <> value ^. #module_name

toFlakeFile :: Branch -> [(ModuleValues.Module, ModuleValues.ModuleConfig)] -> M Text
toFlakeFile branch modulesAndValues = do
  pure
    $ cs
    $ unindent
      [i|
{
  inputs = {
    garnix-lib.url = "github:garnix-io/garnix-lib";#{foldMap extraInputs (fst <$> modulesAndValues)}
  };

  nixConfig = {
    extra-substituters = [ "https://cache.garnix.io" ];
    extra-trusted-public-keys = [ "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=" ];
  };

  outputs = inputs: inputs.garnix-lib.lib.mkModules {
    modules = [#{foldMap moduleList (fst <$> modulesAndValues)}
    ];

    config = { pkgs, ... }: {
#{foldMap _moduleConfig (snd <$> modulesAndValues)}
      garnix.deployBranch = "#{getBranch branch}";
    };
  };
}
      |]
  where
    extraInputs :: ModuleValues.Module -> Text
    extraInputs m =
      let moduleName = m ^. #name
          repoUser = getGhLogin . getGhRepoOwner $ m ^. #repo_user
          repoName = getGhRepoName $ m ^. #repo_name
       in cs
            [i|
    #{moduleName}.url = "github:#{repoUser}/#{repoName}";|]

    moduleList :: ModuleValues.Module -> Text
    moduleList m =
      let moduleName = m ^. #name
       in cs
            [i|
      inputs.#{moduleName}.garnixModules.default|]

_moduleConfig :: ModuleValues.ModuleConfig -> Text
_moduleConfig (ModuleValues.ModuleConfig identifier value) = mkAttr 3 identifier value
  where
    go :: Int -> ModuleValues.NixValue -> Text
    go indent = \case
      ModuleValues.Secret secret -> escapeNixStr $ secret ^. #encryptedValue
      ModuleValues.NixString str -> escapeNixStr str
      ModuleValues.NixPath path -> path
      ModuleValues.NixRaw raw -> raw
      ModuleValues.NixBool b -> if b then "true" else "false"
      ModuleValues.NixInt n -> show n
      ModuleValues.NixNull -> "null"
      ModuleValues.NixList l -> "[ " <> T.intercalate " " (go indent <$> l) <> " ]"
      ModuleValues.NixSet attrs ->
        "{\n"
          <> Map.foldMapWithKey (mkAttr (succ indent)) attrs
          <> mkIndent indent
          <> "}"

    escapeNixStr :: Text -> Text
    escapeNixStr str = "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" str) <> "\""

    mkAttr :: Int -> ModuleValues.NixIdentifier -> ModuleValues.NixValue -> Text
    mkAttr indent key value = mkIndent indent <> ModuleValues.getNixIdentifier key <> " = " <> go indent value <> ";\n"

    mkIndent :: Int -> Text
    mkIndent n = cs $ replicate (2 * n) ' '
