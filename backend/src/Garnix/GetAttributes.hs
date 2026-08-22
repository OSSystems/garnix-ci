module Garnix.GetAttributes where

import Control.Lens.Regex.Text qualified as RE
import Cradle
import Data.Text qualified as T
import Garnix.Attribute
import Garnix.Monad
import Garnix.Monad.Metrics
import Garnix.Monad.Pool (withPoolM)
import Garnix.NixConfig (addNixConfigEnvironment)
import Garnix.Prelude
import Garnix.Sandbox
import Garnix.Types
import Garnix.YamlConfig

subAttrs ::
  (HasCallStack) =>
  GhRepoOwner ->
  FlakeDir ->
  Attribute ->
  M [Attribute]
subAttrs repoOwner flakeDir attr = do
  cacheDir <- getNixXdgCacheDir
  nixConfig <- view #userNixConfig
  curDir <- view #workingDir
  attr' <- localAttr flakeDir attr
  let args = ["eval", attr', "--apply", "builtins.attrNames", "--json"]
  res <-
    withPoolM nixEvalPool repoOwner $ do
      (exitCode, StdoutTrimmed stdout, StderrRaw stderr) <-
        (>>= run)
          $ cmd "nix"
          & addArgs args
          & addNixConfigEnvironment nixConfig
          & setWorkingDir curDir
          & pure
          & inNixSandbox [] (Just cacheDir)
      case exitCode of
        ExitSuccess -> pure stdout
        ExitFailure _ ->
          if isDoesNotProvideAttributeError $ cs stderr
            then pure "[]"
            else throw $ ErrorGettingAttributesToBuild (cs stderr)
  parsed <- aesonDecode ("output of 'nix" <> T.unwords args <> "'") parseJSON res
  pure $ catMaybes [addSubAttr attr r | r <- parsed]

ifIsAttr :: (HasCallStack) => GhRepoOwner -> FlakeDir -> Attribute -> M [Attribute]
ifIsAttr repoOwner flakeDir attr = do
  cacheDir <- getNixXdgCacheDir
  nixConfig <- view #userNixConfig
  curDir <- view #workingDir
  attr' <- localAttr flakeDir attr
  -- Probably there is a prettier way of checking that the attr exists
  let args = ["eval", attr', "--apply", "x : {}", "--json"]
  res <-
    withPoolM nixEvalPool repoOwner $ do
      (exitCode, StdoutTrimmed stdout, StderrRaw stderr) <-
        (>>= run)
          $ cmd "nix"
          & addArgs args
          & addNixConfigEnvironment nixConfig
          & setWorkingDir curDir
          & pure
          & inNixSandbox [] (Just cacheDir)
      case exitCode of
        ExitSuccess -> pure $ Just stdout
        ExitFailure _ ->
          if isDoesNotProvideAttributeError $ cs stderr
            then pure Nothing
            else throw $ ErrorGettingAttributesToBuild (cs stderr)
  case res of
    Nothing -> pure []
    Just _ -> pure [attr]

isDoesNotProvideAttributeError :: Text -> Bool
isDoesNotProvideAttributeError s =
  let regex = [RE.regex|error: flake '.*' does not provide attribute .*|]
   in case s ^.. regex . RE.groups of
        [] -> False
        _ -> True

-- | Get all the attributes that will be built.
getAttributesToBuild ::
  (HasCallStack) =>
  CommitInfo ->
  GarnixConfig ->
  M [Attribute]
getAttributesToBuild commitInfo cfg = timingAs #getAttrsToBuildTime $ do
  -- We first check whether a child could even match a config entry. Only
  -- then do we try to figure out it's children. And finally we filter those
  -- that in fact match.
  -- We do the first step because the second might fail. But we don't care
  -- whether it does if it couldn't have mattered what the result was.
  general <- forM allParentAttrs $ \attr ->
    if attr `mightMatchConfig` cfg
      then subAttrs (commitInfo ^. repoInfo . repoId . repoUser) (cfg ^. flakeDir) attr
      else pure []
  defaults <- forM allDirectAttrs $ \attr ->
    if matchesConfig attr cfg (commitInfo ^. branch)
      then ifIsAttr (commitInfo ^. repoInfo . repoId . repoUser) (cfg ^. flakeDir) attr
      else pure []
  pure [attr | attr <- join (general <> defaults), matchesConfig attr cfg (commitInfo ^. branch)]

{-
Note [Async process exceptions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It's still unclear to me why, but typed-process' commands handle async
exceptions poorly. In particular, running them inside an `async` and cancelling
the process causes the subprocess to persist.

That's bad in general, but is particularly noticeable where we use flock to
figure out how long to tail for.
-}
