module Garnix.Modules
  ( publish,
  )
where

import Data.Row ((.+), (.==))
import Garnix.BuildLogs.Types (mkLogLine)
import Garnix.DB qualified as DB
import Garnix.DB.ModuleValues qualified as DB
import Garnix.Modules.Schema qualified as ModuleSchema
import Garnix.Monad
import Garnix.Prelude
import Garnix.Reporters.Utils (withRunReporter)
import Garnix.Types
import Garnix.YamlConfig qualified as Config

-- | Assumes Env ^. #workingDir is correctly set, i.e., we're running within `withCheckout`.
publish :: Reporter -> Config.GarnixConfig -> CommitInfo -> M ()
publish reporter config commitInfo = withTextSpan ("modules_publish", show publishEnabled) $ do
  case (publishEnabled, commitInfo ^. branch) of
    (True, Just "main") -> do
      run <- DB.newRun "Garnix module publish" commitInfo
      withRunReporter reporter (ReportRun run) $ \runReporter -> do
        case commitInfo ^. repoInfo . repoId . repoUser of
          "garnix-io" -> do
            schema <- view #workingDir >>= ModuleSchema.readModuleSchema
            let repo = commitInfo ^. repoInfo . repoId
                moduleRepoName = repo ^. repoName . to getGhRepoName
                moduleName = ModuleSchema.repoNameToModuleName moduleRepoName
            DB.insertLatestVersion
              $ (#name .== moduleName)
              .+ (#repo_user .== repo ^. repoUser)
              .+ (#repo_name .== repo ^. repoName)
              .+ (#git_commit .== commitInfo ^. commit)
              .+ (#schema .== toJSON schema)
              .+ (#description .== ModuleSchema.description schema)
            reportLogs runReporter $ mkLogLine $ "Module " <> moduleName <> " updated successfully!"
            reportComplete runReporter RunReportStatusSuccess
          otherOrg ->
            throw $ OtherError $ "Publishing modules is not enabled for " <> getGhLogin (getGhRepoOwner otherOrg) <> "."
    _ ->
      log Informational "Modules: skipping publish"
  where
    publishEnabled :: Bool
    publishEnabled = config ^. Config.moduleSection . #publish
