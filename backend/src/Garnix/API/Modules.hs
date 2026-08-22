module Garnix.API.Modules
  ( ModulesAPI,
    modulesAPI,
  )
where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Row
import Garnix.Build qualified as Build
import Garnix.Build.Checkout qualified as Checkout
import Garnix.Build.Module qualified as Build.Module
import Garnix.DB.ModuleValues qualified as ModuleValues
import Garnix.Monad
import Garnix.Monad.SubProcess qualified as SubProcess
import Garnix.Prelude
import Garnix.Types
import Servant.API (OctetStream, Put)
import Servant.Auth.Server

data BuildInfo = BuildInfo
  { _buildInfoCommit :: CommitHash,
    _buildInfoBranch :: Maybe Branch
  }
  deriving stock (Generic)

instance ToJSON BuildInfo where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

data ModulesAPI route = ModulesAPI
  { _modulesAPIgetValues ::
      route
        :- Get '[JSON] ModuleValues.GetRepoAndModuleValues,
    _modulesAPIupdateValues ::
      route
        :- ReqBody '[JSON] ModuleValues.UpdateRepoModuleValues
        :> Put '[JSON] NoContent,
    _modulesAPIgetAvailableModules ::
      route
        :- "available"
        :> Get '[JSON] (Rec ("modules" .== [ModuleValues.Module])),
    _modulesAPIrunBuild ::
      route
        :- "run"
        :> Post '[JSON] BuildInfo,
    _modulesAPIcreatePullRequest ::
      route
        :- "pull-request"
        :> Post '[JSON] PullRequestResult,
    _modulesAPIgetFlake ::
      route
        :- "reset"
        :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] ByteString),
    _modulesAPIReset :: route :- "reset" :> Post '[JSON] NoContent
  }
  deriving (Generic)

modulesAPI :: AuthResult AuthJwtPayload -> ModulesAPI (AsServerT M)
modulesAPI = \case
  Authenticated ((^. #user) -> user) ->
    ModulesAPI
      { _modulesAPIgetValues = getValues user,
        _modulesAPIupdateValues = updateValues user,
        _modulesAPIgetAvailableModules = getAvailableModules,
        _modulesAPIrunBuild = runBuild user,
        _modulesAPIcreatePullRequest = createPullRequest user,
        _modulesAPIgetFlake = getFlake user,
        _modulesAPIReset = reset user
      }
  _ ->
    ModulesAPI
      { _modulesAPIgetValues = throw Unauthorized,
        _modulesAPIupdateValues = const $ throw Unauthorized,
        _modulesAPIgetAvailableModules = getAvailableModules,
        _modulesAPIrunBuild = throw Unauthorized,
        _modulesAPIcreatePullRequest = throw Unauthorized,
        _modulesAPIgetFlake = throw Unauthorized,
        _modulesAPIReset = throw Unauthorized
      }

getValues :: User -> M ModuleValues.GetRepoAndModuleValues
getValues = maybe (throw NotFound) pure <=< ModuleValues.get . _userGithubLogin

updateValues :: User -> ModuleValues.UpdateRepoModuleValues -> M NoContent
updateValues user values = ModuleValues.update (user ^. githubLogin) values $> NoContent

getAvailableModules :: M (Rec ("modules" .== [ModuleValues.Module]))
getAvailableModules = (#modules .==) <$> ModuleValues.getAvailableModules

runBuild :: User -> M BuildInfo
runBuild user = do
  repoAndModuleValues <- getValues user
  commitInfo <- Build.buildModule (user ^. githubLogin) repoAndModuleValues
  pure $ BuildInfo (commitInfo ^. commit) (commitInfo ^. branch)

createPullRequest :: User -> M PullRequestResult
createPullRequest user = do
  ModuleValues.get (user ^. githubLogin) >>= \case
    Nothing -> throw NotFound
    Just repoAndModuleValues -> do
      commitInfo <- Build.Module.getCommitInfo (user ^. githubLogin) repoAndModuleValues
      withSpan commitInfo $ do
        let baseBranch = maybe (Branch "main") identity $ commitInfo ^. branch
        newBranch <- Branch . ("garnix-modules-" <>) <$> randomBase64 8

        pushNewBranch repoAndModuleValues commitInfo baseBranch newBranch

        openPullRequest commitInfo baseBranch newBranch
  where
    pushNewBranch :: ModuleValues.GetRepoAndModuleValues -> CommitInfo -> Branch -> Branch -> M ()
    pushNewBranch repoAndModuleValues commitInfo baseBranch newBranch = do
      let remote = Build.Module.remoteWithFlake baseBranch repoAndModuleValues Checkout.remoteWithConfig
      remoteUrl <- getRemote commitInfo
      Checkout.runWithCheckout remote commitInfo $ \_garnixConfig -> do
        SubProcess.runGitProcess ["checkout", "-b", getBranch newBranch]
        SubProcess.runGitProcess ["commit", "-am", "Add garnix modules."]
        SubProcess.runGitProcess ["push", realRemoteUrl remoteUrl, getBranch newBranch]

    openPullRequest :: CommitInfo -> Branch -> Branch -> M PullRequestResult
    openPullRequest commitInfo baseBranch newBranch =
      openGithubPullRequest
        (commitInfo ^. repoInfo . repoId)
        PullRequest
          { _pullRequestTitle = "Enable garnix modules",
            _pullRequestBody = "This is an automated pull request created using [garnix modules](https://garnix.io/modules).\n\nCreate or edit your existing modules [here](https://garnix.io/modules/configure).",
            _pullRequestHeadBranch = newBranch,
            _pullRequestBaseBranch = baseBranch
          }

getFlake :: User -> M (Headers '[Header "Content-Disposition" Text] ByteString)
getFlake user = do
  ModuleValues.get (user ^. githubLogin) >>= \case
    Nothing -> throw NotFound
    Just repoAndModuleValues -> do
      commitInfo <- Build.Module.getCommitInfo (user ^. githubLogin) repoAndModuleValues
      let defaultBranch = maybe (Branch "main") identity $ commitInfo ^. branch
      contents <- Build.Module.generateFlakeNix defaultBranch repoAndModuleValues
      pure $ addHeader "attachment; filename=\"flake.nix\"" $ cs contents

reset :: User -> M NoContent
reset user =
  ModuleValues.get (user ^. githubLogin) >>= \case
    Nothing -> throw NotFound
    Just _ -> ModuleValues.delete (user ^. githubLogin) $> NoContent
