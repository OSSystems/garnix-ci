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
import Garnix.Access (requireGithubIdentity, withRequiredUser)
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
modulesAPI auth =
  ModulesAPI
    { _modulesAPIgetValues = withGithubIdentity getValues,
      _modulesAPIupdateValues = \values -> withGithubIdentity $ \login' -> updateValues login' values,
      _modulesAPIgetAvailableModules = getAvailableModules,
      _modulesAPIrunBuild = withGithubIdentity runBuild,
      _modulesAPIcreatePullRequest = withGithubIdentity createPullRequest,
      _modulesAPIgetFlake = withGithubIdentity getFlake,
      _modulesAPIReset = withGithubIdentity reset
    }
  where
    -- Modules only exist on github.com, so they belong to the account's
    -- github identity.
    withGithubIdentity :: (ForgeLogin -> M a) -> M a
    withGithubIdentity action = withRequiredUser auth $ requireGithubIdentity >=> action

getValues :: ForgeLogin -> M ModuleValues.GetRepoAndModuleValues
getValues = maybe (throw NotFound) pure <=< ModuleValues.get

updateValues :: ForgeLogin -> ModuleValues.UpdateRepoModuleValues -> M NoContent
updateValues login' values = ModuleValues.update login' values $> NoContent

getAvailableModules :: M (Rec ("modules" .== [ModuleValues.Module]))
getAvailableModules = (#modules .==) <$> ModuleValues.getAvailableModules

runBuild :: ForgeLogin -> M BuildInfo
runBuild login' = do
  repoAndModuleValues <- getValues login'
  commitInfo <- Build.buildModule login' repoAndModuleValues
  pure $ BuildInfo (commitInfo ^. commit) (commitInfo ^. branch)

createPullRequest :: ForgeLogin -> M PullRequestResult
createPullRequest login' = do
  ModuleValues.get login' >>= \case
    Nothing -> throw NotFound
    Just repoAndModuleValues -> do
      commitInfo <- Build.Module.getCommitInfo login' repoAndModuleValues
      withSpan commitInfo $ do
        let baseBranch = maybe (Branch "main") identity $ commitInfo ^. branch
        newBranch <- Branch . ("garnix-modules-" <>) <$> randomBase64 8

        pushNewBranch repoAndModuleValues commitInfo baseBranch newBranch

        openModulesPullRequest commitInfo baseBranch newBranch
  where
    pushNewBranch :: ModuleValues.GetRepoAndModuleValues -> CommitInfo -> Branch -> Branch -> M ()
    pushNewBranch repoAndModuleValues commitInfo baseBranch newBranch = do
      let remote = Build.Module.remoteWithFlake baseBranch repoAndModuleValues Checkout.remoteWithConfig
      remoteUrl <- getRemote commitInfo
      Checkout.runWithCheckout remote commitInfo $ \_garnixConfig -> do
        SubProcess.runGitProcess ["checkout", "-b", getBranch newBranch]
        SubProcess.runGitProcess ["commit", "-am", "Add garnix modules."]
        SubProcess.runGitProcess ["push", realRemoteUrl remoteUrl, getBranch newBranch]

    openModulesPullRequest :: CommitInfo -> Branch -> Branch -> M PullRequestResult
    openModulesPullRequest commitInfo baseBranch newBranch =
      openPullRequest
        (commitInfo ^. repoInfo . repoId)
        PullRequest
          { _pullRequestTitle = "Enable garnix modules",
            _pullRequestBody = "This is an automated pull request created using [garnix modules](https://garnix.io/modules).\n\nCreate or edit your existing modules [here](https://garnix.io/modules/configure).",
            _pullRequestHeadBranch = newBranch,
            _pullRequestBaseBranch = baseBranch
          }

getFlake :: ForgeLogin -> M (Headers '[Header "Content-Disposition" Text] ByteString)
getFlake login' = do
  ModuleValues.get login' >>= \case
    Nothing -> throw NotFound
    Just repoAndModuleValues -> do
      commitInfo <- Build.Module.getCommitInfo login' repoAndModuleValues
      let defaultBranch = maybe (Branch "main") identity $ commitInfo ^. branch
      contents <- Build.Module.generateFlakeNix defaultBranch repoAndModuleValues
      pure $ addHeader "attachment; filename=\"flake.nix\"" $ cs contents

reset :: ForgeLogin -> M NoContent
reset login' =
  ModuleValues.get login' >>= \case
    Nothing -> throw NotFound
    Just _ -> ModuleValues.delete login' $> NoContent
