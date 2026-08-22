module Garnix.API.Cache.Permissions
  ( Permission (..),
    getRepoPermissions,
    __getRepoPermissionsCache,
  )
where

import Garnix.Duration
import Garnix.ExpiringCache
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
import GitHub.Data.Id (Id (..))
import System.IO.Unsafe qualified

data Permission
  = Allowed
  | Disallowed
  deriving (Eq, Ord, Show)

getRepoPermissions :: (HasCallStack) => Maybe GhLogin -> RepoId -> M Permission
getRepoPermissions mUser repoId'@(RepoId _forge owner repo) =
  lookupCache __getRepoPermissionsCache (mUser, owner, repo)
    $ withTextSpans
      [ ("function", "Garnix.API.Cache.Permissions.getRepoPermission"),
        ("repo_perm_mUser", show mUser),
        ("repo_perm_owner", show owner),
        ("repo_perm_repo", show repo)
      ]
    $ getGarnixInstallationId repoId'
    >>= \case
      Nothing -> do
        log Warning "Cache.getRepoPermissions: could not get garnixInstallationId"
        pure Disallowed
      Just id -> do
        log Informational "Cache.getRepoPermissions: got garnixInstallationId"
        iAuth <- getInstallation (Id $ fromInteger id)
        repoPublicity <- try $ getRepoPublicity iAuth repoId'
        log Informational $ "repoPublicity: " <> show repoPublicity
        case (repoPublicity, mUser) of
          (Left err, _) -> do
            log Informational $ "Error fetching repo publicity, disallowing access: " <> show err
            pure Disallowed
          (Right (RepoIsPublic True), _) -> do
            log Informational "repo is public, allowing access"
            pure Allowed
          (Right (RepoIsPublic False), Nothing) -> do
            log Informational "repo is private and no authentication claim"
            pure Disallowed
          (Right (RepoIsPublic False), Just user) -> do
            collaborators <- getRepoCollaborators iAuth repoId'
            case collaborators of
              RepoNotFound -> do
                log Warning "Repository not found, denying access"
                pure Disallowed
              GhCollaborators collaborators ->
                if user `elem` collaborators
                  then do
                    log Informational "User is a collaborator to the repository, allowing"
                    pure Allowed
                  else do
                    log Notice "Access to disallowed resource. Blocking."
                    pure Disallowed

type GithubPermissionCache = ExpiringCache (Maybe GhLogin, GhRepoOwner, GhRepoName) Permission

{-# NOINLINE __getRepoPermissionsCache #-}
__getRepoPermissionsCache :: GithubPermissionCache
__getRepoPermissionsCache =
  System.IO.Unsafe.unsafePerformIO
    $ mkCache
      (Just "__getRepoPermissionsCache")
      (fromHours @Int 1)
      (fromMinutes @Int 5)
