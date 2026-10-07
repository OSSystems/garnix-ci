module Garnix.API.Cache.Permissions
  ( Permission (..),
    PermissionDecision (..),
    decidePermission,
    collaboratorPermission,
    getRepoPermissions,
    __getRepoPermissionsCache,
  )
where

import Garnix.Duration
import Garnix.ExpiringCache
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
import System.IO.Unsafe qualified

data Permission
  = Allowed
  | Disallowed
  deriving (Eq, Ord, Show)

-- | What a repository's publicity and the requester decide on their own, with
-- the severity and reason to log. A private repository requested by an account
-- on its forge still needs that login checked against the collaborators.
data PermissionDecision
  = Decided Permission Severity Text
  | CheckCollaborator GhLogin
  deriving (Eq, Show)

-- | A user only counts as a collaborator on a repository of their own forge:
-- the same login on another forge is somebody else.
decidePermission :: RepoId -> Maybe ForgeLogin -> RepoPublicity -> PermissionDecision
decidePermission repoId' mUser publicity = case (publicity, mUser) of
  (RepoIsPublic True, _) -> Decided Allowed Informational "repo is public, allowing access"
  (RepoIsPublic False, Nothing) -> Decided Disallowed Informational "repo is private and no authentication claim"
  (RepoIsPublic False, Just forgeLogin') -> case loginOnForgeOf repoId' forgeLogin' of
    Nothing -> Decided Disallowed Notice "Access claimed by an account on another forge. Blocking."
    Just user -> CheckCollaborator user

collaboratorPermission :: GhLogin -> GhCollaborators -> (Permission, Severity, Text)
collaboratorPermission user = \case
  RepoNotFound -> (Disallowed, Warning, "Repository not found, denying access")
  GhCollaborators collaborators
    | user `elem` collaborators -> (Allowed, Informational, "User is a collaborator to the repository, allowing")
    | otherwise -> (Disallowed, Notice, "Access to disallowed resource. Blocking.")

getRepoPermissions :: (HasCallStack) => Maybe ForgeLogin -> RepoId -> M Permission
getRepoPermissions mUser repoId'@(RepoId _ owner repo) =
  lookupCache __getRepoPermissionsCache (mUser, repoId')
    $ withTextSpans
      [ ("function", "Garnix.API.Cache.Permissions.getRepoPermission"),
        ("repo_perm_mUser", show mUser),
        ("repo_perm_owner", show owner),
        ("repo_perm_repo", show repo)
      ]
    $ resolveCredentials repoId'
    >>= \case
      Nothing -> do
        log Warning "Cache.getRepoPermissions: could not get garnixInstallationId"
        pure Disallowed
      Just credentials' -> do
        log Informational "Cache.getRepoPermissions: got garnixInstallationId"
        repoPublicity <- try $ getRepoPublicity credentials' repoId'
        log Informational $ "repoPublicity: " <> show repoPublicity
        case decidePermission repoId' mUser <$> repoPublicity of
          Left err -> do
            log Informational $ "Error fetching repo publicity, disallowing access: " <> show err
            pure Disallowed
          Right (Decided permission severity reason) -> do
            log severity reason
            pure permission
          Right (CheckCollaborator user) -> do
            (permission, severity, reason) <-
              collaboratorPermission user <$> getRepoCollaborators credentials' repoId'
            log severity reason
            pure permission

type RepoPermissionCache = ExpiringCache (Maybe ForgeLogin, RepoId) Permission

{-# NOINLINE __getRepoPermissionsCache #-}
__getRepoPermissionsCache :: RepoPermissionCache
__getRepoPermissionsCache =
  System.IO.Unsafe.unsafePerformIO
    $ mkCache
      (Just "__getRepoPermissionsCache")
      (fromHours @Int 1)
      (fromMinutes @Int 5)
