-- | The @forge_installations@ and @forge_installation_repos@ tables: where
-- garnix is installed on Gitea/Forgejo, configured or registered, and the
-- repositories each installation builds.
module Garnix.DB.Installations
  ( insertForgeInstallation,
    setForgeInstallationHook,
    getForgeInstallation,
    getForgeInstallationOn,
    listForgeInstallations,
    deleteForgeInstallation,
    markNeedsReinstall,
    reinstallForgeInstallation,
    enabledInstallation,
    listEnabledRepos,
    enableRepo,
    disableRepo,
  )
where

import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.DB qualified as DB
import Garnix.Forge.Installation
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types

type InstallationColumns = (ForgeInstallationId, ForgeSlug, Text, Text, Maybe GhLogin, Maybe Int64, EncryptedText, Maybe Text, UTCTime)

installationFromColumns :: InstallationColumns -> Either Text ForgeInstallation
installationFromColumns (id', forge', kind', target', installer, hookId, secret, needsReinstall, createdAt) = do
  target'' <- parseInstallTarget kind' target'
  pure
    ForgeInstallation
      { forgeInstallationId = id',
        forgeInstallationForge = forge',
        forgeInstallationTarget = target'',
        forgeInstallationInstaller = installer,
        forgeInstallationHookId = toInteger <$> hookId,
        forgeInstallationWebhookSecret = secret,
        forgeInstallationNeedsReinstall = needsReinstall,
        forgeInstallationCreatedAt = createdAt
      }

decodeInstallations :: [InstallationColumns] -> M [ForgeInstallation]
decodeInstallations = either (throw . OtherError) pure . traverse installationFromColumns

-- | A new installation, without its webhook yet ('setForgeInstallationHook'), or
-- 'Nothing' if the target already has one.
insertForgeInstallation :: ForgeSlug -> InstallTarget -> GhLogin -> EncryptedText -> M (Maybe ForgeInstallationId)
insertForgeInstallation forge' target installer secret = do
  let kind' = renderTargetKind target
      name = targetName target
  listToMaybe
    <$> DB.pgQuery
      [pgSQL|
        INSERT INTO forge_installations (forge, target_kind, target, installer_login, webhook_secret)
        VALUES (${forge'}, ${kind'}, ${name}, ${installer}, ${secret})
        ON CONFLICT DO NOTHING
        RETURNING id
      |]

setForgeInstallationHook :: ForgeInstallationId -> Integer -> M ()
setForgeInstallationHook installation hookId = do
  let hookId' = fromInteger hookId :: Int64
  void $ DB.pgExec [pgSQL| UPDATE forge_installations SET hook_id = ${hookId'}, updated_at = now() WHERE id = ${installation} |]

getForgeInstallation :: ForgeInstallationId -> M (Maybe ForgeInstallation)
getForgeInstallation installation =
  fmap listToMaybe
    . decodeInstallations
    =<< DB.pgQuery
      [pgSQL|
        SELECT id, forge, target_kind, target, installer_login, hook_id, webhook_secret, needs_reinstall, created_at
        FROM forge_installations
        WHERE id = ${installation}
      |]

-- | The installation on a target, if there is one. A forge has a handful of
-- them at most.
getForgeInstallationOn :: ForgeSlug -> InstallTarget -> M (Maybe ForgeInstallation)
getForgeInstallationOn forge' target =
  find (sameInstallTarget target . forgeInstallationTarget) <$> listForgeInstallations forge'

listForgeInstallations :: ForgeSlug -> M [ForgeInstallation]
listForgeInstallations forge' =
  decodeInstallations
    =<< DB.pgQuery
      [pgSQL|
        SELECT id, forge, target_kind, target, installer_login, hook_id, webhook_secret, needs_reinstall, created_at
        FROM forge_installations
        WHERE forge = ${forge'}
        ORDER BY id
      |]

-- | Deletes the installation and the list of repositories it builds. Their
-- builds and commits stay.
deleteForgeInstallation :: ForgeInstallationId -> M ()
deleteForgeInstallation installation =
  void $ DB.pgExec [pgSQL| DELETE FROM forge_installations WHERE id = ${installation} |]

-- | Stops an installation until someone reinstalls it, if it still has the
-- installer whose token failed: a build that held the token of whoever
-- installed it before a reinstall leaves the reinstalled one alone.
markNeedsReinstall :: ForgeInstallationId -> GhLogin -> Text -> M ()
markNeedsReinstall installation installer reason =
  void
    $ DB.pgExec
      [pgSQL|
        UPDATE forge_installations
        SET needs_reinstall = ${reason}, updated_at = now()
        WHERE id = ${installation} AND installer_login = ${installer} AND needs_reinstall IS NULL
      |]

-- | Hands an installation, as it was read, to a new installer, with a new
-- webhook and secret, and lets it build again. 'False', and nothing
-- changed, when it changed since it was read (another reinstall won).
reinstallForgeInstallation :: ForgeInstallation -> GhLogin -> Integer -> EncryptedText -> M Bool
reinstallForgeInstallation read' installer hookId secret = do
  let installation = forgeInstallationId read'
      hookId' = fromInteger hookId :: Int64
      readHookId = fromInteger <$> forgeInstallationHookId read' :: Maybe Int64
      readInstaller = forgeInstallationInstaller read'
  (== 1)
    <$> DB.pgExec
      [pgSQL|
        UPDATE forge_installations
        SET installer_login = ${installer}, hook_id = ${hookId'}, webhook_secret = ${secret},
            needs_reinstall = NULL, updated_at = now()
        WHERE id = ${installation}
          AND hook_id IS NOT DISTINCT FROM ${readHookId}
          AND installer_login IS NOT DISTINCT FROM ${readInstaller}
      |]

-- | The installation a repository is enabled in, if any.
enabledInstallation :: RepoId -> M (Maybe ForgeInstallation)
enabledInstallation repo = do
  let forge' = repo ^. forge
      owner = repo ^. repoUser
      name = repo ^. repoName
  installation <-
    DB.pgQuery
      [pgSQL|
        SELECT installation_id
        FROM forge_installation_repos
        WHERE forge = ${forge'} AND lower(repo_owner) = lower(${owner}) AND lower(repo_name) = lower(${name})
      |]
  maybe (pure Nothing) getForgeInstallation (listToMaybe installation)

-- | Every repository enabled on the forge, with the installation it is
-- enabled in.
listEnabledRepos :: ForgeSlug -> M [(RepoId, ForgeInstallationId)]
listEnabledRepos forge' =
  map (\(owner, name, installation) -> (RepoId forge' owner name, installation))
    <$> DB.pgQuery
      [pgSQL|
        SELECT repo_owner, repo_name, installation_id
        FROM forge_installation_repos
        WHERE forge = ${forge'}
        ORDER BY lower(repo_owner), lower(repo_name)
      |]

-- | Enables a repository in an installation. 'Left' the installation it is
-- already enabled in, when that is another one, even one that needs a
-- reinstall: the reinstall is the way back for its repositories. One
-- statement: it either enables the repository or answers the installation
-- holding it, however that one changes meanwhile.
enableRepo :: ForgeInstallationId -> RepoId -> M (Either ForgeInstallationId ())
enableRepo installation repo = do
  let forge' = repo ^. forge
      owner = repo ^. repoUser
      name = repo ^. repoName
  -- DO UPDATE, not DO NOTHING, so that RETURNING answers the holder.
  holders <-
    DB.pgQuery
      [pgSQL|
        INSERT INTO forge_installation_repos (installation_id, forge, repo_owner, repo_name)
        VALUES (${installation}, ${forge'}, ${owner}, ${name})
        ON CONFLICT (forge, lower(repo_owner), lower(repo_name))
          DO UPDATE SET installation_id = forge_installation_repos.installation_id
        RETURNING installation_id
      |]
  case holders of
    [holder] | holder /= installation -> pure $ Left holder
    [_] -> pure $ Right ()
    rows -> throw $ OtherError $ "enabling " <> showRepoId repo <> " answered " <> show (length rows) <> " rows, not one"

disableRepo :: ForgeInstallationId -> RepoId -> M ()
disableRepo installation repo = do
  let owner = repo ^. repoUser
      name = repo ^. repoName
  void
    $ DB.pgExec
      [pgSQL|
        DELETE FROM forge_installation_repos
        WHERE installation_id = ${installation}
          AND lower(repo_owner) = lower(${owner}) AND lower(repo_name) = lower(${name})
      |]
