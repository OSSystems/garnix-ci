-- | garnix installed on a Gitea/Forgejo account, organisation or instance: a
-- webhook garnix created there with the token of whoever installed it, and
-- the repositories it builds ("Garnix.API.Installations",
-- "Garnix.DB.Installations"). The id is in "Garnix.Types", with the
-- credentials that carry it.
module Garnix.Forge.Installation
  ( InstallTarget (..),
    renderTargetKind,
    targetName,
    sameInstallTarget,
    describeTarget,
    parseInstallTarget,
    ForgeInstallationStatus (..),
    ForgeInstallation (..),
    forgeInstallationStatus,
    coversRepo,
    installationWebhookPath,
  )
where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Garnix.Prelude
import Garnix.Types

-- | Where garnix is installed, and so which repositories its webhook
-- delivers the events of.
data InstallTarget
  = -- | A user's own repositories (a user webhook).
    InstallOnUser GhLogin
  | -- | An organisation's repositories (an organisation webhook), which only
    -- its owners may create.
    InstallOnOrg GhLogin
  | -- | Every repository on the instance (a system webhook), which only its
    -- site administrators may create.
    InstallOnInstance
  deriving stock (Eq, Show)

renderTargetKind :: InstallTarget -> Text
renderTargetKind = \case
  InstallOnUser _ -> "user"
  InstallOnOrg _ -> "org"
  InstallOnInstance -> "instance"

-- | The login of the account or organisation; empty for the instance.
targetName :: InstallTarget -> Text
targetName = \case
  InstallOnUser login' -> getGhLogin login'
  InstallOnOrg org -> getGhLogin org
  InstallOnInstance -> ""

-- | Whether two targets are the same, as Gitea compares logins: without
-- regard to case.
sameInstallTarget :: InstallTarget -> InstallTarget -> Bool
sameInstallTarget a b = renderTargetKind a == renderTargetKind b && T.toLower (targetName a) == T.toLower (targetName b)

-- | The target, in a sentence.
describeTarget :: InstallTarget -> Text
describeTarget = \case
  InstallOnUser login' -> "the account " <> getGhLogin login'
  InstallOnOrg org -> "the organisation " <> getGhLogin org
  InstallOnInstance -> "the instance"

-- | The target of a kind and a name, as 'renderTargetKind' and 'targetName'
-- write them.
parseInstallTarget :: Text -> Text -> Either Text InstallTarget
parseInstallTarget kind' name = case kind' of
  "user" -> InstallOnUser <$> named
  "org" -> InstallOnOrg <$> named
  "instance"
    | T.null name -> Right InstallOnInstance
    | otherwise -> Left "an installation on the instance names no account"
  _ -> Left $ "unknown kind of installation " <> show kind' <> " (user, org or instance)"
  where
    named
      | T.null (T.strip name) = Left $ "an installation on a " <> kind' <> " names it"
      | otherwise = Right $ GhLogin (T.strip name)

-- | Whether builds go through an installation.
data ForgeInstallationStatus
  = InstallationActive
  | -- | Builds stopped, for the given reason, until someone allowed to
    -- install on its target reinstalls it.
    NeedsReinstall Text
  deriving stock (Eq, Show)

data ForgeInstallation = ForgeInstallation
  { forgeInstallationId :: ForgeInstallationId,
    forgeInstallationForge :: ForgeSlug,
    forgeInstallationTarget :: InstallTarget,
    -- | The login, on 'forgeInstallationForge', whose token garnix calls the
    -- forge with. 'Nothing' once that identity is gone.
    forgeInstallationInstaller :: Maybe GhLogin,
    -- | The webhook on the forge, once garnix created it.
    forgeInstallationHookId :: Maybe Integer,
    forgeInstallationWebhookSecret :: EncryptedText,
    -- | Why builds stopped, if they did.
    forgeInstallationNeedsReinstall :: Maybe Text,
    forgeInstallationCreatedAt :: UTCTime
  }
  deriving stock (Show)

-- | An installation builds until something stops it. The database keeps
-- one without an installer stopped (the trigger on @forge_identities@ and a
-- check), so the reason is only ever written there.
forgeInstallationStatus :: ForgeInstallation -> ForgeInstallationStatus
forgeInstallationStatus installation = maybe InstallationActive NeedsReinstall (forgeInstallationNeedsReinstall installation)

-- | What the API shows of an installation: never its secret.
instance ToJSON ForgeInstallation where
  toJSON installation =
    Aeson.object
      [ "id" .= forgeInstallationId installation,
        "forge" .= forgeInstallationForge installation,
        "kind" .= renderTargetKind (forgeInstallationTarget installation),
        "target" .= targetName (forgeInstallationTarget installation),
        "installer" .= forgeInstallationInstaller installation,
        "status" .= case forgeInstallationStatus installation of
          InstallationActive -> "active" :: Text
          NeedsReinstall _ -> "needs_reinstall",
        "reason" .= case forgeInstallationStatus installation of
          InstallationActive -> Nothing
          NeedsReinstall reason -> Just reason,
        "createdAt" .= forgeInstallationCreatedAt installation
      ]

-- | Whether the webhook of an installation on the target delivers the
-- events of the repository: a user's or an organisation's own repositories
-- (Gitea compares logins case-insensitively), or any on the instance.
coversRepo :: InstallTarget -> RepoId -> Bool
coversRepo target repo = case target of
  InstallOnUser login' -> owns login'
  InstallOnOrg org -> owns org
  InstallOnInstance -> True
  where
    owns login' = T.toLower (getGhLogin login') == T.toLower (getGhLogin $ getGhRepoOwner $ repo ^. repoUser)

-- | Where the forge delivers an installation's webhook, relative to garnix.
installationWebhookPath :: ForgeSlug -> ForgeInstallationId -> Text
installationWebhookPath slug' installation =
  "api/forges/" <> getForgeSlug slug' <> "/installations/" <> show (getForgeInstallationId installation) <> "/webhook"
