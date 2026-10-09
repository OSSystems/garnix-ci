-- | Webhook deliveries of the Gitea/Forgejo installations
-- ('ForgeInstallation'), at
-- @/api/forges/:slug/installations/:id/webhook@. GitHub keeps its own route
-- ("Garnix.API.GhWebhooks").
module Garnix.API.ForgeWebhooks
  ( ForgeWebhookAPI,
    forgeWebhookAPI,
    RawJSON,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.Encoding qualified as T
import Garnix.DB.Installations qualified as DB
import Garnix.Forge.Gitea.Webhook
import Garnix.Forge.Installation
import Garnix.GithubUserToken (decryptSecret)
import Garnix.Monad
import Garnix.Monad.Async (logPromiseErrors)
import Garnix.Orchestrator (handleForgeEvent)
import Garnix.Prelude
import Garnix.Types
import Network.HTTP.Media ((//), (/:))
import Servant (Accept (..), MimeUnrender (..))

type ForgeWebhookAPI =
  Capture "slug" ForgeSlug
    :> "installations"
    :> Capture "installation" ForgeInstallationId
    :> "webhook"
    :> Header "X-Gitea-Event" Text
    :> Header "X-Forgejo-Event" Text
    :> Header "X-Gitea-Signature" Text
    :> Header "X-Forgejo-Signature" Text
    :> ReqBody '[RawJSON] LazyByteString
    :> Post '[JSON] ()

-- | A JSON body handed over unparsed: the signature covers the exact bytes
-- that arrived, so they must be checked before anything decodes them.
data RawJSON

instance Accept RawJSON where
  contentTypes _ =
    NonEmpty.fromList
      [ "application" // "json" /: ("charset", "utf-8"),
        "application" // "json"
      ]

instance MimeUnrender RawJSON LazyByteString where
  mimeUnrender _ = Right

forgeWebhookAPI :: (HasCallStack) => ForgeSlug -> ForgeInstallationId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> LazyByteString -> M ()
forgeWebhookAPI slug' delivering giteaEvent forgejoEvent giteaSignature forgejoSignature body = do
  -- A registered forge delivers only while active: a pending one is unproven,
  -- a disabled one refused.
  lookupActiveForge slug' >>= \case
    Just (_, instance') | _forgeInstanceConfig instance' ^. kind == GiteaForgeKind -> pure ()
    _ -> throw NotFound
  installation <-
    DB.getForgeInstallation delivering >>= \case
      Just installation | forgeInstallationForge installation == slug' -> pure installation
      _ -> throw NotFound
  secret <- decryptSecret (forgeInstallationWebhookSecret installation)
  unless (verifyGiteaSignature (T.encodeUtf8 secret) (giteaSignature <|> forgejoSignature) body) $ do
    log Notice $ "forge webhook: rejected a delivery for installation " <> show (getForgeInstallationId delivering) <> " of " <> getForgeSlug slug' <> " with a missing or wrong signature"
    throw Unauthorized
  event <- maybe (throw $ BadRequest "no X-Gitea-Event header") pure (giteaEvent <|> forgejoEvent)
  uniqueId <- randomBase64 64
  withTextSpans [("event_id", uniqueId), ("forge", getForgeSlug slug'), ("installation", show (getForgeInstallationId delivering))] $ do
    hook <- case parseGiteaWebhook slug' event body of
      Left err' -> throw $ BadRequest $ "could not read the " <> event <> " payload: " <> cs err'
      Right hook -> pure hook
    case hook of
      Nothing -> log Informational $ "forge webhook: ignoring a " <> event <> " delivery"
      Just hook' -> do
        withTextSpan ("tag", "forge webhook event") $ log Informational $ show hook'
        let repo = giteaWebhookRepo hook'
        -- Several installations can cover a repository (its owner's, the
        -- instance's): only the one it is enabled in builds it.
        enabledIn <- DB.enabledInstallation repo
        case enabledIn of
          Nothing -> log Informational $ "forge webhook: " <> showRepoId repo <> " is not enabled in any installation, ignoring the delivery"
          Just other
            | forgeInstallationId other /= delivering ->
                log Informational $ "forge webhook: " <> showRepoId repo <> " builds through another installation, ignoring this one's delivery"
          Just _ ->
            resolveRepo repo >>= \case
              Nothing -> log Notice $ "forge webhook: garnix cannot build " <> showRepoId repo <> " (see above), ignoring the delivery"
              Just repoInfo' -> handleForgeEvent (giteaForgeEvent repoInfo' hook') >>= logPromiseErrors
