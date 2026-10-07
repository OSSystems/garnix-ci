-- | Webhook deliveries from the forge instances configured in
-- @GARNIX_FORGES_FILE@, at @/api/forges/:slug/webhook@. GitHub keeps its own
-- route ("Garnix.API.GhWebhooks").
module Garnix.API.ForgeWebhooks
  ( ForgeWebhookAPI,
    forgeWebhookAPI,
    RawJSON,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Garnix.Forge.Gitea.Webhook
import Garnix.Monad
import Garnix.Monad.Async (logPromiseErrors)
import Garnix.Orchestrator (handleForgeEvent)
import Garnix.Prelude
import Garnix.Types
import Network.HTTP.Media ((//), (/:))
import Servant (Accept (..), MimeUnrender (..))

type ForgeWebhookAPI =
  Capture "slug" ForgeSlug
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

forgeWebhookAPI :: (HasCallStack) => ForgeSlug -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> LazyByteString -> M ()
forgeWebhookAPI slug' giteaEvent forgejoEvent giteaSignature forgejoSignature body = do
  configured <- view #forges
  config <- case Map.lookup slug' configured of
    Just instance' | _forgeInstanceConfig instance' ^. kind == GiteaForgeKind -> pure $ _forgeInstanceConfig instance'
    _ -> throw NotFound
  unless (verifyGiteaSignature (config ^. webhookSecret) (giteaSignature <|> forgejoSignature) body) $ do
    log Notice $ "forge webhook: rejected a delivery for " <> getForgeSlug slug' <> " with a missing or wrong signature"
    throw Unauthorized
  event <- maybe (throw $ BadRequest "no X-Gitea-Event header") pure (giteaEvent <|> forgejoEvent)
  uniqueId <- randomBase64 64
  withTextSpans [("event_id", uniqueId), ("forge", getForgeSlug slug')] $ do
    hook <- case parseGiteaWebhook slug' event body of
      Left err -> throw $ BadRequest $ "could not read the " <> event <> " payload: " <> cs err
      Right hook -> pure hook
    case hook of
      Nothing -> log Informational $ "forge webhook: ignoring a " <> event <> " delivery"
      Just hook' -> do
        withTextSpan ("tag", "forge webhook event") $ log Informational $ show hook'
        resolveRepo (giteaWebhookRepo hook') >>= \case
          Nothing ->
            log Notice
              $ "forge webhook: garnix is not installed on "
              <> show (giteaWebhookRepo hook')
              <> " (its bot cannot push there), ignoring the delivery"
          Just repoInfo' ->
            handleForgeEvent (giteaForgeEvent repoInfo' hook') >>= logPromiseErrors
