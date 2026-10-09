-- | The @forges@ table: Gitea/Forgejo instances registered through the UI.
module Garnix.DB.Forges
  ( RegisteredForgeRow (..),
    PendingForge (..),
    databaseNow,
    getRegisteredForge,
    getLiveRegisteredForge,
    listActiveRegisteredForges,
    upsertPendingForge,
    activatePendingForge,
    replaceForgeClientSecret,
    disableForge,
    lastDisabled,
    forgetIdentitiesOn,
    purgeStalePendingForges,
  )
where

import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.DB qualified as DB
import Garnix.Forge.Registered (ForgeStatus (..), LastDisabled (..), disabledForgeQuarantine, parseForgeStatus, pendingForgeLock, pendingForgeTtl)
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types

-- | A row, secrets still encrypted.
data RegisteredForgeRow = RegisteredForgeRow
  { rowSlug :: ForgeSlug,
    rowWebUrl :: Text,
    rowApiUrl :: Text,
    rowOAuthClientId :: Text,
    rowOAuthClientSecret :: EncryptedText,
    rowWebhookSecret :: EncryptedText,
    rowStatus :: ForgeStatus,
    -- | The account that completed its first OAuth.
    rowRegisteredBy :: Maybe UserId,
    -- | When the current registration was submitted (by another browser than
    -- the one that last replaced it, if any).
    rowCreatedAt :: UTCTime,
    -- | Hash of the token of the browser that submitted a pending
    -- registration.
    rowRegistrationTokenHash :: Maybe Text,
    -- | Changes whenever anything in the row does, so it can key a cache of
    -- the decrypted forge.
    rowUpdatedAt :: UTCTime
  }

type Columns = (ForgeSlug, Text, Text, Text, EncryptedText, EncryptedText, Text, Maybe UserId, Maybe Text, UTCTime, UTCTime)

fromColumns :: Columns -> Either Text RegisteredForgeRow
fromColumns (slug', webUrl', apiUrl', clientId, clientSecret, webhookSecret', status', registeredBy, tokenHash, createdAt, updatedAt) = do
  status'' <- maybe (Left $ "unknown status of the forge " <> getForgeSlug slug' <> ": " <> status') Right $ parseForgeStatus status'
  pure
    RegisteredForgeRow
      { rowSlug = slug',
        rowWebUrl = webUrl',
        rowApiUrl = apiUrl',
        rowOAuthClientId = clientId,
        rowOAuthClientSecret = clientSecret,
        rowWebhookSecret = webhookSecret',
        rowStatus = status'',
        rowRegisteredBy = registeredBy,
        rowCreatedAt = createdAt,
        rowRegistrationTokenHash = tokenHash,
        rowUpdatedAt = updatedAt
      }

decodeRows :: [Columns] -> M [RegisteredForgeRow]
decodeRows = either (throw . OtherError) pure . traverse fromColumns

-- | Every time a row is compared with is the database's, the clock its
-- timestamps come from: 'pendingForgeTtl', 'pendingForgeLock' and
-- 'disabledForgeQuarantine' in seconds, for @make_interval@.
ttlSeconds, lockSeconds, quarantineSeconds :: Double
ttlSeconds = realToFrac pendingForgeTtl
lockSeconds = realToFrac pendingForgeLock
quarantineSeconds = realToFrac disabledForgeQuarantine

-- | The time on the database's clock, which stamps every row.
databaseNow :: M UTCTime
databaseNow =
  DB.pgQuery [pgSQL| SELECT now() |] >>= \case
    [Just now] -> pure now
    _ -> throw $ OtherError "the database did not tell the time"

-- | The row, whatever its status.
getRegisteredForge :: ForgeSlug -> M (Maybe RegisteredForgeRow)
getRegisteredForge slug' = do
  rows <-
    DB.pgQuery
      [pgSQL|
        SELECT slug, web_url, api_url, oauth_client_id, oauth_client_secret,
               webhook_secret, status, registered_by, registration_token_hash, created_at, updated_at
        FROM forges
        WHERE slug = ${slug'}
      |]
  listToMaybe <$> decodeRows rows

-- | The row if the forge is active, or pending and not stale.
getLiveRegisteredForge :: ForgeSlug -> M (Maybe RegisteredForgeRow)
getLiveRegisteredForge slug' = do
  rows <-
    DB.pgQuery
      [pgSQL|
        SELECT slug, web_url, api_url, oauth_client_id, oauth_client_secret,
               webhook_secret, status, registered_by, registration_token_hash, created_at, updated_at
        FROM forges
        WHERE slug = ${slug'}
          AND (status = 'active'
               OR (status = 'pending' AND updated_at > now() - make_interval(secs => ${ttlSeconds})))
      |]
  listToMaybe <$> decodeRows rows

listActiveRegisteredForges :: M [RegisteredForgeRow]
listActiveRegisteredForges = do
  rows <-
    DB.pgQuery
      [pgSQL|
        SELECT slug, web_url, api_url, oauth_client_id, oauth_client_secret,
               webhook_secret, status, registered_by, registration_token_hash, created_at, updated_at
        FROM forges
        WHERE status = 'active'
        ORDER BY slug
      |]
  decodeRows rows

-- | A registration as submitted, secrets encrypted.
data PendingForge = PendingForge
  { pendingSlug :: ForgeSlug,
    pendingWebUrl :: Text,
    pendingApiUrl :: Text,
    pendingOAuthClientId :: Text,
    pendingOAuthClientSecret :: EncryptedText,
    pendingWebhookSecret :: EncryptedText,
    -- | Hash of the token of the browser that submits it.
    pendingTokenHash :: Text
  }

-- | Stores a registration as pending, owned by the browser holding the
-- token whose hash it carries. It replaces a disabled forge on the slug
-- (keeping when it was disabled, see 'lastDisabled'), a
-- pending registration submitted more than 'pendingForgeLock' ago, or one the
-- same browser submitted (proven by the hash of the token it presented, the
-- second argument), which keeps its submission time so that re-submitting
-- does not extend the lock. 'False' when the slug is taken: active, or held
-- by another browser.
upsertPendingForge :: PendingForge -> Maybe Text -> M Bool
upsertPendingForge PendingForge {..} presentedHash = do
  rows <-
    DB.pgQuery
      [pgSQL|
        INSERT INTO forges
          (slug, kind, web_url, api_url, oauth_client_id, oauth_client_secret, webhook_secret, status, registration_token_hash)
        VALUES
          (${pendingSlug}, 'gitea', ${pendingWebUrl}, ${pendingApiUrl}, ${pendingOAuthClientId}, ${pendingOAuthClientSecret}, ${pendingWebhookSecret}, 'pending', ${pendingTokenHash})
        ON CONFLICT (slug) DO UPDATE SET
          web_url = EXCLUDED.web_url,
          api_url = EXCLUDED.api_url,
          oauth_client_id = EXCLUDED.oauth_client_id,
          oauth_client_secret = EXCLUDED.oauth_client_secret,
          webhook_secret = EXCLUDED.webhook_secret,
          status = 'pending',
          registered_by = NULL,
          registration_token_hash = EXCLUDED.registration_token_hash,
          created_at = CASE
            WHEN forges.status = 'pending' AND forges.registration_token_hash = ${presentedHash}
              THEN forges.created_at
            ELSE now()
          END,
          updated_at = now()
        WHERE forges.status = 'disabled'
          OR (forges.status = 'pending'
              AND (forges.created_at <= now() - make_interval(secs => ${lockSeconds})
                   OR forges.registration_token_hash = ${presentedHash}))
        RETURNING slug
      |]
  pure $ not (null (rows :: [ForgeSlug]))

-- | Marks a pending, not stale, forge active, recording the account that
-- registered it, if the browser that completed the OAuth is the one that
-- submitted it. 'False' when there was no such forge.
activatePendingForge :: ForgeSlug -> UserId -> Text -> M Bool
activatePendingForge slug' registrant tokenHash =
  (> 0)
    <$> DB.pgExec
      [pgSQL|
        UPDATE forges
        SET status = 'active', registered_by = ${registrant}, registration_token_hash = NULL,
            disabled_at = NULL, updated_at = now()
        WHERE slug = ${slug'} AND status = 'pending'
          AND updated_at > now() - make_interval(secs => ${ttlSeconds})
          AND registration_token_hash = ${tokenHash}
      |]

-- | Replaces the client secret of an active or disabled forge, which makes a
-- disabled one active again at once, with everyone who logged in through it.
replaceForgeClientSecret :: ForgeSlug -> EncryptedText -> M ()
replaceForgeClientSecret slug' clientSecret =
  void
    $ DB.pgExec
      [pgSQL|
        UPDATE forges
        SET oauth_client_secret = ${clientSecret}, status = 'active', disabled_at = NULL, updated_at = now()
        WHERE slug = ${slug'} AND status IN ('active', 'disabled')
      |]

-- | Disables an active forge: no more logins, sessions or webhooks through
-- it. It deletes nothing: a new secret ('replaceForgeClientSecret') brings it
-- back as it was, and so does a new registration within
-- 'disabledForgeQuarantine' ('lastDisabled').
disableForge :: ForgeSlug -> M ()
disableForge slug' =
  void
    $ DB.pgExec
      [pgSQL|
        UPDATE forges
        SET status = 'disabled', disabled_at = now(), updated_at = now()
        WHERE slug = ${slug'} AND status = 'active'
      |]

-- | When the forge on the slug was last disabled, the row locked until the
-- end of the transaction: a registration of a disabled forge keeps the time
-- until it is activated.
lastDisabled :: ForgeSlug -> M LastDisabled
lastDisabled slug' = do
  rows <-
    DB.pgQuery
      [pgSQL|
        SELECT disabled_at > now() - make_interval(secs => ${quarantineSeconds})
        FROM forges
        WHERE slug = ${slug'} AND disabled_at IS NOT NULL
        FOR UPDATE
      |]
  pure $ case rows of
    [Just True] -> DisabledWithinQuarantine
    [_] -> DisabledBeforeQuarantine
    _ -> NeverDisabled

-- | Deletes every identity on the forge, with its stored OAuth credentials
-- and module settings, and then the accounts that are left with no identity
-- at all, with their access tokens. Run when a new registration of the
-- forge's host is activated, in that transaction, unless it brings back a
-- forge disabled within 'disabledForgeQuarantine'
-- ('Garnix.Forge.Registered.forgetOnActivation'): whoever holds the host now
-- (its domain may have expired and been bought) must land in none of the
-- accounts of the people who logged in through it before. Builds, their
-- requesters and other history name logins, not accounts, and stay.
forgetIdentitiesOn :: ForgeSlug -> M ()
forgetIdentitiesOn slug' = DB.withinTransaction $ do
  -- Module settings belong to an identity (none exist outside github.com,
  -- but the key would refuse the delete).
  void
    $ DB.pgExec
      [pgSQL|
        DELETE FROM module_values
        WHERE module_user_repo_id IN (SELECT id FROM module_user_repo WHERE forge = ${slug'})
      |]
  void $ DB.pgExec [pgSQL| DELETE FROM module_user_repo WHERE forge = ${slug'} |]
  -- The accounts whose only identity is on the forge go first, their
  -- identities with them.
  void
    $ DB.pgExec
      [pgSQL|
        DELETE FROM access_tokens
        WHERE user_id IN (SELECT user_id FROM forge_identities WHERE forge = ${slug'})
          AND NOT EXISTS (
            SELECT 1 FROM forge_identities AS other
            WHERE other.user_id = access_tokens.user_id AND other.forge <> ${slug'}
          )
      |]
  void
    $ DB.pgExec
      [pgSQL|
        DELETE FROM users
        WHERE id IN (SELECT user_id FROM forge_identities WHERE forge = ${slug'})
          AND NOT EXISTS (
            SELECT 1 FROM forge_identities AS other
            WHERE other.user_id = users.id AND other.forge <> ${slug'}
          )
      |]
  void $ DB.pgExec [pgSQL| DELETE FROM forge_identities WHERE forge = ${slug'} |]

-- | Deletes registrations that never completed an OAuth in time, except one
-- of a forge disabled within 'disabledForgeQuarantine': its row keeps when
-- that was, so that the next registration still brings everyone back.
purgeStalePendingForges :: M ()
purgeStalePendingForges =
  void
    $ DB.pgExec
      [pgSQL|
        DELETE FROM forges
        WHERE status = 'pending' AND updated_at <= now() - make_interval(secs => ${ttlSeconds})
          AND (disabled_at IS NULL OR disabled_at <= now() - make_interval(secs => ${quarantineSeconds}))
      |]
