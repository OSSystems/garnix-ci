-- | The @forges@ table: Gitea/Forgejo instances registered through the UI.
module Garnix.DB.Forges
  ( RegisteredForgeRow (..),
    databaseNow,
    getRegisteredForge,
    getLiveRegisteredForge,
    listActiveRegisteredForges,
  )
where

import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.DB qualified as DB
import Garnix.Forge.Registered (ForgeStatus (..), parseForgeStatus, pendingForgeTtl)
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
-- timestamps come from: 'pendingForgeTtl' in seconds, for @make_interval@.
ttlSeconds :: Double
ttlSeconds = realToFrac pendingForgeTtl

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
