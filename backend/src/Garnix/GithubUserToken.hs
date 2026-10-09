-- | The OAuth credentials of each forge identity, renewed by the forge that
-- issued them.
module Garnix.GithubUserToken
  ( storeCredentialsFor,
    userTokenFor,
    withUserToken,
    encryptSecret,
    decryptSecret,
  )
where

import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types

renewalLeeway :: Duration
renewalLeeway = fromMinutes @Int 5

storeCredentialsFor :: ForgeLogin -> GhUserCredentials Text -> M ()
storeCredentialsFor identity credentials =
  traverse encryptSecret credentials >>= DB.setIdentityCredentials identity

userTokenFor :: (HasCallStack) => ForgeLogin -> M GhToken
userTokenFor identity = do
  stored <- credentialsOf identity
  now <- liftIO getCurrentTime
  if isRunningOut now stored
    then renewRunningOutToken identity
    else GhToken <$> decryptSecret (stored ^. accessToken)

withUserToken :: (HasCallStack) => ForgeLogin -> (GhToken -> M a) -> M a
withUserToken identity action = do
  token <- userTokenFor identity
  result <- try $ action token
  case result of
    Right a -> pure a
    Left e | err e == GithubUserTokenRejected -> do
      log Notice "the forge rejected the stored user token, renewing it"
      renewRejectedToken identity token >>= action
    Left e -> throwError e

credentialsOf :: ForgeLogin -> M (GhUserCredentials EncryptedText)
credentialsOf identity =
  DB.getIdentityCredentials identity >>= maybe (throw $ ForgeSessionExpired $ identity ^. forge) pure

isRunningOut :: UTCTime -> GhUserCredentials secret -> Bool
isRunningOut now credentials = case credentials ^. accessTokenExpiresAt of
  Nothing -> False
  Just expiresAt -> expiresAt <= addTime renewalLeeway now

renewRunningOutToken :: ForgeLogin -> M GhToken
renewRunningOutToken identity = withCredentialsLock identity $ \stored -> do
  now <- liftIO getCurrentTime
  if isRunningOut now stored
    then renew identity stored
    else GhToken <$> decryptSecret (stored ^. accessToken)

renewRejectedToken :: ForgeLogin -> GhToken -> M GhToken
renewRejectedToken identity rejected = withCredentialsLock identity $ \stored -> do
  current <- GhToken <$> decryptSecret (stored ^. accessToken)
  if current == rejected
    then renew identity stored
    else pure current

withCredentialsLock :: ForgeLogin -> (GhUserCredentials EncryptedText -> M GhToken) -> M GhToken
withCredentialsLock identity action =
  endSessionOnRefusal identity $ DB.pgTransaction $ do
    stored <- DB.lockIdentityCredentials identity >>= maybe (throw $ ForgeSessionExpired $ identity ^. forge) pure
    action stored

-- | Renewed by the forge that issued the credentials: the identity's own.
renew :: ForgeLogin -> GhUserCredentials EncryptedText -> M GhToken
renew identity stored = do
  now <- liftIO getCurrentTime
  encryptedRefreshToken <- maybe (throw $ ForgeSessionExpired $ identity ^. forge) pure $ stored ^. refreshToken
  case stored ^. refreshTokenExpiresAt of
    Just expiresAt | expiresAt <= now -> throw $ ForgeSessionExpired $ identity ^. forge
    _ -> pure ()
  renewed <- refreshUserCredentials (identity ^. forge) =<< decryptSecret encryptedRefreshToken
  storeCredentialsFor identity renewed
  pure $ GhToken $ renewed ^. accessToken

endSessionOnRefusal :: ForgeLogin -> M a -> M a
endSessionOnRefusal identity action = do
  result <- try action
  case result of
    Right a -> pure a
    Left e | refused (err e) -> do
      DB.deleteIdentityCredentials identity
      throw $ ForgeSessionExpired $ identity ^. forge
    Left e -> throwError e
  where
    refused = \case
      GithubDidntGiveUsAToken -> True
      ForgeSessionExpired {} -> True
      _ -> False

encryptSecret :: Text -> M EncryptedText
encryptSecret plaintext = do
  pubKey <- view #repoSecretsEncryptionPubKey
  liftIO (ageEncrypt pubKey plaintext) >>= \case
    Left e -> throw $ OtherError $ "could not encrypt a github credential: " <> e
    Right encrypted -> pure $ EncryptedText encrypted

decryptSecret :: EncryptedText -> M Text
decryptSecret (EncryptedText encrypted) = do
  keyPath <- view #repoSecretsEncryptionKeyPath
  liftIO (ageDecrypt keyPath encrypted) >>= \case
    Left e -> throw $ OtherError $ "could not decrypt a github credential: " <> e
    Right plaintext -> pure plaintext
