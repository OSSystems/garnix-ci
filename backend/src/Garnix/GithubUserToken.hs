module Garnix.GithubUserToken
  ( storeCredentialsFor,
    githubTokenFor,
    withGithubUserToken,
  )
where

import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types

renewalLeeway :: Duration
renewalLeeway = fromMinutes @Int 5

storeCredentialsFor :: UserId -> GhUserCredentials Text -> M ()
storeCredentialsFor userId credentials =
  traverse encryptSecret credentials >>= DB.upsertGithubUserCredentials userId

githubTokenFor :: (HasCallStack) => User -> M GhToken
githubTokenFor user = do
  stored <- credentialsOf (user ^. id)
  now <- liftIO getCurrentTime
  if isRunningOut now stored
    then renewRunningOutToken (user ^. id)
    else GhToken <$> decryptSecret (stored ^. accessToken)

withGithubUserToken :: (HasCallStack) => User -> (GhToken -> M a) -> M a
withGithubUserToken user action = do
  token <- githubTokenFor user
  result <- try $ action token
  case result of
    Right a -> pure a
    Left e | err e == GithubUserTokenRejected -> do
      log Notice "github rejected the stored user token, renewing it"
      renewRejectedToken (user ^. id) token >>= action
    Left e -> throwError e

credentialsOf :: UserId -> M (GhUserCredentials EncryptedText)
credentialsOf userId =
  DB.getGithubUserCredentials userId >>= maybe (throw GithubSessionExpired) pure

isRunningOut :: UTCTime -> GhUserCredentials secret -> Bool
isRunningOut now credentials = case credentials ^. accessTokenExpiresAt of
  Nothing -> False
  Just expiresAt -> expiresAt <= addTime renewalLeeway now

renewRunningOutToken :: UserId -> M GhToken
renewRunningOutToken userId = withCredentialsLock userId $ \stored -> do
  now <- liftIO getCurrentTime
  if isRunningOut now stored
    then renew userId stored
    else GhToken <$> decryptSecret (stored ^. accessToken)

renewRejectedToken :: UserId -> GhToken -> M GhToken
renewRejectedToken userId rejected = withCredentialsLock userId $ \stored -> do
  current <- GhToken <$> decryptSecret (stored ^. accessToken)
  if current == rejected
    then renew userId stored
    else pure current

withCredentialsLock :: UserId -> (GhUserCredentials EncryptedText -> M GhToken) -> M GhToken
withCredentialsLock userId action =
  endSessionOnRefusal userId $ DB.pgTransaction $ do
    stored <- DB.lockGithubUserCredentials userId >>= maybe (throw GithubSessionExpired) pure
    action stored

renew :: UserId -> GhUserCredentials EncryptedText -> M GhToken
renew userId stored = do
  now <- liftIO getCurrentTime
  encryptedRefreshToken <- maybe (throw GithubSessionExpired) pure $ stored ^. refreshToken
  case stored ^. refreshTokenExpiresAt of
    Just expiresAt | expiresAt <= now -> throw GithubSessionExpired
    _ -> pure ()
  renewed <- refreshUserCredentials =<< decryptSecret encryptedRefreshToken
  storeCredentialsFor userId renewed
  pure $ GhToken $ renewed ^. accessToken

endSessionOnRefusal :: UserId -> M a -> M a
endSessionOnRefusal userId action = do
  result <- try action
  case result of
    Right a -> pure a
    Left e | err e `elem` [GithubDidntGiveUsAToken, GithubSessionExpired] -> do
      DB.deleteGithubUserCredentials userId
      throw GithubSessionExpired
    Left e -> throwError e

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
