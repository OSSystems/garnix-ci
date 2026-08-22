module Garnix.API.Keys where

import Data.Text qualified as T
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Monad.SubProcess.Deprecated qualified as Deprecated
import Garnix.Prelude
import Garnix.Types

getRepoPublicKey :: RepoId -> M PublicKey
getRepoPublicKey repo = fst <$> getRepoKeys repo

getActionPublicKey :: RepoId -> PackageName -> M PublicKey
getActionPublicKey repo action = fst <$> getActionKeys repo action

getRepoKeys :: RepoId -> M (PublicKey, PrivateKey)
getRepoKeys repo = do
  mkey <- DB.getRepoKeyDB repo
  case mkey of
    Nothing -> do
      (candidatePubKey, candidatePrivKey) <- generateKeys
      DB.setRepoKeyDB repo candidatePubKey candidatePrivKey
    Just key -> pure key

getActionKeys :: RepoId -> PackageName -> M (PublicKey, PrivateKey)
getActionKeys repo action = do
  mkey <- DB.getActionKeyDB repo action
  case mkey of
    Nothing -> do
      (candidatePubKey, candidatePrivKey) <- generateKeys
      DB.setActionKeyDB repo action candidatePubKey candidatePrivKey
    Just key -> pure key

generateKeys :: M (Candidate PublicKey, Candidate PrivateKey)
generateKeys = do
  output <- Deprecated.runProc "age-keygen" [] []
  case T.lines output of
    [_createdAt, pubKeyLine, privKeyLine] -> do
      unless ("# public key: " `T.isPrefixOf` pubKeyLine)
        $ throw
        $ OtherError "age-keygen responded with unexpected format"
      repoSecretsPubKey <- view #repoSecretsEncryptionPubKey
      privKey <-
        liftIO (makePrivateKey (cs privKeyLine) repoSecretsPubKey) >>= \case
          Left e -> throw $ OtherError e
          Right v -> pure v
      pure
        ( Candidate (PublicKey $ T.drop (T.length "# public key: ") pubKeyLine),
          Candidate privKey
        )
    _ -> throw $ OtherError "age-keygen responded with unexpected format"
