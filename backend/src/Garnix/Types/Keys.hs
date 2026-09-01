{-# LANGUAGE TemplateHaskell #-}

module Garnix.Types.Keys
  ( PublicKey (..),
    _PublicKey,
    PrivateKey,
    makePrivateKey,
    ExportKeysOpts (..),
    RepoSecretsEncryptionKeyPath (..),
    RepoSecretsEncryptionPubKey (..),
    ageEncrypt,
    ageDecrypt,
    unsafeDecryptPrivateKey,
    exportKeys,
  )
where

import Data.ByteString qualified as SBS
import Development.Shake qualified as Shake
import Garnix.Prelude
import Servant qualified
import System.Exit
import System.Process qualified as Proc

newtype PublicKey = PublicKey {getPublicKey :: Text}
  deriving stock (Eq, Show)
  deriving newtype
    ( Servant.MimeRender Servant.PlainText,
      PGColumn "character varying",
      PGColumn "text",
      PGParameter "character varying",
      PGParameter "text"
    )

-- * Private keys

-- Don't export the constructor or allow deserialization (besides DB).
-- That way only in this module can there be dangerous uses.
newtype PrivateKey = PrivateKey SBS.ByteString
  deriving newtype
    ( PGColumn "bytea",
      PGParameter "bytea"
    )

newtype RepoSecretsEncryptionKeyPath = RepoSecretsEncryptionKeyPath FilePath

newtype RepoSecretsEncryptionPubKey = RepoSecretsEncryptionPubKey Text

ageEncrypt :: RepoSecretsEncryptionPubKey -> Text -> IO (Either Text SBS.ByteString)
ageEncrypt (RepoSecretsEncryptionPubKey i) unencrypted = do
  result <-
    Shake.cmd
      ("age" :: String)
      ["--recipient" :: String, cs i]
      (Shake.Stdin $ cs unencrypted)
  case result of
    (Shake.Exit ExitSuccess, Shake.Stdout (stdout :: SBS.ByteString)) -> pure $ Right stdout
    _ -> pure $ Left "Encrypting failed"

ageDecrypt :: RepoSecretsEncryptionKeyPath -> SBS.ByteString -> IO (Either Text Text)
ageDecrypt (RepoSecretsEncryptionKeyPath i) encrypted = do
  result <-
    Shake.cmd
      ("age" :: String)
      ["--decrypt" :: String, "-i", i]
      (Shake.StdinBS $ SBS.fromStrict encrypted)
  case result of
    (Shake.Exit ExitSuccess, Shake.Stdout (stdout :: String)) -> pure . Right $ cs stdout
    _ -> pure $ Left "Decrypting failed"

makePrivateKey :: Text -> RepoSecretsEncryptionPubKey -> IO (Either Text PrivateKey)
makePrivateKey unencrypted pubKey =
  bimap (const "Encrypting keys failed") PrivateKey <$> ageEncrypt pubKey unencrypted

unsafeDecryptPrivateKey :: PrivateKey -> RepoSecretsEncryptionKeyPath -> IO (Either Text Text)
unsafeDecryptPrivateKey (PrivateKey key) keyPath =
  first (const "Decrypting keys failed") <$> ageDecrypt keyPath key

data ExportKeysOpts = ExportKeysOpts
  { privateKey :: PrivateKey,
    ipAddr :: Text,
    targetPath :: FilePath,
    sshArgs :: [Text]
  }

exportKeys :: ExportKeysOpts -> RepoSecretsEncryptionKeyPath -> IO (Either Text ())
exportKeys opts id = do
  eprivKey <- unsafeDecryptPrivateKey (privateKey opts) id
  case eprivKey of
    Left e -> return $ Left e
    Right privKey -> do
      -- Use stdin so we don't have to store the key in the filesystem. We don't
      -- capture or log stderr in case they contain the key.
      (exitCode, _, _) <-
        Proc.readProcessWithExitCode
          "ssh"
          ( (cs <$> sshArgs opts) <> ["root@" <> cs (ipAddr opts), "cat >" <> targetPath opts]
          )
          (cs privKey)
      case exitCode of
        ExitSuccess -> pure $ Right ()
        ExitFailure _ -> pure $ Left "Exporting keys failed"

makePrisms ''PublicKey
