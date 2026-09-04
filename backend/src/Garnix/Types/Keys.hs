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
    InstallPublicFileOpts (..),
    unsafeDecryptPrivateKey,
    exportKeys,
    exportKeysSshArgs,
    installPublicFile,
    installPublicFileSshArgs,
    deriveSshPublicKey,
  )
where

import Data.ByteString qualified as SBS
import Data.Text qualified as T
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
    sshArgs :: [Text],
    -- | Which guest account to write as. First provisioning happens over root;
    -- a redeploy of a persistent guest goes through the guest's established
    -- @garnix@ account instead, so this is not always @root@.
    sshUser :: Text,
    -- | Whether that account needs @sudo@ to write 'targetPath'.
    sshSudo :: Bool
  }

data InstallPublicFileOpts = InstallPublicFileOpts
  { installPublicFileContents :: Text,
    installPublicFileIpAddr :: Text,
    installPublicFileTargetPath :: FilePath,
    installPublicFileSshOptions :: [Text],
    installPublicFileSshUser :: Text,
    installPublicFileSshSudo :: Bool
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
        Proc.readProcessWithExitCode "ssh" (exportKeysSshArgs opts) (cs privKey)
      case exitCode of
        ExitSuccess -> pure $ Right ()
        ExitFailure _ -> pure $ Left "Exporting keys failed"

-- | The ssh argv for streaming a decrypted repo key onto a guest. Exported so
-- specs can pin the privilege boundary rather than infer it.
exportKeysSshArgs :: ExportKeysOpts -> [String]
exportKeysSshArgs opts =
  (cs <$> sshArgs opts)
    <> [cs (sshUser opts) <> "@" <> cs (ipAddr opts)]
    <> (if sshSudo opts then ["sudo", "-n"] else [])
    <> ["tee", targetPath opts, ">/dev/null"]

-- | Write a non-secret file onto a guest over ssh.
installPublicFile :: InstallPublicFileOpts -> IO (Either Text ())
installPublicFile opts = do
  -- Discard command output: even for public contents, neither the transport
  -- nor an unexpected remote error should be able to add them to our logs.
  (exitCode, _, _) <-
    Proc.readProcessWithExitCode
      "ssh"
      (installPublicFileSshArgs opts)
      (cs (installPublicFileContents opts <> "\n"))
  case exitCode of
    ExitSuccess -> pure $ Right ()
    ExitFailure _ -> pure $ Left "Installing public file failed"

installPublicFileSshArgs :: InstallPublicFileOpts -> [String]
installPublicFileSshArgs opts =
  (cs <$> installPublicFileSshOptions opts)
    <> [cs (installPublicFileSshUser opts) <> "@" <> cs (installPublicFileIpAddr opts)]
    <> (if installPublicFileSshSudo opts then ["sudo", "-n"] else [])
    <> ["install", "-D", "-m", "0644", "/dev/stdin", installPublicFileTargetPath opts]

-- | Derive an ssh public key from a private key file, so the backend can tell
-- a guest which key to trust without keeping the pair on disk twice.
deriveSshPublicKey :: FilePath -> IO (Either Text Text)
deriveSshPublicKey privateKeyPath = do
  (exitCode, stdout, _) <-
    Proc.readProcessWithExitCode "ssh-keygen" ["-y", "-f", privateKeyPath] ""
  let publicKey = T.strip (cs stdout)
  case exitCode of
    ExitSuccess | not (T.null publicKey) -> pure $ Right publicKey
    _ -> pure $ Left "Deriving SSH public key failed"

makePrisms ''PublicKey
