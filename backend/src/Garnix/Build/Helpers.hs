module Garnix.Build.Helpers
  ( withPrivateNixXdgCache,
    withInternalCacheToken,
    withNetRcEntries,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Monad.ForkT (safeSystemTempDirectory, safeSystemTempFile)
import Garnix.NixConfig qualified as NixConfig
import Garnix.Prelude
import Garnix.Types as Types
import System.IO (hClose)

-- We need to make sure each build has its own cache in order
-- to avoid leaking private repositories between organisations.
withPrivateNixXdgCache :: M a -> M a
withPrivateNixXdgCache action = do
  tempDir <- safeSystemTempDirectory "garnix-cache"
  local (#nixXdgCacheDir ?~ tempDir) $ do
    action <?> "running action with private nix xdg cache"

withInternalCacheToken :: ForgeLogin -> M a -> M a
withInternalCacheToken reqUser cont = do
  token <- DB.getUserInternalToken reqUser
  withNetRcEntries
    [ NixConfig.NetRcEntry
        { NixConfig._netRcEntryMachine = "cache.garnix.io",
          NixConfig._netRcEntryLogin = forgeLoginText reqUser,
          NixConfig._netRcEntryPassword = getInternalCacheToken token
        }
    ]
    $ withTextSpan ("internal_token", show reqUser) cont

-- | Hands nix a netrc file with these entries on top of the ones in the netrc
-- file it is already handed. Nix takes a single @netrc-file@, and the cache
-- and private flake inputs each need their own machine in it, so replacing
-- the file would lose whichever was set up first.
withNetRcEntries :: [NixConfig.NetRcEntry] -> M a -> M a
withNetRcEntries entries cont = do
  current <- NixConfig.getNetRcFileSetting <$> view #userNixConfig
  previous <- case current of
    Nothing -> pure ""
    Just (NetRcFile file) -> liftIO $ T.readFile file
  (path, handle) <- safeSystemTempFile "garnix-netrc"
  liftIO $ do
    T.hPutStr handle $ previous <> (if T.null previous || "\n" `T.isSuffixOf` previous then "" else "\n") <> NixConfig.renderNetRcEntries entries
    hClose handle
  local (#userNixConfig %~ ((NixConfig.fromNetRcFile . NetRcFile $ path) <>)) cont
