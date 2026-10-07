module Garnix.Build.Helpers
  ( withPrivateNixXdgCache,
    withInternalCacheToken,
  )
where

import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Monad.ForkT (safeSystemTempDirectory, safeSystemTempFile)
import Garnix.NixConfig qualified as NixConfig
import Garnix.Prelude
import Garnix.Types as Types
import System.IO (hClose, hPutStrLn)

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
  (path, handle) <- safeSystemTempFile "garnix-netrc"
  liftIO $ do
    hPutStrLn handle
      . unlines
      $ [ "machine cache.garnix.io",
          "login " <> cs (getGhLogin (_forgeLoginGhLogin reqUser)),
          "password " <> cs (getInternalCacheToken token)
        ]
    hClose handle
  local (#userNixConfig %~ ((NixConfig.fromNetRcFile . NetRcFile $ path) <>)) $ do
    withTextSpan ("internal_token", show reqUser) cont
