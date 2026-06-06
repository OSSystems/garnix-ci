module Garnix.API.CacheSpec where

import Control.Lens
import Garnix.API.Cache (serveNarInfo)
import Garnix.API.Cache.Types (NarInfoFileName (..))
import Garnix.Monad (M)
import Garnix.Nix.Types (StoreHash (..))
import Garnix.Prelude
import Garnix.TestHelpers.Monad
import Garnix.Types (Error (..))
import Test.Hspec

spec :: Spec
spec = inM $ do
  describe "serveNarInfo" $ do
    it "returns NotFound when s3CacheEnabled = False, without touching DB" $ do
      withS3Disabled $
        serveNarInfo Nothing (NarInfoFileName (StoreHash "deadbeef")) Nothing
          `shouldThrowM` NotFound

withS3Disabled :: M a -> M a
withS3Disabled = local (#s3CacheEnabled .~ False)
