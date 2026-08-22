module Garnix.API.Cache.Auth
  ( getStoreHashPermission,
    __accessTokenValidCache,
  )
where

import Control.Monad.Extra
import Data.Functor
import Data.List.Extra
import Garnix.API.Cache.Permissions
import Garnix.AccessToken
import Garnix.AccessToken.Types
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.ExpiringCache
import Garnix.Monad
import Garnix.Nix.Types
import Garnix.ParseHttpBasicAuth
import Garnix.Prelude
import Garnix.Types hiding (getUserId)
import System.IO.Unsafe qualified

getStoreHashPermission :: StoreHash -> Maybe Text -> M Permission
getStoreHashPermission storeHash authorization = do
  mGhLogin <- case parseBasicAuth <$> authorization of
    Nothing -> pure Nothing
    Just (Left err) -> do
      throw $ UnauthorizedWithMessage $ "Failed to parse basic auth: " <> show err
    Just (Right (user, pass)) -> do
      let ghLogin = GhLogin user
      isValid <- isAccessTokenValidCached storeHash ghLogin $ AccessToken pass
      unless isValid $ throw InvalidAccessToken
      pure $ Just ghLogin
  withTextSpan ("auth_claim", show mGhLogin) $ do
    repos <- DB.getReposForHash storeHash
    case repos of
      [] -> pure Allowed
      repos -> do
        permissions <- forM repos $ \repo -> do
          getRepoPermissions mGhLogin repo
        pure $ if Allowed `elem` permissions then Allowed else Disallowed
  where
    isAccessTokenValidCached :: StoreHash -> GhLogin -> AccessToken -> M Bool
    isAccessTokenValidCached storeHash ghLogin accessToken =
      lookupCache __accessTokenValidCache (ghLogin, accessToken) $ do
        (InternalCacheToken internalToken) <- DB.getUserInternalToken ghLogin
        if getAccessTokenText accessToken == internalToken
          then do
            log Informational $ "authentication successful for internal token for " <> getStoreHash storeHash
            pure True
          else do
            log Informational "internal token check failed, trying to match against user tokens."
            userId <-
              DB.getUserId ghLogin `catchError` \err -> do
                log Warning $ "Failed to lookup user id: " <> show err
                throw InvalidAccessToken
            isAccessTokenValid userId accessToken (^. #cache)

type AccessTokenValidCache = ExpiringCache (GhLogin, AccessToken) Bool

{-# NOINLINE __accessTokenValidCache #-}
__accessTokenValidCache :: AccessTokenValidCache
__accessTokenValidCache =
  System.IO.Unsafe.unsafePerformIO
    $ mkCache
      Nothing
      (fromMinutes @Int 5)
      (fromMinutes @Int 5)
