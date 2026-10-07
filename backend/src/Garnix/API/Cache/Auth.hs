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
  mLogin <- case parseBasicAuth <$> authorization of
    Nothing -> pure Nothing
    Just (Left err) -> do
      throw $ UnauthorizedWithMessage $ "Failed to parse basic auth: " <> show err
    Just (Right (user, pass)) -> do
      login <- maybe (throw InvalidAccessToken) pure (parseForgeLoginText user)
      isValid <- isAccessTokenValidCached storeHash login $ AccessToken pass
      unless isValid $ throw InvalidAccessToken
      pure $ Just login
  withTextSpan ("auth_claim", show mLogin) $ do
    repos <- DB.getReposForHash storeHash
    case repos of
      [] -> pure Allowed
      repos -> do
        permissions <- forM repos $ \repo -> do
          getRepoPermissions mLogin repo
        pure $ if Allowed `elem` permissions then Allowed else Disallowed
  where
    isAccessTokenValidCached :: StoreHash -> ForgeLogin -> AccessToken -> M Bool
    isAccessTokenValidCached storeHash login accessToken =
      lookupCache __accessTokenValidCache (login, accessToken) $ do
        (InternalCacheToken internalToken) <- DB.getUserInternalToken login
        if getAccessTokenText accessToken == internalToken
          then do
            log Informational $ "authentication successful for internal token for " <> getStoreHash storeHash
            pure True
          else do
            log Informational "internal token check failed, trying to match against user tokens."
            userId <-
              DB.getUserId login `catchError` \err -> do
                log Warning $ "Failed to lookup user id: " <> show err
                throw InvalidAccessToken
            isAccessTokenValid userId accessToken (^. #cache)

type AccessTokenValidCache = ExpiringCache (ForgeLogin, AccessToken) Bool

{-# NOINLINE __accessTokenValidCache #-}
__accessTokenValidCache :: AccessTokenValidCache
__accessTokenValidCache =
  System.IO.Unsafe.unsafePerformIO
    $ mkCache
      Nothing
      (fromMinutes @Int 5)
      (fromMinutes @Int 5)
