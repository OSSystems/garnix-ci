module Garnix.GithubUserTokenSpec where

import Control.Concurrent.Async.Lifted (concurrently)
import Control.Lens (locally)
import Data.IORef
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.GithubUserToken
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.Monad
import Garnix.Types
import Test.Hspec

spec :: Spec
spec = inM $ beforeM_ truncateDBM $ aroundM_ suppressLogsWhenPassing $ do
  let loggedInUser :: M User
      loggedInUser =
        DB.newUser
          (GhLogin "token-user")
          (Email "token-user@example.com")
          FreeSubscription
          True

      credentialsExpiringAt :: Text -> Maybe UTCTime -> Text -> GhUserCredentials Text
      credentialsExpiringAt access expiresAt refresh =
        GhUserCredentials
          { _ghUserCredentialsAccessToken = access,
            _ghUserCredentialsAccessTokenExpiresAt = expiresAt,
            _ghUserCredentialsRefreshToken = Just refresh,
            _ghUserCredentialsRefreshTokenExpiresAt = Nothing
          }

      mockRefresh :: (Text -> M (GhUserCredentials Text)) -> M a -> M a
      mockRefresh renew =
        locally #githubInterface (\x -> x {_githubInterfaceRefreshUserCredentials = renew})

      refusingToRenew :: M a -> M a
      refusingToRenew = mockRefresh $ \_ -> throw $ OtherError "unexpected renewal"

      isExpired :: Either ErrorWithContext a -> Bool
      isExpired = \case
        Left e -> err e == GithubSessionExpired
        Right _ -> False

  describe "githubTokenFor" $ do
    it "hands out the stored token while it still has time on it" $ do
      user <- loggedInUser
      now <- liftIO getCurrentTime
      storeCredentialsFor (user ^. id)
        $ credentialsExpiringAt "ghu_good" (Just $ addTime (fromHours @Int 4) now) "ghr_old"
      refusingToRenew (githubTokenFor user) `shouldReturnM` GhToken "ghu_good"

    it "hands out the stored token when the app does not expire tokens at all" $ do
      user <- loggedInUser
      storeCredentialsFor (user ^. id) $ credentialsExpiringAt "ghu_forever" Nothing "ghr_old"
      refusingToRenew (githubTokenFor user) `shouldReturnM` GhToken "ghu_forever"

    it "renews a token that is about to run out, and keeps the renewed one" $ do
      user <- loggedInUser
      now <- liftIO getCurrentTime
      storeCredentialsFor (user ^. id)
        $ credentialsExpiringAt "ghu_old" (Just $ addUTCTime 60 now) "ghr_old"
      renewedWith <- liftIO $ newIORef []
      token <-
        mockRefresh
          ( \refresh -> do
              liftIO $ modifyIORef' renewedWith (refresh :)
              pure $ credentialsExpiringAt "ghu_new" (Just $ addTime (fromHours @Int 8) now) "ghr_new"
          )
          $ githubTokenFor user
      liftIO $ token `shouldBe` GhToken "ghu_new"
      liftIO (readIORef renewedWith) `shouldReturnM` ["ghr_old"]
      refusingToRenew (githubTokenFor user) `shouldReturnM` GhToken "ghu_new"

    it "renews only once when two requests find the same token running out" $ do
      user <- loggedInUser
      now <- liftIO getCurrentTime
      storeCredentialsFor (user ^. id)
        $ credentialsExpiringAt "ghu_old" (Just $ addUTCTime 60 now) "ghr_old"
      renewals <- liftIO $ newIORef (0 :: Int)
      tokens <-
        mockRefresh
          ( \_ -> do
              liftIO $ atomicModifyIORef' renewals $ \n -> (n + 1, ())
              threadDelay $ fromSeconds @Double 0.2
              pure $ credentialsExpiringAt "ghu_new" (Just $ addTime (fromHours @Int 8) now) "ghr_new"
          )
          $ concurrently (githubTokenFor user) (githubTokenFor user)
      liftIO $ tokens `shouldBe` (GhToken "ghu_new", GhToken "ghu_new")
      liftIO (readIORef renewals) `shouldReturnM` (1 :: Int)

    it "ends the session when github will not renew the token" $ do
      user <- loggedInUser
      now <- liftIO getCurrentTime
      storeCredentialsFor (user ^. id)
        $ credentialsExpiringAt "ghu_old" (Just $ addUTCTime 60 now) "ghr_revoked"
      result <- try $ mockRefresh (\_ -> throw GithubDidntGiveUsAToken) $ githubTokenFor user
      liftIO $ case result of
        Left e -> statusCode (toErrorDetails e) `shouldBe` 401
        Right _ -> expectationFailure "expected the session to have ended"
      stored <- DB.getGithubUserCredentials (user ^. id)
      liftIO $ stored `shouldSatisfy` isNothing
      again <- try $ refusingToRenew $ githubTokenFor user
      liftIO $ (again :: Either ErrorWithContext GhToken) `shouldSatisfy` isExpired

    it "ends the session when the refresh token itself has expired" $ do
      user <- loggedInUser
      now <- liftIO getCurrentTime
      storeCredentialsFor (user ^. id)
        $ ( credentialsExpiringAt "ghu_old" (Just $ addUTCTime 60 now) "ghr_old"
          )
          { _ghUserCredentialsRefreshTokenExpiresAt = Just $ addUTCTime (-60) now
          }
      result <- try $ refusingToRenew $ githubTokenFor user
      liftIO $ (result :: Either ErrorWithContext GhToken) `shouldSatisfy` isExpired

    it "ends the session when there are no credentials on file" $ do
      user <- loggedInUser
      result <- try $ refusingToRenew $ githubTokenFor user
      liftIO $ (result :: Either ErrorWithContext GhToken) `shouldSatisfy` isExpired

  describe "withGithubUserToken" $ do
    it "renews and retries once when github rejects a token that had not expired" $ do
      user <- loggedInUser
      storeCredentialsFor (user ^. id) $ credentialsExpiringAt "ghu_revoked" Nothing "ghr_old"
      attempts <- liftIO $ newIORef []
      token <-
        mockRefresh (\_ -> pure $ credentialsExpiringAt "ghu_new" Nothing "ghr_new")
          $ withGithubUserToken user
          $ \token -> do
            liftIO $ modifyIORef' attempts (token :)
            when (token == GhToken "ghu_revoked") $ throw GithubUserTokenRejected
            pure token
      liftIO $ token `shouldBe` GhToken "ghu_new"
      liftIO (readIORef attempts) `shouldReturnM` [GhToken "ghu_new", GhToken "ghu_revoked"]

    it "ends the session when the renewed token is rejected as well" $ do
      user <- loggedInUser
      storeCredentialsFor (user ^. id) $ credentialsExpiringAt "ghu_revoked" Nothing "ghr_old"
      result <-
        try
          $ mockRefresh (\_ -> throw GithubDidntGiveUsAToken)
          $ withGithubUserToken user
          $ \_ -> throw GithubUserTokenRejected
      liftIO $ (result :: Either ErrorWithContext ()) `shouldSatisfy` isExpired
