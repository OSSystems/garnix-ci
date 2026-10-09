module Garnix.AccessTokenSpec where

import Control.Lens (iforM_)
import Data.Maybe (fromJust)
import Garnix.AccessToken
import Garnix.AccessToken.Types
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers hiding (testUser)
import Garnix.TestHelpers.Monad
import Garnix.Types
import Test.Hspec

spec :: Spec
spec = inM $ beforeM_ truncateDBM $ do
  describe "isAccessTokenValid" $ do
    it "returns true if the provided access token is valid for the passed scopes" $ do
      userId <- testUser "user"
      token <- generateToken userId "token-name" allAccessTokenScopes
      isAccessTokenValid userId token (^. #cache)
        `shouldReturnM` True
      isAccessTokenValid userId token (^. #api)
        `shouldReturnM` True

    it "returns false if the user id does not match" $ do
      user1 <- testUser "user1"
      user2 <- testUser "user2"
      token <- generateToken user1 "token-name" allAccessTokenScopes
      isAccessTokenValid user2 token (^. #cache)
        `shouldReturnM` False

    it "returns false for invalid access tokens" $ do
      userId <- testUser "user"
      let token = AccessToken "boo"
      isAccessTokenValid userId token (^. #cache)
        `shouldReturnM` False

    it "returns false if the provided access token is not valid for the passed scopes" $ do
      let cases =
            [ (allAccessTokenScopes {api = False}, (^. #api)),
              (allAccessTokenScopes {cache = False}, (^. #cache))
            ]
      iforM_ cases $ \idx (tokenScopes, requiredScope) -> do
        userId <- testUser $ "user-" <> show idx
        token <- generateToken userId "token-name" tokenScopes
        isAccessTokenValid userId token requiredScope
          `shouldReturnM` False

    it "marks the access token as used if it is valid" $ do
      userId <- testUser "user"
      token <- generateToken userId "token-name" allAccessTokenScopes
      now <- liftIO getCurrentTime
      _ <- isAccessTokenValid userId token (^. #cache)
      [result] <- DB.getAccessTokensForUser userId
      fromJust (_accessTokenMetadataLastUsed result) `shouldSatisfyM` (>= now)

    it "does not mark the access token as used if it is invalid" $ do
      userId <- testUser "user"
      token <- generateToken userId "token-name" (allAccessTokenScopes {cache = False})
      _ <- isAccessTokenValid userId token (^. #cache)
      [result] <- DB.getAccessTokensForUser userId
      _accessTokenMetadataLastUsed result `shouldSatisfyM` isNothing

allAccessTokenScopes :: AccessTokenScopes
allAccessTokenScopes =
  AccessTokenScopes
    { cache = True,
      api = True
    }

testUser :: Text -> M UserId
testUser name =
  (^. id)
    <$> DB.newUser
      (ForgeLogin githubForge (GhLogin name))
      (Email $ name <> "@example.com")
