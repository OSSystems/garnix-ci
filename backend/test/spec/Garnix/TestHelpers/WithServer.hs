module Garnix.TestHelpers.WithServer
  ( TestServer (TestServer),
    assert200,
    assertJSON,
    withServer,
    shouldHaveStatusCode,
    get,
    put,
    post,
    delete,
    postWithHeaders,
    putWithHeaders,
    getWithHeaders,
    login,
    apiUrl,
  )
where

import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.ByteString qualified
import Data.ByteString.Lazy (ByteString)
import Data.Yaml.Aeson (decodeThrow)
import Garnix qualified
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude hiding (get, put)
import Garnix.Types hiding (apiUrl, login, statusCode)
import Network.HTTP.Client (ManagerSettings (managerModifyRequest), Request (redirectCount), createCookieJar, defaultManagerSettings)
import Network.HTTP.Types (HeaderName)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Network.Wreq qualified as Wreq
import Network.Wreq.Lens
import Network.Wreq.Session qualified
import Test.Hspec

data TestServer = TestServer
  { get :: String -> M (Response ByteString),
    put :: String -> Value -> M (Response ByteString),
    post :: String -> Value -> M (Response ByteString),
    delete :: String -> M (Response ByteString),
    getWithHeaders :: String -> [(HeaderName, Data.ByteString.ByteString)] -> M (Response ByteString),
    postWithHeaders :: String -> [(HeaderName, Data.ByteString.ByteString)] -> Value -> M (Response ByteString),
    putWithHeaders :: String -> [(HeaderName, Data.ByteString.ByteString)] -> Value -> M (Response ByteString),
    login :: M User,
    apiUrl :: Text
  }

mkTestServer :: Network.Wreq.Session.Session -> Port -> TestServer
mkTestServer session port =
  let opts = Wreq.defaults & checkResponse ?~ (\_ _ -> pure ())
      mkUrl apiPath = "http://localhost:" <> cs (show port) <> apiPath
      get apiPath = do
        liftIO $ Network.Wreq.Session.getWith opts session $ mkUrl apiPath
      put apiPath body = do
        liftIO $ Network.Wreq.Session.putWith opts session (mkUrl apiPath) body
      post apiPath body = do
        liftIO $ Network.Wreq.Session.postWith opts session (mkUrl apiPath) body
      delete apiPath = do
        liftIO $ Network.Wreq.Session.deleteWith opts session (mkUrl apiPath)
      getWithHeaders apiPath headers = do
        liftIO $ Network.Wreq.Session.getWith (opts & Wreq.headers .~ headers) session $ mkUrl apiPath
      putWithHeaders apiPath headers body = do
        liftIO $ Network.Wreq.Session.putWith (opts & Wreq.headers .~ headers) session (mkUrl apiPath) body
      postWithHeaders apiPath headers body = do
        liftIO $ Network.Wreq.Session.postWith (opts & Wreq.headers .~ headers) session (mkUrl apiPath) body
      login = do
        _ <- get "/api/dev/log-me-in"
        response <- get "/api/whoami"
        let username = response ^. responseBody . key "username" . _String
            forge' = response ^. responseBody . key "forge" . _String
        DB.getUser (ForgeLogin (ForgeSlug forge') (GhLogin username))
   in TestServer
        { get,
          put,
          post,
          delete,
          getWithHeaders,
          postWithHeaders,
          putWithHeaders,
          login,
          apiUrl = cs (mkUrl "/api")
        }

withServer :: (TestServer -> M a) -> M a
withServer action = do
  env <- ask
  let app = Garnix.toApplication env
  liftBaseOp (testWithApplication (pure app)) $ \port -> do
    session <-
      liftIO
        $ Network.Wreq.Session.newSessionControl
          (Just (createCookieJar []))
          (defaultManagerSettings {managerModifyRequest = \request -> pure $ request {redirectCount = 0}})
    action $ mkTestServer session port

shouldHaveStatusCode :: (HasCallStack) => Response a -> Int -> M ()
shouldHaveStatusCode response expected =
  liftIO
    $ response
    ^. responseStatus
    . statusCode
    `shouldBe` expected

assert200 :: forall a. (HasCallStack, ConvertibleStrings a Text) => M (Response a) -> M (Response a)
assert200 action = do
  res <- action
  when (res ^. responseStatus . statusCode /= 200) $ do
    liftIO
      $ expectationFailure
      $ cs
        ("assert200 got a " <> show (res ^. responseStatus . statusCode) <> ": " <> cs (res ^. responseBody))
  pure res

assertJSON :: (HasCallStack) => M (Response ByteString) -> M (Response Value)
assertJSON action = do
  res <- action
  mapM (decodeThrow . cs) res
