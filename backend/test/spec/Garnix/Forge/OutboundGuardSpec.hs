module Garnix.Forge.OutboundGuardSpec (spec) where

import Control.Concurrent (modifyMVar_, newMVar, readMVar, threadDelay)
import Control.Exception qualified
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Word (Word16, Word8)
import Garnix.Forge.OutboundGuard
import Garnix.Prelude
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Manager, httpLbs, parseRequest, requestHeaders, responseBody)
import Network.HTTP.Types (status200)
import Network.Socket (SockAddr (..), tupleToHostAddress, tupleToHostAddress6)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Timeout qualified
import Test.Hspec

v4 :: (Word8, Word8, Word8, Word8) -> SockAddr
v4 = SockAddrInet 0 . tupleToHostAddress

v6 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> SockAddr
v6 tuple = SockAddrInet6 0 0 (tupleToHostAddress6 tuple) 0

-- | The fake server's address. It stands in for a public one in these specs:
-- every other non-public address is still refused.
fakeServer :: Int -> SockAddr
fakeServer port = SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1))

allowedInSpecs :: SockAddr -> Bool
allowedInSpecs addr = isPublicAddress addr || isFake addr
  where
    isFake = \case
      SockAddrInet _ a -> a == tupleToHostAddress (127, 0, 0, 1)
      _ -> False

-- | The fake servers speak plain http.
plainHttp :: GuardOptions
plainHttp = defaultGuardOptions {guardAllowPlainHttp = True}

-- | Runs the action with a server on 127.0.0.1 answering @ok@, and the number
-- of requests it got.
withServer :: (Int -> IO a) -> IO (a, Int)
withServer action = do
  hits <- newMVar (0 :: Int)
  let app _ respond = do
        modifyMVar_ hits (pure . (+ 1))
        respond $ Wai.responseLBS status200 [] "ok"
  result <- testWithApplication (pure app) action
  (result,) <$> readMVar hits

fetch :: Manager -> Int -> IO (Either Control.Exception.SomeException LazyByteString)
fetch manager' port = do
  request <- parseRequest $ "http://forge.test:" <> show' port <> "/api/v1/version"
  -- No keep-alive: each request opens its own connection.
  Control.Exception.try $ responseBody <$> httpLbs request {requestHeaders = [("Connection", "close")]} manager'
  where
    show' = cs . show

-- | Runs the action with a server on 127.0.0.1 answering with headers and
-- then the given body, streamed.
withStreamingServer :: IO BS.ByteString -> (Int -> IO a) -> IO a
withStreamingServer nextChunk =
  testWithApplication $ pure $ \_ respond ->
    respond $ Wai.responseStream status200 [] $ \write flush -> forever $ do
      chunk <- nextChunk
      write (byteString chunk)
      flush

-- | The guard's exception, as is or wrapped by http-client.
guardException :: forall e a. (Exception e) => Either Control.Exception.SomeException a -> Maybe e
guardException = \case
  Left e
    | Just inner <- Control.Exception.fromException e -> Just inner
    | Just (HttpExceptionRequest _ (ConnectionFailure inner)) <- Control.Exception.fromException e -> Control.Exception.fromException inner
    | Just (HttpExceptionRequest _ (InternalException inner)) <- Control.Exception.fromException e -> Control.Exception.fromException inner
  _ -> Nothing

isTooLarge :: Either Control.Exception.SomeException a -> Bool
isTooLarge = isJust . guardException @GuardLimitExceeded

-- | The guard's refusal, as is or wrapped by http-client.
isForbidden :: Either Control.Exception.SomeException a -> Bool
isForbidden = \case
  Left e
    | Just (ForbiddenAddress {}) <- Control.Exception.fromException e -> True
    | Just (HttpExceptionRequest _ (ConnectionFailure inner)) <- Control.Exception.fromException e ->
        isJust (Control.Exception.fromException inner :: Maybe ForbiddenAddress)
  _ -> False

spec :: Spec
spec = do
  describe "isPublicAddress" $ do
    it "refuses loopback, private, link-local, CGNAT and their IPv6 counterparts" $ do
      forM_
        [ v4 (127, 0, 0, 1),
          v4 (10, 1, 2, 3),
          v4 (172, 16, 0, 1),
          v4 (192, 168, 1, 1),
          v4 (169, 254, 169, 254),
          v4 (100, 64, 0, 1),
          v4 (0, 0, 0, 0),
          v4 (224, 0, 0, 1),
          v4 (255, 255, 255, 255),
          v6 (0, 0, 0, 0, 0, 0, 0, 1),
          v6 (0, 0, 0, 0, 0, 0, 0, 0),
          v6 (0xfe80, 0, 0, 0, 0, 0, 0, 1),
          v6 (0xfc00, 0, 0, 0, 0, 0, 0, 1),
          v6 (0xfd12, 0x3456, 0, 0, 0, 0, 0, 1),
          v6 (0, 0, 0, 0, 0, 0xffff, 0x7f00, 1), -- ::ffff:127.0.0.1
          v6 (0, 0, 0, 0, 0, 0xffff, 0xa9fe, 0xa9fe), -- ::ffff:169.254.169.254
          v6 (0x2001, 0x0db8, 0, 0, 0, 0, 0, 1),
          v6 (0x2002, 0x7f00, 1, 0, 0, 0, 0, 1)
        ]
        $ \addr -> (addr, isPublicAddress addr) `shouldBe` (addr, False)

    it "accepts public addresses" $ do
      forM_ [v4 (93, 184, 216, 34), v4 (100, 128, 0, 1), v4 (172, 32, 0, 1), v6 (0x2606, 0x4700, 0, 0, 0, 0, 0, 1), v6 (0, 0, 0, 0, 0, 0xffff, 0x5db8, 0xd822)]
        $ \addr -> (addr, isPublicAddress addr) `shouldBe` (addr, True)

  describe "checkedAddresses" $ do
    it "refuses a name if any of its addresses is not public" $ do
      let resolveTo addrs _ _ = pure addrs
      result <- Control.Exception.try $ checkedAddresses isPublicAddress (resolveTo [v4 (93, 184, 216, 34), v4 (10, 0, 0, 1)]) "forge.test" 443
      either (\(ForbiddenAddress host _) -> host) (const "") result `shouldBe` "forge.test"
      checkedAddresses isPublicAddress (resolveTo [v4 (93, 184, 216, 34)]) "forge.test" 443 `shouldReturn` [v4 (93, 184, 216, 34)]

  describe "guardedManager" $ do
    it "connects to an allowed address" $ do
      (response, hits) <- withServer $ \port -> do
        manager' <- guardedManager plainHttp allowedInSpecs (\_ _ -> pure [fakeServer port])
        fetch manager' port
      either (const Nothing) Just response `shouldBe` Just "ok"
      hits `shouldBe` 1

    it "refuses a host resolving to a non-public address before connecting" $ do
      -- The first one is the live server itself: refused all the same.
      forM_ [fakeServer, const $ v4 (10, 0, 0, 1), const $ v4 (169, 254, 169, 254), const $ v6 (0, 0, 0, 0, 0, 0, 0, 1)] $ \target -> do
        (response, hits) <- withServer $ \port -> do
          manager' <- guardedManager plainHttp isPublicAddress (\_ _ -> pure [target port])
          fetch manager' port
        (isForbidden response, hits) `shouldBe` (True, 0)

    it "refuses plain http unless allowed, so a forge cannot redirect garnix to it" $ do
      (response, hits) <- withServer $ \port -> do
        manager' <- guardedManager defaultGuardOptions allowedInSpecs (\_ _ -> pure [fakeServer port])
        fetch manager' port
      isJust (guardException @PlainHttpRefused response) `shouldBe` True
      hits `shouldBe` 0

    it "stops reading an answer past its size limit" $ do
      response <- withStreamingServer (pure $ BS.replicate 4096 120) $ \port -> do
        manager' <- guardedManager plainHttp {guardMaxBytes = 64 * 1024} allowedInSpecs (\_ _ -> pure [fakeServer port])
        System.Timeout.timeout (10 * 1000 * 1000) (fetch manager' port)
      fmap isTooLarge response `shouldBe` Just True

    it "stops reading an answer that trickles in past its deadline" $ do
      response <- withStreamingServer (threadDelay 100000 >> pure "x") $ \port -> do
        manager' <- guardedManager plainHttp {guardDeadline = 1} allowedInSpecs (\_ _ -> pure [fakeServer port])
        System.Timeout.timeout (10 * 1000 * 1000) (fetch manager' port)
      fmap isTooLarge response `shouldBe` Just True

    it "checks each connection, so a name rebound to a private address is refused" $ do
      (responses, hits) <- withServer $ \port -> do
        -- Public (the fake server) when first resolved, then the metadata
        -- endpoint.
        resolutions <- newIORef [fakeServer port, v4 (169, 254, 169, 254)]
        let resolve _ _ = atomicModifyIORef' resolutions $ \case
              next : rest -> (if null rest then [next] else rest, [next])
              [] -> ([], [])
        manager' <- guardedManager plainHttp allowedInSpecs resolve
        first' <- fetch manager' port
        second' <- fetch manager' port
        pure (first', second')
      either (const Nothing) Just (fst responses) `shouldBe` Just "ok"
      hits `shouldBe` 1
      isForbidden (snd responses) `shouldBe` True
