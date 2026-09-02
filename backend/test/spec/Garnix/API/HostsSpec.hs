module Garnix.API.HostsSpec (spec) where

import Control.Lens (asIndex, ifolded, toListOf)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.Lens (key, values, _Object, _String)
import Data.Word (Word8)
import Garnix.API.Hosts
import Garnix.Hosting.Types
import Garnix.Prelude
import Garnix.Types
import Network.Socket (SockAddr (..), tupleToHostAddress)
import Test.Hspec

spec :: Spec
spec = do
  describe "HostList" $ do
    it "routes a server at its canonical name" $ do
      let config = renderTraefik [webHost]
      routerNames config `shouldContain` ["web.main.widgets.acme"]
      ruleOf config "web.main.widgets.acme"
        `shouldBe` Just "Host(`web.main.widgets.acme.hosting.example`)"
      serviceUrl config "web.main.widgets.acme" `shouldBe` Just "http://10.111.0.7"

    it "also answers a primary deploy at the short repo name" $ do
      let config = renderTraefik [webHost {_hostIsPrimary = True}]
      ruleOf config "widgets.acme" `shouldBe` Just "Host(`widgets.acme.hosting.example`)"
      -- The short name points at the same service, not a second copy of it.
      serviceNames config `shouldBe` ["web.main.widgets.acme"]

    it "gives a non-primary deploy no short name" $ do
      routerNames (renderTraefik [webHost]) `shouldNotContain` ["widgets.acme"]

    it "gives a PR deploy a pull-N name and no short name" $ do
      let config = renderTraefik [prHost]
      ruleOf config "web.pull-42.widgets.acme"
        `shouldBe` Just "Host(`web.pull-42.widgets.acme.hosting.example`)"
      routerNames config `shouldNotContain` ["widgets.acme"]

    it "routes each declared http port at its own name and port" $ do
      let config = renderTraefik [webHost {_hostHttpPorts = [("api", 8080)]}]
      ruleOf config "api.web.main.widgets.acme"
        `shouldBe` Just "Host(`api.web.main.widgets.acme.hosting.example`)"
      serviceUrl config "api.web.main.widgets.acme" `shouldBe` Just "http://10.111.0.7:8080"

    it "matches a declared domain verbatim, not under the hosting domain" $ do
      -- A custom domain is the name the user owns; suffixing ours onto it
      -- would produce a name nobody resolves.
      let config = renderTraefik [webHost {_hostDomains = ["shop.example.com"]}]
      ruleOf config "shop.example.com" `shouldBe` Just "Host(`shop.example.com`)"
      -- It reuses the guest's existing service rather than declaring another.
      serviceNames config `shouldBe` ["web.main.widgets.acme"]

    it "puts every route behind the heartbeat middleware" $ do
      -- A route without it never reports traffic, so its server would look
      -- idle and be torn down while it was being used.
      let config =
            renderTraefik
              [ webHost
                  { _hostIsPrimary = True,
                    _hostHttpPorts = [("api", 8080)],
                    _hostDomains = ["shop.example.com"]
                  }
              ]
          middlewares name =
            config ^.. key "http" . key "routers" . key (Aeson.Key.fromText name) . key "middlewares" . values . _String
      forM_ (routerNames config) $ \name ->
        middlewares name `shouldBe` ["heartbeatmiddleware"]

    it "points the heartbeat middleware at this backend" $ do
      renderTraefik [webHost]
        ^? key "http"
        . key "middlewares"
        . key "heartbeatmiddleware"
        . key "plugin"
        . key "heartbeatmiddleware"
        . key "reportEndpoint"
        . _String
        `shouldBe` Just "https://garnix.example/api/hosts/heartbeat"

    it "drops a server with no address rather than routing nowhere" $ do
      let addressless = webHost {_hostAddress = ServerAddress Nothing Nothing}
      serviceNames (renderTraefik [addressless]) `shouldBe` []

  describe "statsSourceAllowed" $ do
    it "accepts a guest connecting straight from the bridge" $ do
      statsSourceAllowed "10.111.0." (peer (10, 111, 0, 7)) Nothing `shouldBe` True

    it "accepts a guest whose request the proxy forwarded" $ do
      statsSourceAllowed "10.111.0." (peer (127, 0, 0, 1)) (Just "10.111.0.7")
        `shouldBe` True

    it "refuses a peer from outside the bridge" $ do
      statsSourceAllowed "10.111.0." (peer (203, 0, 113, 9)) Nothing `shouldBe` False

    it "refuses a loopback peer forwarding an off-bridge client" $ do
      statsSourceAllowed "10.111.0." (peer (127, 0, 0, 1)) (Just "203.0.113.9")
        `shouldBe` False

    it "refuses a loopback peer with no forwarded client at all" $ do
      -- Loopback alone is not evidence of anything: anything on the host can
      -- reach the loopback listener.
      statsSourceAllowed "10.111.0." (peer (127, 0, 0, 1)) Nothing `shouldBe` False

    it "trusts only the last forwarded entry" $ do
      -- Everything before it is whatever the client claimed. A client that
      -- prepends a bridge address must not be believed.
      statsSourceAllowed "10.111.0." (peer (127, 0, 0, 1)) (Just "10.111.0.7, 203.0.113.9")
        `shouldBe` False
      statsSourceAllowed "10.111.0." (peer (127, 0, 0, 1)) (Just "203.0.113.9, 10.111.0.7")
        `shouldBe` True

  describe "statsClientIp" $ do
    it "is the peer for a direct connection" $ do
      statsClientIp (peer (10, 111, 0, 7)) (Just "203.0.113.9")
        `shouldBe` Just "10.111.0.7"

    it "is the forwarded client behind the proxy" $ do
      statsClientIp (peer (127, 0, 0, 1)) (Just "10.111.0.7")
        `shouldBe` Just "10.111.0.7"

-- | The generated Traefik configuration for these hosts.
renderTraefik :: [Host] -> Aeson.Value
renderTraefik hosts = Aeson.toJSON (HostList hosts "https://garnix.example" "hosting.example")

routerNames :: Aeson.Value -> [Text]
routerNames config =
  sort $ map cs $ keysOf (config ^? key "http" . key "routers" . _Object)

serviceNames :: Aeson.Value -> [Text]
serviceNames config =
  sort $ map cs $ keysOf (config ^? key "http" . key "services" . _Object)

keysOf :: Maybe Aeson.Object -> [Text]
keysOf = maybe [] (map Aeson.Key.toText . toListOf (ifolded . asIndex))

ruleOf :: Aeson.Value -> Text -> Maybe Text
ruleOf config name =
  config ^? key "http" . key "routers" . key (Aeson.Key.fromText name) . key "rule" . _String

serviceUrl :: Aeson.Value -> Text -> Maybe Text
serviceUrl config name = do
  service <-
    config ^? key "http" . key "routers" . key (Aeson.Key.fromText name) . key "service" . _String
  config
    ^? key "http"
    . key "services"
    . key (Aeson.Key.fromText service)
    . key "loadBalancer"
    . key "servers"
    . values
    . key "url"
    . _String

-- HashId does not export its constructor (it should only come from the DB),
-- so a test id is built through the Iso.
serverIdOf :: Int -> ServerId
serverIdOf = ServerId . review hashIdInt

peer :: (Word8, Word8, Word8, Word8) -> SockAddr
peer = SockAddrInet 0 . tupleToHostAddress

webHost :: Host
webHost =
  Host
    { _hostRepoOwner = GhRepoOwner (GhLogin "acme"),
      _hostRepoName = GhRepoName "widgets",
      _hostBranch = Branch "main",
      _hostPackageName = PackageName "web",
      _hostPullRequest = Nothing,
      _hostAddress = ServerAddress (Just "10.111.0.7") Nothing,
      _hostDrvPath = Nothing,
      _hostPersistenceName = Nothing,
      _hostServerId = serverIdOf 1,
      _hostInstanceId = Just (InstanceId "guest-1"),
      _hostIsPrimary = False,
      _hostDomains = [],
      _hostHttpPorts = []
    }

prHost :: Host
prHost =
  webHost
    { _hostPullRequest = Just (GhPullRequestId 42),
      _hostServerId = serverIdOf 2
    }
