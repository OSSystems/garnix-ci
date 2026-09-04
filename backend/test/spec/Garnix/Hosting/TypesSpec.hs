module Garnix.Hosting.TypesSpec (spec) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Garnix.Hosting.Types
import Garnix.Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "parseProvider" $ do
    it "round-trips every provider through its name" $ do
      let providers = [minBound .. maxBound] :: [Provider]
      map (parseProvider . providerName) providers `shouldBe` map Right providers

    it "gives every provider a distinct name" $ do
      let names = map providerName ([minBound .. maxBound] :: [Provider])
      Set.size (Set.fromList names) `shouldBe` length names

    it "refuses an unknown provider and lists the known ones" $ do
      case parseProvider "digitalocean" of
        Right provider -> expectationFailure $ cs $ "expected a refusal, got " <> show provider
        Left problem -> do
          problem `shouldSatisfy` T.isInfixOf "digitalocean"
          problem `shouldSatisfy` T.isInfixOf "microvm"
          problem `shouldSatisfy` T.isInfixOf "hetzner"

  describe "serverAddressText" $ do
    it "prefers IPv4 when both families are present" $ do
      serverAddressText (ServerAddress (Just "10.111.0.17") (Just "2001:db8::1"))
        `shouldBe` Just "10.111.0.17"

    it "falls back to IPv6" $ do
      serverAddressText (ServerAddress Nothing (Just "2001:db8::1"))
        `shouldBe` Just "2001:db8::1"

    it "has nothing to offer when neither family is present" $ do
      serverAddressText (ServerAddress Nothing Nothing) `shouldBe` Nothing

  describe "tierResources" $ do
    it "grows monotonically with the tier" $ do
      map (tierResources . ServerTier) ["small", "medium", "large"]
        `shouldBe` [(1, 2048), (2, 4096), (4, 8192)]

    it "treats an unrecognised tier as the smallest one" $ do
      tierResources (ServerTier "enormous") `shouldBe` tierResources (ServerTier "small")

    it "understands the iNxM spelling a servers: entry actually carries" $ do
      -- A `servers:` entry's `machine` defaults to "i1x2"; before this was
      -- understood every non-default tier silently deployed at 1 vCPU / 2 GiB.
      tierResources (ServerTier "i1x2") `shouldBe` (1, 2048)
      tierResources (ServerTier "i2x4") `shouldBe` (2, 4096)
      tierResources (ServerTier "i8x16") `shouldBe` (8, 16384)

    it "rejects malformed iNxM rather than reading half of it" $ do
      map (tierResources . ServerTier) ["i0x2", "i2x0", "ix2", "i2x", "i2x2b", "i-1x2"]
        `shouldBe` replicate 6 (1, 2048)
