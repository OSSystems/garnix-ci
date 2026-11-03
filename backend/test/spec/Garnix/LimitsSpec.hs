module Garnix.LimitsSpec (spec) where

import Garnix.Limits
import Garnix.Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "cgroupPath" $ do
    it "finds the cgroup v2 directory" $ do
      cgroupPath "0::/system.slice/garnix-server.service\n"
        `shouldBe` Just "/sys/fs/cgroup/system.slice/garnix-server.service"

    it "gives nothing under cgroup v1" $ do
      cgroupPath "12:memory:/system.slice/garnix-server.service\n" `shouldBe` Nothing

  describe "parseCgroupLimit" $ do
    it "reads bytes" $ do
      parseCgroupLimit "34359738368\n" `shouldBe` Just 34359738368

    it "reads max as no limit" $ do
      parseCgroupLimit "max\n" `shouldBe` Nothing

  describe "parseMemTotal" $ do
    it "reads the machine's RAM in bytes" $ do
      parseMemTotal "MemTotal:       75497472 kB\nMemFree:         1024 kB\n"
        `shouldBe` Just (75497472 * 1024)
