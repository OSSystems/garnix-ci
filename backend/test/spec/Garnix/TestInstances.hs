{-# OPTIONS_GHC -fno-warn-orphans #-}

module Garnix.TestInstances where

import Data.Binary.Builder qualified as Bin
import Data.Either (fromRight)
import Data.Map qualified as Map
import Garnix.DB.FeatureFlags.Types
import Garnix.Duration
import Garnix.Monad
import Garnix.Nix.Types qualified as Nix
import Garnix.Prelude
import Garnix.Types
import Generic.Random
import GitHub.Data.Webhooks.Events
import GitHub.Data.Webhooks.Payload
import Network.Wai.EventSource (ServerEvent (..))
import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance Arbitrary PackageType where
  arbitrary = genericArbitrary uniform

instance Arbitrary PrFromFork where
  arbitrary = PrFromFork <$> arbitrary

instance Arbitrary Build where
  arbitrary = genericArbitrary uniform

instance Arbitrary BuildOutputsPgColumn where
  arbitrary = genericArbitrary uniform

instance Arbitrary Nix.BuildOutputs where
  arbitrary = do
    pairs <- listOf $ do
      key <- elements ["out", "doc", "bin"]
      value <- arbitrary
      pure (key, value)
    pure $ Nix.BuildOutputs $ Map.fromList pairs

instance Arbitrary Nix.StorePath where
  arbitrary = do
    packageName <- listOf1 (elements ['a' .. 'z'])
    hash :: String <- vectorOf 32 (elements ['a' .. 'z'])
    pure $ fromRight (error "failed to generate arbitrary store path") $ Nix.parseStorePath $ "/nix/store/" <> hash <> "-" <> packageName

instance Arbitrary Branch where
  arbitrary = Branch <$> arbitrary

instance Arbitrary RepoPublicity where
  arbitrary = RepoIsPublic <$> arbitrary

instance Arbitrary GhRepoOwner where
  arbitrary = GhRepoOwner <$> arbitrary

instance Arbitrary GhRepoName where
  arbitrary = GhRepoName <$> arbitrary

instance Arbitrary GhLogin where
  arbitrary = GhLogin <$> arbitrary

instance Arbitrary CommitHash where
  arbitrary = CommitHash <$> arbitrary

instance Arbitrary PackageName where
  arbitrary = genericArbitrary uniform

instance Arbitrary System where
  arbitrary = genericArbitrary uniform

instance Arbitrary MaybeSystem where
  arbitrary = genericArbitrary uniform

instance Arbitrary HashId where
  arbitrary = pgDecode p . pgEncode p <$> (arbitrary :: Gen Int64)
    where
      p :: PGTypeID "bigint"
      p = PGTypeProxy

instance Arbitrary BuildId where
  arbitrary = BuildId <$> arbitrary

instance Arbitrary Status where
  arbitrary = genericArbitrary uniform

instance Arbitrary GhRunId where
  arbitrary = GhRunId <$> arbitrary

instance Arbitrary CheckSuiteEvent where
  arbitrary = genericArbitrary uniform

instance Arbitrary CheckSuiteEventAction where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookCheckSuiteApp where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookCheckSuiteAppPermissions where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookCheckSuite where
  arbitrary = do
    s <- genericArbitrary uniform
    app <- arbitrary
    pure s {whCheckSuiteApp = Just app}

instance Arbitrary HookRepository where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookCheckSuiteStatus where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookSimpleUser where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookOrganization where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookCheckSuiteConclusion where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookUser where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookChecksInstallation where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookChecksPullRequest where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookCheckSuiteCommit where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookChecksPullRequestTarget where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookChecksPullRequestRepository where
  arbitrary = genericArbitrary uniform

instance Arbitrary OwnerType where
  arbitrary = genericArbitrary uniform

instance Arbitrary URL where
  arbitrary = genericArbitrary uniform

instance Arbitrary PullRequestEvent where
  arbitrary = genericArbitrary uniform

instance Arbitrary PullRequestEventAction where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookPullRequest where
  arbitrary = genericArbitrary uniform

instance Arbitrary HookMilestone where
  arbitrary = genericArbitrary uniform

instance Arbitrary PullRequestTarget where
  arbitrary = genericArbitrary uniform

deriving instance Eq ServerEvent

deriving instance Show ServerEvent

instance Eq Bin.Builder where
  (==) = (==) `on` Bin.toLazyByteString

instance MonadFail M where
  fail = throw . OtherError . cs

instance Arbitrary Duration where
  arbitrary =
    oneof
      [ elements
          [ emptyDuration,
            fromMilliSeconds @Int 1,
            fromSeconds @Int 1,
            fromMinutes @Int 1,
            fromHours @Int 1,
            fromDays @Int 1
          ],
        arbitrary
      ]

instance Arbitrary FeatureFlagConfigDbo where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary FeatureId where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary FeatureConfig where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink
