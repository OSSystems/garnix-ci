module Garnix.IncrementalSpec (spec) where

import Control.Lens ((^?!))
import Control.Lens.Regex.Text (match, regexing)
import Cradle
import Data.Aeson.Lens (key, nth, _String)
import Data.Char (isSpace)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Garnix.Incremental
import Garnix.Nix.Types qualified as Nix
import Garnix.NixConfig (nixConfDefaults)
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.Deprecated qualified as Deprecated
import Garnix.TestHelpers.Monad
import Garnix.Types hiding (pending)
import Test.Hspec
import Text.Regex.PCRE.Light (compile)

spec :: Spec
spec = around_ Deprecated.quietWhenPassing $ do
  makeNormalizedFlakeSpec
  renderNormalizedFlakeWithHelpersSpec

makeNormalizedFlakeSpec :: Spec
makeNormalizedFlakeSpec = describe "makeNormalizedFlake" $ do
  let incrementalBuild =
        [i|
          derivation {
            name = "inc";
            builder = "/bin/sh";
            system = "x86_64-linux";
            outputs = [ "out" "intermediates" ];
            args = [ "-c" ''
              echo "foo" > $intermediates
              echo "bar" >  $out
              echo Done
            ''];
          }
        |]
      notIncrementalBuild =
        [i|
          derivation {
            name = "not-inc";
            builder = "/bin/sh";
            system = "x86_64-linux";
            outputs = [ "out" ];
            args = [ "-c" ''
              echo "foo" > $out
              echo Done
            ''];
          }
        |]
  inM $ do
    it "generates a flake with the same structure as the builds" $ do
      StdoutTrimmed o <-
        run
          $ cmd "nix"
          & addArgs
            [ "build",
              "--expr",
              incrementalBuild,
              "--json"
            ]
          & nixConfDefaults
      let drv = o ^?! nth 0 . key "drvPath" . _String
      build <- testBuild (drvPath ?~ cs drv)
      NormalizedFlake result <- makeNormalizedFlake [build]
      let intermediates = o ^?! nth 0 . key "outputs" . key "intermediates" . _String
      liftIO $ toList result `shouldBe` either error pure (Nix.parseStorePath intermediates)

    it "ignores packages without an 'intermediates' output" $ do
      StdoutTrimmed o <-
        run
          $ cmd "nix"
          & addArgs
            [ "build",
              "--expr",
              notIncrementalBuild,
              "--json"
            ]
          & nixConfDefaults
      let drv = o ^?! nth 0 . key "drvPath" . _String
      build <- testBuild (drvPath ?~ cs drv)
      NormalizedFlake result <- makeNormalizedFlake [build]
      liftIO $ toList result `shouldBe` []

    it "ignores builds without a drv path" $ do
      build <- testBuild (drvPath .~ Nothing)
      NormalizedFlake result <- makeNormalizedFlake [build]
      liftIO $ toList result `shouldBe` []

    it "ignores 'Build starting'" $ do
      build <- testBuild (packageType .~ TypeOverall)
      NormalizedFlake result <- makeNormalizedFlake [build]
      liftIO $ toList result `shouldBe` []

renderNormalizedFlakeWithHelpersSpec :: Spec
renderNormalizedFlakeWithHelpersSpec = describe "renderNormalizedFlakeWithHelpers" $ do
  let mkStorePath p = either error identity (Nix.parseStorePath @Text p)
      foo = "/nix/store/AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA-foo"
      bar = "/nix/store/BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB-bar"
      testCacheUrl :: Text
      testCacheUrl = "https://garnix.example/api/cache"
  it "renders simple normalized flakes"
    $ renderNormalizedFlakeWithHelpers
      "emptyDir"
      testCacheUrl
      ( mempty
          & at (TypePackage, IsSystem X8664Linux, "foo")
          ?~ mkStorePath foo
      )
    `withoutWhitespaceShouldBe` cs
      [i|
        {
          outputs = args :
            {
              packages.x86_64-linux.foo.intermediates = builtins.fetchClosure {
                inputAddressed = true;
                fromStore = "#{testCacheUrl}";
                fromPath = "#{foo}";
              };
              .*
            };
        }
      |]

  it "renders multiple attributes correctly"
    $ renderNormalizedFlakeWithHelpers
      "emptyDir"
      testCacheUrl
      ( mempty
          & at (TypePackage, IsSystem X8664Linux, "foo")
          ?~ mkStorePath foo
          & at (TypeCheck, IsSystem X8664Linux, "bar")
          ?~ mkStorePath bar
      )
    `withoutWhitespaceShouldBe` cs
      [i|
        {
          outputs = args :
            {
              checks.x86_64-linux.bar.intermediates = builtins.fetchClosure {
                inputAddressed = true;
                fromStore = "#{testCacheUrl}";
                fromPath = "#{bar}";
              };
              packages.x86_64-linux.foo.intermediates = builtins.fetchClosure {
                inputAddressed = true;
                fromStore = "#{testCacheUrl}";
                fromPath = "#{foo}";
              };
              .*
            };
        }
      |]

  it "renders the correct suffix for nixosConfigurations"
    $ renderNormalizedFlakeWithHelpers
      "emptyDir"
      testCacheUrl
      ( mempty
          & at (TypeNixosConfiguration, NoSystem, "foo")
          ?~ mkStorePath foo
      )
    `withoutWhitespaceShouldBe` cs
      [i|
        {
          outputs = args :
            {
              nixosConfigurations.foo.config.system.build.toplevel.intermediates = builtins.fetchClosure {
                inputAddressed = true;
                fromStore = "#{testCacheUrl}";
                fromPath = "#{foo}";
              };
              .*
            };
        }
      |]

withoutWhitespaceShouldBe :: IO Text -> Text -> Expectation
withoutWhitespaceShouldBe a b =
  a >>= \a' -> case strip a' ^? regex . match of
    Nothing -> expectationFailure $ cs (a' <> " did not match " <> b)
    Just _ -> pure ()
  where
    regex = regexing $ compile (cs $ strip b) []
    strip = T.filter (not . isSpace)
