module Garnix.Build.BatchEvalSpec (spec) where

import Cradle
import Data.ByteString (ByteString)
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Garnix.Attribute (Attribute, asAttribute)
import Garnix.Build.BatchEval
import Garnix.Build.Checkout (withCheckout)
import Garnix.Monad
import Garnix.Nix.Types qualified as Nix
import Garnix.NixConfig (addNixConfigEnvironment)
import Garnix.Prelude
import Garnix.Sandbox (inNixSandbox)
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.Types hiding (context)
import Test.Hspec

spec :: Spec
spec = do
  describe "parseEvalJobsOutput" $ do
    it "keys jobs by their unescaped attribute path" $ do
      Map.lookup "packages.x86_64-linux.foo" (parseEvalJobsOutput jsonl)
        `shouldBe` Just (Right fooDrv)

    it "keeps a job's eval error" $ do
      Map.lookup "checks.x86_64-linux.bad" (parseEvalJobsOutput jsonl)
        `shouldBe` Just (Left "eval failed")

    it "ignores lines that are not jobs" $ do
      Map.keys (parseEvalJobsOutput jsonl)
        `shouldBe` ["checks.x86_64-linux.bad", "packages.x86_64-linux.foo"]

  describe "buildJobsetExpr" $ do
    it "names each entry by its batchKey" $ do
      buildJobsetExpr "git+file:///repo" [attr "packages.x86_64-linux.foo"]
        `shouldContainText` "\"packages.x86_64-linux.foo\" = getPath"

    it "projects nixosConfigurations to their toplevel derivation" $ do
      buildJobsetExpr "git+file:///repo" [attr "nixosConfigurations.machine"]
        `shouldContainText` "\"config\" \"system\" \"build\" \"toplevel\""

    it "loads the flake from the given locked url" $ do
      buildJobsetExpr "git+file:///repo?rev=abc" []
        `shouldContainText` "builtins.getFlake \"git+file:///repo?rev=abc\""

    it "escapes attribute names" $ do
      buildJobsetExpr "git+file:///repo" [attr "packages.x86_64-linux.a\"${b}\\c"]
        `shouldContainText` "\"packages.x86_64-linux.a\\\"\\${b}\\\\c\" = getPath"

  describe "lookupBatched" $ inM . aroundM_ suppressLogsWhenPassing . beforeM_ truncateDBM $ do
    it "gives the drvPath nix evaluates for the attribute, self included" $ GH.withFakeGithubInterface $ \ghState -> do
      GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (setupWithoutLock selfDependentFlake) $ \commitInfo -> do
        withCheckout commitInfo $ do
          build <- testBuild (packageType .~ TypePackage)
          batched <- inBatchMode commitInfo [build] $ lookupBatched build
          expected <- nixEvalDrvPath "packages.x86_64-linux.test-package"
          batched `shouldBeM` BatchEvaluated (Right expected)

    it "does not run on a tree nix had to rewrite flake.lock in" $ GH.withFakeGithubInterface $ \ghState -> do
      GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup selfDependentFlake) $ \commitInfo -> do
        withCheckout commitInfo $ withMock #evalJobsMock (const $ error "nix-eval-jobs ran") $ do
          build <- testBuild (packageType .~ TypePackage)
          inBatchMode commitInfo [build] (lookupBatched build) `shouldReturnM` BatchFailed

    it "is a miss when nix-eval-jobs produces nothing for the attribute" $ GH.withFakeGithubInterface $ \ghState -> do
      GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (setupWithoutLock selfDependentFlake) $ \commitInfo -> do
        withCheckout commitInfo $ withMock #evalJobsMock (const $ pure "") $ do
          build <- testBuild (packageType .~ TypePackage)
          inBatchMode commitInfo [build] (lookupBatched build) `shouldReturnM` BatchMissed

    it "does not batch apps or incremental builds" $ GH.withFakeGithubInterface $ \ghState -> do
      GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (setupWithoutLock selfDependentFlake) $ \commitInfo -> do
        withCheckout commitInfo $ withMock #evalJobsMock (const $ pure jsonl) $ do
          app <- testBuild (packageType .~ TypeApp)
          incremental <- testBuild (wantsIncrementalism .~ True)
          inBatchMode commitInfo [app, incremental] (mapM lookupBatched [app, incremental])
            `shouldReturnM` [NotBatched, NotBatched]

    it "does not run under the perAttr strategy" $ GH.withFakeGithubInterface $ \ghState -> do
      GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (setupWithoutLock selfDependentFlake) $ \commitInfo -> do
        withCheckout commitInfo $ withMock #evalJobsMock (const $ error "nix-eval-jobs ran") $ do
          build <- testBuild (packageType .~ TypePackage)
          withBatchEval defaultRepoConfig commitInfo (FlakeDir ".") [build] (lookupBatched build)
            `shouldReturnM` NotBatched
  where
    inBatchMode :: CommitInfo -> [Build] -> M a -> M a
    inBatchMode commitInfo builds =
      local (#evalStrategy .~ Batch) . withBatchEval defaultRepoConfig commitInfo (FlakeDir ".") builds

    -- The way the per-attribute eval runs nix: same reference, same sandbox.
    nixEvalDrvPath :: Text -> M Nix.DrvPath
    nixEvalDrvPath attribute' = do
      cacheDir <- getNixXdgCacheDir
      workingDir <- view #workingDir
      nixConfig <- view #userNixConfig
      StdoutTrimmed drv <-
        (>>= run)
          $ cmd "nix"
          & addArgs ["eval", "--raw", cs workingDir <> "#" <> attribute' <> ".drvPath" :: Text]
          & addNixConfigEnvironment nixConfig
          & setWorkingDir workingDir
          & pure
          & inNixSandbox [] (Just cacheDir)
      either (error . cs) pure $ Nix.parseDrvPath (drv :: Text)

-- * Fixtures

-- | A flake without inputs needs no lock, so its tree stays clean.
setupWithoutLock :: Text -> FilePath -> M ()
setupWithoutLock flake repoPath = liftIO $ T.writeFile (repoPath </> "flake.nix") flake

-- | Its drvPath changes with @self@ and with the environment: loaded as a
-- @path:@ flake instead of the git checkout it is, or evaluated impurely, it
-- gets a different one.
selfDependentFlake :: Text
selfDependentFlake =
  cs
    [i|
    { outputs = {self}: {
        packages.x86_64-linux.test-package = derivation {
          name = "test-package";
          builder = "/bin/sh";
          system = "x86_64-linux";
          args = [ "-c" "echo ${self.rev or "dirty"} ${self} ${builtins.getEnv "HOME"} > $out" ];
        };
      };
    }
  |]

fooDrv :: Nix.DrvPath
fooDrv = either (error . cs) identity $ Nix.parseDrvPath ("/nix/store/00000000000000000000000000000000-foo.drv" :: Text)

jsonl :: ByteString
jsonl =
  cs
    $ T.unlines
      [ "{\"attr\":\"\\\"packages.x86_64-linux.foo\\\"\",\"attrPath\":[\"packages.x86_64-linux.foo\"],\"drvPath\":\"" <> cs fooDrv <> "\",\"outputs\":{}}",
        "warning: not json at all",
        "{\"attr\":\"\\\"checks.x86_64-linux.bad\\\"\",\"attrPath\":[\"checks.x86_64-linux.bad\"],\"error\":\"eval failed\"}"
      ]

attr :: Text -> Attribute
attr t = fromMaybe (error $ "bad test attr: " <> cs t) (t ^? asAttribute)

shouldContainText :: Text -> Text -> Expectation
shouldContainText haystack needle
  | needle `T.isInfixOf` haystack = pure ()
  | otherwise =
      expectationFailure $ "expected to find " <> cs needle <> " in:\n" <> cs haystack
