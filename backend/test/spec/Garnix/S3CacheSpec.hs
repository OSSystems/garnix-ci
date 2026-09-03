{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Redundant $" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Garnix.S3CacheSpec where

import Amazonka
  ( AccessKey (..),
    S3AddressingStyle (..),
    SecretKey (..),
    newEnv,
    overrideService,
    send,
    setEndpoint,
  )
import Amazonka.Auth qualified as Amazonka
import Amazonka.Data qualified as Amazonka
import Amazonka.S3 as Amazonka
import Control.Concurrent.Lifted (newMVar)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (ErrorCall (..))
import Control.Exception.Safe (throwIO)
import Control.Exception.Safe qualified as Safe
import Control.Lens
import Control.Monad.Trans.Control (liftBaseOp_)
import Cradle
import Cradle.ProcessConfiguration qualified as Cradle
import Data.Aeson.Lens
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as Lazy
import Data.Containers.ListUtils (nubOrd)
import Data.HashSet qualified as HashSet
import Data.HashTable.IO qualified as HashTables
import Data.List.Extra (firstJust)
import Data.Map qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set qualified as Set
import Data.String.Interpolate
import Data.String.Interpolate.Util (unindent)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Yaml (decodeThrow)
import Garnix.API.Cache (XForwardedFor (..), isInternal)
import Garnix.AccessToken.Types (AccessToken (..), AccessTokenScopes (..))
import Garnix.Build.Types (EvaluationResult (..))
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Monad
import Garnix.Monad.Async (resolve)
import Garnix.Monad.SubProcess (runSubProcess_)
import Garnix.Nix.Types (DrvPath (..), StorePath (..))
import Garnix.Nix.Types qualified as Nix
import Garnix.NixConfig (nixConfDefaults)
import Garnix.Orchestrator qualified as Orchestrator
import Garnix.Password (hashPassword)
import Garnix.Prelude
import Garnix.S3Cache (compress, parallelisableBlocks, upload)
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.NixStore (garbageCollectStorePath)
import Garnix.TestHelpers.Reporter
import Garnix.TestHelpers.WithServer
import Garnix.Types
import Network.HTTP.Types (notFound404, status400)
import Network.HTTP.Types.Status (status404)
import Network.Wreq qualified as Wreq
import Network.Wreq.Lens
import System.Directory (canonicalizePath, copyFile, getFileSize)
import System.Environment (getEnv)
import System.FilePath (takeExtension)
import System.IO (IOMode (..), withFile)
import System.IO qualified as IO
import System.IO.Temp (withSystemTempDirectory)
import System.Random (randomIO)
import Test.Hspec
import Test.Mockery.Directory (inTempDirectory)
import Test.Mockery.Environment (withModifiedEnvironment)
import Prelude qualified

spec :: Spec
spec = do
  let wrap =
        aroundAll withGarageS3
          . inMWith
          . beforeM_ (truncateDBM >> clearBuckets)
          . aroundM_ (withUnmock #s3CacheUploadMock . suppressLogsWhenPassing)
  describe "compressionThreadLimit" $ do
    it "never hands out less than one thread" $ do
      fmap compressionThreadLimit [1 .. 8] `shouldBe` [1, 1, 1, 1, 1, 1, 1, 2]

    it "leaves room for the minimum number of concurrent compressions" $ do
      let totals = [minimumConcurrentCompressions .. 256]
      filter (\total -> compressionThreadLimit total * minimumConcurrentCompressions > total) totals
        `shouldBe` []

    it "grows with the machine" $ do
      fmap compressionThreadLimit [16, 32, 64] `shouldBe` [4, 8, 16]

  describe "parallelisableBlocks" $ do
    it "asks for a single thread for anything xz compresses as one block" $ do
      fmap parallelisableBlocks [0, 1, mebibytes 1, mebibytes 24] `shouldBe` [1, 1, 1, 1]

    it "asks for one thread per additional block" $ do
      fmap parallelisableBlocks [mebibytes 24 + 1, mebibytes 48, mebibytes 240]
        `shouldBe` [2, 2, 10]

  wrap $ do
    describe "compress" $ do
      let roundTrip name expected = do
            compressed <- compress name
            run_ $ cmd "xz" & addArgs ["--test", compressed]
            run_ $ cmd "unxz" & addArgs [compressed]
            restored <- liftIO $ T.readFile name
            restored `shouldBeM` expected

      it "produces an archive that passes the xz integrity check" $ do
        liftBaseOp_ inTempDirectory $ do
          let contents = T.replicate 200000 "compressible nar-ish content\n"
          liftIO $ T.writeFile "file" contents
          blocks <- liftIO $ parallelisableBlocks <$> getFileSize "file"
          blocks `shouldBeM` 1
          roundTrip "file" contents

      it "produces an archive that passes the xz integrity check when xz runs multi-threaded" $ do
        liftBaseOp_ inTempDirectory $ do
          let contents = T.replicate 1200000 "compressible nar-ish content\n"
          liftIO $ T.writeFile "file" contents
          blocks <- liftIO $ parallelisableBlocks <$> getFileSize "file"
          blocks `shouldSatisfyM` (> 1)
          roundTrip "file" contents

      it "keeps archives intact when compressions contend for the thread budget" $ do
        liftBaseOp_ inTempDirectory $ do
          let contentsOf name = T.replicate 20 (cs name <> " padded content line\n")
              names = fmap (\n -> "file" <> Prelude.show n) [1 :: Int .. 400]
          liftIO $ forM_ names $ \name -> T.writeFile name (contentsOf name)
          forConcurrently_ names $ \name -> roundTrip name (contentsOf name)

    describe "upload" $ do
      it "uploads public store paths" $ do
        (evalResult, _) <- localTestBuild simpleFlake
        upload mempty "owner" "repo" evalResult (RepoIsPublic True)
        paths <- listBucket "garage-public"
        sort paths
          `shouldBeM` sort
            ( fmap
                (fromRight . Amazonka.fromText . (<> ".nar.xz") . Nix.getRelativeStorePath)
                (evalResult ^. #toUpload)
            )

      it "uploads store paths of private repos of configured owners to the public bucket" $ do
        (evalResult, storePath) <- localTestBuild simpleFlake
        local ((#s3CacheEnv . #publicRepoOwners) .~ Set.fromList ["alice"]) $ do
          upload mempty "Alice" "private-repo" evalResult (RepoIsPublic False)
        paths <- listBucket "garage-public"
        sort paths
          `shouldBeM` sort
            ( fmap
                (fromRight . Amazonka.fromText . (<> ".nar.xz") . Nix.getRelativeStorePath)
                (evalResult ^. #toUpload)
            )
        result <- DB.getS3CacheStoreHash (getHash storePath)
        fmap (^. #public) result `shouldBeM` Just True

      it "uploads store paths of private repos of other owners to the private bucket" $ do
        (evalResult, storePath) <- localTestBuild simpleFlake
        local ((#s3CacheEnv . #publicRepoOwners) .~ Set.fromList ["alice"]) $ do
          upload mempty "bob" "private-repo" evalResult (RepoIsPublic False)
        paths <- listBucket "garage-public"
        paths `shouldBeM` []
        result <- DB.getS3CacheStoreHash (getHash storePath)
        fmap (^. #public) result `shouldBeM` Just False

      it "doesn't re-upload existing store paths" $ do
        (evalResult, _) <- localTestBuild simpleFlake
        upload mempty "owner" "repo" evalResult (RepoIsPublic True)
        clearBuckets
        upload mempty "owner" "repo" evalResult (RepoIsPublic True)
        paths <- listBucket "garage-public"
        paths `shouldBeM` []

      it "correctly updates s3 cache fields on store paths that were uploaded to the old cache" $ do
        (evalResult, storePath) <- localTestBuild simpleFlake
        DB.tagCacheUpload "owner" "repo" [storePath]
        upload mempty "owner" "repo" evalResult (RepoIsPublic True)
        result <- DB.getS3CacheStoreHash (getHash storePath)
        result `shouldSatisfyM` isJust

      it "logs with span_package" $ do
        (evalResult, _) <- localTestBuild simpleFlake
        logs <- captureLogs_ $ upload mempty "owner" "repo" evalResult (RepoIsPublic True)
        nubOrd (fmap (Prelude.lookup "span_package" . (^. #context)) logs) `shouldBeM` [Just "foo"]

      it "doesn't upload store paths that are in cache.nixos.org" $ do
        flakeLockFile <- liftIO $ canonicalizePath "../flake.lock"
        liftBaseOp_ inTempDirectory $ do
          liftIO $ do
            copyFile flakeLockFile "./flake.lock"
            writeFile "flake.nix"
              $ unindent
                [i|
                  {
                    inputs.nixpkgs.url = "#{testNixpkgsUrl}";
                    outputs = { self, nixpkgs }: {
                      packages.x86_64-linux.default =
                        let
                          pkgs = import nixpkgs { system = "x86_64-linux"; };
                        in
                          pkgs.hello;
                    };
                  }
                |]
          storePath <- do
            StdoutTrimmed storePath <-
              run $ cmd "nix"
                & addArgs
                  [ "build",
                    "--no-link",
                    "--print-out-paths" :: Text
                  ]
                & silenceStderr
                & nixConfDefaults
            pure $ fromRight $ Nix.parseStorePath storePath
          drvPath <- do
            StdoutRaw output <-
              run $ cmd "nix"
                & addArgs
                  [ "path-info",
                    "--json" :: Text
                  ]
                & nixConfDefaults
            pure $ (output ^?! key (fromString $ cs storePath) . key "deriver" . _String)
              & DrvPath . fromRight . Nix.parseStorePath
          let evaluationResult = EvaluationResult drvPath [storePath] (Nix.BuildOutputs ("out" ~> storePath))
          upload mempty "owner" "repo" evaluationResult (RepoIsPublic True)
          paths <- listBucket "garage-public"
          paths `shouldBeM` []

      it "uploads output closures" $ do
        (evalResult, _) <- localTestBuild $ liftIO $ do
          random :: Int <- randomIO
          pure
            $ cs
              [i|
                {
                  outputs = {self}: {
                    packages.x86_64-linux = rec {
                      foo = derivation {
                        name = "foo";
                        builder = "/bin/sh";
                        system = "x86_64-linux";
                        args = [ "-c" ''
                          echo ${dep} >> $out
                          echo random number: #{random} >> $out
                        ''];
                      };
                      dep = derivation {
                        name = "dep";
                        builder = "/bin/sh";
                        system = "x86_64-linux";
                        args = [ "-c" ''
                          echo > $out
                        ''];
                      };
                    };
                  };
                }
              |]
        upload mempty "owner" "repo" evalResult (RepoIsPublic True)
        paths <- listBucket "garage-public"
        sort (fmap (T.drop 32 . (^. _ObjectKey)) paths)
          `shouldBeM` sort ["-foo.nar.xz", "-dep.nar.xz"]

      it "allows uploading partial closures of failed builds" $ do
        liftBaseOp_ inTempDirectory $ do
          random :: Int <- randomIO
          let flake =
                cs
                  [i|
                    {
                      outputs = {self}: {
                        packages.x86_64-linux = let
                          mkDerivation = name: script: derivation {
                            inherit name;
                            builder = "/bin/sh";
                            system = "x86_64-linux";
                            args = [ "-c" "# #{random}\n${script}" ];
                          };
                        in rec {
                          good = mkDerivation "good" "echo > $out";
                          parent = mkDerivation "foo" "# ${good}\nexit 1";
                        };
                      };
                    }
                  |]
          liftIO $ T.writeFile "flake.nix" flake
          _ <-
            try $ runSubProcess_ $ cmd "nix"
              & addArgs ["build", ".#parent" :: Text]
              & nixConfDefaults
          (parentDrvPath, parentStorePath) <- getFlakePackageDrvAndStorePath "parent"
          (_, goodStorePath) <- getFlakePackageDrvAndStorePath "good"
          let evalResult = EvaluationResult parentDrvPath [parentStorePath, goodStorePath] $ Nix.BuildOutputs mempty
          upload mempty "owner" "repo" evalResult (RepoIsPublic True)
          paths <- listBucket "garage-public"
          sort (fmap (T.drop 32 . (^. _ObjectKey)) paths)
            `shouldBeM` sort ["-good.nar.xz"]

      it "reports paths uploaded" $ do
        (evalResult, storePath) <- localTestBuild simpleFlake
        result <- withTestReporter_ $ \reporter -> do
          runReporter <- createNewRun reporter $ ReportBuild "build-name" undefined
          upload runReporter "owner" "repo" evalResult (RepoIsPublic True)
        result
          `shouldBeM` ( "build-name"
                          ~> TestReport ("Uploaded " <> Nix.getStorePath storePath <> " to the garnix binary cache.") Nothing
                      )

      it "rejects build artifacts that are too big" $ do
        let bigFlake = liftIO $ do
              random :: Int <- randomIO
              pure
                $ cs
                  [i|
                    {
                      outputs = {self}: {
                        packages.x86_64-linux = rec {
                          foo = derivation {
                            name = "foo";
                            builder = "/bin/sh";
                            system = "x86_64-linux";
                            args = [ "-c" ''
                              # #{random}
                              printf '%*s' 1025 "" > $out
                            ''];
                          };
                        };
                      };
                    }
                  |]
        (evalResult, storePath) <- localTestBuild bigFlake
        result <- withTestReporter_ $ \reporter -> do
          runReporter <- createNewRun reporter $ ReportBuild "build-name" undefined
          local ((#s3CacheEnv . #maxUploadSize) .~ 1024) $ do
            upload runReporter "owner" "repo" evalResult (RepoIsPublic True)
        paths <- listBucket "garage-public"
        paths `shouldBeM` []
        result
          `shouldBeM` ( "build-name"
                          ~> TestReport (Nix.getStorePath storePath <> " is 1025 bytes, the limit is 1024. Not uploading to the garnix binary cache.") Nothing
                      )

    describe "`uploadedToCache` flag" $ do
      beforeM_ truncateDBM $ do
        it "should set the upload flag in the database" $ do
          flake <- simpleFlake
          GH.withFakeGithubInterface $ \ghState -> do
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
              resolve =<< Orchestrator.handleCommit mempty True commitInfo
              waitFor (fromSeconds @Int 40) $ do
                builds <- DB.getBuilds $ User undefined "owner" undefined undefined undefined
                let statuses = Map.fromList $ mapMaybe (\build -> fmap (build ^. package,) (build ^. uploadedToCache)) builds
                statuses `shouldBeM` Map.fromList [("Build starting", False), ("foo", True)]

      it "works if the built derivation is already in the store" $ do
        flake <- simpleFlake
        GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
            repoDir <-
              GH.lookupRepo ghState "owner" "repo"
                <&> fromJust
                  . (^. #localPath)
                  . fromJust
            () <-
              runNix
                [ "build",
                  repoDir <> "#foo"
                ]
            resolve =<< Orchestrator.handleCommit mempty True commitInfo
            builds <- DB.getBuilds $ User undefined "owner" undefined undefined undefined
            let statuses = Map.fromList $ mapMaybe (\build -> fmap (build ^. package,) (build ^. status)) builds
            statuses
              `shouldBeM` ("Build starting" ~> Success)
              <> ("foo" ~> Success)

    describe "cache tagging" $ do
      it "tags uploaded store paths with the repo being built" $ do
        flake <- liftIO flakeWithDep
        GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
            resolve =<< Orchestrator.handleCommit mempty True commitInfo
            hash <- withSystemTempDirectory "mock-garnix-cache" $ \tmp -> do
              liftIO $ writeFile (tmp </> "flake.nix") (cs flake)
              hashForDerivation tmp "foo"
            waitFor (fromSeconds @Int 40) $ do
              repos <- DB.getReposForHash hash
              repos `shouldBeM` [("owner", "repo")]

      it "tags dependencies with the repo being built" $ do
        flake <- liftIO flakeWithDep
        -- Prevent bar from building on its own to ensure it is only built
        -- because it is a dependency of foo
        config <-
          liftIO
            $ decodeThrow
            $ cs
              [i|
                builds:
                  - exclude: ["packages.x86_64-linux.bar"]
              |]
        GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.setupWithGarnixConfig config flake) $ \commitInfo -> do
            resolve =<< Orchestrator.handleCommit mempty True commitInfo
            barHash <- withSystemTempDirectory "mock-garnix-cache" $ \tmp -> do
              liftIO $ writeFile (tmp </> "flake.nix") (cs flake)
              hashForDerivation tmp "bar"
            waitFor (fromSeconds @Int 40) $ do
              repos <- DB.getReposForHash barHash
              liftIO $ repos `shouldBe` [("owner", "repo")]

      it "re-tags dependencies if they have been uploaded before" $ do
        privateRandom <- show <$> (randomIO :: M Int)
        publicRandom <- show <$> (randomIO :: M Int)
        sharedDepRandom <- show <$> (randomIO :: M Int)
        let privateFlake = flakeWithDepWithRandomSnippets privateRandom sharedDepRandom
        let publicFlake = flakeWithDepWithRandomSnippets publicRandom sharedDepRandom
        (privateResult, _) <- localTestBuild $ pure $ privateFlake
        (publicResult, _) <- localTestBuild $ pure $ publicFlake
        upload mempty "alice" "private-repo" privateResult (RepoIsPublic False)
        upload mempty "bob" "public-repo" publicResult (RepoIsPublic True)
        depHash <- withSystemTempDirectory "mock-garnix-cache" $ \tmp -> do
          liftIO $ writeFile (tmp </> "flake.nix") (cs publicFlake)
          hashForDerivation tmp "bar"
        repos <- DB.getReposForHash depHash
        sort repos
          `shouldBeM` [ ("alice", "private-repo"),
                        ("bob", "public-repo")
                      ]

    it "serves nix-cache-info endpoint" $ withServer $ \server -> do
      response <- assert200 $ server.get "/api/cache/nix-cache-info"
      response
        ^. responseBody
          `shouldBeM` cs
            ( unindent
                [i|
                  StoreDir: /nix/store
                  WantMassQuery: 1
                  Priority: 50
                |]
            )

    describe "narinfo endpoint" $ do
      it "allows downloading public uploaded nar files from the new cache" $ withServer $ \server -> do
        (evalResult, storePath) <- localTestBuild simpleFlake
        upload mempty "owner" "repo" evalResult (RepoIsPublic True)
        liftIO $ garbageCollectStorePath (cs storePath)
        response <- assert200 $ server.get ("/api/cache/" <> cs (getHash storePath) <> ".narinfo")
        log Informational $ cs $ response ^. responseBody
        testCachePubKey <- liftIO $ getEnv "TEST_CACHE_PUB_KEY"
        runSubProcess_ $ cmd "nix"
          & addArgs
            [ "copy",
              "--from",
              server.apiUrl <> "/cache",
              cs storePath :: Text
            ]
          & modifyEnvVar "NIX_CONFIG" (const $ Just $ "trusted-public-keys = " <> testCachePubKey)
          & nixConfDefaults
        contents <- liftIO $ T.readFile (cs storePath)
        contents `shouldSatisfyM` ("random number: " `T.isPrefixOf`)

      it "disallows downloading private uploaded nar files without authentication" $ do
        GH.withFakeGithubInterface $ \github -> do
          withServer $ \server -> do
            user <- testUser
            GH.mkRepo github (GhRepoOwner $ user ^. githubLogin) "repo"
              $ (#publicity .~ RepoIsPublic False)
            (evalResult, storePath) <- localTestBuild simpleFlake
            upload mempty (GhRepoOwner $ user ^. githubLogin) "repo" evalResult (RepoIsPublic False)
            liftIO $ garbageCollectStorePath (cs storePath)
            narInfoResponse <- server.get ("/api/cache/" <> cs (getHash storePath) <> ".narinfo")
            narInfoResponse ^. responseStatus `shouldBeM` notFound404

      describe "accessing private uploaded nar files with access tokens" $ do
        let createDerivationInCache user github = do
              GH.mkRepo github (GhRepoOwner $ user ^. githubLogin) "repo"
                $ (#publicity .~ RepoIsPublic False)
              (evalResult, storePath) <- localTestBuild simpleFlake
              upload mempty (GhRepoOwner $ user ^. githubLogin) "repo" evalResult (RepoIsPublic False)
              liftIO $ garbageCollectStorePath (cs storePath)
              pure storePath

        let createAccessToken server scopes = do
              res <- assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "test token", scopes: #{scopes} } |]
              pure $ AccessToken $ res ^?! responseBody . key "token" . _String

        let checkCanDownloadFromCache server user storePath accessToken = do
              liftBaseOp (withSystemTempDirectory "garnix-test") $ \tmp -> do
                let netrcFile = tmp </> "netrc"
                liftIO
                  $ T.writeFile netrcFile
                  $ cs
                  $ unindent
                  $ [i|
                      machine localhost
                      login #{getGhLogin (user ^. githubLogin)}
                      password #{getAccessTokenText accessToken}
                    |]
                testCachePubKey <- liftIO $ getEnv "TEST_CACHE_PUB_KEY"
                runSubProcess_ $ cmd "nix"
                  & addArgs
                    [ "copy",
                      "--from",
                      server.apiUrl <> "/cache",
                      cs storePath :: Text
                    ]
                  & modifyEnvVar
                    "NIX_CONFIG"
                    ( const
                        $ Just
                        $ unlines
                          [ "netrc-file = " <> netrcFile,
                            "trusted-public-keys = " <> testCachePubKey
                          ]
                    )
                  & nixConfDefaults

        it "allows downloading private uploaded nar files when authenticated" $ do
          GH.withFakeGithubInterface $ \github -> do
            withServer $ \server -> do
              user <- server.login
              storePath <- createDerivationInCache user github
              accessToken <- createAccessToken server [aesonQQ| { cache: true } |]
              checkCanDownloadFromCache server user storePath accessToken

        it "does not allow private uploaded nar files for access tokens not including the cache scope" $ do
          GH.withFakeGithubInterface $ \github -> do
            withServer $ \server -> do
              user <- server.login
              storePath <- createDerivationInCache user github
              accessToken <- createAccessToken server [aesonQQ| { api: true, cache: false } |]
              Left err <- try $ checkCanDownloadFromCache server user storePath accessToken
              cs (show err) `shouldContainM` "there is no substituter that can build it"

      it "cannot download a private nar file after the presigned url expired" $ do
        local (#s3CacheEnv . #expiration .~ fromSeconds @Int 1) $ do
          GH.withFakeGithubInterface $ \github -> do
            withServer $ \server -> do
              user <- testUser
              GH.mkRepo github (GhRepoOwner $ user ^. githubLogin) "repo"
                $ (#publicity .~ RepoIsPublic False)
              (evalResult, storePath) <- localTestBuild simpleFlake
              upload mempty (GhRepoOwner $ user ^. githubLogin) "repo" evalResult (RepoIsPublic False)
              liftIO $ garbageCollectStorePath (cs storePath)
              let plainTextToken = "hunter2"
              hashPassword plainTextToken >>= DB.insertAccessTokenForUser (user ^. id) "test token" (AccessTokenScopes {api = False, cache = True})
              narInfoResponse <-
                assert200
                  $ server.getWithHeaders
                    ("/api/cache/" <> cs (getHash storePath) <> ".narinfo")
                    [("Authorization", "Basic " <> Base64.encode ("user:" <> cs plainTextToken))]
              log Informational $ cs $ narInfoResponse ^. responseBody
              threadDelay $ fromSeconds @Double 1.1
              response <-
                liftIO
                  $ Wreq.getWith
                    (Wreq.defaults & checkResponse ?~ (\_ _ -> pure ()))
                    (cs $ extractFromNarInfo narInfoResponse "URL")
              log Informational $ cs $ response ^. responseBody
              (response ^. responseStatus) `shouldBeM` status400

      it "serves compressed nar files" $ do
        withServer $ \server -> do
          (evalResult, storePath) <- localTestBuild simpleFlake
          upload mempty "owner" "repo" evalResult (RepoIsPublic True)
          narInfoResponse <- server.get ("/api/cache/" <> cs (getHash storePath) <> ".narinfo")
          let extract = extractFromNarInfo narInfoResponse
          extract "Compression" `shouldBeM` "xz"
          let narUrl = extract "URL"
          takeExtension (cs narUrl) `shouldBeM` ".xz"
          liftBaseOp_ inTempDirectory $ do
            run_ $ cmd "curl" & addArgs [narUrl, "-o", "file.nar.xz", "--silent"]
            fileSize <- liftIO $ getFileSize "file.nar.xz"
            extract "FileSize" `shouldBeM` show fileSize
            fileHash <- getFileHash "file.nar.xz"
            extract "FileHash" `shouldBeM` ("sha256:" <> fileHash)
            run_ $ cmd "unxz" & addArgs ["file.nar.xz" :: Text]
            narSize <- liftIO $ getFileSize "file.nar"
            extract "NarSize" `shouldBeM` show narSize
            narHash <- getFileHash "file.nar"
            extract "NarHash" `shouldBeM` ("sha256:" <> narHash)
            StdoutRaw content <-
              run $ cmd "nix"
                & addArgs
                  [ "nar",
                    "cat",
                    "file.nar",
                    "/" :: Text
                  ]
                & nixConfDefaults
            content `shouldSatisfyM` (("random number: " `T.isPrefixOf`) . cs)

      it "contains the 'References' field" $ withServer $ \server -> do
        (evalResult, storePath) <- localTestBuild flakeWithDep
        upload mempty "owner" "repo" evalResult (RepoIsPublic True)
        reference <- do
          StdoutRaw output <-
            run $ cmd "nix"
              & addArgs
                [ "path-info",
                  "--json",
                  cs storePath :: Text
                ]
              & nixConfDefaults
          let parsed =
                output
                  ^.. key (fromString $ cs storePath)
                    . key "references"
                    . _Array
                    . traverse
                    . _String
          pure $ fromJust $ T.stripPrefix "/nix/store/" $ fromSingleton parsed
        response <- assert200 $ server.get ("/api/cache/" <> cs (getHash storePath) <> ".narinfo")
        cs (response ^. responseBody) `shouldContainM` ("References: " <> cs reference)

      it "returns 404 when the store path cannot be found" $ withServer $ \server -> do
        (_, storePath) <- localTestBuild simpleFlake
        response <- server.get ("/api/cache/" <> cs (getHash storePath) <> ".narinfo")
        response ^. responseStatus `shouldBeM` status404

      describe "isInternal" $ do
        let cases =
              [ (Nothing, False),
                (Just (XForwardedFor "127.0.0.1"), False),
                (Just (XForwardedFor "2a01:4f9:3a:47cf::1, 127.0.0.1"), True),
                (Just (XForwardedFor "2a01:4f9:3a:47cf::1,127.0.0.1"), True),
                (Just (XForwardedFor "2a01:4f9:3a:47cf::1, 127.0.0.1, 127.0.0.1"), True),
                (Just (XForwardedFor "2a01:4f8:e0:204e::2, 127.0.0.1"), True),
                (Just (XForwardedFor "42.42.42.42, 2a01:4f9:3a:47cf::1, 127.0.0.1"), True)
              ]
        forM_ cases $ \(header, expected) -> do
          it (cs ("isInternal " <> show header <> " == " <> show expected)) $ do
            isInternal header `shouldBeM` expected

localTestBuild :: M Text -> M (EvaluationResult, StorePath)
localTestBuild mkFlake = do
  liftBaseOp_ inTempDirectory $ do
    flake <- mkFlake
    liftIO $ T.writeFile "flake.nix" flake
    runSubProcess_ $ cmd "nix"
      & addArgs
        [ "build",
          ".#foo" :: Text
        ]
      & nixConfDefaults
    (drvPath, storePath) <- getFlakePackageDrvAndStorePath "foo"
    pure (EvaluationResult drvPath [storePath] (Nix.BuildOutputs ("out" ~> storePath)), storePath)

getFlakePackageDrvAndStorePath :: Text -> M (DrvPath, StorePath)
getFlakePackageDrvAndStorePath packageName = do
  StdoutTrimmed output <-
    run
      $ cmd "nix"
      & addArgs
        [ "eval",
          ".#" <> packageName,
          "--apply",
          "x : {storePath = builtins.toString x; drvPath = x.drvPath; }",
          "--json" :: Text
        ]
      & nixConfDefaults
  let drvPath = DrvPath $ fromRight $ Nix.parseStorePath (output ^. key "drvPath" . _String)
  let storePath = fromRight $ Nix.parseStorePath (output ^. key "storePath" . _String)
  pure (drvPath, storePath)

simpleFlake :: (MonadIO m) => m Text
simpleFlake = liftIO $ do
  random :: Int <- randomIO
  pure
    $ cs
      [i|
        {
          outputs = {self}: {
            packages.x86_64-linux = rec {
              foo = derivation {
                name = "foo";
                builder = "/bin/sh";
                system = "x86_64-linux";
                args = [ "-c" ''
                  echo random number: #{random} > $out
                ''];
              };
            };
          };
        }
      |]

flakeWithDep :: (MonadIO m) => m Text
flakeWithDep = liftIO $ do
  random :: Int <- randomIO
  pure $ flakeWithDepWithRandomSnippets (show random) (show random)

flakeWithDepWithRandomSnippets :: Text -> Text -> Text
flakeWithDepWithRandomSnippets fooDrvRandom barDrvRandom =
  cs
    [i|
        { outputs = {self}: let
            mkDerivation = name: random: derivation {
              inherit name;
              builder = "/bin/sh";
              system = "x86_64-linux";
              args = [ "-c" ''
                echo ${random} > $out
              ''];
            };
          in {
            packages.x86_64-linux = rec {
              foo = mkDerivation "foo" "depends on ${bar} #{fooDrvRandom}";
              bar = mkDerivation "bar" "dependency #{barDrvRandom}";
            };
          };
        }
      |]

hashForDerivation :: FilePath -> String -> M Nix.StoreHash
hashForDerivation flakeDir packageName = do
  StdoutTrimmed json <- runNix ["build", flakeDir <> "#" <> packageName, "--json", "--dry-run"]
  let storePath = json ^?! nth 0 . key "outputs" . key "out" . _String
  pure $ Nix.StoreHash $ T.takeWhile (/= '-') $ fromJust $ T.stripPrefix "/nix/store/" storePath

runNix :: (Output output, MonadIO m) => [String] -> m output
runNix args = do
  (output, StderrRaw stderr, exitCode) <-
    liftIO
      $ addNixExperimentalFeatures ["nix-command", "flakes"]
      $ run
      $ cmd "nix"
      & addArgs args
      & silenceStdout
      & silenceStderr
      & nixConfDefaults
  when (exitCode /= ExitSuccess) $ do
    error $ "nix failed with " <> show exitCode <> ":\n" <> cs stderr
  pure output

withGarageS3 :: ((Env -> Env) -> IO a) -> IO a
withGarageS3 inner =
  withSystemTempDirectory "garnix-garage" $ \garageDir -> do
    runSilently_ $ cmd "nix-store"
      & addArgs
        [ "--generate-binary-cache-key",
          "test-key",
          "cache-priv-key.pem",
          "cache-pub-key.pem" :: Text
        ]
      & setWorkingDir garageDir
    cachePubKey <- readFile $ garageDir </> "cache-pub-key.pem"
    liftBaseOp_ (withModifiedEnvironment [("TEST_CACHE_PUB_KEY", cachePubKey)]) $ do
      Safe.bracket (startGarage garageDir) killGarage $ const $ do
        amazonkaEnv <- initializeGarage garageDir
        isInNixosCacheMemoTable <- HashTables.new >>= newMVar
        accessBuffer <- liftIO $ newTVarIO HashSet.empty
        inner
          ( #s3CacheEnv
              .~ S3CacheEnv
                { amazonkaEnv,
                  publicBucket = "garage-public",
                  publicBaseUrl = "http://garage-public.web.garage.localhost:3902/",
                  privateBucket = "garage-private",
                  publicRepoOwners = mempty,
                  cachePrivKeyFile = cs (garageDir </> "cache-priv-key.pem"),
                  cachePrivKeyName = "test-key",
                  expiration = fromSeconds @Int 10,
                  maxUploadSize = 2 ^ (30 :: Integer),
                  isInNixosCacheMemoTable,
                  accessBuffer,
                  accessFlushEvery = defaultAccessFlushEvery,
                  accessFlushMax = defaultAccessFlushMax,
                  accessBumpMinAge = defaultAccessBumpMinAge
                }
          )
  where
    startGarage garageDir = do
      let garageConfig =
            unindent
              [i|
                metadata_dir = "#{garageDir}/meta"
                data_dir = "#{garageDir}/data"
                db_engine = "sqlite"

                replication_factor = 1

                rpc_bind_addr = "[::]:3901"
                rpc_public_addr = "127.0.0.1:3901"
                rpc_secret = "912cf119515d24ef957367b4bba8a0cf82bc6d5e25b6d7ca58ebc1595d05b151"

                [s3_api]
                s3_region = "garage"
                api_bind_addr = "[::]:3900"
                root_domain = ".s3.garage.localhost"

                [s3_web]
                bind_addr = "[::]:3902"
                root_domain = ".web.garage.localhost"
                index = "index.html"
              |]
      writeFile (garageDir </> "garage.toml") garageConfig
      fork $ do
        withFile "./test/spec/garage.log" AppendMode $ \logHandle -> do
          (_ :: ExitCode, StdoutRaw stdout, StderrRaw stderr) <-
            run
              $ cmd "garage"
              & addArgs ["-c", garageDir </> "garage.toml"]
              & addArgs ["server" :: Text]
              & addStdoutHandle logHandle
              & addStderrHandle logHandle
          ByteString.putStr stdout
          ByteString.hPutStr IO.stderr stderr
          error "garage (s3 mock) terminated unexpectedly"

    initializeGarage garageDir = do
      let garage = cmd "garage" & addArgs ["-c", garageDir </> "garage.toml"]
      waitFor (fromSeconds @Int 30) $ do
        runSilently_ $ garage & addArgs ["status" :: Text]
      nodeId <- do
        StdoutTrimmed output <- run $ garage & addArgs (T.words "node id -q")
        pure $ T.takeWhile (/= '@') output
      runSilently_ $ garage & addArgs (T.words "layout assign -z dc1 -c 1G" <> [nodeId])
      runSilently_ $ garage & addArgs (T.words "layout apply --version 1")
      runSilently_ $ garage & addArgs (T.words "bucket create garage-public")
      runSilently_ $ garage & addArgs (T.words "bucket create garage-private")
      (accessKeyId, secretAccessKey) <- do
        StdoutRaw output <- run $ garage & addArgs (T.words "key create test-key")
        let extract field =
              maybe (error ("cannot extract field " <> field)) pure
                $ firstJust (T.stripPrefix (field <> ": "))
                $ T.lines
                $ cs output
        accessKey <- extract "Key ID"
        secretKey <- extract "Secret key"
        pure (AccessKey $ cs accessKey, SecretKey $ cs secretKey)
      runSilently_ $ garage & addArgs (T.words "bucket allow --read --write garage-public --key test-key")
      runSilently_ $ garage & addArgs (T.words "bucket website --allow garage-public")
      runSilently_ $ garage & addArgs (T.words "bucket allow --read --write garage-private --key test-key")
      newEnv (pure . Amazonka.fromKeys accessKeyId secretAccessKey)
        <&> (#region .~ Region' "garage")
        <&> overrideService (setEndpoint False "localhost" 3900)
        <&> overrideService (#s3AddressingStyle .~ S3AddressingStylePath)

    killGarage thread = do
      killThread thread
      _ :: ExitCode <-
        run
          $ cmd "fuser"
          & addArgs ["-k", "3900/tcp" :: Text]
          & silenceStdout
          & silenceStderr
      pure ()

listBucket :: BucketName -> M [ObjectKey]
listBucket bucket = do
  s3Env <- view $ #s3CacheEnv . #amazonkaEnv
  response <- liftIO $ runResourceT $ send s3Env $ newListObjectsV2 bucket
  pure $ case response ^. #contents of
    Nothing -> []
    Just objects -> objects & map (^. #key)

clearBuckets :: M ()
clearBuckets = do
  mapM_ clearBucket ["garage-public", "garage-private"]
  where
    clearBucket bucket = do
      objects <- listBucket bucket
      forM_ objects $ \object -> do
        s3Env <- view $ #s3CacheEnv . #amazonkaEnv
        liftIO $ runResourceT $ send s3Env $ newDeleteObject bucket object

runSilently :: (Output output) => ProcessConfiguration -> IO output
runSilently config = do
  (exitCode, StdoutRaw stdout, StderrRaw stderr, output) <- run config
  when (exitCode /= ExitSuccess) $ do
    throwIO
      $ ErrorCall
      $ cs
      $ "command failed: "
      <> T.unwords (fmap cs (Cradle.executable config : Cradle.arguments config))
      <> "\n"
      <> cs stdout
      <> "\n"
      <> cs stderr
  pure output

runSilently_ :: ProcessConfiguration -> IO ()
runSilently_ = runSilently

getFileHash :: FilePath -> M Text
getFileHash file = do
  StdoutTrimmed narHash <-
    run
      $ cmd "nix-hash"
      & addArgs ["--base32", "--type", "sha256", "--flat", file]
  pure narHash

mebibytes :: Integer -> Integer
mebibytes n = n * 1024 * 1024

extractFromNarInfo :: Response Lazy.ByteString -> Text -> Text
extractFromNarInfo narInfoResponse field =
  case catMaybes
    $ map (T.stripPrefix (field <> ": "))
    $ T.lines
    $ cs
    $ (narInfoResponse ^. responseBody) of
    [field] -> field
    not -> error $ "failed to extract field " <> show field <> ": " <> show not
