module Garnix.DBSpec (spec) where

import Control.Concurrent.Async.Lifted (replicateConcurrently)
import Control.Exception qualified as E
import Control.Monad.Trans.Control (liftBaseDiscard)
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed qualified as PSQL
import Garnix.DB qualified as DB
import Garnix.Duration (fromDays, fromHours, fromSeconds)
import Garnix.Monad (M, throw)
import Garnix.Nix.Types (DrvPath (..), StoreHash (..), StorePath (..))
import Garnix.Prelude
import Garnix.TestHelpers (testBuild, truncateDBM)
import Garnix.TestHelpers.Monad (beforeM_, inM, shouldBeM, shouldReturnM)
import Garnix.Types hiding (context, head)
import System.Environment (getEnv)
import System.IO.Silently (hSilence)
import Test.Hspec
import Test.Mockery.Environment (withEnvironment)
import Test.QuickCheck (generate, shuffle)

spec :: Spec
spec = do
  describe "newBuild" $ inM $ beforeM_ truncateDBM $ do
    it "allows duplicate builds" $ do
      user <-
        DB.newUser
          (GhLogin "user")
          (Email "foo@x.com")
          FreeSubscription
          True
      let go =
            DB.newBuildDB
              ( CommitInfo
                  (user ^. githubLogin)
                  (RepoIsPublic True)
                  ( RepoInfo
                      undefined
                      undefined
                      (GhRepoOwner $ GhLogin "foo")
                      (GhRepoName "bar")
                  )
                  (Just (Branch "branch/name"))
                  Nothing
                  (CommitHash "baz")
              )
              (PackageInfo TypePackage (IsSystem X8664Linux) (PackageName "quux"))
              "garnix-server-test"
              False
      void go
      void go

  context "pgTransaction" $ inM $ beforeM_ truncateDBM $ do
    it "rolls transactions back when throwing errors in M" $ do
      (void . try . DB.pgTransaction) $ do
        void
          $ DB.pgQuery
            [pgSQL|
              INSERT INTO heartbeat
                (hostname, last_heartbeat)
                VALUES ('test', NOW())
            |]
        throw $ OtherError "testing"
      hb <- DB.getRecentHeartbeats
      liftIO $ hb `shouldBe` []

    it "rolls transactions back due to SQL errors" $ do
      (void . liftBaseDiscard (E.try @PGError) . DB.pgTransaction) $ do
        void
          $ DB.pgQuery
            [pgSQL|
        INSERT INTO heartbeat
          (hostname, last_heartbeat)
          VALUES ('test', NOW())
          |]
        void $ DB.newUser (GhLogin "conflict") (Email "a@a") FreeSubscription True
        void $ DB.newUser (GhLogin "conflict") (Email "a@a") FreeSubscription True
      hb <- DB.getRecentHeartbeats
      liftIO $ hb `shouldBe` []

  context "getUserInternalToken" $ inM $ beforeM_ truncateDBM $ do
    it "gets the same token when called by multiple threads concurrently" $ do
      results <- replicateConcurrently 50 (DB.getUserInternalToken $ GhLogin "user")
      liftIO $ results `shouldBe` replicate 50 (head results)

  context "claimS3CachedStorePaths" $ inM $ beforeM_ truncateDBM $ do
    let getCacheEntries :: M [(Text, Maybe Text, Maybe UTCTime)]
        getCacheEntries =
          DB.pgQuery
            [pgSQL|
          SELECT hash, package_name, uploaded_at FROM cache_store_hashes
            |]
    it "never returns the same store path in different calls" $ do
      let storePaths = [StorePath (StoreHash $ show n) (show n) | n <- [1 :: Int .. 100]]
      returned <- replicateConcurrently 100 $ do
        shuffled <- liftIO $ generate $ shuffle storePaths
        DB.claimS3CachedStorePaths shuffled
      liftIO $ sort (mconcat returned) `shouldBe` sort storePaths

    it "returns existing old-style cache entries" $ do
      void
        $ DB.pgQuery
          [pgSQL|
        INSERT INTO cache_store_hashes
          (hash)
          VALUES ('foo')
          |]
      let storePaths = [StorePath (StoreHash "foo") "bar"]
      claimed <- DB.claimS3CachedStorePaths storePaths
      liftIO $ claimed `shouldBe` storePaths

      getCacheEntries `shouldReturnM` [("foo", Just "bar", Nothing)]

    it "doesn't return recent new-style cache entries that have not been uploaded yet" $ do
      void
        $ DB.pgQuery
          [pgSQL|
        INSERT INTO cache_store_hashes
          (hash, package_name)
          VALUES ('foo', 'bar')
          |]
      let storePaths = [StorePath (StoreHash "foo") "bar"]
      claimed <- DB.claimS3CachedStorePaths storePaths
      liftIO $ claimed `shouldBe` []

      getCacheEntries `shouldReturnM` [("foo", Just "bar", Nothing)]

    it "return stale new-style cache entries that have not been uploaded yet" $ do
      (void . liftBaseDiscard (E.try @PGError) . DB.pgTransaction) $ do
        void
          $ DB.pgQuery
            [pgSQL|
        INSERT INTO cache_store_hashes
          (hash, package_name, created_at)
          VALUES ('foo', 'bar', now() - interval '5 days')
          |]
      let storePaths = [StorePath (StoreHash "foo") "bar"]
      claimed <- DB.claimS3CachedStorePaths storePaths
      liftIO $ claimed `shouldBe` storePaths

      getCacheEntries `shouldReturnM` [("foo", Just "bar", Nothing)]

  context "s3 cache retention" $ inM $ beforeM_ truncateDBM $ do
    let storeHashes = fmap DB.gcObjectHash
        uploaded :: Text -> [Text] -> Double -> M ()
        uploaded name references ageInDays = do
          void
            $ DB.pgExec
              [pgSQL|
            INSERT INTO cache_store_hashes (hash) VALUES (${name}) ON CONFLICT DO NOTHING
              |]
          DB.finalizeS3CacheUpload
            DB.S3CacheStoreHash
              { DB.hash = StoreHash name,
                DB.packageName = "pkg",
                DB.narHash = "narHash",
                DB.narSize = 1,
                DB.public = True,
                DB.sig = "sig",
                DB.references = T.unwords (fmap (<> "-pkg") references),
                DB.fileSize = 10,
                DB.fileHash = "fileHash"
              }
          void
            $ DB.pgExec
              [pgSQL|
            UPDATE cache_store_hashes
            SET accessed_at = now() - (${ageInDays}::double precision * interval '1 day')
            WHERE hash = ${name}
              |]
        cutoffInDays :: Double -> M UTCTime
        cutoffInDays days = do
          result <-
            DB.pgQuery
              [pgSQL|
            SELECT now() - (${days}::double precision * interval '1 day')
              |]
          case result of
            [Just cutoff] -> pure cutoff
            _ -> throw $ OtherError "could not compute a cutoff"

    it "keeps a cold store path that a recently read one still references" $ do
      uploaded "aaa" ["bbb"] 0
      uploaded "bbb" ["ccc"] 200
      uploaded "ccc" [] 200
      cutoff <- cutoffInDays 90
      candidates <- DB.markGcCandidates cutoff 100
      liftIO $ storeHashes candidates `shouldBe` []

    it "collects the whole closure once its root goes cold" $ do
      uploaded "aaa" ["bbb"] 200
      uploaded "bbb" ["ccc"] 200
      uploaded "ccc" [] 200
      cutoff <- cutoffInDays 90
      candidates <- DB.markGcCandidates cutoff 100
      liftIO
        $ sort (storeHashes candidates)
        `shouldBe` [StoreHash "aaa", StoreHash "bbb", StoreHash "ccc"]

    it "terminates on reference cycles" $ do
      uploaded "ddd" ["eee"] 200
      uploaded "eee" ["ddd"] 200
      cutoff <- cutoffInDays 90
      candidates <- DB.markGcCandidates cutoff 100
      liftIO $ sort (storeHashes candidates) `shouldBe` [StoreHash "ddd", StoreHash "eee"]

    it "stops serving a store path as soon as it is tombstoned" $ do
      uploaded "aaa" [] 200
      cutoff <- cutoffInDays 90
      tombstoned <- DB.tombstoneGcObjects cutoff [StoreHash "aaa"]
      liftIO $ storeHashes tombstoned `shouldBe` [StoreHash "aaa"]
      served <- DB.getS3CacheStoreHash (StoreHash "aaa")
      liftIO $ isNothing served `shouldBe` True

    it "does not let a tombstoned store path be claimed for upload" $ do
      uploaded "aaa" [] 200
      cutoff <- cutoffInDays 90
      void $ DB.tombstoneGcObjects cutoff [StoreHash "aaa"]
      DB.claimS3CachedStorePaths [StorePath (StoreHash "aaa") "pkg"] `shouldReturnM` []

    it "cancels an eviction when the store path is read between mark and sweep" $ do
      uploaded "aaa" [] 200
      cutoff <- cutoffInDays 90
      candidates <- DB.markGcCandidates cutoff 100
      liftIO $ storeHashes candidates `shouldBe` [StoreHash "aaa"]
      void $ DB.bumpCacheAccessedAt (fromSeconds @Int 0) [StoreHash "aaa"]
      tombstoned <- DB.tombstoneGcObjects cutoff [StoreHash "aaa"]
      liftIO $ storeHashes tombstoned `shouldBe` []

    it "removes the rows and edges of collected store paths" $ do
      uploaded "aaa" ["bbb"] 200
      uploaded "bbb" [] 200
      DB.deleteGcObjects [StoreHash "aaa", StoreHash "bbb"]
      stats <- DB.getCacheSizeStats
      liftIO $ DB.cacheLiveObjects stats `shouldBe` 0
      edges <-
        DB.pgQuery
          [pgSQL|
        SELECT count(*) FROM cache_store_hash_references WHERE hash = 'aaa'
          |]
      liftIO $ edges `shouldBe` [Just (0 :: Int64)]

    it "only bumps accessed_at once per minimum age" $ do
      uploaded "aaa" [] 200
      firstBump <- DB.bumpCacheAccessedAt (fromHours @Int 6) [StoreHash "aaa"]
      liftIO $ firstBump `shouldBe` 1
      secondBump <- DB.bumpCacheAccessedAt (fromHours @Int 6) [StoreHash "aaa"]
      liftIO $ secondBump `shouldBe` 0

    it "is not warmed up until reads have been recorded for the warmup period" $ do
      void $ DB.pgExec [pgSQL| UPDATE cache_gc_state SET reads_recorded_since = NULL |]
      untracked <- DB.getGcCutoff (fromDays @Int 90) (fromDays @Int 7)
      liftIO $ DB.gcCutoffWarmedUp untracked `shouldBe` False
      DB.stampReadsRecordedSince
      fresh <- DB.getGcCutoff (fromDays @Int 90) (fromDays @Int 7)
      liftIO $ DB.gcCutoffWarmedUp fresh `shouldBe` False
      void
        $ DB.pgExec
          [pgSQL|
        UPDATE cache_gc_state SET reads_recorded_since = now() - interval '8 days'
          |]
      warm <- DB.getGcCutoff (fromDays @Int 90) (fromDays @Int 7)
      liftIO $ DB.gcCutoffWarmedUp warm `shouldBe` True
      void $ DB.pgExec [pgSQL| UPDATE cache_gc_state SET reads_recorded_since = NULL |]

    it "holds the collector lease against a second host" $ do
      void $ DB.pgExec [pgSQL| UPDATE cache_gc_state SET lock_owner = NULL, lock_expires_at = NULL |]
      DB.acquireGcLease "host-a" (fromHours @Int 6) `shouldReturnM` True
      DB.acquireGcLease "host-b" (fromHours @Int 6) `shouldReturnM` False
      DB.releaseGcLease "host-a"
      DB.acquireGcLease "host-b" (fromHours @Int 6) `shouldReturnM` True
      DB.releaseGcLease "host-b"

  context "getIncrementalTarget" $ inM $ beforeM_ truncateDBM $ do
    it "returns nothing if no matching commit exists" $ do
      now <- liftIO getCurrentTime
      baseBuild <- testBuild identity
      void $ testBuild ((gitCommit .~ "aaaa") . (endTime ?~ now))
      void $ testBuild ((gitCommit .~ "bbbb") . (endTime ?~ now))
      DB.getIncrementalTarget baseBuild ["cccc", "dddd"] `shouldReturnM` []

    it "returns the matching commit if one exists" $ do
      now <- liftIO getCurrentTime
      baseBuild <- testBuild identity
      build <- testBuild ((gitCommit .~ "aaaa") . (endTime ?~ now))
      DB.getIncrementalTarget baseBuild ["aaaa"] `shouldReturnM` [build]

    it "returns the first one in the argument list if multiple match" $ do
      now <- liftIO getCurrentTime
      baseBuild <- testBuild identity
      build <- testBuild ((gitCommit .~ "aaaa") . (endTime ?~ now))
      _ <- testBuild ((gitCommit .~ "bbbb") . (endTime ?~ now))
      DB.getIncrementalTarget baseBuild ["aaaa", "bbbb"] `shouldReturnM` [build]

    it "does not return builds from a commit for which not all builds have finished" $ do
      now <- liftIO getCurrentTime
      baseBuild <- testBuild identity
      _ <- testBuild (gitCommit .~ "aaaa")
      _ <- testBuild ((gitCommit .~ "aaaa") . (package .~ "blah") . (endTime ?~ now))
      build <- testBuild ((gitCommit .~ "bbbb") . (endTime ?~ now))
      DB.getIncrementalTarget baseBuild ["aaaa", "bbbb"] `shouldReturnM` [build]

    it "returns all the builds for a given commit ignoring duplicates" $ do
      now <- liftIO getCurrentTime
      baseBuild <- testBuild identity
      build1 <- testBuild ((gitCommit .~ "aaaa") . (endTime ?~ now) . (package .~ "foo"))
      _ <- testBuild ((gitCommit .~ "aaaa") . (endTime ?~ now) . (package .~ "foo"))
      build2 <- testBuild ((gitCommit .~ "aaaa") . (endTime ?~ now) . (package .~ "bar"))
      res <- DB.getIncrementalTarget baseBuild ["aaaa", "bbbb"]
      sort (res ^.. traverse . package) `shouldBeM` sort ([build1, build2] ^.. traverse . package)

    it "does not return builds from a different repo even if the commit is the same" $ do
      now <- liftIO getCurrentTime
      build <- testBuild ((gitCommit .~ "aaaa") . (endTime ?~ now))
      DB.getIncrementalTarget (build & repoName .~ "somethingelse") ["aaaa"] `shouldReturnM` []

  let wrap test = do
        socketPath <- getEnv "TPG_SOCK"
        user <- getEnv "TPG_USER"
        withEnvironment [("TPG_SOCK", socketPath), ("TPG_USER", user)] $ do
          hSilence [stderr] test

  describe "keepUnverifiedFods" $ inM $ beforeM_ truncateDBM $ do
    it "removes verified FODs from the input list" $ do
      let verifiedDrvPath = DrvPath (StorePath (StoreHash "hash1") "verified")
          unverifiedDrvPath = DrvPath (StorePath (StoreHash "hash2") "unverified")
      DB.addVerifiedFod verifiedDrvPath (StorePath (StoreHash "hash3") "foo")
      DB.keepUnverifiedFods (Set.fromList [(verifiedDrvPath, ()), (unverifiedDrvPath, ())])
        `shouldReturnM` Set.fromList [(unverifiedDrvPath, ())]

  describe "getDBConnection" $ around_ wrap $ do
    let correctPassword = "garnix"
    let testConnection c = do
          i <- PSQL.pgQuery c [pgSQL| SELECT 1 |]
          i `shouldBe` [Just (1 :: Int32)]

    it "connects with the correct password" $ do
      c <- DB.getDBConnection [correctPassword]
      testConnection c

    it "tries multiple passwords" $ do
      c <- DB.getDBConnection ["foo", correctPassword]
      testConnection c

    it "fails with wrong passwords" $ do
      DB.getDBConnection ["foo", "bar"] `shouldThrow` (\(e :: PGError) -> "password authentication failed" `isInfixOf` cs (show e))
      pure ()
