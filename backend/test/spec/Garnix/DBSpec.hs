module Garnix.DBSpec (spec) where

import Control.Concurrent.Async.Lifted (replicateConcurrently)
import Control.Exception qualified as E
import Control.Monad.Trans.Control (liftBaseDiscard)
import Data.Aeson.Lens (key, _String)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed qualified as PSQL
import Database.PostgreSQL.Typed.Protocol (pgBegin, pgRollback, pgSimpleQueries_)
import Database.PostgreSQL.Typed.TH (getTPGDatabase)
import Garnix.API.Builds (getBuild')
import Garnix.API.Commits (GetCommit (..), getSingleCommit)
import Garnix.API.Keys (getActionPublicKey, getRepoPublicKey)
import Garnix.API.Runs (toRunSummary)
import Garnix.Access (administeredForges, canCancelBuild, hasAccessToRepo)
import Garnix.DB qualified as DB
import Garnix.Duration (fromDays, fromHours, fromMinutes, fromSeconds)
import Garnix.Monad (Forge (..), ForgeInstance (..), M, githubForgeInstance, throw)
import Garnix.Nix.Types (DrvPath (..), StoreHash (..), StorePath (..))
import Garnix.Prelude
import Garnix.TestHelpers (soleForgeLogin, soleLogin, testBuild, truncateDBM)
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
          (ForgeLogin githubForge (GhLogin "user"))
          (Email "foo@x.com")
      let go =
            DB.newBuildDB
              ( CommitInfo
                  (ForgeLogin githubForge (user ^. soleLogin))
                  (RepoIsPublic True)
                  ( RepoInfo
                      undefined
                      undefined
                      (RepoId githubForge (GhRepoOwner $ GhLogin "foo") (GhRepoName "bar"))
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
              INSERT INTO server_heartbeat
                (hostname, last_heartbeat)
                VALUES ('test', NOW())
            |]
        throw $ OtherError "testing"
      hb <- DB.getRecentServerHeartbeats
      liftIO $ hb `shouldBe` []

    it "rolls transactions back due to SQL errors" $ do
      (void . liftBaseDiscard (E.try @PGError) . DB.pgTransaction) $ do
        void
          $ DB.pgQuery
            [pgSQL|
        INSERT INTO server_heartbeat
          (hostname, last_heartbeat)
          VALUES ('test', NOW())
          |]
        -- No such account: the foreign key refuses the identity.
        void
          $ DB.pgQuery
            [pgSQL|
              INSERT INTO forge_identities (user_id, forge, login)
                VALUES (-1, 'github', 'conflict')
            |]
      hb <- DB.getRecentServerHeartbeats
      liftIO $ hb `shouldBe` []

  context "heartbeat reporting" $ inM $ beforeM_ resetHeartbeatReporting $ do
    let window = fromHours @Int 12
        gap = fromMinutes @Int 5
        covered = DB.heartbeatsCoverWindow window gap

    it "claims no coverage before the gateway has reported at all" $ do
      covered `shouldReturnM` False

    it "claims no coverage from a report that only just arrived" $ do
      DB.recordHeartbeatReport gap
      covered `shouldReturnM` False

    it "claims coverage once reporting has run for the whole window" $ do
      DB.recordHeartbeatReport gap
      backdateReportingStart
      covered `shouldReturnM` True

    it "drops coverage when the gateway goes quiet" $ do
      DB.recordHeartbeatReport gap
      backdateReportingStart
      backdateLastReport
      covered `shouldReturnM` False

    it "restarts the window when reporting resumes after a gap" $ do
      DB.recordHeartbeatReport gap
      backdateReportingStart
      backdateLastReport
      DB.recordHeartbeatReport gap
      covered `shouldReturnM` False

  context "getUserInternalToken" $ inM $ beforeM_ truncateDBM $ do
    it "gets the same token when called by multiple threads concurrently" $ do
      results <- replicateConcurrently 50 (DB.getUserInternalToken $ ForgeLogin githubForge (GhLogin "user"))
      case results of
        [] -> liftIO $ expectationFailure "expected 50 tokens, got none"
        firstResult : _ -> liftIO $ results `shouldBe` replicate 50 firstResult

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

    it "ignores an unfinished build of the same commit in a mirror on another forge" $ do
      now <- liftIO getCurrentTime
      baseBuild <- testBuild (forge .~ otherForge)
      build <- testBuild ((forge .~ otherForge) . (gitCommit .~ "aaaa") . (endTime ?~ now))
      _ <- testBuild (gitCommit .~ "aaaa")
      DB.getIncrementalTarget baseBuild ["aaaa"] `shouldReturnM` [build]

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

  describe "repositories on different forges" $ inM $ beforeM_ truncateDBM $ do
    -- The same owner and name on two forges, under a fresh name each run so
    -- that rows left behind in tables truncateDBM does not clear never match.
    let freshRepos :: M (RepoId, RepoId)
        freshRepos = do
          suffix <- randomBase64 8
          let owner = GhRepoOwner (GhLogin ("acme-" <> suffix))
          pure (RepoId githubForge owner "site", RepoId otherForge owner "site")

    it "keeps separate builds" $ do
      (onGithub, onOther) <- freshRepos
      githubBuild <- DB.newBuildDB (commitOn onGithub) packageInfo "garnix-server-test" False
      otherBuild <- DB.newBuildDB (commitOn onOther) packageInfo "garnix-server-test" False
      (githubBuild ^. forge) `shouldBeM` githubForge
      (otherBuild ^. forge) `shouldBeM` otherForge
      (map (^. id) <$> DB.getBuildsByCommit onGithub sharedCommit) `shouldReturnM` [githubBuild ^. id]
      (map (^. id) <$> DB.getBuildsByCommit onOther sharedCommit) `shouldReturnM` [otherBuild ^. id]
      (buildRepoId <$> DB.getBuild (otherBuild ^. id)) `shouldReturnM` onOther

    it "keeps separate commits" $ do
      (onGithub, onOther) <- freshRepos
      DB.newCommit onGithub sharedCommit
      DB.newCommit onOther sharedCommit
      DB.setCommitStatus onGithub sharedCommit Evaluated
      (fmap (^. status) <$> DB.getCommit onGithub sharedCommit) `shouldReturnM` Just Evaluated
      (fmap (^. status) <$> DB.getCommit onOther sharedCommit) `shouldReturnM` Just Evaluating
      (fmap commitRepoId <$> DB.getCommit onOther sharedCommit) `shouldReturnM` Just onOther

    it "keeps separate pushes" $ do
      (onGithub, onOther) <- freshRepos
      DB.registerPush onGithub sharedCommit "main" `shouldReturnM` DB.NewPush
      DB.registerPush onOther sharedCommit "main" `shouldReturnM` DB.NewPush
      DB.registerPush onOther sharedCommit "main" `shouldReturnM` DB.AlreadyPushed

    it "keeps separate deployer and action keys" $ do
      (onGithub, onOther) <- freshRepos
      githubKey <- getRepoPublicKey onGithub
      otherKey <- getRepoPublicKey onOther
      otherKey `shouldNotBeM` githubKey
      getRepoPublicKey onGithub `shouldReturnM` githubKey
      getRepoPublicKey onOther `shouldReturnM` otherKey
      githubActionKey <- getActionPublicKey onGithub "deploy"
      otherActionKey <- getActionPublicKey onOther "deploy"
      otherActionKey `shouldNotBeM` githubActionKey
      getActionPublicKey onOther "deploy" `shouldReturnM` otherActionKey

    it "tags cached store paths with the forge" $ do
      (onGithub, onOther) <- freshRepos
      let hash = StoreHash "00000000000000000000000000forge0"
      DB.tagCacheUploadForS3Cache onGithub hash
      DB.tagCacheUploadForS3Cache onOther hash
      (Set.fromList <$> DB.getReposForHash hash) `shouldReturnM` Set.fromList [onGithub, onOther]

    it "summarises a commit with the forge it was built on" $ do
      (_, onOther) <- freshRepos
      void $ DB.newBuildDB (commitOn onOther) packageInfo "garnix-server-test" False
      (map commitSummaryRepoId <$> DB.getCommitSummaries sharedCommit) `shouldReturnM` [onOther]

    it "summarises a commit built on two forges once per repository" $ do
      (onGithub, onOther) <- freshRepos
      let buildOn repo pkg status' publicity =
            testBuild
              $ (forge .~ (repo ^. forge))
              . (repoUser .~ (repo ^. repoUser))
              . (repoName .~ (repo ^. repoName))
              . (gitCommit .~ sharedCommit)
              . (package .~ pkg)
              . (status ?~ status')
              . (repoIsPublic .~ RepoIsPublic publicity)
      void $ buildOn onGithub "a" Failure False
      void $ buildOn onGithub "b" Failure False
      void $ buildOn onOther "a" Success True
      let counts s = (commitSummaryRepoId s, s ^. succeeded, s ^. failed)
      (Set.fromList . map counts <$> DB.getCommitSummaries sharedCommit)
        `shouldReturnM` Set.fromList [(onGithub, 0, 2), (onOther, 1, 0)]
      -- Anonymous visitors only see the public mirror, never the private
      -- repository's builds merged into it.
      GetCommit summary _ _ <- getSingleCommit Nothing sharedCommit
      counts summary `shouldBeM` (onOther, 1, 0)

    it "names the forge in build, run and commit responses" $ do
      (_, onOther) <- freshRepos
      let forgeField :: (ToJSON a) => a -> Maybe Text
          forgeField = (^? key "forge" . _String) . toJSON
      build <- DB.newBuildDB (commitOn onOther) packageInfo "garnix-server-test" False
      run <- DB.newRun "check" (commitOn onOther)
      forgeField build `shouldBeM` Just "git.example"
      (forgeField <$> getBuild' Nothing (build ^. id)) `shouldReturnM` Just "git.example"
      forgeField (toRunSummary run) `shouldBeM` Just "git.example"
      (map forgeField <$> DB.getCommitSummaries sharedCommit) `shouldReturnM` [Just "git.example"]

  describe "reverting forge_qualified_keys" $ inM $ beforeM_ truncateDBM $ do
    let readRevert = liftIO $ T.lines . cs <$> readFile "../sql/revert/forge_qualified_keys.sql"
        forgeTables =
          catMaybes
            <$> DB.pgQuery
              [pgSQL|
                SELECT table_name::text FROM information_schema.columns
                  WHERE table_schema = 'public' AND column_name = 'forge'
                  ORDER BY table_name
              |]

    it "checks every table that has a forge column" $ do
      script <- T.unlines <$> readRevert
      tables <- forgeTables
      length tables `shouldBeM` 14
      filter (\t -> not (("FROM " <> t <> " WHERE forge <> 'github'") `T.isInfixOf` script)) tables
        `shouldBeM` []

    it "refuses while any row belongs to a forge other than github" $ do
      -- Without the refusal, dropping the column would hand this Gitea admin
      -- to whoever owns the github login "mallory".
      mallory <- DB.newUser (ForgeLogin otherForge "mallory") (Email "mallory@git.example")
      script <- readRevert
      others <- filter (/= "forge_identities") <$> forgeTables
      -- The script commits on its own. Run its body in a transaction that is
      -- always rolled back, so a weakened guard fails this spec instead of
      -- reverting the test database. Inside it, empty the other tables, which
      -- the truncation between specs leaves alone, so that mallory is the one
      -- row the guard can see. Asserts are off, as a server may set.
      let (transactionLines, body) = partition (`elem` ["BEGIN;", "COMMIT;"]) script
          setup =
            [ "SET LOCAL plpgsql.check_asserts = off;",
              "TRUNCATE " <> T.intercalate ", " others <> " CASCADE;"
            ]
      liftIO $ transactionLines `shouldBe` ["BEGIN;", "COMMIT;"]
      result <- liftIO $ E.bracket (pgConnect =<< getTPGDatabase) pgDisconnect $ \conn ->
        E.bracket_ (pgBegin conn) (pgRollback conn)
          $ E.try (pgSimpleQueries_ conn (cs (T.unlines (setup <> body))))
      case result of
        Left (err :: PGError) ->
          liftIO $ show err `shouldSatisfy` T.isInfixOf "rows from forges other than github exist"
        Right () -> liftIO $ expectationFailure "the revert ran although a git.example account exists"
      DB.getUser (ForgeLogin otherForge "mallory") `shouldReturnM` mallory

  describe "identityClash" $ do
    let self = UserId 1
        other = UserId 2
    it "names another account holding the identity" $ do
      DB.identityClash self (Just other) Nothing `shouldBe` DB.OwnedBy other
      DB.identityClash self (Just other) (Just "carol") `shouldBe` DB.OwnedBy other
    it "names the login the account holds on the forge" $ do
      DB.identityClash self Nothing (Just "carol") `shouldBe` DB.AlreadyOnForge "carol"
      DB.identityClash self (Just self) (Just "carol") `shouldBe` DB.AlreadyOnForge "carol"
    it "is a race when neither holds any more" $ do
      DB.identityClash self Nothing Nothing `shouldBe` DB.Raced
      DB.identityClash self (Just self) Nothing `shouldBe` DB.Raced

  describe "removing an identity" $ do
    let onGithub = ForgeIdentity githubForge "alice" False
        onOther = ForgeIdentity otherForge "carol" False
    it "finds the account's identity on the forge" $ do
      DB.identityToRemove otherForge [onGithub, onOther] `shouldBe` Right onOther
    it "refuses a forge the account has no identity on" $ do
      DB.identityToRemove otherForge [onGithub] `shouldBe` Left DB.NoSuchIdentity
    it "removes an identity the account has more than one of" $ do
      DB.mayRemove DB.KeepModuleSettings 2 False onOther `shouldBe` Right onOther
    it "never removes the last identity" $ do
      DB.mayRemove DB.DeleteModuleSettings 1 False onGithub `shouldBe` Left DB.LastIdentity
    it "deletes module settings only once confirmed" $ do
      DB.mayRemove DB.KeepModuleSettings 2 True onGithub `shouldBe` Left DB.HasModuleSettings
      DB.mayRemove DB.DeleteModuleSettings 2 True onGithub `shouldBe` Right onGithub

  describe "users on different forges" $ inM $ beforeM_ truncateDBM $ do
    it "may share an email, and the later account learns it was in use" $ do
      Right (onGithub, firstUsed) <- DB.createAccount (ForgeIdentity githubForge "alice" False) (Email "alice@example.com")
      firstUsed `shouldBeM` DB.EmailAlreadyUsed False
      Right (onOther, againUsed) <- DB.createAccount (ForgeIdentity otherForge "alice" False) (Email "Alice@Example.com")
      againUsed `shouldBeM` DB.EmailAlreadyUsed True
      (onOther ^. id == onGithub ^. id) `shouldBeM` False
      DB.getUser (ForgeLogin githubForge "alice") `shouldReturnM` onGithub
      DB.getUser (ForgeLogin otherForge "alice") `shouldReturnM` onOther

    it "refuses an identity of another account, or a second one on a forge, saying which" $ do
      alice <- DB.newUser (ForgeLogin githubForge "alice") (Email "alice@example.com")
      bob <- DB.newUser (ForgeLogin otherForge "bob") (Email "bob@example.com")
      let conflictOf action =
            try action >>= \case
              Left ErrorWithContext {err = IdentityConflict {message}} -> pure (Just message)
              _ -> pure Nothing
      conflictOf (DB.addIdentity (alice ^. id) (ForgeIdentity otherForge "bob" False))
        `shouldReturnM` Just "The git.example identity bob already belongs to another garnix account."
      conflictOf (DB.addIdentity (bob ^. id) (ForgeIdentity otherForge "carol" False))
        `shouldReturnM` Just "Your account is already connected to git.example as bob. Disconnect it first."

    it "creates no account for an identity another account took meanwhile" $ do
      alice <- DB.newUser (ForgeLogin otherForge "alice") (Email "alice@example.com")
      before <- DB.pgQuery [pgSQL| SELECT count(*) FROM users |]
      result <- DB.createAccount (ForgeIdentity otherForge "alice" False) (Email "alice@example.com")
      either Just (const Nothing) result `shouldBeM` Just (DB.OwnedBy (alice ^. id))
      DB.pgQuery [pgSQL| SELECT count(*) FROM users |] `shouldReturnM` (before :: [Maybe Int64])

    describe "administering repositories" $ do
      -- Forges that know no repository, so access falls through to the
      -- collaborator list, which is empty. git.example lists "root" as admin;
      -- github lists nobody.
      let repoLess slug' admins' =
            let base = githubForgeInstance "other-webhook-secret" "other-client-id" "other-client-secret" Nothing
             in base
                  { _forgeInstanceConfig = (_forgeInstanceConfig base) {_forgeConfigSlug = slug', _forgeConfigAdmins = admins'},
                    _forgeInstanceForge = (_forgeInstanceForge base) {_forgeResolveCredentials = \_ -> pure Nothing}
                  }
          withForges =
            local
              $ (#forges %~ Map.insert otherForge (repoLess otherForge ["root"]))
              . (#forges %~ Map.insert githubForge (repoLess githubForge []))
          private = RepoIsPublic False
          otherRepo = RepoId otherForge "acme" "site"
          githubRepo = RepoId githubForge "acme" "site"

      it "comes with a login listed in the forge's config, on that forge only" $ withForges $ do
        githubRoot <- DB.newUser (ForgeLogin githubForge "root") (Email "root@github.example")
        otherRoot <- DB.newUser (ForgeLogin otherForge "root") (Email "root@other.example")
        canCancelBuild (Just githubRoot) private "someone" otherRepo `shouldReturnM` False
        hasAccessToRepo (Just githubRoot) private otherRepo `shouldReturnM` False
        canCancelBuild (Just otherRoot) private "someone" otherRepo `shouldReturnM` True
        hasAccessToRepo (Just otherRoot) private otherRepo `shouldReturnM` True
        canCancelBuild (Just otherRoot) private "someone" githubRepo `shouldReturnM` False
        hasAccessToRepo (Just otherRoot) private githubRepo `shouldReturnM` False
        drv <- ("/nix/store/" <>) . cs <$> randomBase64 8
        build <-
          testBuild
            $ (forge .~ otherForge)
            . (repoIsPublic .~ private)
            . (drvPath ?~ drv)
        let originalBuild user = do
              adminForges <- administeredForges (Just user)
              fmap _originalBuildId <$> DB.getOriginalBuildForDrvPath (Just user) adminForges drv
        originalBuild githubRoot `shouldReturnM` Nothing
        originalBuild otherRoot `shouldReturnM` Just (build ^. id)

      it "follows the account to a forge it connects" $ withForges $ do
        user <- DB.newUser (ForgeLogin githubForge "alice") (Email "alice@example.com")
        hasAccessToRepo (Just user) private otherRepo `shouldReturnM` False
        DB.addIdentity (user ^. id) (ForgeIdentity otherForge "root" False)
        Just connected <- DB.getUserById (user ^. id)
        hasAccessToRepo (Just connected) private otherRepo `shouldReturnM` True
        hasAccessToRepo (Just connected) private githubRepo `shouldReturnM` False

      it "is nobody's on a forge no longer configured" $ withForges $ do
        let gone = ForgeSlug "gone.example"
            goneRepo = RepoId gone "acme" "site"
        user <- DB.newUser (ForgeLogin gone "root") (Email "root@gone.example")
        canCancelBuild (Just user) private "someone" goneRepo `shouldReturnM` False
        hasAccessToRepo (Just user) private goneRepo `shouldReturnM` False

      it "is not granted by what github says about its own staff" $ withForges $ do
        Right (staff, _) <- DB.createAccount (ForgeIdentity githubForge "staff" True) (Email "staff@github.example")
        (staff ^.. identities . traverse . isForgeAdmin) `shouldBeM` [True]
        canCancelBuild (Just staff) private "someone" githubRepo `shouldReturnM` False
        hasAccessToRepo (Just staff) private githubRepo `shouldReturnM` False
        administeredForges (Just staff) `shouldReturnM` []

    it "are distinct accounts" $ do
      onGithub <- DB.newUser (ForgeLogin githubForge "alice") (Email "alice@github.example")
      onOther <- DB.newUser (ForgeLogin otherForge "alice") (Email "alice@other.example")
      (onOther ^. id) `shouldNotBeM` (onGithub ^. id)
      soleForgeLogin onOther `shouldBeM` ForgeLogin otherForge "alice"
      DB.getUser (ForgeLogin githubForge "alice") `shouldReturnM` onGithub
      DB.getUser (ForgeLogin otherForge "alice") `shouldReturnM` onOther
      DB.getUserId (ForgeLogin otherForge "alice") `shouldReturnM` (onOther ^. id)

    it "get their own internal cache tokens" $ do
      githubToken <- DB.getUserInternalToken (ForgeLogin githubForge "alice")
      otherToken <- DB.getUserInternalToken (ForgeLogin otherForge "alice")
      getInternalCacheToken otherToken `shouldNotBeM` getInternalCacheToken githubToken

    it "only see their own builds" $ do
      onOther <- DB.newUser (ForgeLogin otherForge "alice") (Email "alice@other.example")
      void
        $ DB.newBuildDB
          (commitOn (RepoId githubForge "alice" "site") & reqUser .~ ForgeLogin githubForge "alice")
          packageInfo
          "garnix-server-test"
          False
      DB.getBuilds onOther `shouldReturnM` []

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

shouldNotBeM :: (HasCallStack, Show a, Eq a) => a -> a -> M ()
shouldNotBeM a b = liftIO $ a `shouldNotBe` b

otherForge :: ForgeSlug
otherForge = ForgeSlug "git.example"

sharedCommit :: CommitHash
sharedCommit = "c0ffee"

packageInfo :: PackageInfo
packageInfo = PackageInfo TypePackage (IsSystem X8664Linux) (PackageName "site")

commitOn :: RepoId -> CommitInfo
commitOn repo =
  CommitInfo
    (ForgeLogin (repo ^. forge) "someone")
    (RepoIsPublic True)
    (RepoInfo undefined undefined repo)
    (Just "main")
    Nothing
    sharedCommit

resetHeartbeatReporting :: M ()
resetHeartbeatReporting =
  void
    $ DB.pgExec
      [pgSQL|
        UPDATE heartbeat_reporting
        SET reports_recorded_since = NULL, last_report_at = NULL
        WHERE id
      |]

backdateReportingStart :: M ()
backdateReportingStart =
  void
    $ DB.pgExec
      [pgSQL|
        UPDATE heartbeat_reporting
        SET reports_recorded_since = NOW() - interval '13 hours'
        WHERE id
      |]

backdateLastReport :: M ()
backdateLastReport =
  void
    $ DB.pgExec
      [pgSQL|
        UPDATE heartbeat_reporting
        SET last_report_at = NOW() - interval '1 hour'
        WHERE id
      |]
