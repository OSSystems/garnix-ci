module Garnix.DB where

import Control.Exception.Safe qualified
import Control.Exception.Safe qualified as Safe
import Control.Lens
import Control.Monad.Trans.Control (liftBaseOp_)
import Data.ByteString qualified
import Data.Map qualified as Map
import Data.Map.Strict (Map, fromList)
import Data.Pool (withResource)
import Data.Set qualified as Set
import Data.Text.IO (hPutStrLn)
import Database.PostgreSQL.Typed (PGDatabase (pgDBPass), pgConnect, pgSQL)
import Database.PostgreSQL.Typed qualified as PSQL
import Database.PostgreSQL.Typed.Array ()
import Database.PostgreSQL.Typed.Protocol qualified as PSQLP
import Database.PostgreSQL.Typed.Query (PGQuery, getQueryString)
import Database.PostgreSQL.Typed.TH (getTPGDatabase)
import Database.PostgreSQL.Typed.Types (unknownPGTypeEnv)
import Garnix.AccessToken.Types
import Garnix.Duration
import Garnix.Monad
import Garnix.Monad.Metrics (incrementEvent, timingAs)
import Garnix.Nix.Types as Nix
import Garnix.Password
import Garnix.Prelude
import Garnix.Types

getUser :: GhLogin -> M User
getUser ghLogin = do
  res <-
    pgQuery
      [pgSQL|
    SELECT
      id,
      email,
      subscription_type,
      created_at
    FROM users
    WHERE github_login = ${ghLogin}
  |]
  case res of
    [] -> throw $ NoSuchUser ghLogin
    [(id', email', sub', cre')] -> pure $ User id' ghLogin email' sub' cre'
    _ -> throw $ OtherError "Got more than 1 user from getUser"

getUserId :: GhLogin -> M UserId
getUserId ghLogin = do
  res <- pgQuery [pgSQL| SELECT id FROM users WHERE github_login = ${ghLogin} |]
  case res of
    [] -> throw $ NoSuchUser ghLogin
    [id] -> pure $ UserId id
    _ -> throw $ OtherError "Got more than 1 user from getUserId"

newUser :: GhLogin -> Email -> SubscriptionType -> Bool -> M User
newUser ghLogin email' sub agreeToEmails' = do
  r <-
    pgQuery
      [pgSQL|
    INSERT INTO users
      ( github_login,
        email,
        subscription_type,
        agree_to_emails
      )
    VALUES
      ( ${ghLogin},
        ${email'},
        ${sub},
        ${agreeToEmails'}
      )
    ON CONFLICT DO NOTHING
    RETURNING id, created_at
  |]
  case r of
    [(id', cre')] ->
      pure
        $ User
          { _userId = id',
            _userGithubLogin = ghLogin,
            _userEmail = email',
            _userSubscriptionType = sub,
            _userCreatedAt = cre'
          }
    [] -> throw $ UserAlreadyExists ghLogin
    _ -> throw $ OtherError "impossible: more than two users created"

getRepoConfig :: GhRepoOwner -> GhRepoName -> M RepoConfig
getRepoConfig repoOwner repoName = do
  repoConfig <-
    map (\(skipInputChecks, evalMemory) -> RepoConfig skipInputChecks (fromMaybe (defaultRepoConfig ^. maxEvalMemory) evalMemory))
      <$> pgQuery
        [pgSQL|
          SELECT
            skip_private_inputs_check_for_collaborators,
            max_eval_memory
          FROM repo_config
          WHERE repo_user = ${repoOwner}
            AND repo_name = ${repoName}
        |]
  case repoConfig of
    [] -> pure defaultRepoConfig
    [res] -> pure res
    _ -> throw $ OtherError "impossible: multiple entries for repo config"

getBuild :: BuildId -> M Build
getBuild buildId = do
  res <-
    pgQueryPrism
      _Build
      [pgSQL|
    SELECT
      id,
      repo_user,
      repo_name,
      pr_from_fork,
      branch,
      repo_is_public,
      git_commit,
      package,
      package_type,
      system,
      req_user,
      status,
      start_time,
      end_time,
      drv_path,
      output_paths,
      github_run_id,
      persistence_name,
      wants_incrementalism,
      eval_host,
      uploaded_to_cache,
      already_built
    FROM builds
    WHERE id = ${buildId}
  |]
  case res of
    [r] -> pure r
    [] -> throw $ NoSuchBuild buildId
    _ -> throw $ OtherError "Impossible: more than one result"

getOriginalBuildForDrvPath :: Maybe User -> FilePath -> M (Maybe OriginalBuild)
getOriginalBuildForDrvPath user drvPath = do
  let mghLogin = user ^? _Just . githubLogin
      isAdmin = user ^? _Just . subscriptionType == Just Admin
  res <-
    map (\(id, commit, status) -> OriginalBuild id commit status)
      <$> pgQuery
        [pgSQL|
        SELECT
          id,
          git_commit,
          status
        FROM builds
        WHERE (
          repo_is_public
            OR ${isAdmin}
            OR (${mghLogin}::text IS NOT NULL AND req_user = ${mghLogin})
        )
        AND already_built = false
        AND drv_path = ${drvPath}
        ORDER BY end_time DESC
        LIMIT 1
      |]
  case res of
    [r] -> pure $ Just r
    [] -> pure Nothing
    _ -> throw $ OtherError "Impossible: more than one result"

makeNewBuildForGithubRunId :: GhLogin -> GhRunId -> Text -> M Build
makeNewBuildForGithubRunId reqUser ghRunId evalHost = do
  now <- liftIO getCurrentTime
  res <-
    pgQueryPrism
      _Build
      [pgSQL|
    INSERT INTO builds
      ( repo_user,
        repo_name,
        pr_from_fork,
        branch,
        repo_is_public,
        git_commit,
        package,
        package_type,
        system,
        req_user,
        status,
        start_time,
        end_time,
        drv_path,
        output_paths,
        github_run_id,
        persistence_name,
        wants_incrementalism,
        eval_host,
        uploaded_to_cache)
      SELECT
        repo_user,
        repo_name,
        pr_from_fork,
        branch,
        repo_is_public,
        git_commit,
        package,
        package_type,
        system,
        ${reqUser},
        NULL,
        ${now},
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        wants_incrementalism,
        ${evalHost},
        FALSE
      FROM builds
      WHERE github_run_id = ${ghRunId}
    RETURNING
      id,
      repo_user,
      repo_name,
      pr_from_fork,
      branch,
      repo_is_public,
      git_commit,
      package,
      package_type,
      system,
      req_user,
      status,
      start_time,
      end_time,
      drv_path,
      output_paths,
      github_run_id,
      persistence_name,
      wants_incrementalism,
      eval_host,
      uploaded_to_cache,
      already_built
  |]
  case res of
    [r] -> pure r
    [] -> throw $ NoSuchBuildRunId ghRunId
    _ -> throw $ OtherError "Impossible: more than one result"

getLatestBuildsMatching :: RepoInfo -> CommitHash -> M [Build]
getLatestBuildsMatching repoInfo commit = do
  pgQueryPrism
    _Build
    [pgSQL|
    SELECT DISTINCT ON (repo_user, repo_name, git_commit, package, package_type, system)
      id,
      repo_user,
      repo_name,
      pr_from_fork,
      branch,
      repo_is_public,
      git_commit,
      package,
      package_type,
      system,
      req_user,
      status,
      start_time,
      end_time,
      drv_path,
      output_paths,
      github_run_id,
      persistence_name,
      wants_incrementalism,
      eval_host,
      uploaded_to_cache,
      already_built
    FROM builds
    WHERE repo_user = ${repoInfo ^. ghRepoOwner}
    AND repo_name = ${repoInfo ^. ghRepoName}
    AND git_commit = ${commit}
    ORDER BY
      repo_user, repo_name, git_commit, package, package_type, system,
      start_time DESC
  |]

getBuilds :: User -> M [Build]
getBuilds usr = do
  pgQueryPrism
    _Build
    [pgSQL|
    SELECT
      id,
      repo_user,
      repo_name,
      pr_from_fork,
      branch,
      repo_is_public,
      git_commit,
      package,
      package_type,
      system,
      req_user,
      status,
      start_time,
      end_time,
      drv_path,
      output_paths,
      github_run_id,
      persistence_name,
      wants_incrementalism,
      eval_host,
      uploaded_to_cache,
      already_built
    FROM builds
    WHERE req_user = ${usr ^. githubLogin}
    ORDER BY start_time DESC
    LIMIT 500
  |]

setBuildUploaded :: BuildId -> M ()
setBuildUploaded buildId = do
  void
    $ pgExec
      [pgSQL|
        UPDATE builds
        SET uploaded_to_cache = TRUE
        WHERE id = ${buildId}
      |]

getLatestBuildsForBranch :: GhRepoOwner -> GhRepoName -> Branch -> M [Build]
getLatestBuildsForBranch owner name branch = do
  pgQueryPrism
    _Build
    [pgSQL|
    SELECT
      id,
      repo_user,
      repo_name,
      pr_from_fork,
      branch,
      repo_is_public,
      git_commit,
      package,
      package_type,
      system,
      req_user,
      status,
      start_time,
      end_time,
      drv_path,
      output_paths,
      github_run_id,
      persistence_name,
      wants_incrementalism,
      eval_host,
      uploaded_to_cache,
      already_built
    FROM builds
      WHERE repo_user = ${owner}
        AND branch = ${branch}
        AND repo_name = ${name}
        AND git_commit = (
          SELECT git_commit
          FROM builds
          WHERE repo_user = ${owner}
            AND repo_name = ${name}
            AND branch = ${branch}
            AND package = 'Build starting'
          ORDER BY start_time DESC
          LIMIT 1
        )
      AND repo_is_public = TRUE
  |]

data RegisterPushResult = NewPush | AlreadyPushed
  deriving stock (Eq, Show, Generic)

registerPush :: GhRepoOwner -> GhRepoName -> CommitHash -> Branch -> M RegisterPushResult
registerPush repoOwner repoName commit branch = do
  pgQuery
    [pgSQL|
      INSERT INTO pushes
        (repo_user,
         repo_name,
         git_commit,
         branch
        )
      VALUES
        (${repoOwner},
         ${repoName},
         ${commit},
         ${branch}
        )
      ON CONFLICT DO NOTHING
      RETURNING True
    |]
    >>= \case
      [] -> pure AlreadyPushed
      [_ :: Maybe Bool] -> pure NewPush
      _ -> throw $ OtherError "Impossible: more than one result"

getCommitsByOwnerAndRepo :: GhRepoOwner -> GhRepoName -> M [CommitSummary]
getCommitsByOwnerAndRepo repoOwner repoName = do
  map
    ( \( repoOwner :: GhRepoOwner,
         repoName :: GhRepoName,
         gitCommit :: CommitHash,
         branch :: Maybe Branch,
         reqUser :: GhLogin,
         isPublic :: Bool,
         startTime :: UTCTime,
         succeeded :: Int64,
         failed :: Int64,
         pending :: Int64,
         cancelled :: Int64
         ) ->
          CommitSummary repoOwner repoName (RepoIsPublic isPublic) gitCommit branch reqUser startTime succeeded failed pending cancelled
    )
    <$> pgQuery
      [pgSQL|!
        SELECT
          (array_agg(repo_user))[1],
          (array_agg(repo_name))[1],
          git_commit,
          (array_agg(branch))[1],
          (array_agg(req_user))[1],
          (array_agg(repo_is_public))[1],
          min(start_time) as commit_start_time,
          COUNT(*) FILTER (WHERE status = 'success') as succeeded,
          COUNT(*) FILTER (WHERE status = 'failure' OR status = 'timeout') as failed,
          COUNT(*) FILTER (WHERE status IS NULL) as pending,
          COUNT(*) FILTER (WHERE status = 'cancelled') as pending
        FROM (
          SELECT DISTINCT ON (git_commit, package_type, system, package) * FROM builds
          WHERE repo_user = ${repoOwner}
            AND repo_name = ${repoName}
          ORDER BY git_commit, package_type, system, package, start_time DESC
        ) AS sub
        GROUP BY git_commit
        ORDER BY commit_start_time DESC
        LIMIT 100
      |]

getCommit :: GhRepoOwner -> GhRepoName -> CommitHash -> M (Maybe Commit)
getCommit owner name commit =
  pgQueryPrism
    _Commit
    [pgSQL|
      SELECT
        repo_user,
        repo_name,
        git_commit,
        status,
        meta_check
      FROM commits
      WHERE repo_user = ${owner}
        AND repo_name = ${name}
        AND git_commit = ${commit}
    |]
    >>= \case
      [r] -> pure $ Just r
      [] -> pure Nothing
      _ -> throw $ OtherError "Impossible: more than one result"

newCommit :: GhRepoOwner -> GhRepoName -> CommitHash -> M ()
newCommit owner name commit =
  void
    $ pgExec
      [pgSQL|
        INSERT INTO commits
          (repo_user, repo_name, git_commit, status, meta_check)
        VALUES
            (${owner}, ${name}, ${commit}, 'evaluating', 'pending')
        ON CONFLICT (repo_user, repo_name, git_commit) DO UPDATE
          SET meta_check = 'pending'
      |]

setCommitStatus :: GhRepoOwner -> GhRepoName -> CommitHash -> CommitStatus -> M ()
setCommitStatus owner name commit st =
  void
    $ pgExec
      [pgSQL|
        UPDATE commits
          SET status = ${st}
        WHERE repo_user = ${owner}
          AND repo_name = ${name}
          AND git_commit = ${commit}
      |]

data CheckStatusUpdate = CheckStatusUpdate
  { _checkStatusUpdateFrom :: CheckStatus,
    _checkStatusUpdateTo :: CheckStatus
  }

-- Note: be careful when changing this function.
--
-- One change you might consider doing is, removing the `from` argument and changing the WHERE
-- clause that references it to 'AND meta_check <> ${to}', and at first that may seem equivalent.
--
-- However, as the code is now, that results in a bug. Here's a scenario for two failed builds,
-- both being reran at roughly the same time:
--
-- buildA: completes successfully, gets all builds by commit (A: Success, B: Failure)
-- buildB: completes successfully, gets all builds by commit (A: Success, B: Success),
--         runs this function and sets the flag to 'CheckSuccess'
-- buildA: resumes and tries to set the check to fail
--
-- With the changed mentioned above, the check would go through these states: Pending -> Success -> Fail.
--
-- As it is, buildA's thread would still attempt to set the check to CheckFail, but only if its current
-- state is CheckPending, which is not. So no change will happen, which is what we want.
setMetaCheck :: GhRepoOwner -> GhRepoName -> CommitHash -> CheckStatusUpdate -> M Bool
setMetaCheck owner name commit (CheckStatusUpdate {_checkStatusUpdateFrom = from, _checkStatusUpdateTo = to}) = do
  if from == to
    then pure False
    else
      (== 1)
        <$> pgExec
          [pgSQL|
            UPDATE commits
              SET meta_check = ${to}
            WHERE repo_user = ${owner}
              AND repo_name = ${name}
              AND git_commit = ${commit}
              AND meta_check = ${from}
          |]

getBuildsAndRunsByCommit :: GhRepoOwner -> GhRepoName -> CommitHash -> M FullCommitState
getBuildsAndRunsByCommit repoOwner repoName commitHash = do
  mCommit <- getCommit repoOwner repoName commitHash
  case mCommit of
    Nothing -> pure CommitEvaluating
    Just commit -> case commit ^. status of
      Evaluating -> pure CommitEvaluating
      Evaluated -> do
        builds <- getBuildsByCommit repoOwner repoName commitHash
        runs <- getRuns repoOwner repoName commitHash
        pure $ CommitEvaluated commit builds runs

getBuildsByCommit :: GhRepoOwner -> GhRepoName -> CommitHash -> M [Build]
getBuildsByCommit repoOwner repoName commitHash = do
  pgQuery
    [pgSQL|
      SELECT DISTINCT ON (git_commit, package_type, system, package)
        id,
        repo_user,
        repo_name,
        pr_from_fork,
        branch,
        repo_is_public,
        git_commit,
        package,
        package_type,
        system,
        req_user,
        status,
        start_time,
        end_time,
        drv_path,
        output_paths,
        github_run_id,
        persistence_name,
        wants_incrementalism,
        eval_host,
        uploaded_to_cache,
        already_built
      FROM builds
      WHERE git_commit = ${commitHash}
            AND repo_user = ${repoOwner}
            AND repo_name = ${repoName}
      ORDER BY git_commit, package_type, system, package, start_time DESC
      LIMIT 1000
    |]
    <&> map
      ( \( id,
           repoUser,
           repoName,
           prFromFork,
           branch,
           repoIsPublic,
           gitCommit,
           package,
           packageType,
           system,
           reqUser,
           status,
           startTime,
           endTime,
           drvPath,
           outputPaths,
           githubRunId,
           persistenceName,
           wantsIncrementalism,
           evalHost,
           uploadedToCache,
           alreadyBuilt
           ) ->
            Build
              { _buildId = id,
                _buildRepoUser = repoUser,
                _buildRepoName = repoName,
                _buildPrFromFork = prFromFork,
                _buildBranch = branch,
                _buildRepoIsPublic = repoIsPublic,
                _buildGitCommit = gitCommit,
                _buildPackage = package,
                _buildPackageType = packageType,
                _buildSystem = system,
                _buildReqUser = reqUser,
                _buildStatus = status,
                _buildStartTime = startTime,
                _buildEndTime = endTime,
                _buildDrvPath = drvPath,
                _buildOutputPaths = outputPaths,
                _buildGithubRunId = githubRunId,
                _buildPersistenceName = persistenceName,
                _buildWantsIncrementalism = wantsIncrementalism,
                _buildEvalHost = evalHost,
                _buildUploadedToCache = uploadedToCache,
                _buildAlreadyBuilt = alreadyBuilt
              }
      )

getRuns :: GhRepoOwner -> GhRepoName -> CommitHash -> M [Run]
getRuns repoOwner repoName commitHash = do
  pgQuery
    [pgSQL|
      SELECT id, name, repo_user, repo_name, git_commit, branch, status, req_user, start_time, end_time
      FROM runs
      WHERE git_commit = ${commitHash}
        AND repo_user = ${repoOwner}
        AND repo_name = ${repoName}
    |]
    <&> map
      ( \(id, name, repoOwner, repoName, gitCommit, branch, status, reqUser, startTime, endTime) ->
          Run
            { _runId = id,
              _runName = name,
              _runRepoUser = repoOwner,
              _runRepoName = repoName,
              _runGitCommit = gitCommit,
              _runBranch = branch,
              _runStatus = status,
              _runReqUser = reqUser,
              _runStartTime = startTime,
              _runEndTime = endTime
            }
      )

getRun :: RunId -> M (Maybe Run)
getRun runId = do
  result <-
    pgQuery
      [pgSQL|
        SELECT id, name, repo_user, repo_name, git_commit, branch, status, req_user, start_time, end_time
        FROM runs
        WHERE id = ${runId}
      |]
      <&> map
        ( \(id, name, repoOwner, repoName, gitCommit, branch, status, reqUser, startTime, endTime) ->
            Run
              { _runId = id,
                _runName = name,
                _runRepoUser = repoOwner,
                _runRepoName = repoName,
                _runGitCommit = gitCommit,
                _runBranch = branch,
                _runStatus = status,
                _runReqUser = reqUser,
                _runStartTime = startTime,
                _runEndTime = endTime
              }
        )
  case result of
    [run] -> pure $ Just run
    [] -> pure Nothing
    _ -> throw $ OtherError "Impossible: more than one result"

setRunStatus :: RunId -> Maybe Status -> M ()
setRunStatus runId status =
  void
    $ pgExec
      [pgSQL|
        UPDATE runs
        SET status = ${status},
            end_time = NOW()
        WHERE id = ${runId}
      |]

newRun :: Text -> CommitInfo -> M Run
newRun name commitInfo = do
  let repoOwner = commitInfo ^. repoInfo . ghRepoOwner
  let repoName = commitInfo ^. repoInfo . ghRepoName
  let commitHash = commitInfo ^. commit
  let branch = commitInfo ^. Garnix.Types.branch
  let reqUser = commitInfo ^. Garnix.Types.reqUser
  result <-
    pgQuery
      [pgSQL|
        INSERT INTO runs
          (name, repo_user, repo_name, git_commit, branch, status, req_user)
        VALUES
          (${name}, ${repoOwner}, ${repoName}, ${commitHash}, ${branch}, NULL, ${reqUser})
        RETURNING
          id, name, repo_user, repo_name, git_commit, branch, status, req_user, start_time
      |]
      <&> map
        ( \(id, name, repoOwner, repoName, commitHash, branch, status, reqUser, startTime) ->
            Run
              { _runId = id,
                _runName = name,
                _runRepoUser = repoOwner,
                _runRepoName = repoName,
                _runGitCommit = commitHash,
                _runBranch = branch,
                _runStatus = status,
                _runReqUser = reqUser,
                _runStartTime = startTime,
                _runEndTime = Nothing
              }
        )
  case result of
    [x] -> pure x
    _ -> throw $ OtherError "newRun: Unexpected number of updates"

-- todo remove?
tagCacheUpload :: GhRepoOwner -> GhRepoName -> [StorePath] -> M ()
tagCacheUpload repoOwner repoName =
  \case
    [] -> pure ()
    storePaths -> do
      let hashes = getHash <$> storePaths
      void
        $ pgExec
          [pgSQL|
            INSERT INTO cache_store_hashes
              (hash) VALUES (UNNEST(${hashes}::text[]))
            ON CONFLICT (hash) DO UPDATE SET accessed_at = NOW()
          |]
      void
        $ pgExec
          [pgSQL|
            INSERT INTO cache_store_hash_tags
              (hash, repo_owner, repo_name)
              VALUES (UNNEST(${hashes}::text[]), ${repoOwner}, ${repoName})
              ON CONFLICT DO NOTHING
          |]

getReposForHash :: StoreHash -> M [(GhRepoOwner, GhRepoName)]
getReposForHash hash = do
  pgQuery
    [pgSQL|
      SELECT repo_owner, repo_name
      FROM cache_store_hash_tags
      WHERE hash = ${hash}
    |]

data S3CacheStoreHash = S3CacheStoreHash
  { hash :: StoreHash,
    packageName :: Text,
    narHash :: Text,
    narSize :: Int64,
    public :: Bool,
    sig :: Text,
    references :: Text,
    fileSize :: Int64,
    fileHash :: Text
  }
  deriving (Generic, Show)

finalizeS3CacheUpload :: S3CacheStoreHash -> M ()
finalizeS3CacheUpload s3CacheStoreHash = do
  let S3CacheStoreHash
        { hash,
          packageName,
          narHash,
          narSize,
          public,
          sig,
          references,
          fileSize,
          fileHash
        } = s3CacheStoreHash
  void
    $ pgExec
      [pgSQL|
        UPDATE cache_store_hashes
        SET
          accessed_at = NOW(),
          package_name = ${packageName},
          nar_hash = ${narHash},
          nar_size = ${narSize},
          public = ${public},
          sig = ${sig},
          "references" = ${references},
          file_size = ${fileSize},
          file_hash = ${fileHash},
          uploaded_at = NOW()
        WHERE hash = ${hash};
      |]

tagCacheUploadForS3Cache :: GhRepoOwner -> GhRepoName -> StoreHash -> M ()
tagCacheUploadForS3Cache repoOwner repoName hash = do
  void
    $ pgExec
      [pgSQL|
        INSERT INTO cache_store_hash_tags
          (hash, repo_owner, repo_name)
          VALUES (${hash}, ${repoOwner}, ${repoName})
          ON CONFLICT DO NOTHING
      |]

getS3CacheStoreHash :: StoreHash -> M (Maybe S3CacheStoreHash)
getS3CacheStoreHash hash = do
  result <-
    pgQuery
      [pgSQL|
        SELECT
          package_name,
          nar_hash,
          nar_size,
          public,
          sig,
          "references",
          file_size,
          file_hash
        FROM cache_store_hashes
        WHERE hash = ${hash}
          AND uploaded_at IS NOT NULL
      |]
      <&> catMaybes
        . fmap
          ( \case
              ( Just packageName,
                Just narHash,
                Just narSize,
                Just public,
                Just sig,
                Just references,
                Just fileSize,
                Just fileHash
                ) ->
                  Just
                    $ S3CacheStoreHash
                      { hash,
                        packageName,
                        narHash,
                        narSize,
                        public,
                        sig,
                        references,
                        fileSize,
                        fileHash
                      }
              _ -> Nothing
          )
  case result of
    [cacheStoreHash] -> pure $ Just cacheStoreHash
    [] -> pure Nothing
    _ -> throw $ OtherError "Impossible: more than one result"

-- | Figure out what store paths you'll upload.
--
-- The argument is what store paths you want in the cache. The returned value
-- are the ones that are now the caller's responsibility. This is tracked in
-- the DB, so no one else will try uploading.
claimS3CachedStorePaths :: [StorePath] -> M [StorePath]
claimS3CachedStorePaths (sort -> storePaths) = do
  let hashes = fmap getHash storePaths
  let packageNames = fmap getName storePaths
  filtered :: [(Maybe StoreHash, Maybe Text)] <-
    pgQuery
      -- There are five states possible in the DB:
      --  - No cache entry exists
      --  - Old-style (non-S3) cache entry exists
      --  - The cache entry has been claimed, but not yet uploaded, and was
      --    claimed more than 10 hours ago
      --  - The cache entry has been claimed, but not yet uploaded
      --  - The cache entry has been uploaded to S3 (uploaded_at is set)
      --
      --  If it's either of the first three cases, we want to claim it. We do
      --  this by making sure the INSERT RETURNING returns it, which happens
      --  when there's an update or insert.
      [pgSQL|
        INSERT INTO cache_store_hashes (hash, package_name)
          SELECT hash, package_name
            FROM UNNEST(${hashes}::text[], ${packageNames}::text[]) AS t(hash, package_name)
        ON CONFLICT (hash) DO UPDATE SET
          accessed_at = NOW(),
          package_name = EXCLUDED.package_name
        WHERE cache_store_hashes.package_name IS NULL
           OR (cache_store_hashes.uploaded_at IS NULL AND
               cache_store_hashes.created_at < now() - interval '10 hours')
        RETURNING
          hash, package_name;
      |]
  forM filtered $ \case
    (Just hash, Just packageName) -> pure $ StorePath hash packageName
    _ -> throw $ OtherError "impossible: hashes and packageNames have the same length"

-- * /api/account/tokens

getAccessTokensForUser :: UserId -> M [AccessTokenMetadata]
getAccessTokensForUser userId = do
  map
    ( \(id, name, created_at, last_used, scope_cache, scope_api) ->
        let scopes =
              AccessTokenScopes
                { cache = scope_cache,
                  api = scope_api
                }
         in AccessTokenMetadata id name created_at last_used scopes
    )
    <$> pgQuery
      [pgSQL|
        SELECT id, name, created_at, last_used, scope_cache, scope_api
        FROM access_tokens
        WHERE user_id = ${userId}
      |]

getAccessTokenHashesForUser :: UserId -> M [(Int64, HashedPassword, AccessTokenScopes)]
getAccessTokenHashesForUser userId = do
  map
    ( \(id, token, scope_cache, scope_api) ->
        ( id,
          token,
          AccessTokenScopes
            { cache = scope_cache,
              api = scope_api
            }
        )
    )
    <$> pgQuery
      [pgSQL|
        SELECT id, token, scope_cache, scope_api
        FROM access_tokens
        WHERE user_id = ${userId}
      |]

markAccessTokenUsed :: UserId -> Int64 -> M ()
markAccessTokenUsed userId tokenId = do
  void
    $ pgExec
      [pgSQL|
        UPDATE access_tokens
          SET last_used = NOW()
          WHERE id = ${tokenId}
            AND user_id = ${userId}
      |]

insertAccessTokenForUser :: UserId -> Text -> AccessTokenScopes -> HashedPassword -> M ()
insertAccessTokenForUser userId name scopes tokenHash = do
  let cache = scopes ^. #cache
  let api = scopes ^. #api
  void
    $ pgExec
      [pgSQL|
        INSERT INTO access_tokens
          (name, token, user_id, scope_cache, scope_api)
          VALUES (${name}, ${tokenHash}, ${userId}, ${cache}, ${api})
      |]

deleteAccessTokenForUser :: UserId -> Int64 -> M ()
deleteAccessTokenForUser userId tokenId = do
  void
    $ pgExec
      [pgSQL|
        DELETE FROM access_tokens
          WHERE id = ${tokenId}
            AND user_id = ${userId}
      |]

-- * /api/build/commits

getCommitsForReqUser :: User -> M [CommitSummary]
getCommitsForReqUser user = do
  map
    ( \( repoOwner :: GhRepoOwner,
         repoName :: GhRepoName,
         gitCommit :: CommitHash,
         branch :: Maybe Branch,
         reqUser :: GhLogin,
         isPublic :: Bool,
         startTime :: UTCTime,
         succeeded :: Int64,
         failed :: Int64,
         pending :: Int64,
         cancelled :: Int64
         ) ->
          CommitSummary repoOwner repoName (RepoIsPublic isPublic) gitCommit branch reqUser startTime succeeded failed pending cancelled
    )
    <$> pgQuery
      [pgSQL|!
        WITH
        -- First we collect all of the commits for the user. We do this since
        -- this query has an index specifically to make this fast, and all
        -- future queries just operate on this or use the git_commit index.
        commits_for_req_user AS (
          SELECT git_commit, max(start_time) as commit_start_time FROM builds
          WHERE req_user = ${user ^. githubLogin}
          GROUP BY git_commit
          ORDER BY commit_start_time DESC
          LIMIT 100
        ),

        -- Now we can find all the builds we care about efficiently by joining:
        all_related_builds AS (
          SELECT
            repo_user,
            repo_name,
            package_type,
            system,
            package,
            builds.git_commit,
            branch,
            req_user,
            repo_is_public,
            builds.start_time,
            status
          FROM commits_for_req_user
          LEFT JOIN builds
            ON commits_for_req_user.git_commit = builds.git_commit
          WHERE req_user = ${user ^. githubLogin}
        ),

        -- Now filter out re-runs using `SELECT DISTINCT`
        without_reruns AS (
          SELECT DISTINCT ON (git_commit, package_type, system, package) *
          FROM all_related_builds
          ORDER BY git_commit, package_type, system, package, start_time DESC
        )

        -- Finally, aggregate the status totals by git_commit
        SELECT
          (array_agg(repo_user))[1],
          (array_agg(repo_name))[1],
          git_commit,
          (array_agg(branch))[1],
          (array_agg(req_user))[1],
          (array_agg(repo_is_public))[1],
          max(start_time) as commit_start_time,
          COUNT(*) FILTER (WHERE status = 'success') as succeeded,
          COUNT(*) FILTER (WHERE status = 'failure' OR status = 'timeout') as failed,
          COUNT(*) FILTER (WHERE status IS NULL) as pending,
          COUNT(*) FILTER (WHERE status = 'cancelled') as cancelled
        FROM without_reruns
        GROUP BY git_commit
        ORDER BY commit_start_time DESC
      |]

-- * /api/build/commit/{commit}

getCommitSummary :: CommitHash -> M CommitSummary
getCommitSummary commit = do
  res <-
    map
      ( \( repoOwner :: GhRepoOwner,
           repoName :: GhRepoName,
           gitCommit :: CommitHash,
           branch :: Maybe Branch,
           reqUser :: GhLogin,
           isPublic :: Bool,
           startTime :: UTCTime,
           succeeded :: Int64,
           failed :: Int64,
           pending :: Int64,
           cancelled :: Int64
           ) ->
            CommitSummary repoOwner repoName (RepoIsPublic isPublic) gitCommit branch reqUser startTime succeeded failed pending cancelled
      )
      <$> pgQuery
        [pgSQL|!
        SELECT
          (array_agg(repo_user))[1],
          (array_agg(repo_name))[1],
          git_commit,
          (array_agg(branch))[1],
          (array_agg(req_user))[1],
          (array_agg(repo_is_public))[1],
          min(start_time),
          COUNT(*) FILTER (WHERE status = 'success') as succeeded,
          COUNT(*) FILTER (WHERE status = 'failure' OR status = 'timeout') as failed,
          COUNT(*) FILTER (WHERE status IS NULL) as pending,
          COUNT(*) FILTER (WHERE status = 'cancelled') as cancelled
        FROM (
          SELECT DISTINCT ON (git_commit, package_type, system, package) * FROM builds
          WHERE git_commit = ${commit}
          ORDER BY git_commit, package_type, system, package, start_time DESC
        ) AS sub
        GROUP BY git_commit
      |]
  case res of
    [r] -> pure r
    [] -> throw $ NoSuchCommit commit
    _ -> throw $ OtherError "Impossible: more than one result"

-- * Internal stuff

newBuildDB :: CommitInfo -> PackageInfo -> Text -> Bool -> M Build
newBuildDB commitInfo packageInfo evalHost wantsIncrementalism = do
  now <- liftIO getCurrentTime
  changes <-
    pgQueryPrism
      _Build
      [pgSQL|
    INSERT INTO builds
        (repo_user,
         repo_name,
         pr_from_fork,
         branch,
         repo_is_public,
         git_commit,
         package,
         package_type,
         system,
         req_user,
         start_time,
         wants_incrementalism,
         eval_host,
         uploaded_to_cache
        )
    VALUES
        (${commitInfo ^. (repoInfo . ghRepoOwner)},
         ${commitInfo ^. (repoInfo . ghRepoName)},
         ${commitInfo ^. prFromFork},
         ${commitInfo ^. branch},
         ${commitInfo ^. repoPublicity},
         ${commitInfo ^. commit},
         ${packageInfo ^. Garnix.Types.packageName},
         ${packageInfo ^. packageType},
         ${packageInfo ^. maybeSystem},
         ${commitInfo ^. reqUser},
         ${now},
         ${wantsIncrementalism},
         ${evalHost},
         FALSE
        )
    ON CONFLICT DO NOTHING
    RETURNING
      id,
      repo_user,
      repo_name,
      pr_from_fork,
      branch,
      repo_is_public,
      git_commit,
      package,
      package_type,
      system,
      req_user,
      status,
      start_time,
      end_time,
      drv_path,
      output_paths,
      github_run_id,
      persistence_name,
      wants_incrementalism,
      eval_host,
      uploaded_to_cache,
      already_built
  |]
  case changes of
    [build] -> pure build
    _ -> throw $ OtherError "Expected 1 column to be updated"

reportBuildResultDB :: Build -> M ()
reportBuildResultDB build = do
  colsChanged <-
    pgExec
      [pgSQL|
    UPDATE builds
    SET status = ${build ^. status},
        end_time = ${build ^. endTime},
        drv_path = ${build ^. drvPath},
        output_paths = ${build ^. outputPaths},
        github_run_id = ${build ^. githubRunId},
        persistence_name = ${build ^. persistenceName},
        eval_host = ${build ^. evalHost},
        already_built = ${build ^. alreadyBuilt}
    WHERE id = ${build ^. id}
  |]
  case colsChanged of
    0 -> throw $ NoSuchBuild (build ^. id)
    1 -> pure ()
    _ -> throw $ OtherError "Somehow updated more than 0 or 1 columns"

upsertHeartbeat :: [Text] -> M ()
upsertHeartbeat hosts =
  forM_ hosts $ \host -> do
    pgQuery
      [pgSQL|
    INSERT INTO heartbeat
      (hostname, last_heartbeat)
      VALUES (${host}, NOW())
    ON CONFLICT (hostname) DO UPDATE set last_heartbeat = NOW()
      |]

getRecentHeartbeats :: M [Text]
getRecentHeartbeats =
  pgQuery
    [pgSQL|
  SELECT hostname
    FROM heartbeat
    WHERE NOW() - last_heartbeat < interval '12 hours'
    |]

getPrDeployDurationForOwner :: GhRepoOwner -> M Duration
getPrDeployDurationForOwner _ = pure emptyDuration

getCurrentMonthUsage :: GhRepoOwner -> M Duration
getCurrentMonthUsage owner = do
  fromMaybe emptyDuration . Map.lookup owner <$> getCurrentMonthUsages [owner]

getCurrentMonthUsages ::
  [GhRepoOwner] ->
  M (Map GhRepoOwner Duration)
getCurrentMonthUsages owners = do
  fromList
    . map (\(repoOwner :: GhRepoOwner, seconds :: Maybe Double) -> (repoOwner, fromSeconds $ fromMaybe 0 seconds))
    <$> pgQuery
      [pgSQL|
        SELECT
          repo_user,
          SUM(LEAST(120 * 60, date_part('EPOCH', (end_time - start_time)))) AS total_build_time
        FROM builds
        WHERE repo_user = ANY(${owners})
        AND end_time >= date_trunc('month', NOW())
        GROUP BY repo_user
      |]

getRepoKeyDB :: GhRepoOwner -> GhRepoName -> M (Maybe (PublicKey, PrivateKey))
getRepoKeyDB owner name = do
  results <-
    pgQuery
      [pgSQL|
        SELECT public_key, private_key
        FROM repo_secrets
        WHERE repo_user = ${owner}
        AND repo_name = ${name}
    |]
  case results of
    [result] -> pure $ Just result
    [] -> pure Nothing
    _ -> throw $ OtherError "Impossible: Got more than 1 result from getRepoKeyDB"

-- | In case of conflict, we return the key already in the DB, to prevent
-- overwriting
setRepoKeyDB ::
  GhRepoOwner ->
  GhRepoName ->
  Candidate PublicKey ->
  Candidate PrivateKey ->
  M (PublicKey, PrivateKey)
setRepoKeyDB owner name (Candidate pub) (Candidate priv) = do
  void
    $ pgQuery
      [pgSQL|
     INSERT INTO repo_secrets
       ( repo_user,
         repo_name,
         public_key,
         private_key
       )
     VALUES
       ( ${owner},
         ${name},
         ${pub},
         ${priv}
       )
     ON CONFLICT DO NOTHING
     |]
  getRepoKeyDB owner name >>= \case
    Nothing -> throw $ OtherError "Impossible setRepoKeyDB: expected a set key"
    Just v -> pure v

getActionKeyDB :: GhRepoOwner -> GhRepoName -> PackageName -> M (Maybe (PublicKey, PrivateKey))
getActionKeyDB owner name action = do
  results <-
    pgQuery
      [pgSQL|
        SELECT public_key, private_key
        FROM action_secrets
        WHERE repo_user = ${owner}
        AND repo_name = ${name}
        AND action_name = ${action}
    |]
  case results of
    [result] -> pure $ Just result
    [] -> pure Nothing
    _ -> throw $ OtherError "Impossible: Got more than 1 result from getRepoKeyDB"

-- | In case of conflict, we return the key already in the DB, to prevent
-- overwriting
setActionKeyDB ::
  GhRepoOwner ->
  GhRepoName ->
  PackageName ->
  Candidate PublicKey ->
  Candidate PrivateKey ->
  M (PublicKey, PrivateKey)
setActionKeyDB owner name action (Candidate pub) (Candidate priv) = do
  void
    $ pgQuery
      [pgSQL|
     INSERT INTO action_secrets
       ( repo_user,
         repo_name,
         action_name,
         public_key,
         private_key
       )
     VALUES
       ( ${owner},
         ${name},
         ${action},
         ${pub},
         ${priv}
       )
     ON CONFLICT DO NOTHING
     |]
  getActionKeyDB owner name action >>= \case
    Nothing -> throw $ OtherError "Impossible setRepoKeyDB: expected a set key"
    Just v -> pure v

isDenylisted :: GhRepoOwner -> GhRepoName -> M Bool
isDenylisted owner name = do
  result :: [Text] <-
    pgQuery
      [pgSQL|
        SELECT repo_user
        FROM denylist
        WHERE repo_user = ${owner}
        AND (repo_name = ${name} OR repo_name IS NULL)
      |]
  pure $ not $ null result

addToWaitlist :: Email -> M ()
addToWaitlist email = do
  void
    $ pgExec
      [pgSQL|
        INSERT INTO waitlist
          ( email )
        VALUES
          ( ${email} )
        ON CONFLICT DO NOTHING
      |]

getIncrementalTarget :: Build -> [CommitHash] -> M [Build]
getIncrementalTarget build commits =
  pgQueryPrism
    _Build
    [pgSQL|
        SELECT
          DISTINCT ON
            (repo_user,
             repo_name,
             package,
             package_type,
             system,
             git_commit
             )
          id,
          repo_user,
          repo_name,
          pr_from_fork,
          branch,
          repo_is_public,
          git_commit,
          package,
          package_type,
          system,
          req_user,
          status,
          start_time,
          end_time,
          drv_path,
          output_paths,
          github_run_id,
          persistence_name,
          wants_incrementalism,
          eval_host,
          uploaded_to_cache,
          already_built
        FROM builds
        WHERE git_commit =
             (SELECT
                git_commit
              FROM builds
              WHERE git_commit = ANY(${commits}::text[])
              GROUP BY git_commit
              HAVING
                bool_and(CASE WHEN end_time IS NULL THEN FALSE else TRUE END)
              ORDER BY ARRAY_POSITION(${commits}, git_commit)
              LIMIT 1)
          AND repo_user = ${build ^. repoUser}
          AND repo_name = ${build ^. repoName}
        ORDER BY
          repo_user,
          repo_name,
          package,
          package_type,
          system,
          git_commit,
          end_time ASC
      |]

-- * Health

checkHealth :: M ()
checkHealth = do
  res <- (pgQuery [pgSQL|SELECT 1|] :: M [Maybe Int32])
  case res of
    [Just 1] -> pure ()
    _ -> throw $ OtherError "DB Health check failed"

-- * Tokens

getUserInternalToken :: GhLogin -> M InternalCacheToken
getUserInternalToken reqUser =
  maybeGetDbToken >>= \case
    Just token -> pure token
    Nothing -> generateInternalCacheToken >>= insertUserToken
  where
    maybeGetDbToken :: M (Maybe InternalCacheToken)
    maybeGetDbToken =
      pgQuery
        [pgSQL|
          SELECT internal_token
            FROM internal_access_tokens
            WHERE github_login = ${reqUser}
        |]
        >>= \case
          [] -> pure Nothing
          [token] -> pure $ Just $ InternalCacheToken token
          _ -> throw $ OtherError "getUserInternalToken/get: internal token should be unique"

    insertUserToken :: InternalCacheToken -> M InternalCacheToken
    insertUserToken (InternalCacheToken rawToken) =
      pgQuery
        [pgSQL|
          INSERT INTO internal_access_tokens
            (github_login, internal_token)
          VALUES (${reqUser}, ${rawToken})
          ON CONFLICT DO NOTHING
          RETURNING internal_token
        |]
        >>= \case
          [] -> throw $ OtherError "getUserInternalToken/insert: no token returned"
          [token] -> pure $ InternalCacheToken token
          _ -> throw $ OtherError "getUserInternalToken/insert: internal token should be unique"

addVerifiedFod :: DrvPath -> StorePath -> M ()
addVerifiedFod drvPath storePath = do
  void
    $ pgExec
      [pgSQL|
        INSERT INTO verified_fods
          (drv_hash, store_path_hash)
        VALUES
          (${Nix.getHash $ Nix.getDrvPath drvPath}, ${Nix.getHash storePath})
        ON CONFLICT DO NOTHING
      |]

keepUnverifiedFods :: Set (DrvPath, a) -> M (Set (DrvPath, a))
keepUnverifiedFods drvPaths = do
  let drvHashes :: [Text] = fmap (cs . getHash . getDrvPath . fst) $ Set.toList drvPaths
  -- Use a Set for better asymptotics
  verifiedResults :: Set StoreHash <-
    Set.fromList
      <$> pgQuery
        [pgSQL|
          SELECT drv_hash FROM verified_fods
            WHERE drv_hash = ANY(${drvHashes}::text[])
        |]
  pure $ drvPaths
    & Set.filter (\(drv, _a) -> getHash (getDrvPath drv) `Set.notMember` verifiedResults)

-- * Helpers

getDBConnection :: [Data.ByteString.ByteString] -> IO PGConnection
getDBConnection dbPasswords = do
  case dbPasswords of
    [] -> error "No database passwords provided"
    [dbPass] -> do
      db <- getTPGDatabase
      pgConnect $ db {pgDBPass = dbPass}
    dbPass : rest -> do
      db <- getTPGDatabase
      result <- Control.Exception.Safe.try $ pgConnect $ db {pgDBPass = dbPass}
      case result of
        Left (e :: PSQLP.PGError) -> do
          hPutStrLn stderr $ "error connecting to the DB with one (of multiple) passwords: " <> show e
          hPutStrLn stderr "trying other passwords now"
          getDBConnection rest
        Right conn -> pure conn

pgQueryPrism :: (PGQuery q a) => Prism' x a -> q -> M [x]
pgQueryPrism p q = withConnection $ \conn -> timingAs #dbQueryTime $ do
  incrementEvent #dbQueries
  let queryStr = cs $ getQueryString unknownPGTypeEnv q
  res <-
    liftIO (PSQL.pgQuery conn q)
      `catchIOError` (throw . DbError queryStr . cs . show)
  pure $ res ^.. traverse . re p

pgQuery :: (PGQuery q a) => q -> M [a]
pgQuery q = withConnection $ \conn -> timingAs #dbQueryTime $ do
  incrementEvent #dbQueries
  let queryStr = cs $ getQueryString unknownPGTypeEnv q
  liftIO (PSQL.pgQuery conn q)
    `catchIOError` (throw . DbError queryStr . cs . show)

pgTransaction :: M a -> M a
pgTransaction inner = do
  view #dbConn >>= \case
    Transaction _ -> throw TransactionAlreadyStarted
    ConnectionPool pool -> do
      liftBaseOp (withResource pool)
        $ \conn ->
          liftBaseOp_ (Safe.bracketOnError_ (PSQLP.pgBegin conn) (PSQLP.pgRollback conn)) $ do
            result <- try $ local (#dbConn .~ Transaction conn) inner
            case result of
              Left e -> do
                liftIO $ PSQLP.pgRollback conn
                throw $ err e
              Right r -> do
                liftIO $ PSQLP.pgCommit conn
                pure r

pgExec :: (PGQuery q ()) => q -> M Int
pgExec q = withConnection $ \conn -> timingAs #dbQueryTime $ do
  incrementEvent #dbQueries
  let queryStr = cs $ getQueryString unknownPGTypeEnv q
  liftIO (PSQL.pgExecute conn q)
    `catchIOError` (throw . DbError queryStr . cs . show)

withConnection :: (PGConnection -> M a) -> M a
withConnection act = do
  view #dbConn >>= \case
    Transaction conn -> act conn
    ConnectionPool pool -> liftBaseOp (withResource pool) act
