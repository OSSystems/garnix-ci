module Garnix.DB where

import Control.Exception.Safe qualified
import Control.Exception.Safe qualified as Safe
import Control.Lens
import Control.Monad.Trans.Control (liftBaseOp_)
import Data.ByteString qualified
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Map.Strict (Map, fromList)
import Data.Pool (withResource)
import Data.Set qualified as Set
import Data.Text qualified as T
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

-- * Accounts and their forge identities

-- | The account a session names, with its identities as they are now.
getUserById :: UserId -> M (Maybe User)
getUserById userId = do
  res <- pgQuery [pgSQL| SELECT email, created_at FROM users WHERE id = ${userId} |]
  case res of
    [] -> pure Nothing
    [(email', createdAt')] -> Just . User userId email' createdAt' <$> getIdentities userId
    _ -> throw $ OtherError "Got more than 1 user from getUserById"

getIdentities :: UserId -> M [ForgeIdentity]
getIdentities userId =
  map (\(forge', login', isForgeAdmin') -> ForgeIdentity forge' login' isForgeAdmin')
    <$> pgQuery
      [pgSQL|
        SELECT forge, login, is_forge_admin
        FROM forge_identities
        WHERE user_id = ${userId}
        ORDER BY created_at, forge
      |]

-- | The account an identity belongs to.
getUser :: ForgeLogin -> M User
getUser login' = Garnix.DB.getUserId login' >>= getUserById >>= maybe (throw $ NoSuchUser (login' ^. ghLogin)) pure

getUserId :: ForgeLogin -> M UserId
getUserId login' = lookupIdentityOwner login' >>= maybe (throw $ NoSuchUser (login' ^. ghLogin)) pure

-- | The account holding an identity; 'Nothing' for an identity garnix does not
-- know yet, as at its first login.
lookupIdentityOwner :: ForgeLogin -> M (Maybe UserId)
lookupIdentityOwner (ForgeLogin forge' ghLogin') = do
  res <- pgQuery [pgSQL| SELECT user_id FROM forge_identities WHERE forge = ${forge'} AND login = ${ghLogin'} |]
  case res of
    [] -> pure Nothing
    [id] -> pure $ Just $ UserId id
    _ -> throw $ OtherError "Got more than 1 user from lookupIdentityOwner"

-- | Whether another account has the email of an account just created,
-- compared without case. An email is contact data only: it never identifies
-- an account, nor links two.
newtype EmailAlreadyUsed = EmailAlreadyUsed {getEmailAlreadyUsed :: Bool}
  deriving stock (Eq, Show)

-- | A new account holding one identity. When the identity is attached
-- meanwhile (two callbacks of one first login), no account is created and
-- the clash says who holds it.
createAccount :: ForgeIdentity -> Email -> M (Either IdentityClash (User, EmailAlreadyUsed))
createAccount identity email' = withinTransaction $ do
  emailAlreadyUsed <-
    pgQuery [pgSQL| SELECT EXISTS (SELECT 1 FROM users WHERE lower(email) = lower(${email'})) |]
      <&> EmailAlreadyUsed
      . (== [Just True])
  created <-
    pgQuery
      [pgSQL|
        INSERT INTO users (email, subscription_type)
        VALUES (${email'}, 'free')
        RETURNING id, created_at
      |]
  case created of
    [(id', createdAt')] -> do
      let userId = UserId id'
      tryAddIdentity userId identity >>= \case
        Right () -> pure $ Right (User userId email' createdAt' [identity], emailAlreadyUsed)
        Left clash -> do
          void $ pgExec [pgSQL| DELETE FROM users WHERE id = ${userId} |]
          pure $ Left clash
    _ -> throw $ OtherError "impossible: not exactly one user created"

-- | An account holding one identity, for tests and the dev login.
newUser :: ForgeLogin -> Email -> M User
newUser login' email' =
  createAccount (ForgeIdentity (login' ^. forge) (login' ^. ghLogin) False) email'
    >>= either (throw . IdentityConflict . identityClashMessage login') (pure . fst)

-- | Why an identity could not be attached to an account.
data IdentityClash
  = -- | Another account holds the identity.
    OwnedBy UserId
  | -- | The account holds another identity on that forge: this login.
    AlreadyOnForge GhLogin
  | -- | Neither any more: the identity or the account's other one went away
    -- between the insert and the reads.
    Raced
  deriving stock (Eq, Show)

-- | The clash, from who holds the identity and which login the account holds
-- on its forge.
identityClash :: UserId -> Maybe UserId -> Maybe GhLogin -> IdentityClash
identityClash userId owner onForge = case (owner, onForge) of
  (Just ownerId, _) | ownerId /= userId -> OwnedBy ownerId
  (_, Just login') -> AlreadyOnForge login'
  (_, Nothing) -> Raced

identityClashMessage :: ForgeLogin -> IdentityClash -> Text
identityClashMessage (ForgeLogin forge' login') = \case
  OwnedBy _ ->
    "The " <> getForgeSlug forge' <> " identity " <> getGhLogin login' <> " already belongs to another garnix account."
  AlreadyOnForge other ->
    "Your account is already connected to " <> getForgeSlug forge' <> " as " <> getGhLogin other <> ". Disconnect it first."
  Raced -> "The " <> getForgeSlug forge' <> " identity " <> getGhLogin login' <> " could not be attached. Try again."

-- | Attaches an identity to an account, unless the identity belongs to an
-- account already or the account has another identity on that forge.
tryAddIdentity :: UserId -> ForgeIdentity -> M (Either IdentityClash ())
tryAddIdentity userId identity = do
  let forge' = identity ^. forge
      login' = identity ^. ghLogin
  inserted <-
    pgExec
      [pgSQL|
        INSERT INTO forge_identities (user_id, forge, login, is_forge_admin)
        VALUES (${userId}, ${forge'}, ${login'}, ${identity ^. isForgeAdmin})
        ON CONFLICT DO NOTHING
      |]
  if inserted == 1
    then pure $ Right ()
    else do
      owner <- lookupIdentityOwner (identityForgeLogin identity)
      onForge <- pgQuery [pgSQL| SELECT login FROM forge_identities WHERE user_id = ${userId} AND forge = ${forge'} |]
      pure $ Left $ identityClash userId owner (GhLogin <$> listToMaybe onForge)

-- | 'tryAddIdentity', failing with the clash's message.
addIdentity :: UserId -> ForgeIdentity -> M ()
addIdentity userId identity =
  tryAddIdentity userId identity
    >>= either (throw . IdentityConflict . identityClashMessage (identityForgeLogin identity)) pure

-- | Records what the forge said about the identity at this login.
setIdentityIsForgeAdmin :: ForgeLogin -> Bool -> M ()
setIdentityIsForgeAdmin (ForgeLogin forge' ghLogin') isForgeAdmin' =
  void
    $ pgExec
      [pgSQL|
        UPDATE forge_identities SET is_forge_admin = ${isForgeAdmin'}
        WHERE forge = ${forge'} AND login = ${ghLogin'}
      |]

-- | Whether module settings were saved through this identity.
identityHasModuleSettings :: ForgeLogin -> M Bool
identityHasModuleSettings (ForgeLogin forge' ghLogin') = do
  res <-
    pgQuery
      [pgSQL|
        SELECT id FROM module_user_repo
        WHERE forge = ${forge'} AND github_login = ${ghLogin'}
      |]
  pure $ not $ null (res :: [Int64])

-- | What a disconnect does with the module settings saved through the
-- identity: they are deleted only once the request confirms it.
data ModuleSettings = KeepModuleSettings | DeleteModuleSettings
  deriving stock (Eq, Show)

-- | Why an identity is not removed.
data RemovalRefusal = NoSuchIdentity | LastIdentity | HasModuleSettings
  deriving stock (Eq, Show)

-- | The account's identity on a forge, among its identities.
identityToRemove :: ForgeSlug -> [ForgeIdentity] -> Either RemovalRefusal ForgeIdentity
identityToRemove forge' = maybe (Left NoSuchIdentity) Right . find ((== forge') . (^. forge))

-- | Whether that identity may go, from how many of the account's other
-- identities are on an active forge, and so still log it in, and whether
-- module settings were saved through it.
mayRemove :: ModuleSettings -> Int -> Bool -> ForgeIdentity -> Either RemovalRefusal ForgeIdentity
mayRemove moduleSettings otherLiveIdentities hasModuleSettings identity
  | otherLiveIdentities < 1 = Left LastIdentity
  | hasModuleSettings && moduleSettings == KeepModuleSettings = Left HasModuleSettings
  | otherwise = Right identity

-- | Detaches the account's identity on a forge, with its credentials and the
-- module settings saved through it (see 'mayRemove'). Builds and other
-- history it requested stay. The account's row is locked, so two concurrent
-- removals cannot leave it with no identity.
removeIdentity :: UserId -> ForgeSlug -> ModuleSettings -> M (Either RemovalRefusal ())
removeIdentity userId forge' moduleSettings = withinTransaction $ do
  _ <- pgQuery [pgSQL| SELECT id FROM users WHERE id = ${userId} FOR UPDATE |] :: M [Int32]
  identities <- getIdentities userId
  plan <- forM (identityToRemove forge' identities) $ \identity -> do
    hasModuleSettings <- identityHasModuleSettings (identityForgeLogin identity)
    otherLive <- liveIdentities (filter (/= identity) identities)
    pure $ mayRemove moduleSettings (length otherLive) hasModuleSettings identity
  forM (join plan) $ \identity -> do
    void
      $ pgExec
        [pgSQL|
          DELETE FROM module_values
          WHERE module_user_repo_id IN (
            SELECT id FROM module_user_repo
            WHERE forge = ${forge'} AND github_login = ${identity ^. ghLogin}
          )
        |]
    void $ pgExec [pgSQL| DELETE FROM module_user_repo WHERE forge = ${forge'} AND github_login = ${identity ^. ghLogin} |]
    void $ pgExec [pgSQL| DELETE FROM forge_identities WHERE user_id = ${userId} AND forge = ${forge'} |]

-- | The identities of an account as two parallel arrays, for queries that
-- match @(forge, req_user)@ against any of them.
identityArrays :: [ForgeIdentity] -> ([Text], [Text])
identityArrays identities =
  ( getForgeSlug . (^. forge) <$> identities,
    getGhLogin . (^. ghLogin) <$> identities
  )

getRepoConfig :: RepoId -> M RepoConfig
getRepoConfig (RepoId forge' repoOwner repoName) = do
  configuredEvalMemory <- getConfiguredEvalMemory repoOwner repoName
  repoConfig <-
    map (\(skipInputChecks, evalMemory) -> RepoConfig skipInputChecks (fromMaybe configuredEvalMemory evalMemory))
      <$> pgQuery
        [pgSQL|
          SELECT
            skip_private_inputs_check_for_collaborators,
            max_eval_memory
          FROM repo_config
          WHERE forge = ${forge'}
            AND repo_user = ${repoOwner}
            AND repo_name = ${repoName}
        |]
  case repoConfig of
    [] -> pure $ defaultRepoConfig & maxEvalMemory .~ configuredEvalMemory
    [res] -> pure res
    _ -> throw $ OtherError "impossible: multiple entries for repo config"

getConfiguredEvalMemory :: GhRepoOwner -> GhRepoName -> M Memory
getConfiguredEvalMemory repoOwner repoName = do
  config <- view #evalMemoryConfig
  pure
    $ fromMaybe (config ^. #defaultEvalMemory)
    $ Map.lookup (repoOwner, repoName) (config ^. #perRepositoryEvalMemory)

getBuild :: BuildId -> M Build
getBuild buildId = do
  res <-
    pgQueryPrism
      _Build
      [pgSQL|
    SELECT
      id,
      forge,
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

-- | @adminForges@ are the forges whose every repository the user administers.
getOriginalBuildForDrvPath :: Maybe User -> [ForgeSlug] -> FilePath -> M (Maybe OriginalBuild)
getOriginalBuildForDrvPath user adminForges drvPath = do
  let (forges, logins) = identityArrays $ user ^. _Just . identities
      adminForges' = getForgeSlug <$> adminForges
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
            OR forge = ANY(${adminForges'}::text[])
            OR (forge, req_user) IN (SELECT * FROM unnest(${forges}::text[], ${logins}::text[]))
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
      ( forge,
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
        uploaded_to_cache)
      SELECT
        forge,
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
      forge,
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
    SELECT DISTINCT ON (forge, repo_user, repo_name, git_commit, package, package_type, system)
      id,
      forge,
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
    WHERE forge = ${repoInfo ^. (repoId . forge)}
    AND repo_user = ${repoInfo ^. (repoId . repoUser)}
    AND repo_name = ${repoInfo ^. (repoId . repoName)}
    AND git_commit = ${commit}
    ORDER BY
      forge, repo_user, repo_name, git_commit, package, package_type, system,
      start_time DESC
  |]

-- | Builds any identity of the account requested.
getBuilds :: User -> M [Build]
getBuilds usr = do
  let (forges, logins) = identityArrays $ usr ^. identities
  pgQueryPrism
    _Build
    [pgSQL|
    SELECT
      id,
      forge,
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
    WHERE (forge, req_user) IN (SELECT * FROM unnest(${forges}::text[], ${logins}::text[]))
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

getLatestBuildsForBranch :: RepoId -> Branch -> M [Build]
getLatestBuildsForBranch (RepoId forge' owner name) branch = do
  pgQueryPrism
    _Build
    [pgSQL|
    SELECT
      id,
      forge,
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
      WHERE forge = ${forge'}
        AND repo_user = ${owner}
        AND branch = ${branch}
        AND repo_name = ${name}
        AND git_commit = (
          SELECT git_commit
          FROM builds
          WHERE forge = ${forge'}
            AND repo_user = ${owner}
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

registerPush :: RepoId -> CommitHash -> Branch -> M RegisterPushResult
registerPush (RepoId forge' repoOwner repoName) commit branch = do
  pgQuery
    [pgSQL|
      INSERT INTO pushes
        (forge,
         repo_user,
         repo_name,
         git_commit,
         branch
        )
      VALUES
        (${forge'},
         ${repoOwner},
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

getCommitsByOwnerAndRepo :: RepoId -> M [CommitSummary]
getCommitsByOwnerAndRepo (RepoId repoForge repoOwner repoName) = do
  map
    ( \( forge' :: ForgeSlug,
         repoOwner :: GhRepoOwner,
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
          CommitSummary forge' repoOwner repoName (RepoIsPublic isPublic) gitCommit branch reqUser startTime succeeded failed pending cancelled
    )
    <$> pgQuery
      [pgSQL|!
        SELECT
          (array_agg(forge))[1],
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
          WHERE forge = ${repoForge}
            AND repo_user = ${repoOwner}
            AND repo_name = ${repoName}
          ORDER BY git_commit, package_type, system, package, start_time DESC
        ) AS sub
        GROUP BY git_commit
        ORDER BY commit_start_time DESC
        LIMIT 100
      |]

getCommit :: RepoId -> CommitHash -> M (Maybe Commit)
getCommit (RepoId forge' owner name) commit =
  pgQueryPrism
    _Commit
    [pgSQL|
      SELECT
        forge,
        repo_user,
        repo_name,
        git_commit,
        status,
        meta_check
      FROM commits
      WHERE forge = ${forge'}
        AND repo_user = ${owner}
        AND repo_name = ${name}
        AND git_commit = ${commit}
    |]
    >>= \case
      [r] -> pure $ Just r
      [] -> pure Nothing
      _ -> throw $ OtherError "Impossible: more than one result"

newCommit :: RepoId -> CommitHash -> M ()
newCommit (RepoId forge' owner name) commit = do
  evalHost <- view #hostname
  evalInstance <- view #evalInstance
  now <- liftIO getCurrentTime
  void
    $ pgExec
      [pgSQL|
        INSERT INTO commits
          (forge, repo_user, repo_name, git_commit, status, meta_check,
           eval_host, eval_instance, started_at)
        VALUES
            (${forge'}, ${owner}, ${name}, ${commit}, 'evaluating', 'pending',
             ${evalHost}, ${evalInstance}, ${now})
        ON CONFLICT (forge, repo_user, repo_name, git_commit) DO UPDATE
          SET meta_check = 'pending',
              eval_host = ${evalHost},
              eval_instance = ${evalInstance},
              started_at = ${now}
      |]

setCommitStatus :: RepoId -> CommitHash -> CommitStatus -> M ()
setCommitStatus (RepoId forge' owner name) commit st =
  void
    $ pgExec
      [pgSQL|
        UPDATE commits
          SET status = ${st}
        WHERE forge = ${forge'}
          AND repo_user = ${owner}
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
setMetaCheck :: RepoId -> CommitHash -> CheckStatusUpdate -> M Bool
setMetaCheck (RepoId forge' owner name) commit (CheckStatusUpdate {_checkStatusUpdateFrom = from, _checkStatusUpdateTo = to}) = do
  if from == to
    then pure False
    else
      (== 1)
        <$> pgExec
          [pgSQL|
            UPDATE commits
              SET meta_check = ${to}
            WHERE forge = ${forge'}
              AND repo_user = ${owner}
              AND repo_name = ${name}
              AND git_commit = ${commit}
              AND meta_check = ${from}
          |]

-- | Atomically claim the right to post the pull request failure comment for
-- this commit. Returns True exactly once per commit, even across re-runs -
-- unlike 'setMetaCheck', whose 'pending' state 'newCommit' resets on every run.
claimFailureComment :: RepoId -> CommitHash -> M Bool
claimFailureComment (RepoId forge' owner name) commit =
  (== 1)
    <$> pgExec
      [pgSQL|
        UPDATE commits
          SET failure_commented = true
        WHERE forge = ${forge'}
          AND repo_user = ${owner}
          AND repo_name = ${name}
          AND git_commit = ${commit}
          AND NOT failure_commented
      |]

getBuildsAndRunsByCommit :: RepoId -> CommitHash -> M FullCommitState
getBuildsAndRunsByCommit repo commitHash = do
  mCommit <- getCommit repo commitHash
  case mCommit of
    Nothing -> pure CommitEvaluating
    Just commit -> case commit ^. status of
      Evaluating -> pure CommitEvaluating
      Evaluated -> do
        builds <- getBuildsByCommit repo commitHash
        runs <- getRuns repo commitHash
        pure $ CommitEvaluated commit builds runs

getBuildsByCommit :: RepoId -> CommitHash -> M [Build]
getBuildsByCommit (RepoId forge' repoOwner repoName) commitHash = do
  pgQuery
    [pgSQL|
      SELECT DISTINCT ON (git_commit, package_type, system, package)
        id,
        forge,
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
            AND forge = ${forge'}
            AND repo_user = ${repoOwner}
            AND repo_name = ${repoName}
      ORDER BY git_commit, package_type, system, package, start_time DESC
      LIMIT 1000
    |]
    <&> map
      ( \( id,
           buildForge,
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
                _buildForge = buildForge,
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

getRuns :: RepoId -> CommitHash -> M [Run]
getRuns (RepoId forge' repoOwner repoName) commitHash = do
  pgQuery
    [pgSQL|
      SELECT id, name, forge, repo_user, repo_name, git_commit, branch, status, req_user, start_time, end_time
      FROM runs
      WHERE git_commit = ${commitHash}
        AND forge = ${forge'}
        AND repo_user = ${repoOwner}
        AND repo_name = ${repoName}
    |]
    <&> map
      ( \(id, name, runForge, repoOwner, repoName, gitCommit, branch, status, reqUser, startTime, endTime) ->
          Run
            { _runId = id,
              _runName = name,
              _runForge = runForge,
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
        SELECT id, name, forge, repo_user, repo_name, git_commit, branch, status, req_user, start_time, end_time
        FROM runs
        WHERE id = ${runId}
      |]
      <&> map
        ( \(id, name, runForge, repoOwner, repoName, gitCommit, branch, status, reqUser, startTime, endTime) ->
            Run
              { _runId = id,
                _runName = name,
                _runForge = runForge,
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
  let runForge = commitInfo ^. repoInfo . repoId . forge
  let repoOwner = commitInfo ^. repoInfo . repoId . repoUser
  let repoName = commitInfo ^. repoInfo . repoId . Garnix.Types.repoName
  let commitHash = commitInfo ^. commit
  let branch = commitInfo ^. Garnix.Types.branch
  let reqUser = commitInfo ^. (Garnix.Types.reqUser . ghLogin)
  evalHost <- view #hostname
  evalInstance <- view #evalInstance
  result <-
    pgQuery
      [pgSQL|
        INSERT INTO runs
          (name, forge, repo_user, repo_name, git_commit, branch, status, req_user,
           eval_host, eval_instance)
        VALUES
          (${name}, ${runForge}, ${repoOwner}, ${repoName}, ${commitHash}, ${branch}, NULL, ${reqUser},
           ${evalHost}, ${evalInstance})
        RETURNING
          id, name, repo_user, repo_name, git_commit, branch, status, req_user, start_time
      |]
      <&> map
        ( \(id, name, repoOwner, repoName, commitHash, branch, status, reqUser, startTime) ->
            Run
              { _runId = id,
                _runName = name,
                _runForge = runForge,
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
tagCacheUpload :: RepoId -> [StorePath] -> M ()
tagCacheUpload (RepoId forge' repoOwner repoName) =
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
              (hash, forge, repo_owner, repo_name)
              VALUES (UNNEST(${hashes}::text[]), ${forge'}, ${repoOwner}, ${repoName})
              ON CONFLICT DO NOTHING
          |]

getReposForHash :: StoreHash -> M [RepoId]
getReposForHash hash = do
  map (\(forge', owner, name) -> RepoId forge' owner name)
    <$> pgQuery
      [pgSQL|
      SELECT forge, repo_owner, repo_name
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
        DELETE FROM cache_store_hash_references WHERE hash = ${hash}
      |]
  void
    $ pgExec
      [pgSQL|
        INSERT INTO cache_store_hash_references (hash, reference_hash)
        SELECT ${hash}::text, split_part(ref, '-', 1)
          FROM unnest(string_to_array(${references}::text, ' ')) AS ref
         WHERE ref <> ''
        ON CONFLICT DO NOTHING
      |]
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

tagCacheUploadForS3Cache :: RepoId -> StoreHash -> M ()
tagCacheUploadForS3Cache (RepoId forge' repoOwner repoName) hash = do
  void
    $ pgExec
      [pgSQL|
        INSERT INTO cache_store_hash_tags
          (hash, forge, repo_owner, repo_name)
          VALUES (${hash}, ${forge'}, ${repoOwner}, ${repoName})
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
          AND deleting_since IS NULL
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
        WHERE cache_store_hashes.deleting_since IS NULL
          AND (cache_store_hashes.package_name IS NULL
               OR (cache_store_hashes.uploaded_at IS NULL AND
                   cache_store_hashes.created_at < now() - interval '10 hours'))
        RETURNING
          hash, package_name;
      |]
  forM filtered $ \case
    (Just hash, Just packageName) -> pure $ StorePath hash packageName
    _ -> throw $ OtherError "impossible: hashes and packageNames have the same length"

-- * S3 cache retention

data GcObject = GcObject
  { gcObjectHash :: StoreHash,
    gcObjectPackageName :: Text,
    gcObjectPublic :: Bool,
    gcObjectFileSize :: Int64
  }
  deriving (Generic, Show)

data GcCutoff = GcCutoff
  { gcCutoffTime :: UTCTime,
    gcCutoffWarmedUp :: Bool
  }
  deriving (Generic, Show)

data CacheSizeStats = CacheSizeStats
  { cacheLiveObjects :: Int64,
    cacheLiveBytes :: Int64
  }
  deriving (Generic, Show)

data TombstoneStats = TombstoneStats
  { tombstonesPending :: Int64,
    oldestTombstone :: Maybe UTCTime
  }
  deriving (Generic, Show)

toGcObject :: (StoreHash, Maybe Text, Maybe Bool, Maybe Int64) -> Maybe GcObject
toGcObject = \case
  (gcObjectHash, Just gcObjectPackageName, Just gcObjectPublic, Just gcObjectFileSize) ->
    Just GcObject {gcObjectHash, gcObjectPackageName, gcObjectPublic, gcObjectFileSize}
  _ -> Nothing

countDeletingStoreHashes :: [StoreHash] -> M Int64
countDeletingStoreHashes [] = pure 0
countDeletingStoreHashes (sort -> hashes) = do
  result <-
    pgQuery
      [pgSQL|
        SELECT count(*)
        FROM cache_store_hashes
        WHERE hash = ANY(${hashes}::text[])
          AND deleting_since IS NOT NULL
      |]
  pure $ case result of
    [Just count] -> count
    _ -> 0

bumpCacheAccessedAt :: Duration -> [StoreHash] -> M Int
bumpCacheAccessedAt _ [] = pure 0
bumpCacheAccessedAt minAge (sort -> hashes) = do
  let minAgeSeconds = toSeconds minAge
  pgExec
    [pgSQL|
      UPDATE cache_store_hashes
      SET accessed_at = NOW()
      WHERE hash = ANY(${hashes}::text[])
        AND accessed_at < NOW() - (${minAgeSeconds}::double precision * interval '1 second')
    |]

stampReadsRecordedSince :: M ()
stampReadsRecordedSince =
  void
    $ pgExec
      [pgSQL|
        UPDATE cache_gc_state
        SET reads_recorded_since = NOW()
        WHERE id AND reads_recorded_since IS NULL
      |]

getGcCutoff :: Duration -> Duration -> M GcCutoff
getGcCutoff retentionPeriod warmupPeriod = do
  let retentionSeconds = toSeconds retentionPeriod
  let warmupSeconds = toSeconds warmupPeriod
  result <-
    pgQuery
      [pgSQL|
        SELECT
          NOW() - (${retentionSeconds}::double precision * interval '1 second'),
          reads_recorded_since IS NOT NULL
            AND NOW() >= reads_recorded_since + (${warmupSeconds}::double precision * interval '1 second')
        FROM cache_gc_state
        WHERE id
      |]
  case result of
    [(Just gcCutoffTime, Just gcCutoffWarmedUp)] -> pure GcCutoff {gcCutoffTime, gcCutoffWarmedUp}
    _ -> throw $ OtherError "getGcCutoff: cache_gc_state is missing its singleton row"

acquireGcLease :: Text -> Duration -> M Bool
acquireGcLease owner lease = do
  let leaseSeconds = toSeconds lease
  updated <-
    pgExec
      [pgSQL|
        UPDATE cache_gc_state
        SET lock_owner = ${owner},
            lock_expires_at = NOW() + (${leaseSeconds}::double precision * interval '1 second')
        WHERE id
          AND (lock_expires_at IS NULL OR lock_expires_at < NOW() OR lock_owner = ${owner})
      |]
  pure $ updated > 0

releaseGcLease :: Text -> M ()
releaseGcLease owner =
  void
    $ pgExec
      [pgSQL|
        UPDATE cache_gc_state
        SET lock_owner = NULL,
            lock_expires_at = NULL,
            last_run_at = NOW()
        WHERE id AND lock_owner = ${owner}
      |]

countExpiredStoreHashes :: UTCTime -> M Int64
countExpiredStoreHashes cutoff = do
  result <-
    pgQuery
      [pgSQL|
        SELECT count(*)
        FROM cache_store_hashes
        WHERE deleting_since IS NULL
          AND uploaded_at IS NOT NULL
          AND accessed_at < ${cutoff}
      |]
  pure $ case result of
    [Just count] -> count
    _ -> 0

markGcCandidates :: UTCTime -> Int -> M [GcObject]
markGcCandidates cutoff batchSize = do
  let limit = fromIntegral batchSize :: Int64
  rows <-
    pgQuery
      [pgSQL|
        WITH RECURSIVE reachable(hash) AS (
            SELECT hash
              FROM cache_store_hashes
             WHERE deleting_since IS NULL
               AND (uploaded_at IS NULL OR accessed_at >= ${cutoff})
          UNION
            SELECT edges.reference_hash
              FROM reachable
              JOIN cache_store_hash_references AS edges ON edges.hash = reachable.hash
        )
        SELECT candidate.hash,
               candidate.package_name,
               candidate.public,
               candidate.file_size
          FROM cache_store_hashes AS candidate
         WHERE candidate.deleting_since IS NULL
           AND candidate.uploaded_at IS NOT NULL
           AND candidate.accessed_at < ${cutoff}
           AND NOT EXISTS (
                 SELECT 1 FROM reachable WHERE reachable.hash = candidate.hash
               )
         ORDER BY candidate.accessed_at
         LIMIT ${limit}::bigint
      |]
  pure $ catMaybes $ fmap toGcObject rows

tombstoneGcObjects :: UTCTime -> [StoreHash] -> M [GcObject]
tombstoneGcObjects _ [] = pure []
tombstoneGcObjects cutoff (sort -> hashes) = do
  rows <-
    pgQuery
      [pgSQL|
        UPDATE cache_store_hashes
        SET deleting_since = NOW()
        WHERE hash = ANY(${hashes}::text[])
          AND deleting_since IS NULL
          AND uploaded_at IS NOT NULL
          AND accessed_at < ${cutoff}
        RETURNING hash, package_name, public, file_size
      |]
  pure $ catMaybes $ fmap toGcObject rows

getPendingTombstones :: Int -> M [GcObject]
getPendingTombstones batchSize = do
  let limit = fromIntegral batchSize :: Int64
  rows <-
    pgQuery
      [pgSQL|
        SELECT hash, package_name, public, file_size
        FROM cache_store_hashes
        WHERE deleting_since IS NOT NULL
        ORDER BY deleting_since
        LIMIT ${limit}::bigint
      |]
  pure $ catMaybes $ fmap toGcObject rows

deleteGcObjects :: [StoreHash] -> M ()
deleteGcObjects [] = pure ()
deleteGcObjects (sort -> hashes) = do
  void
    $ pgExec
      [pgSQL|
        DELETE FROM cache_store_hash_tags WHERE hash = ANY(${hashes}::text[])
      |]
  void
    $ pgExec
      [pgSQL|
        DELETE FROM cache_store_hash_references WHERE hash = ANY(${hashes}::text[])
      |]
  void
    $ pgExec
      [pgSQL|
        DELETE FROM cache_store_hashes WHERE hash = ANY(${hashes}::text[])
      |]

getCacheSizeStats :: M CacheSizeStats
getCacheSizeStats = do
  result <-
    pgQuery
      [pgSQL|
        SELECT count(*), COALESCE(sum(file_size), 0)::bigint
        FROM cache_store_hashes
        WHERE uploaded_at IS NOT NULL
          AND deleting_since IS NULL
      |]
  pure $ case result of
    [(Just cacheLiveObjects, Just cacheLiveBytes)] -> CacheSizeStats {cacheLiveObjects, cacheLiveBytes}
    _ -> CacheSizeStats {cacheLiveObjects = 0, cacheLiveBytes = 0}

getTombstoneStats :: M TombstoneStats
getTombstoneStats = do
  result <-
    pgQuery
      [pgSQL|
        SELECT count(*), min(deleting_since)
        FROM cache_store_hashes
        WHERE deleting_since IS NOT NULL
      |]
  pure $ case result of
    [(Just tombstonesPending, oldestTombstone)] -> TombstoneStats {tombstonesPending, oldestTombstone}
    _ -> TombstoneStats {tombstonesPending = 0, oldestTombstone = Nothing}

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

-- | The OAuth credentials of an identity, if the forge's last answer left any.
getIdentityCredentials :: ForgeLogin -> M (Maybe (GhUserCredentials EncryptedText))
getIdentityCredentials (ForgeLogin forge' ghLogin') = do
  res <-
    pgQuery
      [pgSQL|
    SELECT
      access_token,
      access_token_expires_at,
      refresh_token,
      refresh_token_expires_at
    FROM forge_identities
    WHERE forge = ${forge'}
      AND login = ${ghLogin'}
      AND access_token IS NOT NULL
  |]
  singleCredentialsRow "getIdentityCredentials" res

lockIdentityCredentials :: ForgeLogin -> M (Maybe (GhUserCredentials EncryptedText))
lockIdentityCredentials (ForgeLogin forge' ghLogin') = do
  res <-
    pgQuery
      [pgSQL|
    SELECT
      access_token,
      access_token_expires_at,
      refresh_token,
      refresh_token_expires_at
    FROM forge_identities
    WHERE forge = ${forge'}
      AND login = ${ghLogin'}
      AND access_token IS NOT NULL
    FOR UPDATE
  |]
  singleCredentialsRow "lockIdentityCredentials" res

singleCredentialsRow ::
  Text ->
  [(Maybe EncryptedText, Maybe UTCTime, Maybe EncryptedText, Maybe UTCTime)] ->
  M (Maybe (GhUserCredentials EncryptedText))
singleCredentialsRow caller = \case
  [] -> pure Nothing
  [(Just accessToken', accessTokenExpiresAt', refreshToken', refreshTokenExpiresAt')] ->
    pure
      $ Just
      $ GhUserCredentials
        { _ghUserCredentialsAccessToken = accessToken',
          _ghUserCredentialsAccessTokenExpiresAt = accessTokenExpiresAt',
          _ghUserCredentialsRefreshToken = refreshToken',
          _ghUserCredentialsRefreshTokenExpiresAt = refreshTokenExpiresAt'
        }
  [(Nothing, _, _, _)] -> pure Nothing
  _ -> throw $ OtherError $ "Got more than 1 row from " <> caller

setIdentityCredentials :: ForgeLogin -> GhUserCredentials EncryptedText -> M ()
setIdentityCredentials (ForgeLogin forge' ghLogin') credentials = do
  let accessToken' = credentials ^. accessToken
      accessTokenExpiresAt' = credentials ^. accessTokenExpiresAt
      refreshToken' = credentials ^. refreshToken
      refreshTokenExpiresAt' = credentials ^. refreshTokenExpiresAt
  updated <-
    pgExec
      [pgSQL|
    UPDATE forge_identities SET
      access_token = ${accessToken'},
      access_token_expires_at = ${accessTokenExpiresAt'},
      refresh_token = ${refreshToken'},
      refresh_token_expires_at = ${refreshTokenExpiresAt'},
      credentials_updated_at = now()
    WHERE forge = ${forge'}
      AND login = ${ghLogin'}
  |]
  when (updated /= 1) $ throw $ NoSuchUser ghLogin'

-- | Forgets the credentials of an identity the forge stopped renewing; the
-- identity stays, and the next login stores new ones.
deleteIdentityCredentials :: ForgeLogin -> M ()
deleteIdentityCredentials (ForgeLogin forge' ghLogin') = do
  void
    $ pgExec
      [pgSQL|
    UPDATE forge_identities SET
      access_token = NULL,
      access_token_expires_at = NULL,
      refresh_token = NULL,
      refresh_token_expires_at = NULL,
      credentials_updated_at = now()
    WHERE forge = ${forge'}
      AND login = ${ghLogin'}
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

-- | Commits any identity of the account requested builds for.
getCommitsForReqUser :: User -> M [CommitSummary]
getCommitsForReqUser user = do
  let (forges, logins) = identityArrays $ user ^. identities
  map
    ( \( forge' :: ForgeSlug,
         repoOwner :: GhRepoOwner,
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
          CommitSummary forge' repoOwner repoName (RepoIsPublic isPublic) gitCommit branch reqUser startTime succeeded failed pending cancelled
    )
    <$> pgQuery
      [pgSQL|!
        WITH
        -- First we collect all of the commits for the user. We do this since
        -- this query has an index specifically to make this fast, and all
        -- future queries just operate on this or use the git_commit index.
        commits_for_req_user AS (
          SELECT git_commit, max(start_time) as commit_start_time FROM builds
          WHERE (forge, req_user) IN (SELECT * FROM unnest(${forges}::text[], ${logins}::text[]))
          GROUP BY git_commit
          ORDER BY commit_start_time DESC
          LIMIT 100
        ),

        -- Now we can find all the builds we care about efficiently by joining:
        all_related_builds AS (
          SELECT
            forge,
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
          WHERE (forge, req_user) IN (SELECT * FROM unnest(${forges}::text[], ${logins}::text[]))
        ),

        -- Now filter out re-runs using `SELECT DISTINCT`
        without_reruns AS (
          SELECT DISTINCT ON (git_commit, package_type, system, package) *
          FROM all_related_builds
          ORDER BY git_commit, package_type, system, package, start_time DESC
        )

        -- Finally, aggregate the status totals by git_commit
        SELECT
          (array_agg(forge))[1],
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

-- | One summary per repository that built the commit, newest first. A hash
-- alone does not name a repository: forks and mirrors on other forges share it.
getCommitSummaries :: CommitHash -> M [CommitSummary]
getCommitSummaries commit = do
  map
    ( \( forge' :: ForgeSlug,
         repoOwner :: GhRepoOwner,
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
          CommitSummary forge' repoOwner repoName (RepoIsPublic isPublic) gitCommit branch reqUser startTime succeeded failed pending cancelled
    )
    <$> pgQuery
      [pgSQL|!
        SELECT
          forge,
          repo_user,
          repo_name,
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
          SELECT DISTINCT ON (forge, repo_user, repo_name, git_commit, package_type, system, package) * FROM builds
          WHERE git_commit = ${commit}
          ORDER BY forge, repo_user, repo_name, git_commit, package_type, system, package, start_time DESC
        ) AS sub
        GROUP BY forge, repo_user, repo_name, git_commit
        ORDER BY min(start_time) DESC, forge, repo_user, repo_name
      |]

-- * Internal stuff

newBuildDB :: CommitInfo -> PackageInfo -> Text -> Bool -> M Build
newBuildDB commitInfo packageInfo evalHost wantsIncrementalism = do
  now <- liftIO getCurrentTime
  evalInstance <- view #evalInstance
  changes <-
    pgQueryPrism
      _Build
      [pgSQL|
    INSERT INTO builds
        (forge,
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
         start_time,
         wants_incrementalism,
         eval_host,
         eval_instance,
         uploaded_to_cache
        )
    VALUES
        (${commitInfo ^. (repoInfo . repoId . forge)},
         ${commitInfo ^. (repoInfo . repoId . repoUser)},
         ${commitInfo ^. (repoInfo . repoId . repoName)},
         ${commitInfo ^. prFromFork},
         ${commitInfo ^. branch},
         ${commitInfo ^. repoPublicity},
         ${commitInfo ^. commit},
         ${packageInfo ^. Garnix.Types.packageName},
         ${packageInfo ^. packageType},
         ${packageInfo ^. maybeSystem},
         ${commitInfo ^. (reqUser . ghLogin)},
         ${now},
         ${wantsIncrementalism},
         ${evalHost},
         ${evalInstance},
         FALSE
        )
    ON CONFLICT DO NOTHING
    RETURNING
      id,
      forge,
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

upsertServerHeartbeat :: [Text] -> M ()
upsertServerHeartbeat hosts =
  forM_ (map T.toLower hosts) $ \host -> do
    pgQuery
      [pgSQL|
    INSERT INTO server_heartbeat
      (hostname, last_heartbeat)
      VALUES (${host}, NOW())
    ON CONFLICT (hostname) DO UPDATE set last_heartbeat = NOW()
      |]

recordHeartbeatReport :: Duration -> M ()
recordHeartbeatReport maxGap = do
  let gapSeconds = toSeconds maxGap
  void
    $ pgExec
      [pgSQL|
        UPDATE heartbeat_reporting
        SET reports_recorded_since =
              CASE
                WHEN last_report_at IS NULL
                  OR last_report_at
                       < NOW() - (${gapSeconds}::double precision * interval '1 second')
                THEN NOW()
                ELSE reports_recorded_since
              END,
            last_report_at = NOW()
        WHERE id
      |]

heartbeatsCoverWindow :: Duration -> Duration -> M Bool
heartbeatsCoverWindow window maxGap = do
  let windowSeconds = toSeconds window
      gapSeconds = toSeconds maxGap
  result <-
    pgQuery
      [pgSQL|
        SELECT COALESCE(
          NOW() >= reports_recorded_since
                     + (${windowSeconds}::double precision * interval '1 second')
            AND last_report_at
                  >= NOW() - (${gapSeconds}::double precision * interval '1 second'),
          false)
        FROM heartbeat_reporting
        WHERE id
      |]
  case result of
    [Just covered] -> pure covered
    _ ->
      throw
        $ OtherError "heartbeatsCoverWindow: heartbeat_reporting is missing its singleton row"

getRecentServerHeartbeats :: M [Text]
getRecentServerHeartbeats =
  pgQuery
    [pgSQL|
  SELECT hostname
    FROM server_heartbeat
    WHERE NOW() - last_heartbeat < interval '12 hours'
    |]

-- * Eval ownership

upsertEvalHeartbeat :: M ()
upsertEvalHeartbeat = do
  hostname <- view #hostname
  instance_ <- view #evalInstance
  void
    $ pgExec
      [pgSQL|
        INSERT INTO eval_heartbeat
          (hostname, instance, last_beat)
        VALUES (${hostname}, ${instance_}, NOW())
        ON CONFLICT (hostname) DO UPDATE
          SET instance = ${instance_},
              last_beat = NOW()
      |]

getLiveEvalInstances :: M [Text]
getLiveEvalInstances = do
  window <- view #evalHeartbeatWindow
  let seconds = toSeconds window
  pgQuery
    [pgSQL|
      SELECT instance
      FROM eval_heartbeat
      WHERE NOW() - last_beat < (${seconds}::double precision * interval '1 second')
    |]

getOrphanedBuilds :: [Text] -> M [Build]
getOrphanedBuilds liveInstances =
  pgQuery
    [pgSQL|
      SELECT
        id,
        forge,
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
      WHERE end_time IS NULL
        AND (eval_instance IS NULL
             OR NOT (eval_instance = ANY(${liveInstances}::text[])))
      ORDER BY start_time
      LIMIT 200
    |]
    <&> map
      ( \( id,
           buildForge,
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
                _buildForge = buildForge,
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

getOrphanedRuns :: [Text] -> M [(Run, Maybe GhRunId)]
getOrphanedRuns liveInstances =
  pgQuery
    [pgSQL|
      SELECT id, name, forge, repo_user, repo_name, git_commit, branch, status,
             req_user, start_time, end_time, github_run_id
      FROM runs
      WHERE end_time IS NULL
        AND (eval_instance IS NULL
             OR NOT (eval_instance = ANY(${liveInstances}::text[])))
      ORDER BY start_time
      LIMIT 200
    |]
    <&> map
      ( \(id, name, runForge, repoOwner, repoName, gitCommit, branch, status, reqUser, startTime, endTime, githubRunId) ->
          ( Run
              { _runId = id,
                _runName = name,
                _runForge = runForge,
                _runRepoUser = repoOwner,
                _runRepoName = repoName,
                _runGitCommit = gitCommit,
                _runBranch = branch,
                _runStatus = status,
                _runReqUser = reqUser,
                _runStartTime = startTime,
                _runEndTime = endTime
              },
            githubRunId
          )
      )

getStuckMetaChecks :: [Text] -> M [(RepoId, CommitHash)]
getStuckMetaChecks liveInstances =
  map (\(forge', owner, name, commit) -> (RepoId forge' owner name, commit))
    <$> pgQuery
    [pgSQL|
      SELECT c.forge, c.repo_user, c.repo_name, c.git_commit
      FROM commits c
      WHERE c.status = 'evaluated'
        AND c.meta_check = 'pending'
        AND (c.eval_instance IS NULL
             OR NOT (c.eval_instance = ANY(${liveInstances}::text[])))
        AND NOT EXISTS (
          SELECT 1
          FROM builds b
          WHERE b.forge = c.forge
            AND b.repo_user = c.repo_user
            AND b.repo_name = c.repo_name
            AND b.git_commit = c.git_commit
            AND b.end_time IS NULL
        )
      ORDER BY c.started_at
      LIMIT 200
    |]

getOrphanedEvaluations :: [Text] -> M [(RepoId, CommitHash)]
getOrphanedEvaluations liveInstances =
  map (\(forge', owner, name, commit) -> (RepoId forge' owner name, commit))
    <$> pgQuery
    [pgSQL|
      SELECT forge, repo_user, repo_name, git_commit
      FROM commits
      WHERE status = 'evaluating'
        AND (eval_instance IS NULL
             OR NOT (eval_instance = ANY(${liveInstances}::text[])))
      ORDER BY started_at
      LIMIT 200
    |]

setRunGithubId :: RunId -> GhRunId -> M ()
setRunGithubId runId ghRunId =
  void
    $ pgExec
      [pgSQL|
        UPDATE runs
        SET github_run_id = ${ghRunId}
        WHERE id = ${runId}
      |]

getPrDeployDurationForOwner :: ForgeSlug -> GhRepoOwner -> M Duration
getPrDeployDurationForOwner forge' owner = do
  res <-
    pgQuery
      [pgSQL|
        SELECT
          SUM(date_part('EPOCH',
            ( COALESCE(servers.ended_at, NOW()) -
              GREATEST(servers.ready_at, date_trunc('month', NOW(), 'UTC'))
            )
          )) as _server_seconds
        FROM servers
        INNER JOIN builds
        ON servers.configuration_build_id = builds.id
        WHERE builds.forge = ${forge'}
        AND builds.repo_user = ${owner}
        AND servers.pull_request IS NOT NULL
        AND servers.ready_at IS NOT NULL
        AND (servers.ended_at IS NULL OR
             servers.ended_at >= date_trunc('month', NOW(), 'UTC'));
      |]
  case res of
    [Just serverSeconds] -> pure $ fromSeconds @Double serverSeconds
    [Nothing] -> pure emptyDuration
    _ -> throw $ OtherError "Impossible: more than one result"

getCurrentMonthUsages ::
  ForgeSlug ->
  [GhRepoOwner] ->
  M (Map GhRepoOwner Duration)
getCurrentMonthUsages forge' owners = do
  fromList
    . map (\(repoOwner :: GhRepoOwner, seconds :: Maybe Double) -> (repoOwner, fromSeconds $ fromMaybe 0 seconds))
    <$> pgQuery
      [pgSQL|
        SELECT
          repo_user,
          SUM(LEAST(120 * 60, date_part('EPOCH', (end_time - start_time)))) AS total_build_time
        FROM builds
        WHERE forge = ${forge'}
        AND repo_user = ANY(${owners})
        AND end_time >= date_trunc('month', NOW())
        GROUP BY repo_user
      |]

getRepoKeyDB :: RepoId -> M (Maybe (PublicKey, PrivateKey))
getRepoKeyDB (RepoId forge' owner name) = do
  results <-
    pgQuery
      [pgSQL|
        SELECT public_key, private_key
        FROM repo_secrets
        WHERE forge = ${forge'}
        AND repo_user = ${owner}
        AND repo_name = ${name}
    |]
  case results of
    [result] -> pure $ Just result
    [] -> pure Nothing
    _ -> throw $ OtherError "Impossible: Got more than 1 result from getRepoKeyDB"

-- | In case of conflict, we return the key already in the DB, to prevent
-- overwriting
setRepoKeyDB ::
  RepoId ->
  Candidate PublicKey ->
  Candidate PrivateKey ->
  M (PublicKey, PrivateKey)
setRepoKeyDB repo@(RepoId forge' owner name) (Candidate pub) (Candidate priv) = do
  void
    $ pgQuery
      [pgSQL|
     INSERT INTO repo_secrets
       ( forge,
         repo_user,
         repo_name,
         public_key,
         private_key
       )
     VALUES
       ( ${forge'},
         ${owner},
         ${name},
         ${pub},
         ${priv}
       )
     ON CONFLICT DO NOTHING
     |]
  getRepoKeyDB repo >>= \case
    Nothing -> throw $ OtherError "Impossible setRepoKeyDB: expected a set key"
    Just v -> pure v

getActionKeyDB :: RepoId -> PackageName -> M (Maybe (PublicKey, PrivateKey))
getActionKeyDB (RepoId forge' owner name) action = do
  results <-
    pgQuery
      [pgSQL|
        SELECT public_key, private_key
        FROM action_secrets
        WHERE forge = ${forge'}
        AND repo_user = ${owner}
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
  RepoId ->
  PackageName ->
  Candidate PublicKey ->
  Candidate PrivateKey ->
  M (PublicKey, PrivateKey)
setActionKeyDB repo@(RepoId forge' owner name) action (Candidate pub) (Candidate priv) = do
  void
    $ pgQuery
      [pgSQL|
     INSERT INTO action_secrets
       ( forge,
         repo_user,
         repo_name,
         action_name,
         public_key,
         private_key
       )
     VALUES
       ( ${forge'},
         ${owner},
         ${name},
         ${action},
         ${pub},
         ${priv}
       )
     ON CONFLICT DO NOTHING
     |]
  getActionKeyDB repo action >>= \case
    Nothing -> throw $ OtherError "Impossible setRepoKeyDB: expected a set key"
    Just v -> pure v

isDenylisted :: RepoId -> M Bool
isDenylisted (RepoId forge' owner name) = do
  result :: [Text] <-
    pgQuery
      [pgSQL|
        SELECT repo_user
        FROM denylist
        WHERE forge = ${forge'}
        AND repo_user = ${owner}
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
            (forge,
             repo_user,
             repo_name,
             package,
             package_type,
             system,
             git_commit
             )
          id,
          forge,
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
                AND forge = ${build ^. forge}
                AND repo_user = ${build ^. repoUser}
                AND repo_name = ${build ^. repoName}
              GROUP BY git_commit
              HAVING
                bool_and(CASE WHEN end_time IS NULL THEN FALSE else TRUE END)
              ORDER BY ARRAY_POSITION(${commits}, git_commit)
              LIMIT 1)
          AND forge = ${build ^. forge}
          AND repo_user = ${build ^. repoUser}
          AND repo_name = ${build ^. repoName}
        ORDER BY
          forge,
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

getUserInternalToken :: ForgeLogin -> M InternalCacheToken
getUserInternalToken (ForgeLogin forge' reqUser) =
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
            WHERE forge = ${forge'}
              AND github_login = ${reqUser}
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
            (forge, github_login, internal_token)
          VALUES (${forge'}, ${reqUser}, ${rawToken})
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

-- | 'pgTransaction', or, inside one already, that one.
withinTransaction :: M a -> M a
withinTransaction inner =
  view #dbConn >>= \case
    Transaction _ -> inner
    ConnectionPool _ -> pgTransaction inner

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
