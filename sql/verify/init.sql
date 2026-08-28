BEGIN;

SELECT 'success'::build_status;
SELECT 'pending'::check_status;
SELECT 'evaluating'::commit_status;
SELECT 'package'::package_type;
SELECT 'free'::subscription_type;
SELECT 'x86_64-linux'::system;
SELECT 'citext'::citext;

SELECT last_value FROM access_tokens_id_seq;
SELECT last_value FROM builds_id_seq;
SELECT last_value FROM module_user_repo_id_seq;
SELECT last_value FROM modules_id_seq;
SELECT last_value FROM runs_id_seq;
SELECT last_value FROM users_id_seq;
SELECT last_value FROM waitlist_id_seq;

SELECT id, name, token, user_id, created_at, last_used, scope_cache, scope_api
    FROM access_tokens WHERE FALSE;

SELECT repo_user, repo_name, action_name, private_key, public_key
    FROM action_secrets WHERE FALSE;

SELECT repo_user, repo_name, git_commit, package, status, start_time, end_time,
       drv_path, github_run_id, extra_message, package_type, req_user,
       installation_id, system, id, repo_is_public, pr_from_fork, branch,
       persistence_name, eval_host, wants_incrementalism, uploaded_to_cache,
       output_paths, already_built
    FROM builds WHERE FALSE;

SELECT hash, repo_owner, repo_name
    FROM cache_store_hash_tags WHERE FALSE;

SELECT hash, created_at, accessed_at, package_name, nar_hash, nar_size, public,
       sig, "references", file_size, file_hash, uploaded_at
    FROM cache_store_hashes WHERE FALSE;

SELECT repo_user, repo_name, git_commit, status, meta_check
    FROM commits WHERE FALSE;

SELECT repo_user, repo_name
    FROM denylist WHERE FALSE;

SELECT created_at, config
    FROM feature_flags WHERE FALSE;

SELECT hostname, last_heartbeat
    FROM heartbeat WHERE FALSE;

SELECT repo_owner
    FROM installations WHERE FALSE;

SELECT github_login, internal_token
    FROM internal_access_tokens WHERE FALSE;

SELECT id, github_login, repo_user, repo_name
    FROM module_user_repo WHERE FALSE;

SELECT module_user_repo_id, module_id, "values"
    FROM module_values WHERE FALSE;

SELECT id, repo_user, repo_name, git_commit, schema, enabled, name, description
    FROM modules WHERE FALSE;

SELECT repo_user, repo_name, git_commit, branch, pushed_at
    FROM pushes WHERE FALSE;

SELECT repo_user, repo_name, skip_private_inputs_check_for_collaborators,
       max_eval_memory, private_cache
    FROM repo_config WHERE FALSE;

SELECT repo_user, repo_name, private_key, public_key
    FROM repo_secrets WHERE FALSE;

SELECT id, name, repo_user, repo_name, git_commit, branch, status, req_user,
       start_time, end_time
    FROM runs WHERE FALSE;

SELECT id, github_login, email, subscription_type, created_at, agree_to_emails
    FROM users WHERE FALSE;

SELECT created_at, drv_hash, store_path_hash
    FROM verified_fods WHERE FALSE;

SELECT id, email, created_at
    FROM waitlist WHERE FALSE;

ROLLBACK;
