-- Verify garnix:forge_qualified_keys on pg

BEGIN;

DO $$
BEGIN
    ASSERT (
        SELECT count(*) FROM information_schema.columns
        WHERE table_schema = 'public'
          AND column_name = 'forge'
          AND is_nullable = 'NO'
          AND table_name IN ('builds', 'runs', 'commits', 'pushes', 'repo_config',
                             'repo_secrets', 'action_secrets', 'cache_store_hash_tags',
                             'denylist', 'modules', 'deploy_comments', 'users',
                             'internal_access_tokens', 'module_user_repo')
    ) = 14, 'a table is missing its forge column';

    ASSERT (
        SELECT count(*) FROM information_schema.table_constraints
        WHERE table_schema = 'public'
          AND constraint_name IN ('commits_pkey', 'pushes_pkey', 'repo_config_pkey',
                                  'repo_secrets_pkey', 'action_secrets_pkey',
                                  'internal_access_tokens_pkey')
          AND constraint_type = 'PRIMARY KEY'
    ) = 6, 'a forge-qualified primary key is missing';

    ASSERT (
        SELECT count(*) FROM information_schema.key_column_usage
        WHERE table_schema = 'public'
          AND column_name = 'forge'
          AND constraint_name IN ('commits_pkey', 'pushes_pkey', 'repo_config_pkey',
                                  'repo_secrets_pkey', 'action_secrets_pkey',
                                  'internal_access_tokens_pkey',
                                  'cache_store_hash_tags_hash_forge_repo_owner_repo_name_key',
                                  'modules_forge_repo_user_repo_name_git_commit_key',
                                  'users_forge_github_login_key',
                                  'users_forge_email_key',
                                  'module_user_repo_forge_github_login_key')
    ) = 11, 'a key does not include the forge';

    ASSERT EXISTS (
        SELECT 1 FROM information_schema.table_constraints
        WHERE table_schema = 'public'
          AND table_name = 'module_user_repo'
          AND constraint_name = 'module_user_repo_forge_github_login_fkey'
          AND constraint_type = 'FOREIGN KEY'
    ), 'module_user_repo is not tied to the forge-qualified users key';

    ASSERT EXISTS (
        SELECT 1 FROM pg_indexes
        WHERE schemaname = 'public'
          AND indexname = 'deploy_comments_url_idx'
          AND indexdef LIKE '%(forge, repo_user, repo_name, pull_request)%'
    ), 'deploy_comments_url_idx does not include the forge';

    ASSERT EXISTS (
        SELECT 1 FROM pg_indexes
        WHERE schemaname = 'public'
          AND indexname = 'deploy_comments_failure_idx'
          AND indexdef LIKE '%(forge, repo_user, repo_name, pull_request, git_commit)%'
    ), 'deploy_comments_failure_idx does not include the forge';

    ASSERT EXISTS (
        SELECT 1 FROM pg_indexes
        WHERE schemaname = 'public'
          AND indexname = 'builds_forge_repo_owner_name'
    ), 'the builds_forge_repo_owner_name index is missing';
END $$;

ROLLBACK;
