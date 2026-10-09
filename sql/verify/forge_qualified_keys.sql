-- Verify garnix:forge_qualified_keys on pg

BEGIN;

-- RAISE rather than ASSERT: plpgsql.check_asserts = off would skip an ASSERT.
DO $$
BEGIN
    IF NOT ((
        SELECT count(*) FROM information_schema.columns
        WHERE table_schema = 'public'
          AND column_name = 'forge'
          AND is_nullable = 'NO'
          AND table_name IN ('builds', 'runs', 'commits', 'pushes', 'repo_config',
                             'repo_secrets', 'action_secrets', 'cache_store_hash_tags',
                             'denylist', 'modules', 'deploy_comments',
                             'internal_access_tokens', 'module_user_repo')
    ) = 13
    ) THEN
        RAISE EXCEPTION 'a table is missing its forge column';
    END IF;

    IF NOT ((
        SELECT count(*) FROM information_schema.table_constraints
        WHERE table_schema = 'public'
          AND constraint_name IN ('commits_pkey', 'pushes_pkey', 'repo_config_pkey',
                                  'repo_secrets_pkey', 'action_secrets_pkey',
                                  'internal_access_tokens_pkey')
          AND constraint_type = 'PRIMARY KEY'
    ) = 6
    ) THEN
        RAISE EXCEPTION 'a forge-qualified primary key is missing';
    END IF;

    IF NOT ((
        SELECT count(*) FROM information_schema.key_column_usage
        WHERE table_schema = 'public'
          AND column_name = 'forge'
          AND constraint_name IN ('commits_pkey', 'pushes_pkey', 'repo_config_pkey',
                                  'repo_secrets_pkey', 'action_secrets_pkey',
                                  'internal_access_tokens_pkey',
                                  'cache_store_hash_tags_hash_forge_repo_owner_repo_name_key',
                                  'modules_forge_repo_user_repo_name_git_commit_key',
                                  'module_user_repo_forge_github_login_key')
    ) = 9
    ) THEN
        RAISE EXCEPTION 'a key does not include the forge';
    END IF;

    IF NOT (NOT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = 'users'
          AND column_name IN ('github_login', 'forge')
    )
    ) THEN
        RAISE EXCEPTION 'users still names a login: logins belong to forge_identities';
    END IF;

    IF NOT (NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = 'github_user_credentials'
    )
    ) THEN
        RAISE EXCEPTION 'credentials still live apart from the identities';
    END IF;

    IF NOT ((
        SELECT count(*) FROM information_schema.table_constraints
        WHERE table_schema = 'public'
          AND table_name = 'forge_identities'
          AND constraint_name IN ('forge_identities_pkey', 'forge_identities_user_id_forge_key')
    ) = 2
    ) THEN
        RAISE EXCEPTION 'forge_identities is missing a key';
    END IF;

    IF EXISTS (
        SELECT 1 FROM information_schema.table_constraints
        WHERE table_schema = 'public' AND constraint_name = 'users_email_key'
    ) THEN
        RAISE EXCEPTION 'emails are still unique: they never identify an account';
    END IF;

    PERFORM user_id, forge, login, is_forge_admin, access_token,
            access_token_expires_at, refresh_token, refresh_token_expires_at,
            credentials_updated_at, created_at
    FROM forge_identities WHERE false;

    IF NOT (EXISTS (
        SELECT 1 FROM information_schema.table_constraints
        WHERE table_schema = 'public'
          AND table_name = 'module_user_repo'
          AND constraint_name = 'module_user_repo_forge_github_login_fkey'
          AND constraint_type = 'FOREIGN KEY'
    )
    ) THEN
        RAISE EXCEPTION 'module_user_repo is not tied to an identity';
    END IF;

    IF NOT (EXISTS (
        SELECT 1 FROM pg_indexes
        WHERE schemaname = 'public'
          AND indexname = 'deploy_comments_url_idx'
          AND indexdef LIKE '%(forge, repo_user, repo_name, pull_request)%'
    )
    ) THEN
        RAISE EXCEPTION 'deploy_comments_url_idx does not include the forge';
    END IF;

    IF NOT (EXISTS (
        SELECT 1 FROM pg_indexes
        WHERE schemaname = 'public'
          AND indexname = 'deploy_comments_failure_idx'
          AND indexdef LIKE '%(forge, repo_user, repo_name, pull_request, git_commit)%'
    )
    ) THEN
        RAISE EXCEPTION 'deploy_comments_failure_idx does not include the forge';
    END IF;

    IF NOT (EXISTS (
        SELECT 1 FROM pg_indexes
        WHERE schemaname = 'public'
          AND indexname = 'builds_forge_repo_owner_name'
    )
    ) THEN
        RAISE EXCEPTION 'the builds_forge_repo_owner_name index is missing';
    END IF;
END $$;

ROLLBACK;
