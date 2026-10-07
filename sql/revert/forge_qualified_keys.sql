-- Revert garnix:forge_qualified_keys from pg
--
-- This refuses, and changes nothing, once any row belongs to a forge other
-- than github. Dropping the column would silently turn those rows into github
-- rows: a Gitea account would become the github account of the same login,
-- and a Gitea repository's builds would join the github repository's history.

BEGIN;

-- RAISE rather than ASSERT: plpgsql.check_asserts = off would skip an ASSERT.
DO $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM builds WHERE forge <> 'github'
        UNION ALL SELECT 1 FROM runs WHERE forge <> 'github'
        UNION ALL SELECT 1 FROM commits WHERE forge <> 'github'
        UNION ALL SELECT 1 FROM pushes WHERE forge <> 'github'
        UNION ALL SELECT 1 FROM repo_config WHERE forge <> 'github'
        UNION ALL SELECT 1 FROM repo_secrets WHERE forge <> 'github'
        UNION ALL SELECT 1 FROM action_secrets WHERE forge <> 'github'
        UNION ALL SELECT 1 FROM cache_store_hash_tags WHERE forge <> 'github'
        UNION ALL SELECT 1 FROM denylist WHERE forge <> 'github'
        UNION ALL SELECT 1 FROM modules WHERE forge <> 'github'
        UNION ALL SELECT 1 FROM deploy_comments WHERE forge <> 'github'
        UNION ALL SELECT 1 FROM users WHERE forge <> 'github'
        UNION ALL SELECT 1 FROM internal_access_tokens WHERE forge <> 'github'
        UNION ALL SELECT 1 FROM module_user_repo WHERE forge <> 'github'
    ) THEN
        RAISE EXCEPTION 'rows from forges other than github exist; reverting would turn them into github rows';
    END IF;
END
$$;

ALTER TABLE module_user_repo DROP CONSTRAINT module_user_repo_forge_github_login_fkey;
ALTER TABLE module_user_repo DROP CONSTRAINT module_user_repo_forge_github_login_key;
ALTER TABLE users DROP CONSTRAINT users_forge_email_key;
ALTER TABLE users ADD CONSTRAINT users_email_key UNIQUE (email);
ALTER TABLE users DROP CONSTRAINT users_forge_github_login_key;
ALTER TABLE users ADD CONSTRAINT users_github_login_key UNIQUE (github_login);
ALTER TABLE module_user_repo ADD CONSTRAINT module_user_repo_github_login_key UNIQUE (github_login);
ALTER TABLE module_user_repo
    ADD CONSTRAINT module_user_repo_github_login_fkey
    FOREIGN KEY (github_login) REFERENCES users(github_login);

ALTER TABLE internal_access_tokens DROP CONSTRAINT internal_access_tokens_pkey;
ALTER TABLE internal_access_tokens ADD CONSTRAINT internal_access_tokens_pkey PRIMARY KEY (github_login);

DROP INDEX builds_forge_repo_owner_name;
CREATE INDEX builds_repo_owner_name ON builds USING btree (repo_user, repo_name);

DROP INDEX deploy_comments_failure_idx;
CREATE UNIQUE INDEX deploy_comments_failure_idx
    ON deploy_comments (repo_user, repo_name, pull_request, git_commit)
    WHERE kind = 'failure';

DROP INDEX deploy_comments_url_idx;
CREATE UNIQUE INDEX deploy_comments_url_idx
    ON deploy_comments (repo_user, repo_name, pull_request)
    WHERE kind = 'url';

ALTER TABLE modules DROP CONSTRAINT modules_forge_repo_user_repo_name_git_commit_key;
ALTER TABLE modules
    ADD CONSTRAINT modules_repo_user_repo_name_git_commit_key UNIQUE (repo_user, repo_name, git_commit);

ALTER TABLE cache_store_hash_tags DROP CONSTRAINT cache_store_hash_tags_hash_forge_repo_owner_repo_name_key;
ALTER TABLE cache_store_hash_tags
    ADD CONSTRAINT cache_store_hash_tags_hash_repo_owner_repo_name_key UNIQUE (hash, repo_owner, repo_name);

ALTER TABLE action_secrets DROP CONSTRAINT action_secrets_pkey;
ALTER TABLE action_secrets ADD CONSTRAINT action_secrets_pkey PRIMARY KEY (repo_user, repo_name, action_name);

ALTER TABLE repo_secrets DROP CONSTRAINT repo_secrets_pkey;
ALTER TABLE repo_secrets ADD CONSTRAINT repo_secrets_pkey PRIMARY KEY (repo_user, repo_name);

ALTER TABLE repo_config DROP CONSTRAINT repo_config_pkey;
ALTER TABLE repo_config ADD CONSTRAINT repo_config_pkey PRIMARY KEY (repo_user, repo_name);

ALTER TABLE pushes DROP CONSTRAINT pushes_pkey;
ALTER TABLE pushes ADD CONSTRAINT pushes_pkey PRIMARY KEY (repo_user, repo_name, git_commit, branch);

ALTER TABLE commits DROP CONSTRAINT commits_pkey;
ALTER TABLE commits ADD CONSTRAINT commits_pkey PRIMARY KEY (repo_user, repo_name, git_commit);

ALTER TABLE module_user_repo DROP COLUMN forge;
ALTER TABLE internal_access_tokens DROP COLUMN forge;
ALTER TABLE users DROP COLUMN forge;
ALTER TABLE deploy_comments DROP COLUMN forge;
ALTER TABLE modules DROP COLUMN forge;
ALTER TABLE denylist DROP COLUMN forge;
ALTER TABLE cache_store_hash_tags DROP COLUMN forge;
ALTER TABLE action_secrets DROP COLUMN forge;
ALTER TABLE repo_secrets DROP COLUMN forge;
ALTER TABLE repo_config DROP COLUMN forge;
ALTER TABLE pushes DROP COLUMN forge;
ALTER TABLE commits DROP COLUMN forge;
ALTER TABLE runs DROP COLUMN forge;
ALTER TABLE builds DROP COLUMN forge;

COMMIT;
