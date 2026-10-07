-- Deploy garnix:forge_qualified_keys to pg
-- requires: init
-- requires: deploy_pr_comments

BEGIN;

-- Every row that names a repository or an account also names the forge
-- instance it lives on, so that `acme/site` on GitHub and `acme/site` on a
-- Gitea server never share a build, a commit, a key or an account. The column
-- holds a forge slug (an instance name such as 'github' or 'git.example'),
-- which is open-ended, hence text rather than an enum. The default backfills
-- every existing row with the only forge garnix knew before this change.

ALTER TABLE builds ADD COLUMN forge text DEFAULT 'github' NOT NULL;
ALTER TABLE runs ADD COLUMN forge text DEFAULT 'github' NOT NULL;
ALTER TABLE commits ADD COLUMN forge text DEFAULT 'github' NOT NULL;
ALTER TABLE pushes ADD COLUMN forge text DEFAULT 'github' NOT NULL;
ALTER TABLE repo_config ADD COLUMN forge text DEFAULT 'github' NOT NULL;
ALTER TABLE repo_secrets ADD COLUMN forge text DEFAULT 'github' NOT NULL;
ALTER TABLE action_secrets ADD COLUMN forge text DEFAULT 'github' NOT NULL;
ALTER TABLE cache_store_hash_tags ADD COLUMN forge text DEFAULT 'github' NOT NULL;
ALTER TABLE denylist ADD COLUMN forge text DEFAULT 'github' NOT NULL;
ALTER TABLE modules ADD COLUMN forge text DEFAULT 'github' NOT NULL;
ALTER TABLE deploy_comments ADD COLUMN forge text DEFAULT 'github' NOT NULL;
ALTER TABLE users ADD COLUMN forge text DEFAULT 'github' NOT NULL;
ALTER TABLE internal_access_tokens ADD COLUMN forge text DEFAULT 'github' NOT NULL;
ALTER TABLE module_user_repo ADD COLUMN forge text DEFAULT 'github' NOT NULL;

ALTER TABLE commits DROP CONSTRAINT commits_pkey;
ALTER TABLE commits ADD CONSTRAINT commits_pkey PRIMARY KEY (forge, repo_user, repo_name, git_commit);

ALTER TABLE pushes DROP CONSTRAINT pushes_pkey;
ALTER TABLE pushes ADD CONSTRAINT pushes_pkey PRIMARY KEY (forge, repo_user, repo_name, git_commit, branch);

ALTER TABLE repo_config DROP CONSTRAINT repo_config_pkey;
ALTER TABLE repo_config ADD CONSTRAINT repo_config_pkey PRIMARY KEY (forge, repo_user, repo_name);

ALTER TABLE repo_secrets DROP CONSTRAINT repo_secrets_pkey;
ALTER TABLE repo_secrets ADD CONSTRAINT repo_secrets_pkey PRIMARY KEY (forge, repo_user, repo_name);

ALTER TABLE action_secrets DROP CONSTRAINT action_secrets_pkey;
ALTER TABLE action_secrets ADD CONSTRAINT action_secrets_pkey PRIMARY KEY (forge, repo_user, repo_name, action_name);

ALTER TABLE cache_store_hash_tags DROP CONSTRAINT cache_store_hash_tags_hash_repo_owner_repo_name_key;
ALTER TABLE cache_store_hash_tags
    ADD CONSTRAINT cache_store_hash_tags_hash_forge_repo_owner_repo_name_key UNIQUE (hash, forge, repo_owner, repo_name);

ALTER TABLE modules DROP CONSTRAINT modules_repo_user_repo_name_git_commit_key;
ALTER TABLE modules
    ADD CONSTRAINT modules_forge_repo_user_repo_name_git_commit_key UNIQUE (forge, repo_user, repo_name, git_commit);

DROP INDEX deploy_comments_url_idx;
CREATE UNIQUE INDEX deploy_comments_url_idx
    ON deploy_comments (forge, repo_user, repo_name, pull_request)
    WHERE kind = 'url';

DROP INDEX deploy_comments_failure_idx;
CREATE UNIQUE INDEX deploy_comments_failure_idx
    ON deploy_comments (forge, repo_user, repo_name, pull_request, git_commit)
    WHERE kind = 'failure';

DROP INDEX builds_repo_owner_name;
CREATE INDEX builds_forge_repo_owner_name ON builds USING btree (forge, repo_user, repo_name);

-- Accounts: the same login on two forges is two accounts.
ALTER TABLE internal_access_tokens DROP CONSTRAINT internal_access_tokens_pkey;
ALTER TABLE internal_access_tokens ADD CONSTRAINT internal_access_tokens_pkey PRIMARY KEY (forge, github_login);

ALTER TABLE module_user_repo DROP CONSTRAINT module_user_repo_github_login_fkey;
ALTER TABLE module_user_repo DROP CONSTRAINT module_user_repo_github_login_key;
ALTER TABLE users DROP CONSTRAINT users_github_login_key;
ALTER TABLE users ADD CONSTRAINT users_forge_github_login_key UNIQUE (forge, github_login);
-- One account per login on each forge, so one per email on each forge too:
-- the same person signs up on two forges with the same address.
ALTER TABLE users DROP CONSTRAINT users_email_key;
ALTER TABLE users ADD CONSTRAINT users_forge_email_key UNIQUE (forge, email);
ALTER TABLE module_user_repo ADD CONSTRAINT module_user_repo_forge_github_login_key UNIQUE (forge, github_login);
ALTER TABLE module_user_repo
    ADD CONSTRAINT module_user_repo_forge_github_login_fkey
    FOREIGN KEY (forge, github_login) REFERENCES users(forge, github_login);

COMMIT;
