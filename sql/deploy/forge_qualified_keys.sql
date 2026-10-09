-- Deploy garnix:forge_qualified_keys to pg
-- requires: init
-- requires: deploy_pr_comments
-- requires: github_user_credentials

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

-- Logins: the same login on two forges is two different people, so whatever
-- garnix keeps per login is keyed by the forge too.
ALTER TABLE internal_access_tokens DROP CONSTRAINT internal_access_tokens_pkey;
ALTER TABLE internal_access_tokens ADD CONSTRAINT internal_access_tokens_pkey PRIMARY KEY (forge, github_login);

-- Accounts: one account holds at most one identity on each forge, and an
-- identity belongs to one account. The OAuth credentials are the identity's,
-- as each forge issues its own; they are null once a forge refused to renew
-- them, until the next login. Every existing account is a github login.
CREATE TABLE forge_identities (
    user_id integer NOT NULL,
    forge text NOT NULL,
    login character varying NOT NULL,
    -- What the forge said at the last login: on github.com this means GitHub
    -- staff, so it grants nothing there by itself.
    is_forge_admin boolean DEFAULT false NOT NULL,
    access_token bytea,
    access_token_expires_at timestamp with time zone,
    refresh_token bytea,
    refresh_token_expires_at timestamp with time zone,
    credentials_updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT forge_identities_pkey PRIMARY KEY (forge, login),
    CONSTRAINT forge_identities_user_id_forge_key UNIQUE (user_id, forge),
    CONSTRAINT forge_identities_user_id_fkey
        FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

INSERT INTO forge_identities
    (user_id, forge, login, access_token, access_token_expires_at,
     refresh_token, refresh_token_expires_at, credentials_updated_at)
SELECT users.id, 'github', users.github_login,
       credentials.access_token, credentials.access_token_expires_at,
       credentials.refresh_token, credentials.refresh_token_expires_at,
       credentials.updated_at
FROM users
LEFT JOIN github_user_credentials AS credentials ON credentials.user_id = users.id;

DROP TABLE github_user_credentials;

-- Module settings belong to an identity (modules only exist on github.com).
ALTER TABLE module_user_repo DROP CONSTRAINT module_user_repo_github_login_fkey;
ALTER TABLE module_user_repo DROP CONSTRAINT module_user_repo_github_login_key;
ALTER TABLE module_user_repo ADD CONSTRAINT module_user_repo_forge_github_login_key UNIQUE (forge, github_login);
ALTER TABLE module_user_repo
    ADD CONSTRAINT module_user_repo_forge_github_login_fkey
    FOREIGN KEY (forge, github_login) REFERENCES forge_identities(forge, login);

ALTER TABLE users DROP CONSTRAINT users_github_login_key;
ALTER TABLE users DROP COLUMN github_login;

-- An email is contact data only: identities on two forges may report the same
-- address for two different accounts, and an address never links accounts.
ALTER TABLE users DROP CONSTRAINT users_email_key;

COMMIT;
