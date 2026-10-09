-- Deploy garnix:forge_installations to pg
-- requires: forge_qualified_keys
-- requires: registered_forges

BEGIN;

-- Every installation has its own webhook secret; the one a registered forge
-- was given at registration signed nothing anymore.
ALTER TABLE forges DROP COLUMN webhook_secret;

-- garnix installed on a Gitea/Forgejo account, organisation or whole
-- instance: one webhook garnix created there with the installer's token.
CREATE TABLE forge_installations (
    id serial NOT NULL,
    forge text NOT NULL,
    target_kind text NOT NULL,
    -- the account or organisation login; '' for the instance
    target text NOT NULL,
    -- the identity whose OAuth token garnix calls the forge with; NULL once
    -- that identity is gone
    installer_login character varying,
    -- the webhook garnix created on the forge, once it has
    hook_id bigint,
    -- age-encrypted with the instance key, like user OAuth tokens
    webhook_secret bytea NOT NULL,
    -- why builds stopped until someone reinstalls it, if they did
    needs_reinstall text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT forge_installations_pkey PRIMARY KEY (id),
    -- what forge_installation_repos refers to, so its forge is the
    -- installation's
    CONSTRAINT forge_installations_id_forge_key UNIQUE (id, forge),
    CONSTRAINT forge_installations_target_kind_check CHECK (target_kind IN ('user', 'org', 'instance')),
    CONSTRAINT forge_installations_instance_target_check CHECK ((target_kind = 'instance') = (target = '')),
    -- one without an installer builds nothing, and says why
    CONSTRAINT forge_installations_installer_check CHECK (installer_login IS NOT NULL OR needs_reinstall IS NOT NULL)
);

-- Gitea compares logins case-insensitively.
CREATE UNIQUE INDEX forge_installations_target_key
    ON forge_installations (forge, target_kind, lower(target));

-- The repositories an installation builds. A repository is built through at
-- most one installation, however many of its webhooks deliver its events.
CREATE TABLE forge_installation_repos (
    installation_id integer NOT NULL,
    forge text NOT NULL,
    repo_owner character varying NOT NULL,
    repo_name character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT forge_installation_repos_installation_fkey
        FOREIGN KEY (installation_id, forge) REFERENCES forge_installations (id, forge) ON DELETE CASCADE
);

CREATE UNIQUE INDEX forge_installation_repos_repo_key
    ON forge_installation_repos (forge, lower(repo_owner), lower(repo_name));

-- An installation whose installer's identity goes (disconnected, its account
-- deleted, or forgotten with its forge) stops building until someone
-- reinstalls it. A trigger, so that cascades from users are caught too.
CREATE FUNCTION forge_installations_installer_gone() RETURNS trigger AS $$
BEGIN
    UPDATE forge_installations
    SET installer_login = NULL,
        needs_reinstall = 'the identity of whoever installed it is no longer connected to garnix',
        updated_at = now()
    WHERE forge = OLD.forge AND installer_login = OLD.login;
    RETURN OLD;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER forge_identities_installer_gone
    AFTER DELETE ON forge_identities
    FOR EACH ROW EXECUTE FUNCTION forge_installations_installer_gone();

COMMIT;
