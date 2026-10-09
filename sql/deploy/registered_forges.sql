-- Deploy garnix:registered_forges to pg
-- requires: init

BEGIN;

-- Gitea/Forgejo instances people registered through the garnix UI, besides
-- the ones in the forges file. One row per host, shared by everyone on it.
CREATE TABLE forges (
    slug text NOT NULL,
    kind text NOT NULL,
    web_url text NOT NULL,
    api_url text NOT NULL,
    oauth_client_id text NOT NULL,
    -- age-encrypted with the instance key, like user OAuth tokens
    oauth_client_secret bytea NOT NULL,
    webhook_secret bytea NOT NULL,
    status text NOT NULL,
    -- the garnix account that completed its first OAuth; it stays the
    -- registrant whichever identities it later holds
    registered_by integer,
    -- sha256 (hex) of the token in the cookie of the browser that submitted
    -- a pending registration: only that browser may activate it
    registration_token_hash text,
    -- when it was last disabled: registering its host again within 30 days
    -- brings everyone back, later forgets whoever logged in through it
    disabled_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT forges_pkey PRIMARY KEY (slug),
    CONSTRAINT forges_kind_check CHECK (kind IN ('gitea')),
    CONSTRAINT forges_status_check CHECK (status IN ('pending', 'active', 'disabled')),
    CONSTRAINT forges_registered_by_fkey
        FOREIGN KEY (registered_by) REFERENCES users(id) ON DELETE SET NULL
);

COMMIT;
