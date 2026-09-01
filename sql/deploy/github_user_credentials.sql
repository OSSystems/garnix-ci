-- Deploy garnix:github_user_credentials to pg
-- requires: init

BEGIN;

CREATE TABLE github_user_credentials (
    user_id integer NOT NULL,
    access_token bytea NOT NULL,
    access_token_expires_at timestamp with time zone,
    refresh_token bytea,
    refresh_token_expires_at timestamp with time zone,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

ALTER TABLE ONLY github_user_credentials
    ADD CONSTRAINT github_user_credentials_pkey PRIMARY KEY (user_id);

ALTER TABLE ONLY github_user_credentials
    ADD CONSTRAINT github_user_credentials_user_id_fkey
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE;

COMMIT;
