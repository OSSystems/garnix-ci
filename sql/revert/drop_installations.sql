-- Revert garnix:drop_installations from pg

BEGIN;

CREATE TABLE installations (
    repo_owner text NOT NULL
);

ALTER TABLE ONLY installations
    ADD CONSTRAINT installations_pkey PRIMARY KEY (repo_owner);

COMMIT;
