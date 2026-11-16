-- Deploy garnix:superseded_pushes to pg
-- requires: forge_qualified_keys

BEGIN;

ALTER TABLE pushes ADD COLUMN superseded_at timestamp with time zone;

COMMIT;
