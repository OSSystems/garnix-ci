-- Revert garnix:superseded_pushes from pg

BEGIN;

ALTER TABLE pushes DROP COLUMN superseded_at;

COMMIT;
