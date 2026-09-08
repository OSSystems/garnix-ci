-- Revert garnix:record_run_check_run from pg

BEGIN;

ALTER TABLE runs DROP COLUMN github_run_id;

COMMIT;
