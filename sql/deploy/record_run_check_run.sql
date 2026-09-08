-- Deploy garnix:record_run_check_run to pg
-- requires: init

BEGIN;

ALTER TABLE runs ADD COLUMN github_run_id bigint;

COMMIT;
