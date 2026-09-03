-- Deploy garnix:commit_failure_comment to pg
-- requires: init

BEGIN;

ALTER TABLE commits
    ADD COLUMN failure_commented boolean DEFAULT false NOT NULL;

COMMIT;
