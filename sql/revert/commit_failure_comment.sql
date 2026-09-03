-- Revert garnix:commit_failure_comment from pg

BEGIN;

ALTER TABLE commits
    DROP COLUMN failure_commented;

COMMIT;
