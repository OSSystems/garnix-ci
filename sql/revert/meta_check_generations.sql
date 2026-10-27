-- Revert garnix:meta_check_generations from pg

BEGIN;

DROP TABLE meta_check_runs;

ALTER TABLE commits
    DROP COLUMN meta_check_generation;

COMMIT;
