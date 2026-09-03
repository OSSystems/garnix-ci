-- Verify garnix:commit_failure_comment on pg

BEGIN;

DO $$
BEGIN
    ASSERT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_schema = 'public'
          AND table_name = 'commits'
          AND column_name = 'failure_commented'
    ), 'the commits.failure_commented column is missing';
END $$;

ROLLBACK;
