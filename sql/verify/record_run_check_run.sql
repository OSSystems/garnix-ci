-- Verify garnix:record_run_check_run on pg

BEGIN;

DO $$
BEGIN
    ASSERT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_schema = 'public'
          AND table_name = 'runs'
          AND column_name = 'github_run_id'
    ), 'runs.github_run_id is missing';
END $$;

ROLLBACK;
