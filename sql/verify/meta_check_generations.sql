-- Verify garnix:meta_check_generations on pg

BEGIN;

DO $$
BEGIN
    ASSERT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_schema = 'public'
          AND table_name = 'commits'
          AND column_name = 'meta_check_generation'
    ), 'commits.meta_check_generation is missing';
    ASSERT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name = 'meta_check_runs'
    ), 'the meta_check_runs table is missing';
END $$;

ROLLBACK;
