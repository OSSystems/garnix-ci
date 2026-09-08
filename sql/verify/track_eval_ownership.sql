-- Verify garnix:track_eval_ownership on pg

BEGIN;

DO $$
BEGIN
    ASSERT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name = 'eval_heartbeat'
    ), 'the eval_heartbeat table is missing';
    ASSERT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_schema = 'public'
          AND table_name = 'builds'
          AND column_name = 'eval_instance'
    ), 'builds.eval_instance is missing';
    ASSERT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_schema = 'public'
          AND table_name = 'runs'
          AND column_name = 'eval_instance'
    ), 'runs.eval_instance is missing';
    ASSERT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_schema = 'public'
          AND table_name = 'commits'
          AND column_name = 'started_at'
    ), 'commits.started_at is missing';
    ASSERT EXISTS (
        SELECT 1 FROM pg_indexes
        WHERE schemaname = 'public'
          AND indexname = 'builds_open_eval_instance'
    ), 'the builds_open_eval_instance index is missing';
    ASSERT EXISTS (
        SELECT 1 FROM pg_indexes
        WHERE schemaname = 'public'
          AND indexname = 'commits_pending_meta_check'
    ), 'the commits_pending_meta_check index is missing';
END $$;

ROLLBACK;
