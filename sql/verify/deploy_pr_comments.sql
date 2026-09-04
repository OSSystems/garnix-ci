-- Verify garnix:deploy_pr_comments on pg

BEGIN;

DO $$
BEGIN
    ASSERT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name = 'deploy_comments'
    ), 'the deploy_comments table is missing';
    ASSERT EXISTS (
        SELECT 1 FROM pg_indexes
        WHERE schemaname = 'public'
          AND indexname = 'deploy_comments_url_idx'
    ), 'the deploy_comments_url_idx index is missing';
    ASSERT EXISTS (
        SELECT 1 FROM pg_indexes
        WHERE schemaname = 'public'
          AND indexname = 'deploy_comments_failure_idx'
    ), 'the deploy_comments_failure_idx index is missing';
END $$;

ROLLBACK;
