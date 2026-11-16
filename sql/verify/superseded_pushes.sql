-- Verify garnix:superseded_pushes on pg

BEGIN;

DO $$
BEGIN
    ASSERT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_schema = 'public'
          AND table_name = 'pushes'
          AND column_name = 'superseded_at'
    ), 'pushes.superseded_at is missing';
END $$;

ROLLBACK;
