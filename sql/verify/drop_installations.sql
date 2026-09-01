-- Verify garnix:drop_installations on pg

BEGIN;

DO $$
BEGIN
    ASSERT NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name = 'installations'
    ), 'the installations table is still present';
END $$;

ROLLBACK;
