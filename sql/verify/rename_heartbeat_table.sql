-- Verify garnix:rename_heartbeat_table on pg

BEGIN;

DO $$
BEGIN
    ASSERT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name = 'server_heartbeat'
    ), 'the heartbeat table was not renamed to server_heartbeat';
    ASSERT NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name = 'heartbeat'
    ), 'the old heartbeat table is still present';
END $$;

ROLLBACK;
