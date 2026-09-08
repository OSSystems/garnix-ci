-- Verify garnix:heartbeat_reporting on pg

BEGIN;

DO $$
BEGIN
    ASSERT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name = 'heartbeat_reporting'
    ), 'the heartbeat_reporting table is missing';
    ASSERT (SELECT count(*) FROM heartbeat_reporting) = 1,
        'heartbeat_reporting is missing its singleton row';
END $$;

ROLLBACK;
