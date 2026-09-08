-- Revert garnix:heartbeat_reporting from pg

BEGIN;

DROP TABLE heartbeat_reporting;

COMMIT;
