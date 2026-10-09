-- Revert garnix:registered_forges from pg

BEGIN;

DROP TABLE IF EXISTS forges;

COMMIT;
