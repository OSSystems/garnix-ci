-- Revert garnix:rename_heartbeat_table from pg

BEGIN;

ALTER TABLE server_heartbeat RENAME CONSTRAINT server_heartbeat_pkey TO heartbeat_pkey;
ALTER TABLE server_heartbeat RENAME TO heartbeat;

COMMIT;
