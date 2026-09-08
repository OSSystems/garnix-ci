-- Deploy garnix:rename_heartbeat_table to pg
-- requires: init

BEGIN;

ALTER TABLE heartbeat RENAME TO server_heartbeat;
ALTER TABLE server_heartbeat RENAME CONSTRAINT heartbeat_pkey TO server_heartbeat_pkey;

COMMIT;
