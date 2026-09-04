-- Revert garnix:add_hosting_servers from pg

BEGIN;

DROP TABLE IF EXISTS server_pool;
DROP TABLE IF EXISTS servers;

COMMIT;
