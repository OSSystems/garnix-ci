-- Revert garnix:add_hosting_deploy from pg

BEGIN;

DROP TABLE IF EXISTS server_stats;

ALTER TABLE servers DROP COLUMN IF EXISTS ssh_users;
ALTER TABLE servers DROP COLUMN IF EXISTS domains;
ALTER TABLE servers DROP COLUMN IF EXISTS exposed;

COMMIT;
