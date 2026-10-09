-- Revert garnix:forge_installations from pg

BEGIN;

DROP TRIGGER IF EXISTS forge_identities_installer_gone ON forge_identities;
DROP FUNCTION IF EXISTS forge_installations_installer_gone();
DROP TABLE IF EXISTS forge_installation_repos;
DROP TABLE IF EXISTS forge_installations;

-- A lossy revert, on purpose: Gitea/multi-forge has no users to keep
-- compatible with. Every installation goes, and the secrets registered forges
-- had are gone: they get an empty one, which does not decrypt, so each has to
-- be registered again.
ALTER TABLE forges ADD COLUMN webhook_secret bytea NOT NULL DEFAULT ''::bytea;
ALTER TABLE forges ALTER COLUMN webhook_secret DROP DEFAULT;

COMMIT;
