-- Revert garnix:github_user_credentials from pg

BEGIN;

DROP TABLE IF EXISTS github_user_credentials;

COMMIT;
