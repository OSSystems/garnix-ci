-- Deploy garnix:drop_installations to pg
-- requires: init

BEGIN;

DROP TABLE IF EXISTS installations;

COMMIT;
