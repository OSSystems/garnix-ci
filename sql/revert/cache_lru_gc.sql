-- Revert garnix:cache_lru_gc from pg

BEGIN;

DROP TABLE IF EXISTS cache_gc_state;

DROP TABLE IF EXISTS cache_store_hash_references;

DROP INDEX IF EXISTS cache_store_hashes_deleting_since;

DROP INDEX IF EXISTS cache_store_hashes_accessed_at;

ALTER TABLE cache_store_hashes DROP COLUMN IF EXISTS deleting_since;

COMMIT;
