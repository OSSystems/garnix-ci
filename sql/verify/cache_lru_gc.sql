-- Verify garnix:cache_lru_gc on pg

BEGIN;

SELECT hash, created_at, accessed_at, package_name, nar_hash, nar_size, public,
       sig, "references", file_size, file_hash, uploaded_at, deleting_since
    FROM cache_store_hashes WHERE FALSE;

SELECT hash, reference_hash
    FROM cache_store_hash_references WHERE FALSE;

SELECT id, reads_recorded_since, last_run_at, lock_owner, lock_expires_at
    FROM cache_gc_state WHERE FALSE;

DO $$
BEGIN
    ASSERT (SELECT count(*) FROM cache_gc_state) = 1,
        'cache_gc_state must hold exactly one row';
END $$;

ROLLBACK;
