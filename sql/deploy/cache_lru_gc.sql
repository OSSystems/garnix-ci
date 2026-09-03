-- Deploy garnix:cache_lru_gc to pg
-- requires: init

BEGIN;

ALTER TABLE cache_store_hashes ADD COLUMN deleting_since timestamp with time zone;

CREATE TABLE cache_store_hash_references (
    hash text NOT NULL,
    reference_hash text NOT NULL
);

ALTER TABLE ONLY cache_store_hash_references
    ADD CONSTRAINT cache_store_hash_references_hash_reference_hash_key UNIQUE (hash, reference_hash);

CREATE INDEX cache_store_hash_references_reference_hash
    ON cache_store_hash_references USING btree (reference_hash);

CREATE INDEX cache_store_hashes_accessed_at
    ON cache_store_hashes USING btree (accessed_at)
    WHERE uploaded_at IS NOT NULL AND deleting_since IS NULL;

CREATE INDEX cache_store_hashes_deleting_since
    ON cache_store_hashes USING btree (deleting_since)
    WHERE deleting_since IS NOT NULL;

CREATE TABLE cache_gc_state (
    id boolean DEFAULT true NOT NULL,
    reads_recorded_since timestamp with time zone,
    last_run_at timestamp with time zone,
    lock_owner text,
    lock_expires_at timestamp with time zone,
    CONSTRAINT cache_gc_state_singleton CHECK (id)
);

ALTER TABLE ONLY cache_gc_state
    ADD CONSTRAINT cache_gc_state_pkey PRIMARY KEY (id);

INSERT INTO cache_gc_state (id) VALUES (true);

INSERT INTO cache_store_hash_references (hash, reference_hash)
SELECT h.hash, split_part(ref, '-', 1)
  FROM cache_store_hashes h,
       LATERAL unnest(string_to_array(h."references", ' ')) AS ref
 WHERE h."references" IS NOT NULL
   AND ref <> ''
ON CONFLICT DO NOTHING;

COMMIT;
