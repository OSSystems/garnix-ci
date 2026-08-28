BEGIN;

ALTER TABLE builds DROP COLUMN IF EXISTS comped;

ALTER TABLE installations DROP COLUMN IF EXISTS stripe_customer;
ALTER TABLE installations DROP COLUMN IF EXISTS current_period_start;
ALTER TABLE installations DROP COLUMN IF EXISTS current_period_end;
ALTER TABLE installations DROP COLUMN IF EXISTS requested_cancellation;

DROP TABLE IF EXISTS repo_owner_has_product;
DROP TABLE IF EXISTS products;
DROP TABLE IF EXISTS repo_owner_usage_limits;
DROP TABLE IF EXISTS servers;
DROP TABLE IF EXISTS server_pool;

DROP SEQUENCE IF EXISTS servers_id_seq;
DROP SEQUENCE IF EXISTS server_pool_id_seq;

COMMIT;
