BEGIN;

DO $$
BEGIN
    ASSERT NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name IN ('products', 'repo_owner_has_product',
                             'repo_owner_usage_limits', 'server_pool', 'servers')
    ), 'billing and Hetzner tables are still present';

    ASSERT NOT EXISTS (
        SELECT 1 FROM information_schema.sequences
        WHERE sequence_schema = 'public'
          AND sequence_name IN ('server_pool_id_seq', 'servers_id_seq')
    ), 'billing and Hetzner sequences are still present';

    ASSERT NOT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_schema = 'public'
          AND ((table_name = 'builds' AND column_name = 'comped')
               OR (table_name = 'installations'
                   AND column_name IN ('stripe_customer', 'current_period_start',
                                       'current_period_end', 'requested_cancellation')))
    ), 'billing columns are still present';
END $$;

ROLLBACK;
