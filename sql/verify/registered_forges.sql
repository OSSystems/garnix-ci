-- Verify garnix:registered_forges on pg

BEGIN;

DO $$
BEGIN
    ASSERT (
        SELECT count(*) FROM information_schema.columns
        WHERE table_schema = 'public'
          AND table_name = 'forges'
          AND column_name IN ('slug', 'kind', 'web_url', 'api_url', 'oauth_client_id',
                              'oauth_client_secret', 'webhook_secret', 'status',
                              'registered_by', 'registration_token_hash', 'disabled_at', 'created_at', 'updated_at')
    ) = 13, 'the forges table is missing or lacks columns';

    ASSERT EXISTS (
        SELECT 1 FROM information_schema.table_constraints
        WHERE table_schema = 'public'
          AND table_name = 'forges'
          AND constraint_name = 'forges_pkey'
          AND constraint_type = 'PRIMARY KEY'
    ), 'forges has no primary key on slug';

    ASSERT EXISTS (
        SELECT 1 FROM information_schema.table_constraints
        WHERE table_schema = 'public'
          AND table_name = 'forges'
          AND constraint_name = 'forges_registered_by_fkey'
          AND constraint_type = 'FOREIGN KEY'
    ), 'forges.registered_by does not reference users';
END $$;

ROLLBACK;
