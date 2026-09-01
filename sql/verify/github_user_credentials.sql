-- Verify garnix:github_user_credentials on pg

BEGIN;

DO $$
BEGIN
    ASSERT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name = 'github_user_credentials'
    ), 'the github_user_credentials table is missing';

    ASSERT (
        SELECT count(*) FROM information_schema.columns
        WHERE table_schema = 'public'
          AND table_name = 'github_user_credentials'
          AND column_name IN ('user_id', 'access_token', 'access_token_expires_at',
                              'refresh_token', 'refresh_token_expires_at', 'updated_at')
    ) = 6, 'the github_user_credentials table is missing columns';

    ASSERT EXISTS (
        SELECT 1 FROM information_schema.table_constraints
        WHERE table_schema = 'public'
          AND table_name = 'github_user_credentials'
          AND constraint_name = 'github_user_credentials_user_id_fkey'
          AND constraint_type = 'FOREIGN KEY'
    ), 'github_user_credentials is not tied to the users table';
END $$;

ROLLBACK;
