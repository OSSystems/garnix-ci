-- Verify garnix:forge_installations on pg

BEGIN;

DO $$
BEGIN
    ASSERT (
        SELECT count(*) FROM information_schema.columns
        WHERE table_schema = 'public'
          AND table_name = 'forge_installations'
          AND column_name IN ('id', 'forge', 'target_kind', 'target', 'installer_login', 'hook_id',
                              'webhook_secret', 'needs_reinstall', 'created_at', 'updated_at')
    ) = 10, 'the forge_installations table is missing or lacks columns';

    ASSERT (
        SELECT count(*) FROM information_schema.columns
        WHERE table_schema = 'public'
          AND table_name = 'forge_installation_repos'
          AND column_name IN ('installation_id', 'forge', 'repo_owner', 'repo_name', 'created_at')
    ) = 5, 'the forge_installation_repos table is missing or lacks columns';

    ASSERT NOT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = 'forges' AND column_name = 'webhook_secret'
    ), 'forges still has a webhook_secret';

    ASSERT EXISTS (
        SELECT 1 FROM pg_constraint WHERE conname = 'forge_installations_installer_check'
    ), 'an installation without an installer could look active';

    ASSERT EXISTS (
        SELECT 1 FROM pg_constraint WHERE conname = 'forge_installation_repos_installation_fkey'
    ), 'a repository could be enabled under another forge than its installation''s';

    ASSERT EXISTS (
        SELECT 1 FROM pg_trigger WHERE tgname = 'forge_identities_installer_gone'
    ), 'no trigger marks installations whose installer is gone';
END $$;

ROLLBACK;
