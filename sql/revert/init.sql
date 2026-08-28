BEGIN;

DROP TABLE module_values;
DROP TABLE module_user_repo;
DROP TABLE modules;
DROP TABLE access_tokens;
DROP TABLE users;

DROP TABLE action_secrets;
DROP TABLE builds;
DROP TABLE cache_store_hash_tags;
DROP TABLE cache_store_hashes;
DROP TABLE commits;
DROP TABLE denylist;
DROP TABLE feature_flags;
DROP TABLE heartbeat;
DROP TABLE installations;
DROP TABLE internal_access_tokens;
DROP TABLE pushes;
DROP TABLE repo_config;
DROP TABLE repo_secrets;
DROP TABLE runs;
DROP TABLE verified_fods;
DROP TABLE waitlist;

DROP EXTENSION citext;

DROP TYPE build_status;
DROP TYPE check_status;
DROP TYPE commit_status;
DROP TYPE package_type;
DROP TYPE subscription_type;
DROP TYPE system;

COMMIT;
