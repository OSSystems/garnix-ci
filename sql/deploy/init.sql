-- Deploy garnix:init to pg

BEGIN;

CREATE TYPE build_status AS ENUM (
    'success',
    'failure',
    'timeout',
    'cancelled'
);

CREATE TYPE check_status AS ENUM (
    'pending',
    'fail',
    'success'
);

CREATE TYPE commit_status AS ENUM (
    'evaluating',
    'evaluated'
);

CREATE TYPE package_type AS ENUM (
    'package',
    'check',
    'homeConfiguration',
    'nixosConfiguration',
    'overall',
    'devShell',
    'defaultPackage',
    'defaultDevShell',
    'darwinConfiguration',
    'app'
);

CREATE TYPE subscription_type AS ENUM (
    'free',
    'admin'
);

CREATE TYPE system AS ENUM (
    'x86_64-linux',
    'aarch64-linux',
    'x86_64-darwin',
    'armv6l-linux',
    'armv7l-linux',
    'i686-linux',
    'mipsel-linux',
    'aarch64-darwin',
    'noSystem'
);


CREATE TABLE access_tokens (
    id bigint NOT NULL,
    name text NOT NULL,
    token text NOT NULL,
    user_id integer,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    last_used timestamp with time zone,
    scope_cache boolean DEFAULT true NOT NULL,
    scope_api boolean DEFAULT false NOT NULL
);

CREATE SEQUENCE access_tokens_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE access_tokens_id_seq OWNED BY access_tokens.id;

CREATE TABLE action_secrets (
    repo_user text NOT NULL,
    repo_name text NOT NULL,
    action_name text NOT NULL,
    private_key bytea NOT NULL,
    public_key text NOT NULL
);

CREATE TABLE builds (
    repo_user character varying NOT NULL,
    repo_name character varying NOT NULL,
    git_commit character varying NOT NULL,
    package character varying NOT NULL,
    status build_status,
    start_time timestamp with time zone DEFAULT now() NOT NULL,
    end_time timestamp with time zone,
    drv_path character varying,
    github_run_id bigint,
    extra_message character varying,
    package_type package_type NOT NULL,
    req_user character varying CONSTRAINT builds_req_user1_not_null NOT NULL,
    installation_id integer,
    system system DEFAULT 'noSystem'::system NOT NULL,
    id bigint NOT NULL,
    repo_is_public boolean NOT NULL,
    pr_from_fork text,
    branch text,
    persistence_name character varying,
    eval_host text,
    wants_incrementalism boolean DEFAULT false NOT NULL,
    uploaded_to_cache boolean DEFAULT false NOT NULL,
    output_paths json,
    already_built boolean
);

ALTER TABLE builds ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME builds_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);

CREATE TABLE cache_store_hash_tags (
    hash text NOT NULL,
    repo_owner text NOT NULL,
    repo_name text NOT NULL
);

CREATE TABLE cache_store_hashes (
    hash text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    accessed_at timestamp with time zone DEFAULT now() NOT NULL,
    package_name text,
    nar_hash text,
    nar_size bigint,
    public boolean,
    sig text,
    "references" text,
    file_size bigint,
    file_hash text,
    uploaded_at timestamp with time zone
);

CREATE TABLE commits (
    repo_user character varying NOT NULL,
    repo_name character varying NOT NULL,
    git_commit character varying NOT NULL,
    status commit_status NOT NULL,
    meta_check check_status NOT NULL
);

CREATE TABLE denylist (
    repo_user text NOT NULL,
    repo_name text
);

CREATE TABLE feature_flags (
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    config json NOT NULL
);

CREATE TABLE heartbeat (
    hostname character varying NOT NULL,
    last_heartbeat timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE installations (
    repo_owner text NOT NULL
);

CREATE TABLE internal_access_tokens (
    github_login character varying NOT NULL,
    internal_token text NOT NULL
);

CREATE TABLE module_user_repo (
    id bigint NOT NULL,
    github_login character varying NOT NULL,
    repo_user character varying,
    repo_name character varying
);

CREATE SEQUENCE module_user_repo_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE module_user_repo_id_seq OWNED BY module_user_repo.id;

CREATE TABLE module_values (
    module_user_repo_id bigint NOT NULL,
    module_id bigint NOT NULL,
    "values" json NOT NULL
);

CREATE EXTENSION citext;

CREATE TABLE modules (
    id bigint NOT NULL,
    repo_user character varying NOT NULL,
    repo_name character varying NOT NULL,
    git_commit character varying NOT NULL,
    schema json NOT NULL,
    enabled boolean DEFAULT false NOT NULL,
    name citext NOT NULL,
    description character varying
);

CREATE SEQUENCE modules_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE modules_id_seq OWNED BY modules.id;

CREATE TABLE pushes (
    repo_user character varying NOT NULL,
    repo_name character varying NOT NULL,
    git_commit character varying NOT NULL,
    branch character varying NOT NULL,
    pushed_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE repo_config (
    repo_user character varying NOT NULL,
    repo_name character varying NOT NULL,
    skip_private_inputs_check_for_collaborators boolean DEFAULT false CONSTRAINT repo_config_skip_private_inputs_check_for_collaborator_not_null NOT NULL,
    max_eval_memory bigint,
    private_cache boolean DEFAULT false NOT NULL
);

CREATE TABLE repo_secrets (
    repo_user text NOT NULL,
    repo_name text NOT NULL,
    private_key bytea NOT NULL,
    public_key text NOT NULL
);

CREATE TABLE runs (
    id bigint NOT NULL,
    name text NOT NULL,
    repo_user text NOT NULL,
    repo_name text NOT NULL,
    git_commit text NOT NULL,
    branch text,
    status build_status,
    req_user text NOT NULL,
    start_time timestamp with time zone DEFAULT now() NOT NULL,
    end_time timestamp with time zone
);

CREATE SEQUENCE runs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE runs_id_seq OWNED BY runs.id;

CREATE TABLE users (
    id integer NOT NULL,
    github_login character varying NOT NULL,
    email character varying NOT NULL,
    subscription_type subscription_type NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    agree_to_emails boolean DEFAULT false NOT NULL
);

CREATE SEQUENCE users_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE users_id_seq OWNED BY users.id;

CREATE TABLE verified_fods (
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    drv_hash text NOT NULL,
    store_path_hash text NOT NULL
);

CREATE TABLE waitlist (
    id bigint NOT NULL,
    email character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE SEQUENCE waitlist_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE waitlist_id_seq OWNED BY waitlist.id;

ALTER TABLE ONLY access_tokens ALTER COLUMN id SET DEFAULT nextval('access_tokens_id_seq'::regclass);

ALTER TABLE ONLY module_user_repo ALTER COLUMN id SET DEFAULT nextval('module_user_repo_id_seq'::regclass);

ALTER TABLE ONLY modules ALTER COLUMN id SET DEFAULT nextval('modules_id_seq'::regclass);

ALTER TABLE ONLY runs ALTER COLUMN id SET DEFAULT nextval('runs_id_seq'::regclass);

ALTER TABLE ONLY users ALTER COLUMN id SET DEFAULT nextval('users_id_seq'::regclass);

ALTER TABLE ONLY waitlist ALTER COLUMN id SET DEFAULT nextval('waitlist_id_seq'::regclass);

ALTER TABLE ONLY action_secrets
    ADD CONSTRAINT action_secrets_pkey PRIMARY KEY (repo_user, repo_name, action_name);

ALTER TABLE ONLY builds
    ADD CONSTRAINT builds_pkey PRIMARY KEY (id);

ALTER TABLE ONLY cache_store_hash_tags
    ADD CONSTRAINT cache_store_hash_tags_hash_repo_owner_repo_name_key UNIQUE (hash, repo_owner, repo_name);

ALTER TABLE ONLY cache_store_hashes
    ADD CONSTRAINT cache_store_hashes_pkey PRIMARY KEY (hash);

ALTER TABLE ONLY commits
    ADD CONSTRAINT commits_pkey PRIMARY KEY (repo_user, repo_name, git_commit);

ALTER TABLE ONLY heartbeat
    ADD CONSTRAINT heartbeat_pkey PRIMARY KEY (hostname);

ALTER TABLE ONLY installations
    ADD CONSTRAINT installations_pkey PRIMARY KEY (repo_owner);

ALTER TABLE ONLY internal_access_tokens
    ADD CONSTRAINT internal_access_tokens_pkey PRIMARY KEY (github_login);

ALTER TABLE ONLY module_user_repo
    ADD CONSTRAINT module_user_repo_github_login_key UNIQUE (github_login);

ALTER TABLE ONLY module_user_repo
    ADD CONSTRAINT module_user_repo_pkey PRIMARY KEY (id);

ALTER TABLE ONLY module_values
    ADD CONSTRAINT module_values_pkey PRIMARY KEY (module_user_repo_id, module_id);

ALTER TABLE ONLY modules
    ADD CONSTRAINT modules_name_git_commit UNIQUE (name, git_commit);

ALTER TABLE ONLY modules
    ADD CONSTRAINT modules_pkey PRIMARY KEY (id);

ALTER TABLE ONLY modules
    ADD CONSTRAINT modules_repo_user_repo_name_git_commit_key UNIQUE (repo_user, repo_name, git_commit);

ALTER TABLE ONLY pushes
    ADD CONSTRAINT pushes_pkey PRIMARY KEY (repo_user, repo_name, git_commit, branch);

ALTER TABLE ONLY repo_config
    ADD CONSTRAINT repo_config_pkey PRIMARY KEY (repo_user, repo_name);

ALTER TABLE ONLY repo_secrets
    ADD CONSTRAINT repo_secrets_pkey PRIMARY KEY (repo_user, repo_name);

ALTER TABLE ONLY runs
    ADD CONSTRAINT runs_pkey PRIMARY KEY (id);

ALTER TABLE ONLY verified_fods
    ADD CONSTRAINT unique_drv_hash_store_path_hash UNIQUE (drv_hash, store_path_hash);

ALTER TABLE ONLY users
    ADD CONSTRAINT users_email_key UNIQUE (email);

ALTER TABLE ONLY users
    ADD CONSTRAINT users_github_login_key UNIQUE (github_login);

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);

ALTER TABLE ONLY waitlist
    ADD CONSTRAINT waitlist_email_key UNIQUE (email);

ALTER TABLE ONLY waitlist
    ADD CONSTRAINT waitlist_pkey PRIMARY KEY (id);

CREATE INDEX build_start_time_status ON builds USING btree (start_time, status);

CREATE INDEX builds_drv_path ON builds USING btree (drv_path);

CREATE INDEX builds_git_commit ON builds USING btree (git_commit);

CREATE INDEX builds_repo_owner_name ON builds USING btree (repo_user, repo_name);

CREATE INDEX builds_req_user ON builds USING btree (req_user);

CREATE INDEX builds_req_user_git_commit_start_time ON builds USING btree (req_user, git_commit, start_time);

CREATE INDEX verified_fods_drv_hash ON verified_fods USING btree (drv_hash);

CREATE INDEX verified_fods_store_path_hash ON verified_fods USING btree (store_path_hash);

ALTER TABLE ONLY access_tokens
    ADD CONSTRAINT access_tokens_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);

ALTER TABLE ONLY module_user_repo
    ADD CONSTRAINT module_user_repo_github_login_fkey FOREIGN KEY (github_login) REFERENCES users(github_login);

ALTER TABLE ONLY module_values
    ADD CONSTRAINT module_values_module_id_fkey FOREIGN KEY (module_id) REFERENCES modules(id);

ALTER TABLE ONLY module_values
    ADD CONSTRAINT module_values_module_user_repo_id_fkey FOREIGN KEY (module_user_repo_id) REFERENCES module_user_repo(id);

COMMIT;
