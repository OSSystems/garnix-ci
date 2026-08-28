BEGIN;

ALTER TABLE builds ADD COLUMN comped boolean DEFAULT false NOT NULL;

ALTER TABLE installations ADD COLUMN stripe_customer text;
ALTER TABLE installations ADD COLUMN current_period_start timestamp with time zone;
ALTER TABLE installations ADD COLUMN current_period_end timestamp with time zone;
ALTER TABLE installations ADD COLUMN requested_cancellation boolean DEFAULT false NOT NULL;

ALTER TABLE ONLY installations
    ADD CONSTRAINT installations_stripe_customer_key UNIQUE (stripe_customer);

CREATE TABLE products (
    name character varying NOT NULL,
    hosting bigint,
    pr_hosting bigint,
    ci_minutes bigint,
    title text,
    description text,
    price_id text,
    packages_per_flake integer,
    visible boolean DEFAULT false NOT NULL,
    token text,
    package_eval_timeout_in_minutes smallint,
    package_build_timeout_in_minutes smallint,
    larger_servers boolean DEFAULT false NOT NULL
);

ALTER TABLE ONLY products
    ADD CONSTRAINT products_pkey PRIMARY KEY (name);

ALTER TABLE ONLY products
    ADD CONSTRAINT products_price_id_key UNIQUE (price_id);

ALTER TABLE ONLY products
    ADD CONSTRAINT products_token_key UNIQUE (token);

CREATE TABLE repo_owner_has_product (
    repo_owner character varying NOT NULL,
    product character varying NOT NULL
);

ALTER TABLE ONLY repo_owner_has_product
    ADD CONSTRAINT repo_owner_has_product_pkey PRIMARY KEY (repo_owner, product);

ALTER TABLE ONLY repo_owner_has_product
    ADD CONSTRAINT repo_owner_has_product_product_fkey FOREIGN KEY (product) REFERENCES products(name);

CREATE TABLE repo_owner_usage_limits (
    repo_owner text CONSTRAINT repo_owner_max_extra_ci_time_repo_owner_not_null NOT NULL,
    extra_ci_time_in_minutes integer DEFAULT 0 CONSTRAINT repo_owner_max_extra_ci_time_extra_ci_time_in_minutes_not_null NOT NULL,
    extra_pr_hosting_in_minutes integer DEFAULT 0 NOT NULL,
    extra_hosting_spending_limit_in_usd integer DEFAULT 0 CONSTRAINT repo_owner_usage_limits_extra_hosting_spending_limit_i_not_null NOT NULL
);

ALTER TABLE ONLY repo_owner_usage_limits
    ADD CONSTRAINT repo_owner_max_extra_ci_time_pkey PRIMARY KEY (repo_owner);

CREATE TABLE server_pool (
    id bigint NOT NULL,
    hetzner_id integer,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    ready_at timestamp with time zone,
    ipv4 text,
    ipv6 text,
    server_tier text NOT NULL,
    CONSTRAINT ready_must_have_hetzner_id_and_ips CHECK ((((ready_at IS NOT NULL) AND (hetzner_id IS NOT NULL) AND (ipv4 IS NOT NULL) AND (ipv6 IS NOT NULL)) OR (ready_at IS NULL)))
);

CREATE SEQUENCE server_pool_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE server_pool_id_seq OWNED BY server_pool.id;

ALTER TABLE ONLY server_pool ALTER COLUMN id SET DEFAULT nextval('server_pool_id_seq'::regclass);

ALTER TABLE ONLY server_pool
    ADD CONSTRAINT server_pool_pkey PRIMARY KEY (id);

CREATE TABLE servers (
    id bigint NOT NULL,
    configuration_build_id bigint NOT NULL,
    hetzner_id integer NOT NULL,
    ipv4 text NOT NULL,
    ipv6 text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    ended_at timestamp with time zone,
    deploy_logs text DEFAULT ''::text NOT NULL,
    pull_request bigint,
    ready_at timestamp with time zone,
    server_tier text NOT NULL,
    is_primary boolean DEFAULT false NOT NULL
);

CREATE SEQUENCE servers_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE servers_id_seq OWNED BY servers.id;

ALTER TABLE ONLY servers ALTER COLUMN id SET DEFAULT nextval('servers_id_seq'::regclass);

ALTER TABLE ONLY servers
    ADD CONSTRAINT servers_pkey PRIMARY KEY (id);

ALTER TABLE ONLY servers
    ADD CONSTRAINT servers_build_fkey FOREIGN KEY (configuration_build_id) REFERENCES builds(id);

CREATE INDEX servers_configuration_build_id ON servers USING btree (configuration_build_id);

CREATE INDEX servers_ended_at ON servers USING btree (ended_at);

COMMIT;
