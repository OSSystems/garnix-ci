-- Deploy garnix:add_hosting_servers to pg
-- requires: init

BEGIN;

CREATE TABLE servers (
    id bigint NOT NULL,
    configuration_build_id bigint NOT NULL,
    provider text NOT NULL,
    instance_id text,
    ipv4 inet,
    ipv6 inet,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    ended_at timestamp with time zone,
    deploy_logs text DEFAULT ''::text NOT NULL,
    pull_request bigint,
    ready_at timestamp with time zone,
    server_tier text NOT NULL,
    is_primary boolean DEFAULT false NOT NULL,
    CONSTRAINT servers_ready_is_addressable CHECK (
        ready_at IS NULL
        OR (instance_id IS NOT NULL AND (ipv4 IS NOT NULL OR ipv6 IS NOT NULL))
    )
);

ALTER TABLE servers ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME servers_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);

ALTER TABLE ONLY servers ADD CONSTRAINT servers_pkey PRIMARY KEY (id);

CREATE INDEX servers_live_idx ON servers (server_tier) WHERE ended_at IS NULL;

CREATE TABLE server_pool (
    id bigint NOT NULL,
    provider text NOT NULL,
    instance_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    ready_at timestamp with time zone,
    ipv4 inet,
    ipv6 inet,
    server_tier text NOT NULL,
    CONSTRAINT server_pool_ready_is_addressable CHECK (
        ready_at IS NULL
        OR (instance_id IS NOT NULL AND (ipv4 IS NOT NULL OR ipv6 IS NOT NULL))
    )
);

ALTER TABLE server_pool ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME server_pool_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);

ALTER TABLE ONLY server_pool ADD CONSTRAINT server_pool_pkey PRIMARY KEY (id);

CREATE INDEX server_pool_claimable_idx ON server_pool (server_tier, provider)
    WHERE ready_at IS NOT NULL;

COMMIT;
