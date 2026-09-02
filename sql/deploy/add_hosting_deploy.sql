-- Deploy garnix:add_hosting_deploy to pg
-- requires: add_hosting_servers

BEGIN;

-- Per-server SSH/port exposure decided by the provisioner at spin-up time,
-- derived from garnix.server.{exposeSSH,ports}:
--   {"ssh_port": 2201 | null,
--    "tcp": [{"name": "db", "guest": 5432, "host": 32001}]}
-- Null until the server has been exposed.
ALTER TABLE servers ADD COLUMN exposed json;

-- Extra hostnames the server answers on, declared by garnix.server.domains.
ALTER TABLE servers ADD COLUMN domains json DEFAULT '[]'::json NOT NULL;

-- Real login usernames read off the guest at deploy time (getent passwd), so
-- we can tell a user which account to ssh in as.
ALTER TABLE servers ADD COLUMN ssh_users json DEFAULT '[]'::json NOT NULL;

-- Resource samples pushed by the guest's stats reporter. We keep a short
-- rolling window per server; the backend prunes on insert.
CREATE TABLE server_stats (
    id bigint NOT NULL,
    server_id bigint NOT NULL REFERENCES servers (id) ON DELETE CASCADE,
    sampled_at timestamp with time zone DEFAULT now() NOT NULL,
    cpu_pct double precision NOT NULL,
    mem_used_kb bigint NOT NULL,
    mem_total_kb bigint NOT NULL
);

ALTER TABLE server_stats ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME server_stats_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);

ALTER TABLE ONLY server_stats ADD CONSTRAINT server_stats_pkey PRIMARY KEY (id);

CREATE INDEX server_stats_recent_idx ON server_stats (server_id, sampled_at DESC);

COMMIT;
