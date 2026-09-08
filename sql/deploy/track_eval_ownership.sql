-- Deploy garnix:track_eval_ownership to pg
-- requires: init

BEGIN;

CREATE TABLE eval_heartbeat (
    hostname text NOT NULL,
    instance text NOT NULL,
    last_beat timestamp with time zone DEFAULT now() NOT NULL
);

ALTER TABLE ONLY eval_heartbeat ADD CONSTRAINT eval_heartbeat_pkey PRIMARY KEY (hostname);

ALTER TABLE builds ADD COLUMN eval_instance text;

ALTER TABLE runs
    ADD COLUMN eval_host text,
    ADD COLUMN eval_instance text;

ALTER TABLE commits
    ADD COLUMN eval_host text,
    ADD COLUMN eval_instance text,
    ADD COLUMN started_at timestamp with time zone DEFAULT now() NOT NULL;

CREATE INDEX builds_open_eval_instance ON builds USING btree (eval_instance) WHERE end_time IS NULL;

CREATE INDEX runs_open_eval_instance ON runs USING btree (eval_instance) WHERE end_time IS NULL;

CREATE INDEX commits_evaluating_eval_instance ON commits USING btree (eval_instance) WHERE status = 'evaluating';

CREATE INDEX commits_pending_meta_check ON commits USING btree (eval_instance) WHERE meta_check = 'pending';

COMMIT;
