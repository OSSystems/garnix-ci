-- Deploy garnix:heartbeat_reporting to pg
-- requires: init

BEGIN;

CREATE TABLE heartbeat_reporting (
    id boolean DEFAULT true NOT NULL,
    reports_recorded_since timestamp with time zone,
    last_report_at timestamp with time zone,
    CONSTRAINT heartbeat_reporting_singleton CHECK (id)
);

ALTER TABLE ONLY heartbeat_reporting
    ADD CONSTRAINT heartbeat_reporting_pkey PRIMARY KEY (id);

INSERT INTO heartbeat_reporting (id) VALUES (true);

COMMIT;
