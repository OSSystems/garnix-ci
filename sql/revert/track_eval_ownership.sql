-- Revert garnix:track_eval_ownership from pg

BEGIN;

DROP INDEX commits_pending_meta_check;
DROP INDEX commits_evaluating_eval_instance;
DROP INDEX runs_open_eval_instance;
DROP INDEX builds_open_eval_instance;

ALTER TABLE commits
    DROP COLUMN started_at,
    DROP COLUMN eval_instance,
    DROP COLUMN eval_host;

ALTER TABLE runs
    DROP COLUMN eval_instance,
    DROP COLUMN eval_host;

ALTER TABLE builds DROP COLUMN eval_instance;

DROP TABLE eval_heartbeat;

COMMIT;
