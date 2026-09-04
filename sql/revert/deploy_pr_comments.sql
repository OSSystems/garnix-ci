-- Revert garnix:deploy_pr_comments from pg

BEGIN;

DROP TABLE deploy_comments;

COMMIT;
