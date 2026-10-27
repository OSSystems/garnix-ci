-- Deploy garnix:meta_check_generations to pg
-- requires: forge_qualified_keys

BEGIN;

ALTER TABLE commits
    ADD COLUMN meta_check_generation bigint DEFAULT 0 NOT NULL;

CREATE TABLE meta_check_runs (
    forge text NOT NULL,
    repo_user character varying NOT NULL,
    repo_name character varying NOT NULL,
    git_commit character varying NOT NULL,
    generation bigint NOT NULL,
    github_run_id bigint NOT NULL,
    completed boolean DEFAULT false NOT NULL,
    PRIMARY KEY (forge, repo_user, repo_name, git_commit, generation),
    FOREIGN KEY (forge, repo_user, repo_name, git_commit)
        REFERENCES commits (forge, repo_user, repo_name, git_commit) ON DELETE CASCADE
);

CREATE INDEX meta_check_runs_open ON meta_check_runs USING btree (forge, repo_user, repo_name, git_commit) WHERE NOT completed;

COMMIT;
