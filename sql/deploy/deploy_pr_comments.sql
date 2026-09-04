-- Deploy garnix:deploy_pr_comments to pg
-- requires: init

BEGIN;

-- Which pull request deploy comments we have already posted.
--
-- Separate from commits.failure_commented on purpose: that column is one flag
-- per commit shared with the build failure comment, so reusing it would make
-- whichever comment ran first silently swallow the other.
--
--   'url'     - the deployed addresses, once per pull request.
--   'failure' - a failed deploy, once per commit.
CREATE TABLE deploy_comments (
    id bigint NOT NULL,
    repo_user text NOT NULL,
    repo_name text NOT NULL,
    pull_request bigint NOT NULL,
    kind text NOT NULL,
    git_commit text,
    commented_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT deploy_comments_kind CHECK (kind IN ('url', 'failure')),
    CONSTRAINT deploy_comments_failure_has_commit CHECK (
        kind <> 'failure' OR git_commit IS NOT NULL
    )
);

ALTER TABLE deploy_comments ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME deploy_comments_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);

ALTER TABLE ONLY deploy_comments ADD CONSTRAINT deploy_comments_pkey PRIMARY KEY (id);

CREATE UNIQUE INDEX deploy_comments_url_idx
    ON deploy_comments (repo_user, repo_name, pull_request)
    WHERE kind = 'url';

CREATE UNIQUE INDEX deploy_comments_failure_idx
    ON deploy_comments (repo_user, repo_name, pull_request, git_commit)
    WHERE kind = 'failure';

COMMIT;
