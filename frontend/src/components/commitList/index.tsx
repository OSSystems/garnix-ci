import React from "react";
import { Text } from "@/components/text";
import { Modal, ModalActions, ModalSection } from "@/components/modal";
import { CommitBuildsSummary } from "@/components/build";
import { Button } from "@/components/button";
import { GithubIcon } from "@/components/icons/github";
import { useConfig } from "@/store/configContext";
import { useLoading } from "@/hooks/useLoading";
import { getCommits, getCommitsForRepo } from "@/services/commit";
import { fromSecs } from "@/utils/duration";
import { Link } from "@/components/link";
import styles from "./styles.module.css";

type CommitListFor = "reqUser" | { forge: string; owner: string; repo: string };

export const CommitList = (props: { for: CommitListFor }) => {
  const { githubAppName } = useConfig();
  const setupUrl = `https://github.com/apps/${githubAppName}/installations/new`;

  const getCommitsFn = React.useCallback(
    () =>
      props.for === "reqUser"
        ? getCommits()
        : getCommitsForRepo(props.for.forge, props.for.owner, props.for.repo),
    [props.for],
  );

  const loadingCommits = useLoading(getCommitsFn, { poll: fromSecs(5) });
  if (loadingCommits.loading) return null;
  if (!loadingCommits.data.ok) {
    return (
      <div className={styles.error}>{loadingCommits.data.error.message}</div>
    );
  }
  const commits = loadingCommits.data.data;
  return commits.length > 0 ? (
    <div className={styles.container}>
      <Text type="h1" className={styles.h1}>
        Builds
      </Text>
      <ol>
        {commits.map((commit) => (
          <CommitBuildsSummary
            className={styles.build}
            key={commit.gitCommit}
            commit={commit}
            link
          />
        ))}
      </ol>
    </div>
  ) : (
    <div className={styles.containerZeroState}>
      <Modal>
        <ModalSection>
          <Text type="h1" className={styles.h1}>
            Your builds will appear here once some have been created.
          </Text>
          <Text type="p" className={styles.p}>
            To build, just push a commit to any enabled repository that contains
            a flake.nix. If you need help creating a flake.nix for your repo,
            check out our{" "}
            <Link href="/modules/configure" className={styles.link}>
              modules page
            </Link>
            .
          </Text>
        </ModalSection>
        <ModalSection>
          <Text className={styles.p}>Need help getting started?</Text>
          <ModalActions>
            <Button href={setupUrl} eventName="install-the-app" target="">
              <GithubIcon />
              Setup Garnix on a simple repo
            </Button>
          </ModalActions>
        </ModalSection>
      </Modal>
    </div>
  );
};
