import Image from "next/image";
import { ReactNode } from "react";
import { Link } from "@/components/link";
import { Text } from "@/components/text";
import { StatusIcon } from "@/components/statusIcon";
import repoIcon from "@/components/icons/repo.svg";
import branchIcon from "@/components/icons/branch.svg";
import commitIcon from "@/components/icons/commit.svg";
import timestampIcon from "@/components/icons/timestamp.svg";
import shareIcon from "@/components/icons/share.svg";
import { formatCommitSha } from "@/utils/format";
import { diffTime, formatDurationLong } from "@/utils/duration";
import { CommitSummary } from "@/services/commit";
import { Forge, forgeHost, forgeLinks, repoPath } from "@/services/forges";
import { useForge } from "@/store/configContext";
import styles from "./styles.module.css";

type Props = {
  commit: CommitSummary;
  link?: boolean;
  className?: string;
};

const createHeaderProps = (commit: CommitSummary, forge?: Forge) => {
  const links = forge && forgeLinks(forge);
  return [
    {
      icon: repoIcon,
      label: "repository",
      url: repoPath(commit),
      value: `${commit.repoUser}/${commit.repoName}`,
    },
    {
      icon: branchIcon,
      label: "branch",
      url: links && commit.branch && links.branch(commit, commit.branch),
      value: commit.branch,
    },
    {
      icon: commitIcon,
      label: "commit",
      url: links?.commit(commit, commit.gitCommit),
      value: formatCommitSha(commit),
    },
    ...(forge && links
      ? [
          {
            icon: shareIcon,
            label: "source",
            url: links.repo(commit),
            value: `view on ${forgeHost(forge)}`,
          },
        ]
      : []),
  ];
};

export const CommitBuildsSummary = ({
  link = false,
  commit,
  className,
}: Props) => {
  const forge = useForge(commit.forge);
  const wrapper = link
    ? (children: ReactNode) => (
        <Link href={`/commit/${commit.gitCommit}`} variant="wrapper">
          {children}
        </Link>
      )
    : (children: ReactNode) => <>{children}</>;
  return wrapper(
    <div
      className={`${styles.container} ${link ? styles.link : ""} ${className}`}
    >
      <div>
        <div className={styles.header}>
          {createHeaderProps(commit, forge).map(
            ({ icon, label, url, value }) => (
              <Text key={label} className={styles.status}>
                <Image key={label} src={icon} alt={label} />
                {link || !url ? value : <Link href={url}>{value}</Link>}
              </Text>
            ),
          )}
        </div>
        <Text className={`${styles.timestamp} ${styles.status}`}>
          <Image src={timestampIcon} alt="timestamp" />
          {formatDurationLong(diffTime(new Date(), commit.startTime))} ago by{" "}
          {link || !forge ? (
            `@${commit.reqUser}`
          ) : (
            <Link href={forgeLinks(forge).user(commit.reqUser)}>
              @{commit.reqUser}
            </Link>
          )}
        </Text>
      </div>
      <div className={styles.statuses}>
        {(() => {
          const { succeeded, failed, pending, cancelled } = commit;
          return (
            <>
              {succeeded > 0 && (
                <Text className={styles.status}>
                  <StatusIcon status="Success" />
                  {getSuccessText(succeeded, failed, pending, cancelled)}
                </Text>
              )}
              {failed > 0 && (
                <Text className={styles.status}>
                  <StatusIcon status="Failure" />
                  {failed}
                </Text>
              )}
              {pending > 0 && (
                <Text className={styles.status}>
                  <StatusIcon status="Pending" />
                  {pending}
                </Text>
              )}
              {cancelled > 0 && (
                <Text className={styles.status}>
                  <StatusIcon status="Cancelled" />
                  {cancelled}
                </Text>
              )}
            </>
          );
        })()}
      </div>
    </div>,
  );
};

const getSuccessText = (
  success: number,
  fail: number,
  pending: number,
  cancelled: number,
): string => {
  if (success <= 0) throw Error("unreachable");
  if (fail > 0 || pending > 0 || cancelled > 0) return success.toString();
  return `${success} build${success > 1 ? "s" : ""} succeeded`;
};
