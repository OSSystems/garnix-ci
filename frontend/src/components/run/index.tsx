import Image from "next/image";
import React from "react";
import { StatusIcon } from "@/components/statusIcon";
import { Text } from "@/components/text";
import { formatCommitSha, formatRunName } from "@/utils/format";
import branchIcon from "@/components/icons/branch.svg";
import commitIcon from "@/components/icons/commit.svg";
import repoIcon from "@/components/icons/repo.svg";
import stopwatchIcon from "@/components/icons/stopwatch.svg";
import statusIcon from "@/components/icons/status.svg";
import { Link } from "@/components/link";
import { formatDurationShort, diffTime } from "@/utils/duration";
import { Run } from "@/services/run";
import { repoPath } from "@/services/forges";
import { RunLog } from "../buildLog";
import styles from "./styles.module.css";

const createHeaderProps = (module: Run) => {
  return [
    {
      icon: repoIcon,
      label: "Repo",
      url: repoPath(module),
      value: `${module.repoUser}/${module.repoName}`,
    },
    {
      icon: branchIcon,
      label: "Branch",
      value: module.branch,
    },
    {
      icon: commitIcon,
      label: "Commit",
      url: `/commit/${module.git_commit}`,
      value: formatCommitSha(module),
    },
    {
      icon: stopwatchIcon,
      label: "Total time",
      value: module.endTime
        ? formatDurationShort(diffTime(module.endTime, module.startTime))
        : "-",
    },
    {
      icon: statusIcon,
      label: "Status",
      value: (
        <span className={styles.status}>
          <StatusIcon status={module.status} /> {module.status}
        </span>
      ),
    },
  ];
};

export const RunPage = ({ run }: { run: Run }) => {
  return (
    <main className={styles.container}>
      <>
        <Text type="h1" className={styles.h1}>
          {formatRunName(run)}
        </Text>
        <div className={`${styles.section} ${styles.summary}`}>
          {createHeaderProps(run).map(({ icon, label, url, value }) => (
            <div key={label} className={styles.field}>
              <label className={styles.label}>
                <Image src={icon} alt={label} />{" "}
                <Text className={styles.labelText}>{label}</Text>
              </label>
              <Text className={styles.value}>
                {url ? <Link href={url}>{value}</Link> : value}
              </Text>
            </div>
          ))}
        </div>
        <div className={styles.section}>
          <Text type="h2" className={styles.h2}>
            Logs
          </Text>
          <RunLog runId={run.id} />
        </div>
      </>
    </main>
  );
};
