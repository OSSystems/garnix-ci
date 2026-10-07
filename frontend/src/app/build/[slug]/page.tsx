"use client";

import Image from "next/image";
import { useCallback } from "react";
import React from "react";
import { P, match } from "ts-pattern";
import { BuildLog } from "@/components/buildLog";
import { Button } from "@/components/button";
import { StatusIcon } from "@/components/statusIcon";
import { Text } from "@/components/text";
import { formatCommitSha, formatRunName } from "@/utils/format";
import branchIcon from "@/components/icons/branch.svg";
import commitIcon from "@/components/icons/commit.svg";
import repoIcon from "@/components/icons/repo.svg";
import clockIcon from "@/components/icons/clock.svg";
import stopwatchIcon from "@/components/icons/stopwatch.svg";
import folderIcon from "@/components/icons/folder.svg";
import cubeIcon from "@/components/icons/cube.svg";
import terminalIcon from "@/components/icons/terminal.svg";
import statusIcon from "@/components/icons/status.svg";
import { DownloadIcon } from "@/components/icons/download";
import { Build, getBuild } from "@/services/build";
import { repoPath } from "@/services/forges";
import { Link } from "@/components/link";
import { useLoading } from "@/hooks/useLoading";
import { formatDurationShort, diffTime, fromSecs } from "@/utils/duration";
import { Err, Ok } from "@/services";
import { useForm } from "@/hooks/useForm";
import { cancelBuild } from "@/services/build";
import { trackSubmit } from "@/utils/analytics";
import styles from "./styles.module.css";

const createHeaderProps = (module: Build) => {
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
      icon: clockIcon,
      label: "Started at",
      value: formatDateTime(module.startTime),
    },
    {
      icon: stopwatchIcon,
      label: "Total time",
      value: module.endTime
        ? formatDurationShort(diffTime(module.endTime, module.startTime))
        : "-",
    },
    {
      icon: folderIcon,
      label: "Type",
      value: formatPackageType(module.packageType),
    },
    {
      icon: cubeIcon,
      label: "Package",
      value: module.package,
    },
    {
      icon: terminalIcon,
      label: "System",
      value: module.system || "-",
    },
    {
      icon: clockIcon,
      label: "Finished at",
      value: formatDateTime(module.endTime),
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

const Page = ({ params }: { params: { slug: string } }) => {
  const build = useLoading(
    useCallback(() => getBuild(params.slug), [params.slug]),
    {
      poll: fromSecs(5),
      shouldPoll: (result) =>
        match(result)
          .with(Err(P._), () => true)
          .with(Ok({ status: "Pending" }), () => true)
          .with(Ok(P._), () => false)
          .exhaustive(),
    },
  );
  const form = useForm({}, async () => {
    trackSubmit("cancel-build");
    await cancelBuild(params.slug);
    build.reload();
    return Ok(null);
  });

  if (build.loading) return null;
  return (
    <main className={styles.container}>
      {match(build.data)
        .with(Ok(P.select()), (build) => (
          <>
            <Text type="h1" className={styles.h1}>
              {formatRunName(build)}
            </Text>
            <div className={`${styles.section} ${styles.summary}`}>
              {createHeaderProps(build).map(({ icon, label, url, value }) => (
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
            {build.status === "Pending" ? (
              <div className={`${styles.section} ${styles.actions}`}>
                <form {...form.props}>
                  <Button submit={true} style="warning">
                    Cancel build
                  </Button>
                </form>
              </div>
            ) : null}
            <div className={styles.section}>
              <Text type="h2" className={styles.h2}>
                Logs{" "}
                <Link
                  href={`/api/build/${params.slug}/logs/raw`}
                  target="_blank"
                  className={styles.downloadLogs}
                  title="Download Raw Logs"
                >
                  <DownloadIcon width={15} fill="#d6d3d1" />
                </Link>
              </Text>
              <BuildLog build={build} />
            </div>
          </>
        ))
        .with(Err(P._), () => (
          <Text>
            Uh oh! No build matching that description could be found. Either it
            doesn&apos;t exist, or you don&apos;t have access to it.
          </Text>
        ))
        .exhaustive()}
    </main>
  );
};

const formatDateTime = (date: Date | null): string => {
  if (!date) return "-";
  const day = date.toLocaleDateString(undefined, { dateStyle: "short" });
  const time = date.toLocaleTimeString(undefined, { hourCycle: "h24" });
  return `${day} ${time}`;
};

const formatPackageType = (packageType: string): string => {
  if (packageType === "nixosConfiguration") {
    return "NixOS Configuration";
  }
  return (
    packageType
      .replace(/([A-Z])/g, " $1")
      // uppercase the first character
      .replace(/^./, function (str) {
        return str.toUpperCase();
      })
  );
};

export default Page;
