"use client";
import { P, match } from "ts-pattern";
import { Text } from "@/components/text";
import { useLoading } from "@/hooks/useLoading";
import { getAccountUsage, type InstallationStatus } from "@/services/account";
import { formatMinutes, fromMinutes } from "@/utils/duration";
import { Err, Ok } from "@/services";
import { Table } from "@/components/table";
import { Link } from "@/components/link";
import { Loading } from "@/components/loading";
import styles from "./styles.module.css";

const InstallationStatusCell = ({
  org,
  status,
}: {
  org: string;
  status: InstallationStatus;
}) =>
  match(status)
    .with("AppInstalled", () => <td>Installed</td>)
    .with("AppNotInstalled", () => <td>Not installed</td>)
    .with("AppInstalledWithoutMemberAccess", () => (
      <td>
        Installed
        <div className={styles.small}>
          (garnix hasn&apos;t been granted permission to read this
          organization&apos;s membership, so it can&apos;t tell who administers
          it. An admin can grant that in{" "}
          <Link
            href={`https://github.com/organizations/${org}/settings/installations`}
          >
            the organization&apos;s installation settings
          </Link>
          .)
        </div>
      </td>
    ))
    .exhaustive();

export const UsageComponent = () => {
  const usage = useLoading(getAccountUsage, { poll: fromMinutes(1) });
  return (
    <>
      <Text type="h2">Usage this month</Text>
      <Table className={styles.usageTable}>
        <thead>
          <tr>
            <th>Organization</th>
            <th>CI minutes</th>
            <th>garnix app</th>
          </tr>
        </thead>
        <tbody>
          {match(usage)
            .with({ loading: true }, () => (
              <tr>
                <td colSpan={3}>
                  <Loading />
                </td>
              </tr>
            ))
            .with({ data: Err(P.select()) }, (error) => (
              <tr>
                <td colSpan={3}>
                  <Text className={styles.error}>
                    Sorry, there was an error!
                  </Text>
                  <Text className={`${styles.error} ${styles.errorSmall}`}>
                    ({error.message})
                  </Text>
                </td>
              </tr>
            ))
            .with({ data: Ok(P.select()) }, (usage) => (
              <>
                {Object.entries(usage.by_org).map(([name, orgUsage]) => (
                  <tr key={name}>
                    <td>
                      <Link href={`https://github.com/${name}`}>{name}</Link>
                    </td>
                    <td>{formatMinutes(orgUsage.ci_time)}</td>
                    <InstallationStatusCell
                      org={name}
                      status={orgUsage.installation_status}
                    />
                  </tr>
                ))}
              </>
            ))
            .exhaustive()}
        </tbody>
      </Table>
    </>
  );
};
