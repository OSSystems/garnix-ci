"use client";
import { P, match } from "ts-pattern";
import { Text } from "@/components/text";
import { useLoading } from "@/hooks/useLoading";
import { getAccountUsage } from "@/services/account";
import { formatMinutes, fromMinutes } from "@/utils/duration";
import { Err, Ok } from "@/services";
import { Table } from "@/components/table";
import { Berlin } from "@/utils/fonts";
import { Link } from "@/components/link";
import { Loading } from "@/components/loading";
import styles from "./styles.module.css";

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
          </tr>
        </thead>
        <tbody>
          {match(usage)
            .with({ loading: true }, () => (
              <tr>
                <td colSpan={2}>
                  <Loading />
                </td>
              </tr>
            ))
            .with({ data: Err(P.select()) }, (error) => (
              <tr>
                <td colSpan={2}>
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
                  </tr>
                ))}
              </>
            ))
            .exhaustive()}
        </tbody>
      </Table>
      <div className={`${Berlin.className} ${styles.small}`}>
        (Some GitHub organizations may not show up here if the
        organization&apos;s admins haven&apos;t granted us permission to query
        membership information. If you&apos;re an admin you can review
        permissions by visiting
        https://github.com/organizations/$YOUR_ORG_NAME/settings/installations.)
      </div>
    </>
  );
};
