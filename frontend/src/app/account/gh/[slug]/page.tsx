"use client";
import Image from "next/image";
import React from "react";
import { match, P } from "ts-pattern";
import { Link } from "@/components/link";
import { Berlin } from "@/utils/fonts";
import { Plan, getOrgUsage, setUsageLimits } from "@/services/account";
import {
  add,
  fromMinutes,
  subtract,
  toMinutes,
  toSecs,
} from "@/utils/duration";
import arrowLeft from "@/components/icons/arrow-left.svg";
import checkmark from "@/components/icons/success.svg";
import { useLoading } from "@/hooks/useLoading";
import { AppPage } from "@/utils/appPage";
import { Button } from "@/components/button";
import { useField, useForm } from "@/hooks/useForm";
import { UnstyledIntInput } from "@/components/input";
import { Err, Ok } from "@/services";
import { FormSubmitResult } from "@/components/formSubmitResult";
import styles from "./styles.module.css";

const Page = ({ params }: { params: Record<string, string> }) => {
  const orgName = params.slug!;
  const usageLoading = useLoading(
    React.useCallback(() => getOrgUsage(orgName), [orgName]),
    { poll: fromMinutes(1) },
  );
  if (usageLoading.loading) return null;
  if (!usageLoading.data.ok)
    return <div>{usageLoading.data.error.message}</div>;
  const usage = usageLoading.data.data;

  return (
    <div className={styles.root}>
      <header>
        <Link
          className={styles.backButton}
          href="/account"
          title="Back to account overview"
        >
          <Image src={arrowLeft} alt="Back" />
        </Link>
        <h1>Account/{orgName}</h1>
      </header>
      <h2>Current usage</h2>

      <div className={styles.threeUp}>
        <UsageMeter
          name="CI Minutes"
          current={toMinutes(usage.ci_time)}
          total={toMinutes(usage.plan.base_ci_time)}
          precision={2}
        />
        <UsageMeter
          name="PR Minutes"
          current={toMinutes(usage.pr_deployment_time)}
          total={toMinutes(usage.plan.maximum_pr_deployment_time)}
          precision={2}
        />
        <UsageMeter
          name="Hosts"
          current={usage.branch_deployment_hosts}
          total={usage.plan.included_branch_deployment_hosts}
          precision={0}
        />
      </div>

      <h2>Usage limits</h2>
      <div className={`${styles.panel} ${styles.section} ${Berlin.className}`}>
        <header className={styles.planHeader}>
          <span>Current plan</span>
          <span className={styles.planName}>{usage.plan.display_name}</span>
          <div style={{ flexGrow: 1 }} />
          {match(usage.installation_status)
            .with({ tag: "NoActiveInstallation" }, () => null)
            .with(
              { tag: "InstallationCancelling", contents: P.select() },
              (endDate) => (
                <span className={styles.installationStatus}>
                  Your subscription will end on {endDate.toDateString()}.
                </span>
              ),
            )
            .with(
              { tag: "InstallationRenewing", contents: P.select() },
              (renewalDate) => (
                <span className={styles.installationStatus}>
                  Your subscription will renew on {renewalDate.toDateString()}
                </span>
              ),
            )
            .exhaustive()}
        </header>
        <div className={`${styles.box} ${styles.planFeatures}`}>
          <header>Plan features</header>
          <ul>
            {[
              <>
                <strong>
                  {toMinutes(usage.plan.base_ci_time).toLocaleString(
                    undefined,
                    {
                      maximumFractionDigits: 0,
                    },
                  )}
                </strong>{" "}
                minutes/month
              </>,
              <>Public binary cache</>,
              <>
                <strong>
                  {toMinutes(
                    usage.plan.maximum_pr_deployment_time,
                  ).toLocaleString(undefined, { maximumFractionDigits: 0 })}
                </strong>{" "}
                pull-request deployment minutes (Beta)
              </>,
              <>
                <strong>
                  {usage.plan.included_branch_deployment_hosts} server
                </strong>{" "}
                deployment
                {usage.plan.included_branch_deployment_hosts !== 1 && "s"}{" "}
                (Alpha)
              </>,
            ].map((feature, i) => (
              <li key={i}>
                <Image src={checkmark} alt="" />
                <span>{feature}</span>
              </li>
            ))}
          </ul>
        </div>
      </div>

      {usage.plan.is_paid && (
        <UsageLimits org={params.slug!} plan={usage.plan} />
      )}
    </div>
  );
};

const UsageMeter = (props: {
  name: string;
  current: number;
  total: number;
  precision: number;
}) => {
  return (
    <div className={`${styles.usageMeter} ${styles.panel} ${Berlin.className}`}>
      <header>{props.name}</header>
      <div className={styles.values}>
        <strong>{props.current.toFixed(props.precision)}</strong> /{" "}
        {props.total.toFixed(props.precision)}
      </div>
      <progress value={props.current} max={props.total} />
      {props.total > 0 && (
        <div className={styles.percent}>
          {((props.current / props.total) * 100).toFixed(props.precision)}%
        </div>
      )}
    </div>
  );
};

const UsageLimits = ({ org, plan }: { org: string; plan: Plan }) => {
  const ciMinutes = useField(
    toMinutes(add(plan.base_ci_time, plan.extra_usage.ciTime)),
  );
  const prMinutes = useField(
    toMinutes(
      add(plan.maximum_pr_deployment_time, plan.extra_usage.prDeployTime),
    ),
  );
  const hostingSpend = useField(plan.extra_usage.hostingSpend);
  const form = useForm(
    { ciMinutes, prMinutes, hostingSpend },
    async ({ ciMinutes, prMinutes, hostingSpend }) => {
      const extraCiTime = subtract(fromMinutes(ciMinutes), plan.base_ci_time);
      const extraPrDeployTime = subtract(
        fromMinutes(prMinutes),
        plan.maximum_pr_deployment_time,
      );
      if (toSecs(extraCiTime) < 0) {
        return Err({
          message: `You cannot set your ci usage lower than what is provided by your plan (${toMinutes(plan.base_ci_time)}m)`,
        });
      }
      if (toSecs(extraPrDeployTime) < 0) {
        return Err({
          message: `You cannot set your pr usage lower than what is provided by your plan (${toMinutes(plan.maximum_pr_deployment_time)}m)`,
        });
      }
      const res = await setUsageLimits(org, {
        ciTime: extraCiTime,
        prDeployTime: extraPrDeployTime,
        hostingSpend,
      });
      if (!res.ok) return res;
      return Ok(null);
    },
  );

  return (
    <div className={`${styles.panel} ${styles.section} ${Berlin.className}`}>
      <header>Usage limits</header>
      <form {...form.props}>
        <div className={`${styles.usageLimits} ${styles.threeUp}`}>
          <div>
            <header>CI Minutes</header>
            <p>
              Your {plan.display_name.toUpperCase()} plan includes{" "}
              {toMinutes(plan.base_ci_time).toLocaleString(undefined, {
                maximumFractionDigits: 0,
              })}{" "}
              CI Minutes. Additional minutes may incur extra costs.
            </p>
            <div className={styles.input}>
              <UnstyledIntInput {...ciMinutes.props} /> minutes
            </div>
          </div>
          <div>
            <header>PR Deployment Minutes</header>
            <p>
              Your {plan.display_name.toUpperCase()} plan includes{" "}
              {toMinutes(plan.maximum_pr_deployment_time).toLocaleString(
                undefined,
                { maximumFractionDigits: 0 },
              )}{" "}
              PR Deployment Minutes (Beta).
            </p>
            <div className={styles.input}>
              <UnstyledIntInput {...prMinutes.props} /> minutes
            </div>
          </div>
          <div>
            <header>Deployed Hosts</header>
            <p>
              Your {plan.display_name.toUpperCase()} plan includes{" "}
              {plan.included_branch_deployment_hosts} deployed host
              {plan.included_branch_deployment_hosts !== 1 && "s"}. You can add
              more hosts by increasing your budget.
            </p>
            <div className={styles.input}>
              <UnstyledIntInput {...hostingSpend.props} /> $
            </div>
          </div>
        </div>
        <div style={{ marginTop: 20 }}>
          {!form.loading &&
            match(form.result)
              .with(null, () => null)
              .with(Ok(P._), () => (
                <FormSubmitResult success>
                  Usage limits updated.
                </FormSubmitResult>
              ))
              .with(Err({ message: P.select() }), (message) => (
                <FormSubmitResult>{message}</FormSubmitResult>
              ))
              .exhaustive()}
          <Button style="primaryInverse" submit loading={form.loading}>
            Update
          </Button>
        </div>
      </form>
    </div>
  );
};

export default AppPage(Page, { requireAuth: true });
