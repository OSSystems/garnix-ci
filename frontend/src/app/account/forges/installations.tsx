"use client";
import React, { useCallback, useEffect, useState } from "react";
import Image from "next/image";
import { Text } from "@/components/text";
import { Button } from "@/components/button";
import { TextInput } from "@/components/input";
import { ToggleSwitch } from "@/components/toggleSwitch";
import { FloatingModal, ModalActions, ModalSection } from "@/components/modal";
import { StatusIcon } from "@/components/statusIcon";
import arrowIcon from "@/components/icons/arrow-right.svg";
import lockIcon from "@/components/icons/lock.svg";
import { useLoading } from "@/hooks/useLoading";
import { useAction } from "@/hooks/useAction";
import { APIResult, Ok, userMessage } from "@/services";
import {
  Installation,
  InstallationKind,
  InstallationRepo,
  InstallationRepos,
  disableRepo,
  enableRepo,
  getAllInstallationRepos,
  getInstallable,
  getInstallationRepos,
  getInstallations,
  install,
  reinstall,
  uninstall,
} from "@/services/installations";
import styles from "./installations.module.css";

export const reposPerPage = 20;

export const Status = (props: { ok: boolean; children: React.ReactNode }) => (
  <span className={styles.status}>
    <StatusIcon status={props.ok ? "Success" : "Failure"} />
    <Text type="span" className={styles.small}>
      {props.children}
    </Text>
  </span>
);

type Row = {
  kind: InstallationKind;
  target: string;
  id: number | null;
  installation?: Installation;
};

const loadRows = async (
  forge: string,
): Promise<
  APIResult<{ rows: Array<Row>; installations: Array<Installation> }>
> => {
  const [installable, installations] = await Promise.all([
    getInstallable(forge),
    getInstallations(forge),
  ]);
  if (!installable.ok) return installable;
  if (!installations.ok) return installations;
  return Ok({
    installations: installations.data,
    rows: installable.data.map((entry) => ({
      kind: entry.kind,
      target: entry.target,
      id: entry.installation,
      installation: installations.data.find(
        (installation) => installation.id === entry.installation,
      ),
    })),
  });
};

const kinds: Record<
  InstallationKind,
  { label: string; describe: (target: string) => string }
> = {
  user: {
    label: "Your account",
    describe: (target) => `the account ${target}`,
  },
  org: {
    label: "Organisation",
    describe: (target) => `the organisation ${target}`,
  },
  instance: { label: "Instance", describe: () => "the whole instance" },
};

const describe = (installation: { kind: InstallationKind; target: string }) =>
  kinds[installation.kind].describe(installation.target);

const formatDate = (iso: string) =>
  new Date(iso).toLocaleDateString("en-GB", {
    day: "numeric",
    month: "short",
    year: "numeric",
  });

export const Installations = (props: { forge: string }) => {
  const { forge } = props;
  const loader = useCallback(() => loadRows(forge), [forge]);
  const loaded = useLoading(loader);
  return (
    <div className={styles.installations}>
      <Text type="h3" className={styles.subheading}>
        Installations
      </Text>
      {loaded.loading ? (
        <Text className={styles.small}>Loading installations…</Text>
      ) : !loaded.data.ok ? (
        <Text className={styles.error}>
          Failed to load the installations: {userMessage(loaded.data.error)}
        </Text>
      ) : loaded.data.data.rows.length === 0 ? (
        <Text className={styles.small}>
          There is nothing on this forge you may install garnix on.
        </Text>
      ) : (
        <div className={styles.installationList}>
          {loaded.data.data.rows.map((row) => (
            <InstallationRow
              key={`${row.kind}/${row.target}`}
              forge={forge}
              row={row}
              installations={
                loaded.data.ok ? loaded.data.data.installations : []
              }
              reload={loaded.reload}
            />
          ))}
        </div>
      )}
    </div>
  );
};

const InstallationRow = (props: {
  forge: string;
  row: Row;
  installations: Array<Installation>;
  reload: () => void;
}) => {
  const { forge, row } = props;
  const [open, setOpen] = useState(false);
  const { busy, error, go } = useAction();
  const id = row.id;
  const needsReinstall = row.installation?.status === "needs_reinstall";

  const onInstall = () =>
    go(() => install(forge, row.kind, row.target), props.reload);
  const onReinstall = (id: number) =>
    go(() => reinstall(forge, id), props.reload);

  return (
    <div
      className={`${styles.installation} ${open ? styles.open : ""}`}
      data-testid={`installation-${row.kind}-${row.target}`}
    >
      <div
        className={`${styles.installationRow} ${id != null ? styles.expandable : ""}`}
        onClick={id != null ? () => setOpen(!open) : undefined}
      >
        <Text type="span" className={styles.small}>
          {kinds[row.kind].label}
        </Text>
        <span className={styles.target}>
          <Text type="code">{row.target || "Whole instance"}</Text>
          {id != null && (
            <Status ok={!needsReinstall}>
              {needsReinstall ? "Needs reinstall" : "Active"}
            </Status>
          )}
        </span>
        <span className={styles.actions} onClick={(e) => e.stopPropagation()}>
          {id == null ? (
            <Button loading={busy} onClick={onInstall}>
              Install
            </Button>
          ) : (
            needsReinstall && (
              <Button loading={busy} onClick={() => onReinstall(id)}>
                Reinstall
              </Button>
            )
          )}
          {id != null && (
            <button
              type="button"
              className={styles.chevron}
              aria-expanded={open}
              aria-label={`Repositories of ${describe(row)}`}
              onClick={() => setOpen(!open)}
            >
              <Image src={arrowIcon} alt="" />
            </button>
          )}
        </span>
      </div>
      {needsReinstall && row.installation?.reason && (
        <Text className={`${styles.error} ${styles.reason}`}>
          {row.installation.reason}
        </Text>
      )}
      {error && (
        <Text className={`${styles.error} ${styles.reason}`}>{error}</Text>
      )}
      {open && id != null && (
        <Repos
          forge={forge}
          id={id}
          description={describe(row)}
          installation={row.installation}
          installations={props.installations}
          onUninstalled={() => {
            setOpen(false);
            props.reload();
          }}
        />
      )}
    </div>
  );
};

type Loaded<T> = { loading: true } | { loading: false; result: APIResult<T> };

const mapLoaded = <T, U>(loaded: Loaded<T>, f: (t: T) => U): Loaded<U> =>
  loaded.loading
    ? loaded
    : {
        loading: false,
        result: loaded.result.ok ? Ok(f(loaded.result.data)) : loaded.result,
      };

const withEnabledIn = (
  repos: Array<InstallationRepo>,
  repo: InstallationRepo,
  enabledIn: number | null,
) =>
  repos.map((r) =>
    r.owner === repo.owner && r.name === repo.name ? { ...r, enabledIn } : r,
  );

const Repos = (props: {
  forge: string;
  id: number;
  description: string;
  installation?: Installation;
  installations: Array<Installation>;
  onUninstalled: () => void;
}) => {
  const { forge, id } = props;
  const [page, setPage] = useState(1);
  const [reloads, setReloads] = useState(0);
  const [paged, setPaged] = useState<Loaded<InstallationRepos>>({
    loading: true,
  });
  const [everything, setEverything] = useState<Loaded<Array<InstallationRepo>>>(
    { loading: true },
  );
  const [filter, setFilter] = useState("");
  const [onlyBuilding, setOnlyBuilding] = useState(false);
  const [confirming, setConfirming] = useState(false);
  const uninstalling = useAction();
  const needle = filter.trim().toLowerCase();
  const filtering = needle !== "" || onlyBuilding;

  useEffect(() => {
    let current = true;
    void getInstallationRepos(forge, id, page, reposPerPage).then((result) => {
      if (current) setPaged({ loading: false, result });
    });
    return () => {
      current = false;
    };
  }, [forge, id, page, reloads]);

  useEffect(() => {
    if (!filtering) return;
    let current = true;
    void getAllInstallationRepos(forge, id).then((result) => {
      if (current) setEverything({ loading: false, result });
    });
    return () => {
      current = false;
    };
  }, [forge, id, filtering, reloads]);

  const setEnabledIn = (repo: InstallationRepo, enabledIn: number | null) => {
    setPaged((paged) =>
      mapLoaded(paged, (data) => ({
        ...data,
        repos: withEnabledIn(data.repos, repo, enabledIn),
      })),
    );
    setEverything((everything) =>
      mapLoaded(everything, (repos) => withEnabledIn(repos, repo, enabledIn)),
    );
  };

  const heldBy = (other: number) => {
    const holder = props.installations.find(
      (installation) => installation.id === other,
    );
    return holder ? describe(holder) : "another installation";
  };

  const doUninstall = () => {
    setConfirming(false);
    return uninstalling.go(() => uninstall(forge, id), props.onUninstalled);
  };

  const shown = filtering
    ? mapLoaded(everything, (repos) =>
        repos.filter(
          (repo) =>
            `${repo.owner}/${repo.name}`.toLowerCase().includes(needle) &&
            (!onlyBuilding || repo.enabledIn === id),
        ),
      )
    : mapLoaded(paged, (data) => data.repos);

  return (
    <div className={styles.repos}>
      <div className={styles.tools}>
        <TextInput
          className={styles.grow}
          aria-label="Filter repositories"
          placeholder="Filter repositories"
          value={filter}
          onChange={setFilter}
        />
        <div className={styles.segmented}>
          <Button
            style={onlyBuilding ? "primary" : "primaryInverse"}
            onClick={() => setOnlyBuilding(false)}
          >
            All
          </Button>
          <Button
            style={onlyBuilding ? "primaryInverse" : "primary"}
            onClick={() => setOnlyBuilding(true)}
          >
            Building
          </Button>
        </div>
      </div>
      {shown.loading ? (
        <Text className={styles.small}>Loading repositories…</Text>
      ) : !shown.result.ok ? (
        <Text className={styles.error}>
          Failed to load the repositories: {userMessage(shown.result.error)}
        </Text>
      ) : (
        <>
          <div className={styles.repoList}>
            {shown.result.data.map((repo) => (
              <RepoRow
                key={`${repo.owner}/${repo.name}`}
                forge={forge}
                id={id}
                repo={repo}
                heldBy={heldBy}
                setEnabledIn={(enabledIn) => setEnabledIn(repo, enabledIn)}
                onRefused={() => setReloads((reloads) => reloads + 1)}
              />
            ))}
          </div>
          {filtering
            ? shown.result.data.length === 0 && (
                <Text className={`${styles.small} ${styles.pager}`}>
                  No repositories match.
                </Text>
              )
            : !paged.loading &&
              paged.result.ok && (
                <Pager
                  page={page}
                  shown={paged.result.data.repos.length}
                  total={paged.result.data.total}
                  setPage={setPage}
                />
              )}
        </>
      )}
      <div className={styles.footer}>
        <Text className={styles.small}>
          {props.installation &&
            `Installed by ${props.installation.installer} on ${formatDate(props.installation.createdAt)}`}
        </Text>
        <Button
          style="warning"
          loading={uninstalling.busy}
          onClick={() => setConfirming(true)}
        >
          Uninstall
        </Button>
      </div>
      {uninstalling.error && (
        <Text className={styles.error}>{uninstalling.error}</Text>
      )}
      {confirming && (
        <FloatingModal onRequestClose={() => setConfirming(false)}>
          <ModalSection>
            <Text type="h2">Uninstall garnix from {props.description}?</Text>
          </ModalSection>
          <ModalSection>
            <Text>
              garnix stops building its repositories and deletes its webhook.
              Past builds stay.
            </Text>
          </ModalSection>
          <ModalSection>
            <ModalActions align="right">
              <Button style="secondary" onClick={() => setConfirming(false)}>
                Cancel
              </Button>
              <Button style="warning" onClick={doUninstall}>
                Uninstall
              </Button>
            </ModalActions>
          </ModalSection>
        </FloatingModal>
      )}
    </div>
  );
};

const RepoRow = (props: {
  forge: string;
  id: number;
  repo: InstallationRepo;
  heldBy: (other: number) => string;
  setEnabledIn: (enabledIn: number | null) => void;
  onRefused: () => void;
}) => {
  const { forge, id, repo } = props;
  const { busy, error, go } = useAction();
  const elsewhere = repo.enabledIn != null && repo.enabledIn !== id;
  const fullName = `${repo.owner}/${repo.name}`;

  const toggle = async (on: boolean) => {
    let done = false;
    await go(
      () =>
        on
          ? enableRepo(forge, id, repo.owner, repo.name)
          : disableRepo(forge, id, repo.owner, repo.name),
      () => {
        done = true;
        props.setEnabledIn(on ? id : null);
      },
    );
    if (!done) props.onRefused();
  };

  return (
    <div className={styles.repo} data-testid={`repo-${fullName}`}>
      <span className={styles.repoName}>
        <Text type="code">
          <span className={styles.owner}>{repo.owner}/</span>
          {repo.name}
        </Text>
        {repo.private && (
          <Image
            src={lockIcon}
            alt="private"
            title="private"
            width={12}
            height={12}
            className={styles.lock}
          />
        )}
      </span>
      <Text type="span" className={styles.small}>
        {elsewhere && `Builds through ${props.heldBy(repo.enabledIn!)}`}
      </Text>
      <ToggleSwitch
        aria-label={`Build ${fullName}`}
        value={repo.enabledIn === id}
        disabled={elsewhere || busy}
        onChange={(on) => void toggle(on)}
      />
      {error && (
        <Text className={`${styles.error} ${styles.repoError}`}>{error}</Text>
      )}
    </div>
  );
};

const Pager = (props: {
  page: number;
  shown: number;
  total: number;
  setPage: (page: number) => void;
}) => {
  const first = (props.page - 1) * reposPerPage + 1;
  const last = first + props.shown - 1;
  return (
    <div className={styles.pager}>
      <Text className={styles.small}>
        {props.shown === 0
          ? `None of ${props.total}`
          : `${first}–${last} of ${props.total}`}
      </Text>
      <div className={styles.row}>
        <Button
          style="secondary"
          disabled={props.page <= 1}
          onClick={() => props.setPage(props.page - 1)}
        >
          Previous
        </Button>
        <Button
          style="secondary"
          disabled={last >= props.total}
          onClick={() => props.setPage(props.page + 1)}
        >
          Next
        </Button>
      </div>
    </div>
  );
};
