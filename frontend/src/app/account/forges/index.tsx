"use client";
import React, { useState } from "react";
import { useRouter } from "next/navigation";
import { Text } from "@/components/text";
import { Button } from "@/components/button";
import { TextInput } from "@/components/input";
import { FloatingModal, ModalActions, ModalSection } from "@/components/modal";
import { useLoading } from "@/hooks/useLoading";
import { leaving, useAction } from "@/hooks/useAction";
import { APIResult, Ok, userMessage } from "@/services";
import {
  Identity,
  disconnectIdentity,
  getConnectLink,
  getCurrentUser,
  getIdentities,
  setLoginTargetPage,
} from "@/services/auth";
import { useUser } from "@/store/userContext";
import { useConfig } from "@/store/configContext";
import {
  Forge,
  disableForge,
  forgeLabel,
  getForges,
  replaceForgeSecret,
  startAuth,
} from "@/services/forges";
import { goTo } from "@/utils/navigate";
import { followStartAnswer } from "@/app/login/register/register";
import { Installations, Status } from "./installations";
import styles from "./styles.module.css";

type Loaded = { forges: Array<Forge>; identities: Array<Identity> };

const loadForgesAndIdentities = async (): Promise<APIResult<Loaded>> => {
  const [forges, identities] = await Promise.all([
    getForges(),
    getIdentities(),
  ]);
  if (!forges.ok) return forges;
  if (!identities.ok) return identities;
  return Ok({ forges: forges.data, identities: identities.data });
};

export const ForgesComponent = () => {
  const loaded = useLoading(loadForgesAndIdentities);
  if (loaded.loading) return null;
  if (!loaded.data.ok) {
    return (
      <Text className={styles.error}>
        Failed to load your forges: {userMessage(loaded.data.error)}
      </Text>
    );
  }
  const { forges, identities } = loaded.data.data;
  const onlyIdentity = identities.length <= 1;
  return (
    <div className={styles.section}>
      <Text type="h2">Forges</Text>
      <Text className={styles.small}>
        You log in to this account with any of these. Install garnix where your
        repositories live, then choose which ones build.
      </Text>
      {forges
        .filter((forge) => forge.status === "active" || forge.canManage)
        .map((forge) => (
          <ForgeBlock
            key={forge.slug}
            forge={forge}
            identities={identities}
            reload={loaded.reload}
          />
        ))}
      {onlyIdentity && (
        <Text className={`${styles.small} ${styles.note}`}>
          You cannot disconnect the only forge you log in with. Connect or
          register another forge first.
        </Text>
      )}
      <RegisterForge />
    </div>
  );
};

const ForgeBlock = (props: {
  forge: Forge;
  identities: Array<Identity>;
  reload: () => void;
}) => {
  const { forge } = props;
  const { busy, error, go } = useAction();
  const [confirming, setConfirming] = useState(false);
  const [settingsOpen, setSettingsOpen] = useState(forge.status === "disabled");
  const { setUser } = useUser();
  const { githubAppName } = useConfig();
  const identity = props.identities.find(
    (identity) => identity.forge === forge.slug,
  );
  const onlyIdentity = props.identities.length <= 1;
  const disabled = forge.status === "disabled";
  const isGithub = forge.kind === "github";

  // The account's name is its main identity's login: it may have just gone.
  const refreshUser = async () => {
    const current = await getCurrentUser();
    if (current.ok && current.data != null) setUser(current.data);
  };

  const disconnect = (deleteModuleSettings: boolean) => {
    setConfirming(false);
    return go(
      () => disconnectIdentity(forge.slug, deleteModuleSettings),
      (result) => {
        if (result === "holds-module-settings") {
          setConfirming(true);
        } else {
          void refreshUser();
          props.reload();
        }
      },
    );
  };

  const connect = () =>
    go(
      () => getConnectLink(forge.slug),
      (link) => {
        goTo(link);
        return leaving;
      },
    );

  return (
    <div className={styles.forge} data-testid={`forge-${forge.slug}`}>
      <div className={styles.forgeHeader}>
        <Text type="h3">{forgeLabel(forge)}</Text>
        <Text type="span" className={styles.small}>
          {identity?.login ?? (disabled ? "" : "Not connected")}
        </Text>
        {!isGithub && (
          <Status ok={!disabled}>{disabled ? "Disabled" : "Active"}</Status>
        )}
        <span className={styles.spacer} />
        {isGithub && identity && githubAppName && (
          <Button
            style="secondary"
            href={`https://github.com/apps/${githubAppName}/installations/new`}
          >
            Manage installations
          </Button>
        )}
        {forge.canManage && (
          <Button
            style="secondary"
            onClick={() => setSettingsOpen(!settingsOpen)}
          >
            Settings
          </Button>
        )}
        {identity ? (
          <Button
            style="warning"
            loading={busy}
            disabled={onlyIdentity}
            title={
              onlyIdentity
                ? "This is the only forge you log in with. Connect another forge before disconnecting it."
                : undefined
            }
            onClick={() => disconnect(false)}
          >
            Disconnect
          </Button>
        ) : (
          !disabled && (
            <Button loading={busy} onClick={connect}>
              Connect
            </Button>
          )
        )}
      </div>
      {isGithub ? (
        identity && (
          <Text className={styles.small}>
            Installations and repositories are managed through the garnix GitHub
            App.
          </Text>
        )
      ) : (
        <Text className={styles.small}>{forge.webUrl}</Text>
      )}
      {error && <Text className={styles.error}>{error}</Text>}
      {forge.canManage && settingsOpen && (
        <ForgeSettings
          forge={forge}
          identities={props.identities}
          reload={props.reload}
        />
      )}
      {!isGithub && identity && !disabled && (
        <Installations forge={forge.slug} />
      )}
      {confirming && (
        <FloatingModal onRequestClose={() => setConfirming(false)}>
          <ModalSection>
            <Text type="h2">Disconnect {forgeLabel(forge)}?</Text>
          </ModalSection>
          <ModalSection>
            <Text>
              Module configurations were saved through your {forgeLabel(forge)}{" "}
              login. Disconnecting it deletes them with it. This cannot be
              undone.
            </Text>
          </ModalSection>
          <ModalSection>
            <ModalActions align="right">
              <Button style="secondary" onClick={() => setConfirming(false)}>
                Cancel
              </Button>
              <Button style="warning" onClick={() => disconnect(true)}>
                Disconnect and delete the configurations
              </Button>
            </ModalActions>
          </ModalSection>
        </FloatingModal>
      )}
    </div>
  );
};

const RegisterForge = () => {
  const router = useRouter();
  const [url, setUrl] = useState("");
  const { busy, error, go } = useAction();

  const register = () =>
    go(
      () => startAuth(url.trim()),
      (answer) => {
        setLoginTargetPage("/account");
        return followStartAnswer(router, url.trim(), "account")(answer);
      },
    );

  return (
    <div className={styles.addForge}>
      <Text type="h3">Add a Gitea/Forgejo instance</Text>
      <Text className={styles.small}>
        Connects it to this account, registering it first if garnix does not
        know it yet.
      </Text>
      <form
        className={styles.row}
        onSubmit={(e) => {
          e.preventDefault();
          void register();
        }}
      >
        <TextInput
          className={styles.grow}
          aria-label="Gitea/Forgejo URL"
          type="url"
          placeholder="https://git.acme.com"
          required
          value={url}
          onChange={setUrl}
          disabled={busy}
        />
        <Button submit loading={busy}>
          Add
        </Button>
      </form>
      {error && (
        <Text className={styles.error} data-testid="register-error">
          {error}
        </Text>
      )}
    </div>
  );
};

const quarantineWarning =
  "If this forge stays disabled for 30 days, registering it again deletes its old identities.";

const ForgeSettings = (props: {
  forge: Forge;
  identities: Array<Identity>;
  reload: () => void;
}) => {
  const { forge } = props;
  // A session holds only identities on active forges: without one elsewhere,
  // disabling this forge ends the caller's own session, and they can no
  // longer re-enable it from this account.
  const logsOutCaller = props.identities.every(
    (identity) => identity.forge === forge.slug,
  );
  const [replacing, setReplacing] = useState(false);
  const [confirmingDisable, setConfirmingDisable] = useState(false);
  const { busy, error, go } = useAction();
  const disabled = forge.status === "disabled";

  const disable = () => {
    setConfirmingDisable(false);
    return go(() => disableForge(forge.slug), props.reload);
  };

  return (
    <div
      className={styles.settings}
      data-testid={`managed-forge-${forge.slug}`}
    >
      {replacing ? (
        <SecretForm
          forge={forge}
          submitLabel={disabled ? "Re-enable" : "Replace"}
          onCancel={() => setReplacing(false)}
          onDone={() => {
            setReplacing(false);
            props.reload();
          }}
        />
      ) : disabled ? (
        <div className={styles.row}>
          <Text>Logins and sessions through this forge are ended.</Text>
          <Button onClick={() => setReplacing(true)}>
            Re-enable with a new secret
          </Button>
        </div>
      ) : (
        <div className={styles.row}>
          <Text>Client secret: configured ✓</Text>
          <Button style="secondary" onClick={() => setReplacing(true)}>
            Replace
          </Button>
        </div>
      )}
      {!disabled && (
        <div className={styles.row}>
          <Button
            style="warning"
            loading={busy}
            onClick={() => setConfirmingDisable(true)}
          >
            Disable
          </Button>
          <Text className={styles.small}>{quarantineWarning}</Text>
        </div>
      )}
      {disabled && <Text className={styles.small}>{quarantineWarning}</Text>}
      {error && <Text className={styles.error}>{error}</Text>}
      {confirmingDisable && (
        <FloatingModal onRequestClose={() => setConfirmingDisable(false)}>
          <ModalSection>
            <Text type="h2">Disable {forge.name}?</Text>
          </ModalSection>
          <ModalSection>
            <Text>
              This ends all logins and sessions through {forge.name}. Nothing is
              deleted: re-enable it with a new client secret.
            </Text>
            {logsOutCaller && (
              <Text className={styles.error} data-testid="disable-logs-out">
                You log in only through {forge.name}: disabling it logs you out,
                and this account cannot re-enable it. Registering {forge.name}{" "}
                again within 30 days brings it back with everyone who logged in
                through it.
              </Text>
            )}
            <Text className={styles.small}>{quarantineWarning}</Text>
          </ModalSection>
          <ModalSection>
            <ModalActions align="right">
              <Button
                style="secondary"
                onClick={() => setConfirmingDisable(false)}
              >
                Cancel
              </Button>
              <Button style="warning" loading={busy} onClick={disable}>
                Disable
              </Button>
            </ModalActions>
          </ModalSection>
        </FloatingModal>
      )}
    </div>
  );
};

// The secret is write-only: it is replaced, never shown.
const SecretForm = (props: {
  forge: Forge;
  submitLabel: string;
  onCancel: () => void;
  onDone: () => void;
}) => {
  const [secret, setSecret] = useState("");
  const { busy, error, go } = useAction();
  return (
    <form
      className={styles.row}
      onSubmit={(e) => {
        e.preventDefault();
        void go(
          () => replaceForgeSecret(props.forge.slug, secret),
          props.onDone,
        );
      }}
    >
      <TextInput
        label="New client secret"
        type="password"
        autoComplete="off"
        required
        value={secret}
        onChange={setSecret}
        disabled={busy}
      />
      <Button submit loading={busy}>
        {props.submitLabel}
      </Button>
      <Button style="secondary" onClick={props.onCancel}>
        Cancel
      </Button>
      {error && <Text className={styles.error}>{error}</Text>}
    </form>
  );
};
