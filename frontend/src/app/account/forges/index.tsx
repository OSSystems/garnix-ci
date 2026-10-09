"use client";
import React, { useState } from "react";
import { Table } from "@/components/table";
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
} from "@/services/auth";
import { useUser } from "@/store/userContext";
import {
  Forge,
  disableForge,
  forgeLabel,
  getForges,
  replaceForgeSecret,
} from "@/services/forges";
import { goTo } from "@/utils/navigate";
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

// The account's identities, one per forge, and the forges it may still
// connect; and the registered forges the account may manage.
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
  const managed = forges.filter((forge) => forge.canManage);
  return (
    <>
      <div className={styles.section}>
        <ConnectedForges
          forges={forges.filter((forge) => forge.status === "active")}
          identities={identities}
          reload={loaded.reload}
        />
      </div>
      {managed.length > 0 && (
        <div className={styles.section}>
          <RegisteredForges
            forges={managed}
            identities={identities}
            reload={loaded.reload}
          />
        </div>
      )}
    </>
  );
};

const ConnectedForges = (props: {
  forges: Array<Forge>;
  identities: Array<Identity>;
  reload: () => void;
}) => {
  const { busy, error, go } = useAction();
  // The forge whose button the running action belongs to.
  const [acting, setActing] = useState<string>();
  const [confirming, setConfirming] = useState<Forge>();
  const { setUser } = useUser();
  const onlyIdentity = props.identities.length <= 1;
  const busyOn = (forge: Forge) => busy && acting === forge.slug;

  // The account's name is its main identity's login: it may have just gone.
  const refreshUser = async () => {
    const current = await getCurrentUser();
    if (current.ok && current.data != null) setUser(current.data);
  };

  const disconnect = (forge: Forge, deleteModuleSettings: boolean) => {
    setActing(forge.slug);
    // A refusal is shown beside the table, not in the dialog.
    setConfirming(undefined);
    return go(
      () => disconnectIdentity(forge.slug, deleteModuleSettings),
      (result) => {
        if (result === "holds-module-settings") {
          setConfirming(forge);
        } else {
          void refreshUser();
          props.reload();
        }
      },
    );
  };

  const connect = (forge: Forge) => {
    setActing(forge.slug);
    return go(
      () => getConnectLink(forge.slug),
      (link) => {
        goTo(link);
        return leaving;
      },
    );
  };

  return (
    <>
      <Text type="h2">Connected forges</Text>
      <Text className={styles.small}>
        You log in to this account with any of these. Connecting a forge logs
        you in to it there.
      </Text>
      <Table className={styles.table}>
        <thead>
          <tr>
            <th>Forge</th>
            <th>Login</th>
            <th></th>
          </tr>
        </thead>
        <tbody>
          {props.forges.map((forge) => {
            const identity = props.identities.find(
              (identity) => identity.forge === forge.slug,
            );
            return (
              <tr key={forge.slug}>
                <td>{forgeLabel(forge)}</td>
                <td>{identity?.login ?? "Not connected"}</td>
                <td>
                  {identity ? (
                    <Button
                      style="warning"
                      loading={busyOn(forge)}
                      disabled={onlyIdentity}
                      title={
                        onlyIdentity
                          ? "This is the only forge you log in with. Connect another forge before disconnecting it."
                          : undefined
                      }
                      onClick={() => disconnect(forge, false)}
                    >
                      Disconnect
                    </Button>
                  ) : (
                    <Button
                      loading={busyOn(forge)}
                      onClick={() => connect(forge)}
                    >
                      Connect
                    </Button>
                  )}
                </td>
              </tr>
            );
          })}
        </tbody>
      </Table>
      {onlyIdentity && (
        <Text className={styles.small}>
          You cannot disconnect the only forge you log in with. Connect another
          forge first.
        </Text>
      )}
      {error && <Text className={styles.error}>{error}</Text>}
      {confirming && (
        <FloatingModal onRequestClose={() => setConfirming(undefined)}>
          <ModalSection>
            <Text type="h2">Disconnect {forgeLabel(confirming)}?</Text>
          </ModalSection>
          <ModalSection>
            <Text>
              Module configurations were saved through your{" "}
              {forgeLabel(confirming)} login. Disconnecting it deletes them with
              it. This cannot be undone.
            </Text>
          </ModalSection>
          <ModalSection>
            <ModalActions align="right">
              <Button
                style="secondary"
                onClick={() => setConfirming(undefined)}
              >
                Cancel
              </Button>
              <Button
                style="warning"
                onClick={() => disconnect(confirming, true)}
              >
                Disconnect and delete the configurations
              </Button>
            </ModalActions>
          </ModalSection>
        </FloatingModal>
      )}
    </>
  );
};

const quarantineWarning =
  "If this forge stays disabled for 30 days, registering it again deletes its old identities.";

const RegisteredForges = (props: {
  forges: Array<Forge>;
  identities: Array<Identity>;
  reload: () => void;
}) => (
  <>
    <Text type="h2">Registered forges</Text>
    <Text className={styles.small}>
      Gitea/Forgejo instances registered through garnix that you may manage.
    </Text>
    {props.forges.map((forge) => (
      <ManagedForge
        key={forge.slug}
        forge={forge}
        identities={props.identities}
        reload={props.reload}
      />
    ))}
  </>
);

const ManagedForge = (props: {
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
    <div className={styles.forge} data-testid={`managed-forge-${forge.slug}`}>
      <div className={styles.forgeHeader}>
        <Text type="h3">{forge.name}</Text>
        <Text className={disabled ? styles.disabled : styles.small}>
          {disabled ? "Disabled" : "Active"}
        </Text>
      </div>
      <Text className={styles.small}>{forge.webUrl}</Text>
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
