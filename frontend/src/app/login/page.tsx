"use client";

import { useRouter } from "next/navigation";
import { useState } from "react";
import { Text } from "@/components/text";
import { Button } from "@/components/button";
import { TextInput } from "@/components/input";
import { Modal, ModalSection } from "@/components/modal";
import { GithubIcon } from "@/components/icons/github";
import { getLoginLink, setLoginTargetPage } from "@/services/auth";
import { Forge, forgeLabel, getForges, startAuth } from "@/services/forges";
import { userMessage } from "@/services";
import { useLoading } from "@/hooks/useLoading";
import { leaving, useAction } from "@/hooks/useAction";
import { goTo } from "@/utils/navigate";
import styles from "./styles.module.css";

type PageProps = {
  searchParams: Record<string, string>;
};

const Page = (props: PageProps) => {
  const page = props.searchParams.page || null;
  const router = useRouter();
  const forges = useLoading(getForges);
  const [url, setUrl] = useState("");
  const { busy, error, go } = useAction();

  const logInThrough = (forge: Forge) =>
    go(
      () => getLoginLink(page, forge.slug),
      (link) => {
        goTo(link);
        return leaving;
      },
    );

  const logInAt = () =>
    go(
      () => startAuth(url.trim()),
      (answer) => {
        setLoginTargetPage(page ?? "/");
        if (answer.t === "login") {
          goTo(answer.link);
        } else {
          // The registration page asks again: it trusts no slug or redirect
          // URI from its own URL.
          const query = new URLSearchParams({ url: url.trim() });
          router.push(`/login/register?${query.toString()}`);
        }
        return leaving;
      },
    );

  return (
    <div className={styles.container}>
      <Modal>
        <ModalSection className={styles.section}>
          <Text type="h1">Log in</Text>
          {forges.loading ? null : !forges.data.ok ? (
            <Text className={styles.error}>
              Could not list the forges: {userMessage(forges.data.error)}
            </Text>
          ) : (
            <div className={styles.forges}>
              {forges.data.data
                .filter((forge) => forge.status === "active")
                .map((forge) => (
                  <Button
                    key={forge.slug}
                    loading={busy}
                    eventName="login-with-forge"
                    onClick={() => logInThrough(forge)}
                  >
                    {forge.kind === "github" && <GithubIcon />}
                    Log in with {forgeLabel(forge)}
                  </Button>
                ))}
            </div>
          )}
        </ModalSection>
        <ModalSection className={styles.section}>
          <Text type="h3">Other Gitea/Forgejo</Text>
          <form
            className={styles.other}
            onSubmit={(e) => {
              e.preventDefault();
              void logInAt();
            }}
          >
            <TextInput
              className={styles.url}
              aria-label="Gitea/Forgejo URL"
              type="url"
              placeholder="https://git.acme.com"
              required
              value={url}
              onChange={setUrl}
              disabled={busy}
            />
            <Button submit loading={busy}>
              Log in
            </Button>
          </form>
          {error && (
            <Text className={styles.error} data-testid="login-error">
              {error}
            </Text>
          )}
        </ModalSection>
      </Modal>
    </div>
  );
};

export default Page;
