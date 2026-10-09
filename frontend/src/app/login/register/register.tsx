"use client";

import { useRouter } from "next/navigation";
import { useCallback, useEffect, useState } from "react";
import { Text } from "@/components/text";
import { Button } from "@/components/button";
import { TextInput } from "@/components/input";
import { Link } from "@/components/link";
import { Modal, ModalActions, ModalSection } from "@/components/modal";
import { leaving, useAction } from "@/hooks/useAction";
import { Loading, useLoading } from "@/hooks/useLoading";
import { APIResult, userMessage } from "@/services";
import {
  StartAnswer,
  registerForge,
  registrationError,
  startAuth,
} from "@/services/forges";
import { goTo } from "@/utils/navigate";
import styles from "./styles.module.css";

type RegisterView =
  | { t: "loading" }
  // Only a forge garnix does not know yet is registered here: anything else
  // starts again from the login page.
  | { t: "leave" }
  | { t: "refused"; message: string }
  | { t: "form"; slug: string; callback: string };

// What the page shows for the backend's answer about its URL. The slug and
// the redirect URI come from that answer, never from the page's own URL,
// which anybody may have crafted.
export const registerView = (
  answer: Loading<APIResult<StartAnswer>>,
): RegisterView => {
  if (answer.loading) return { t: "loading" };
  if (!answer.data.ok)
    return { t: "refused", message: userMessage(answer.data.error) };
  if (answer.data.data.t !== "register") return { t: "leave" };
  const { slug, callback } = answer.data.data;
  return { t: "form", slug, callback };
};

export const ToLogin = () => {
  const router = useRouter();
  useEffect(() => {
    router.replace("/login");
  }, [router]);
  return null;
};

export const Register = ({ url }: { url: string }) => {
  const answer = useLoading(useCallback(() => startAuth(url), [url]));
  const view = registerView(answer);
  switch (view.t) {
    case "loading":
      return null;
    case "leave":
      return <ToLogin />;
    case "refused":
      return (
        <div className={styles.container}>
          <Modal>
            <ModalSection className={styles.section}>
              <Text type="h1">Register a forge</Text>
              <Text className={styles.error} data-testid="register-error">
                {view.message}
              </Text>
              <ModalActions align="right">
                <Link href="/login">Back</Link>
              </ModalActions>
            </ModalSection>
          </Modal>
        </div>
      );
    case "form":
      return (
        <RegisterForm url={url} slug={view.slug} callback={view.callback} />
      );
  }
};

const RegisterForm = ({
  url,
  slug,
  callback,
}: {
  url: string;
  slug: string;
  callback: string;
}) => {
  const [clientId, setClientId] = useState("");
  const [clientSecret, setClientSecret] = useState("");
  const [copied, setCopied] = useState<"copied" | "failed">();
  const { busy, error, go } = useAction(registrationError);

  const submit = () =>
    go(
      () => registerForge({ url, clientId, clientSecret }),
      (link) => {
        goTo(link);
        return leaving;
      },
    );

  return (
    <div className={styles.container}>
      <Modal>
        <form
          onSubmit={(e) => {
            e.preventDefault();
            void submit();
          }}
        >
          <ModalSection className={styles.section}>
            <Text type="h1">Register {slug}</Text>
            <Text>
              garnix does not know this Gitea/Forgejo instance yet. On {slug},
              create an OAuth2 application under Settings → Applications (or
              Site Administration → Applications for the whole instance) with
              this redirect URI:
            </Text>
            <div className={styles.callback}>
              <Text type="code" data-testid="redirect-uri">
                {callback}
              </Text>
              <Button
                style="secondary"
                onClick={async () => {
                  try {
                    await navigator.clipboard.writeText(callback);
                    setCopied("copied");
                  } catch {
                    setCopied("failed");
                  }
                }}
              >
                {copied === "copied" ? "Copied" : "Copy"}
              </Button>
            </div>
            {copied === "failed" && (
              <Text className={styles.error}>
                Could not copy: select the redirect URI and copy it yourself.
              </Text>
            )}
            <TextInput
              className={styles.field}
              label="URL"
              value={url}
              onChange={() => {}}
              readOnly
            />
            <TextInput
              className={styles.field}
              label="Client ID"
              value={clientId}
              onChange={setClientId}
              required
              disabled={busy}
            />
            <TextInput
              className={styles.field}
              label="Client Secret"
              type="password"
              autoComplete="off"
              value={clientSecret}
              onChange={setClientSecret}
              required
              disabled={busy}
            />
            <Text className={styles.hint}>
              garnix keeps the secret encrypted and never shows it again. The
              forge stays pending until you log in through it from this browser.
            </Text>
            {error && (
              <Text className={styles.error} data-testid="register-error">
                {error}
              </Text>
            )}
            <ModalActions align="right">
              <Link href="/login">Back</Link>
              <Button submit loading={busy}>
                Register and log in
              </Button>
            </ModalActions>
          </ModalSection>
        </form>
      </Modal>
    </div>
  );
};
