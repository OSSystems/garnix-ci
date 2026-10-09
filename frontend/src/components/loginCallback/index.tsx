"use client";

import { useRouter, useSearchParams } from "next/navigation";
import { useEffect, useState } from "react";
import { LoginResult, finishLogin, getLoginTargetPage } from "@/services/auth";
import { defaultForgeSlug } from "@/services/forges";
import { APIResult, userMessage } from "@/services";
import { useUser } from "@/store/userContext";
import { LoginAnimation } from "@/components/loginAnimation";
import { Modal, ModalActions, ModalSection } from "@/components/modal";
import { Button } from "@/components/button";
import { Text } from "@/components/text";
import styles from "./styles.module.css";

// Where the callback is. A connect starts logged in already, so only
// `leaving` leaves the page.
export type Phase =
  | { t: "finishing" }
  | { t: "failed"; title: string; message: string }
  // The login may have created a second account: said once, before leaving.
  | { t: "notice" }
  | { t: "leaving" };

export const afterFinish = (
  result: APIResult<LoginResult>,
  forge: string,
): Phase => {
  if (!result.ok)
    return {
      t: "failed",
      title:
        result.error.status === 403 && forge !== defaultForgeSlug
          ? "This login cannot go on."
          : "Something went wrong.",
      message: userMessage(result.error),
    };
  return result.data.emailAlreadyUsed ? { t: "notice" } : { t: "leaving" };
};

// Where a forge sends the browser back to after a login, or a connect, through
// it: finishes it, then goes back to the page that started it.
export const LoginCallback = ({ forge }: { forge: string }) => {
  const router = useRouter();
  const params = useSearchParams();
  const { user, setUser } = useUser();
  const [phase, setPhase] = useState<Phase>({ t: "finishing" });
  const [animationDone, setAnimationDone] = useState(false);
  useEffect(() => {
    void (async () => {
      const response = await finishLogin(params, forge);
      if (response.ok) setUser(response.data.user);
      setPhase(afterFinish(response, forge));
    })();
  }, [params, setUser, forge]);
  useEffect(() => {
    if (phase.t === "leaving" && animationDone && user.state === "logged-in") {
      router.replace(getLoginTargetPage());
    }
  }, [phase, animationDone, user, router]);
  if (phase.t === "failed") {
    return (
      <div className={styles.container}>
        <Modal>
          <ModalSection className={styles.section}>
            <Text type="h2">{phase.title}</Text>
            <Text>{phase.message}</Text>
            <ModalActions>
              <Button
                onClick={() =>
                  // A connect that failed goes back to where it started, the
                  // account page; a login, to the forge chooser.
                  router.push(
                    user.state === "logged-in"
                      ? getLoginTargetPage()
                      : "/login",
                  )
                }
              >
                Back
              </Button>
            </ModalActions>
          </ModalSection>
        </Modal>
      </div>
    );
  }
  if (phase.t === "notice") {
    return (
      <div className={styles.container}>
        <Modal>
          <ModalSection className={styles.section}>
            <Text type="h2">You may already have an account</Text>
            <Text>
              An account with this email already exists. If it is yours, log in
              with it and connect this forge from settings.
            </Text>
            <ModalActions align="right">
              <Button onClick={() => setPhase({ t: "leaving" })}>
                Continue
              </Button>
            </ModalActions>
          </ModalSection>
        </Modal>
      </div>
    );
  }
  return (
    <div className={styles.container}>
      <LoginAnimation
        text="Redirecting, please wait..."
        onAnimationDone={() => {
          if (!animationDone) {
            setAnimationDone(true);
          }
        }}
      />
    </div>
  );
};
