"use client";

import { useRouter, useSearchParams } from "next/navigation";
import { Suspense, useEffect, useState } from "react";
import { finishLogin, getLoginTargetPage } from "@/services/auth";
import { useUser } from "@/store/userContext";
import { LoginAnimation } from "@/components/loginAnimation";
import { Modal, ModalActions, ModalSection } from "@/components/modal";
import { Button } from "@/components/button";
import { Text } from "@/components/text";
import styles from "./styles.module.css";

const Inner = () => {
  const router = useRouter();
  const params = useSearchParams();
  const { user, setUser } = useUser();
  const [error, setError] = useState<string>();
  const [animationDone, setAnimationDone] = useState(false);
  useEffect(() => {
    void (async () => {
      const response = await finishLogin(params);
      if (!response.ok) {
        setError(response.error.message);
      } else if (response.data) {
        setUser(response.data);
      }
    })();
  }, [router, params, setUser]);
  useEffect(() => {
    if (animationDone && user.state === "logged-in") {
      router.replace(getLoginTargetPage());
    }
  }, [animationDone, user, router]);
  if (error) {
    return (
      <div className={styles.container}>
        <Modal>
          <ModalSection className={styles.section}>
            <Text type="h2">Something went wrong.</Text>
            <Text>{error}</Text>
            <ModalActions>
              <Button onClick={() => router.back()}>Back</Button>
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

const Page = () => (
  <Suspense fallback={null}>
    <Inner />
  </Suspense>
);

export default Page;
