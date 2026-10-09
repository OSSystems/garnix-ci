"use client";

import { useRouter, useSearchParams } from "next/navigation";
import { Suspense, useEffect, useState } from "react";
import { getLoginTargetPage, getSignupData } from "@/services/auth";
import { useUser } from "@/store/userContext";
import { Modal, ModalActions, ModalSection } from "@/components/modal";
import { Button } from "@/components/button";
import { Text } from "@/components/text";
import { LoginAnimation } from "@/components/loginAnimation";
import styles from "./styles.module.css";

const Inner = () => {
  const router = useRouter();
  const params = useSearchParams();
  const { setUser } = useUser();
  const [error, setError] = useState<string>();
  const [animationDone, setAnimationDone] = useState(false);
  const [redirectUrl, setRedirectUrl] = useState<string | null>(null);
  useEffect(() => {
    void (async () => {
      if (!params.get("code"))
        return setError("No signup code given from github.");
      const response = await getSignupData(params);
      if (!response.ok) setError(response.error.message);
      else if (response.data) {
        setUser({ name: response.data.name, email: response.data.email });
        if (response.data.exists) {
          setRedirectUrl(getLoginTargetPage());
        } else {
          setRedirectUrl("/signup/start");
        }
      }
    })();
  }, [router, params, setUser]);
  useEffect(() => {
    if (animationDone && redirectUrl) {
      router.replace(redirectUrl);
    }
  }, [animationDone, redirectUrl, router]);
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
          setAnimationDone(true);
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
