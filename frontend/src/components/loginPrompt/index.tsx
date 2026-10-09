"use client";

import React from "react";
import { Modal, ModalSection } from "@/components/modal";
import { Text } from "@/components/text";
import { Button } from "@/components/button";
import { useLoginLinkForCurrentPage } from "@/hooks/useLoginLinkForCurrentPage";
import styles from "./styles.module.css";

export const LoginPrompt = () => {
  const loginLink = useLoginLinkForCurrentPage().loginLink;

  return (
    <Modal className={styles.root}>
      <ModalSection>
        <Text type="h1">Please login to continue</Text>
        <Text className={styles.spacing}>
          You must be authenticated to view this page. Please click the link
          below to log in.
        </Text>
        <div className={styles.actions}>
          <Button
            href={loginLink}
            eventName="login-from-auth-required-page"
            target=""
          >
            Log in
          </Button>
        </div>
      </ModalSection>
    </Modal>
  );
};
