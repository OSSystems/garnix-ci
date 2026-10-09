"use client";
import { Text } from "@/components/text";
import { AppPage } from "@/utils/appPage";
import styles from "./styles.module.css";
import { UsageComponent } from "./usage";
import { AccessTokensComponent } from "./access_tokens";
import { ForgesComponent } from "./forges";

const Page = () => {
  return (
    <div className={styles.container}>
      <Text type="h1" className={styles.h1}>
        Account
      </Text>
      <div className={styles.section}>
        <UsageComponent />
      </div>
      <div className={styles.section}>
        <AccessTokensComponent />
      </div>
      <ForgesComponent />
    </div>
  );
};

export default AppPage(Page, { requireAuth: true });
