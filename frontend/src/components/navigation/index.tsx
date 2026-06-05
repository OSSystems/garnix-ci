"use client";

import Image from "next/image";
import { usePathname, useRouter } from "next/navigation";
import React from "react";
import { AppRouterInstance } from "next/dist/shared/lib/app-router-context.shared-runtime";
import { Link } from "@/components/link";
import { Text } from "@/components/text";
import logoIcon from "@/components/icons/logo.svg";
import { BuildsIcon } from "@/components/icons/builds";
import { ModulesIcon } from "@/components/icons/modules";
import { DocumentationIcon } from "@/components/icons/documentation";
import { DiscordIcon } from "@/components/icons/discord";
import { MatrixIcon } from "@/components/icons/matrix";
import { GithubIcon } from "@/components/icons/github";
import { EmailIcon } from "@/components/icons/email";
import { AccountIcon } from "@/components/icons/account";
import { SettingsIcon } from "@/components/icons/settings";
import { LogoutIcon } from "@/components/icons/logout";
import hamburgerMenuIcon from "@/components/icons/hamburgerMenu.svg";
import { useConfig } from "@/store/configContext";
import { trackClick } from "@/utils/analytics";
import { filterNull } from "@/utils";
import { BillingIcon } from "../icons/billing";
import styles from "./styles.module.css";

type Props = {
  className?: string;
};

type LinkProps = BasicLinkProps &
  (
    | {
        href: string;
        onClick?: undefined;
      }
    | {
        onClick: () => void;
        href?: undefined;
      }
  );

type BasicLinkProps = {
  icon: React.ReactNode;
  label: string;
  openNewPage?: boolean;
  eventName?: string;
};

const MAIN_LINK_GROUP: Array<LinkProps> = filterNull([
  {
    icon: <BuildsIcon className={styles.icon} />,
    label: "Builds",
    href: "/",
  },
  {
    icon: <ModulesIcon className={styles.icon} />,
    label: "Modules",
    href: "/modules/configure",
  },
  {
    icon: <DocumentationIcon className={styles.icon} />,
    label: "Documentation",
    href: "https://garnix.io/docs",
    openNewPage: true,
  },
  {
    icon: <BillingIcon className={styles.icon} />,
    label: "Pricing",
    href: "https://garnix.io/pricing",
    openNewPage: true,
  },
]);

const SOCIAL_LINK_GROUP: LinkProps[] = [
  {
    icon: <DiscordIcon className={styles.icon} />,
    label: "Discord",
    href: "https://discord.gg/XtDrPsqpVx",
    openNewPage: true,
  },
  {
    icon: <MatrixIcon className={styles.icon} />,
    label: "Matrix",
    href: "https://matrix.to/#/#garnix-main:matrix.org",
    openNewPage: true,
  },
  {
    icon: <EmailIcon className={styles.icon} />,
    label: "Email",
    href: "mailto:contact@garnix.io",
    eventName: "mailto-contact-garnix.io",
  },
  {
    icon: <GithubIcon className={styles.icon} />,
    label: "Report an issue",
    href: "https://github.com/garnix-io/issues/issues",
    openNewPage: true,
  },
];

const ACCOUNT_LINK_GROUP = (
  githubAppName: string,
  router: AppRouterInstance,
): LinkProps[] => [
  {
    icon: <AccountIcon className={styles.icon} />,
    label: "Account",
    href: "/account",
  },
  {
    icon: <SettingsIcon className={styles.icon} />,
    label: "Configure",
    href: `https://github.com/apps/${githubAppName}`,
    openNewPage: true,
    eventName: "configure-github-app",
  },
  {
    icon: <LogoutIcon className={styles.icon} />,
    label: "Logout",
    onClick: () => {
      trackClick("log-out");
      router.push("/logout");
    },
  },
];

export const Navigation = ({ className }: Props) => {
  const pathname = usePathname();
  const [menuOpen, setMenuOpen] = React.useState(false);
  const { githubAppName } = useConfig();
  const router = useRouter();
  return (
    <div className={`${styles.container} ${className}`}>
      <div className={styles.header}>
        <Link href="/">
          <Image
            className={styles.logo}
            src={logoIcon}
            alt="share"
            width={118}
            height={20}
          />
        </Link>
        <Image
          src={hamburgerMenuIcon}
          alt="menu"
          className={styles.menu}
          onClick={() => {
            trackClick("logged-in-menu");
            setMenuOpen(!menuOpen);
          }}
        />
      </div>
      {/* <div className={`${styles.inputContainer} ${Berlin.className}`}>
        <input className={styles.input} placeholder="Search" />
        <div className={styles.hints}>
          <div className={styles.hint}>&#8984;</div>
          <div className={styles.hint}>K</div>
        </div>
      </div> */}
      <div
        className={`${styles.linkGroupContainer} ${menuOpen && styles.open}`}
      >
        <div className={styles.linkGroup}>
          {MAIN_LINK_GROUP.map((link) => (
            <NavLink key={link.label} link={link} pathname={pathname} />
          ))}
        </div>
        <div>
          <div className={`${styles.linkGroup} ${styles.socialLinkGroup}`}>
            {SOCIAL_LINK_GROUP.map((link) => (
              <NavLink
                key={link.label}
                link={link}
                pathname={pathname}
                useStroke
              />
            ))}
          </div>
          <div className={styles.linkGroup}>
            {ACCOUNT_LINK_GROUP(githubAppName, router).map((link) => (
              <NavLink key={link.label} link={link} pathname={pathname} />
            ))}
          </div>
        </div>
      </div>
    </div>
  );
};

const NavLink = ({
  link,
  pathname,
  useStroke,
}: {
  link: LinkProps;
  pathname: string;
  useStroke?: boolean;
}) => {
  const wrapper = (children: React.ReactNode) => {
    if (link.href)
      return (
        <Link
          href={link.href}
          eventName={link.eventName}
          className={`${styles.linkContainer} ${
            link.href === pathname && styles.active
          } ${useStroke ? styles.stroke : styles.fill}`}
          target={link.openNewPage ? "_blank" : ""}
        >
          {children}
        </Link>
      );
    return (
      <button
        onClick={() => {
          link.eventName && trackClick(link.eventName);
          link.onClick && link.onClick();
        }}
        className={`${styles.linkContainer} ${styles.button} ${
          useStroke ? styles.stroke : styles.fill
        }`}
      >
        {children}
      </button>
    );
  };
  return wrapper(
    <>
      <span className={styles.link}>
        {link.icon}
        <Text type="span">{link.label}</Text>
      </span>
    </>,
  );
};
