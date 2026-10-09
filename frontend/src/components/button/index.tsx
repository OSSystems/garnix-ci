"use client";

import { PropsWithChildren } from "react";
import { Berlin } from "@/utils/fonts";
import { Link } from "@/components/link";
import { trackClick } from "@/utils/analytics";
import styles from "./styles.module.css";

const styleClassNames = {
  primary: styles.primary,
  primaryInverse: styles.primaryInverse,
  secondary: styles.secondary,
  warning: styles.warning,
};

type Props = {
  style?: keyof typeof styleClassNames;
  eventName?: string;
  loading?: boolean;
  disabled?: boolean;
  // Says why when it is disabled.
  title?: string;
} & PropsWithChildren &
  (
    | {
        href: string;
        onClick?: never;
        target?: string;
        submit?: false;
        submitAction?: never;
      }
    | {
        href?: never;
        target?: never;
        onClick: () => void | Promise<void>;
        submit?: false;
        submitAction?: never;
      }
    | {
        href?: never;
        target?: never;
        onClick?: never;
        submit: true;
        submitAction?: string | null;
      }
  );

export const Button = ({
  style = "primary",
  href,
  eventName,
  onClick,
  submit,
  submitAction,
  children,
  target,
  loading,
  disabled,
  ...rest
}: Props) => {
  if (href) {
    return (
      <Link
        eventName={eventName}
        href={href}
        className={`${styles.container} ${Berlin.className} ${styleClassNames[style]}`}
        target={target}
        variant="wrapper"
        disabled={loading || disabled}
        {...rest}
      >
        {children}
      </Link>
    );
  }
  return (
    <button
      type={submit ? "submit" : "button"}
      onClick={() => {
        eventName && trackClick(eventName);
        onClick && void onClick();
      }}
      disabled={loading || disabled}
      className={`${styles.container} ${Berlin.className} ${styleClassNames[style]}`}
      data-submit-action={submitAction}
      {...rest}
    >
      {children}
    </button>
  );
};
