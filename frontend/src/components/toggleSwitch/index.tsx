import React from "react";
import { InputProps } from "@/components/input";
import styles from "./styles.module.css";

export const ToggleSwitch = (
  props: InputProps<boolean> & {
    className?: string;
    disabled?: boolean;
    "aria-label"?: string;
  },
) => {
  return (
    <label className={`${styles.root} ${props.className}`}>
      <input
        type="checkbox"
        checked={props.value}
        disabled={props.disabled}
        aria-label={props["aria-label"]}
        onChange={(e) => {
          e.stopPropagation();
          props.onChange(e.target.checked);
        }}
        onClick={(e) => e.stopPropagation()}
      />
      <div className={styles.toggle} />
    </label>
  );
};
