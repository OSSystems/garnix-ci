const SECONDS = Symbol();

export type Duration = { [SECONDS]: number };

/** @lintignore */
export const emptyDuration: Duration = fromSecs(0);

export function diffTime(a: Date, b: Date): Duration {
  return { [SECONDS]: (a.getTime() - b.getTime()) / 1000 };
}

export function fromSecs(secs: number): Duration {
  return { [SECONDS]: secs };
}

export function fromMinutes(minutes: number): Duration {
  return fromSecs(minutes * 60);
}

export function toMillis(d: Duration): number {
  return d[SECONDS] * 1000;
}

export function double(d: Duration): Duration {
  return { [SECONDS]: d[SECONDS] * 2 };
}

export function formatMinutes(d: Duration): string {
  return (d[SECONDS] / 60).toFixed(2);
}

export function formatDurationShort(d: Duration): string {
  const inSeconds = d[SECONDS];
  const hours = Math.floor(inSeconds / (60 * 60));
  const minutes = Math.floor((inSeconds % (60 * 60)) / 60);
  const seconds = Math.floor(inSeconds % 60);
  if (hours > 0) return `${hours}h ${minutes}m`;
  if (minutes > 0) return `${minutes}m ${seconds}s`;
  if (seconds > 0) return `${seconds}s`;
  return `${Math.floor(inSeconds * 1000)}ms`;
}

export const formatDurationLong = (d: Duration): string => {
  const inSeconds = d[SECONDS];
  const years = Math.floor(inSeconds / (60 * 60 * 24 * 365));
  const months = Math.floor(inSeconds / (60 * 60 * 24 * 30.4375));
  const weeks = Math.floor(inSeconds / (60 * 60 * 24 * 7));
  const days = Math.floor(inSeconds / (60 * 60 * 24));
  const hours = Math.floor(inSeconds / (60 * 60));
  const minutes = Math.floor(inSeconds / 60);
  if (years > 0) return `${years} year${years > 1 ? "s" : ""}`;
  if (months > 0) return `${months} month${months > 1 ? "s" : ""}`;
  if (weeks > 0) return `${weeks} week${weeks > 1 ? "s" : ""}`;
  if (days > 0) return `${days} day${days > 1 ? "s" : ""}`;
  if (hours > 0) return `${hours} hour${hours > 1 ? "s" : ""}`;
  if (minutes > 0) return `${minutes} minute${minutes > 1 ? "s" : ""}`;
  return "a few seconds";
};
