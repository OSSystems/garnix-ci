import { useCallback, useState } from "react";
import { APIError, APIResult, userMessage } from "@/services";

// What `onOk` answers when it leaves the page: the action then stays busy, so
// that nothing is clicked twice on the way out.
export const leaving = "leaving";

// One request a form or a button makes: busy while it runs, and what to tell
// the person when it fails, in one place for every such request.
export const useAction = (
  toMessage: (error: APIError) => string = userMessage,
) => {
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string>();
  const go = useCallback(
    async <T>(
      run: () => Promise<APIResult<T>>,
      onOk: (t: T) => void | typeof leaving,
    ): Promise<void> => {
      setBusy(true);
      setError(undefined);
      const result = await run();
      if (!result.ok) {
        setError(toMessage(result.error));
        setBusy(false);
        return;
      }
      if (onOk(result.data) !== leaving) setBusy(false);
    },
    [toMessage],
  );
  return { busy, error, go };
};
