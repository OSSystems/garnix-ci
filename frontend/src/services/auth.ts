import { URLSearchParams } from "url";
import { z } from "zod";
import { User } from "@/store/userContext";
import { sanitizeRedirectPath } from "@/utils";
import { defaultForgeSlug } from "./forges";
import { Ok, fetchFromAPI } from ".";
import { APIResult } from "./index";

const loginTargetPageLocalstorageKey = "login-target-page";

// What `whoami` answers: the session's account, or null.
const whoamiSchema = z.nullable(
  z.object({
    username: z.string(),
    email: z.string(),
  }),
);

export const getCurrentUser = async (): Promise<APIResult<User | null>> => {
  const response = await fetchFromAPI(whoamiSchema, "GET", "whoami");
  if (!response.ok) return response;
  if (response.data == null) return Ok(null);
  return Ok({
    name: response.data.username,
    email: response.data.email,
  });
};

// Every flow that leaves for a forge sets where it comes back to, so that
// an abandoned one never sends a later login there.
export const setLoginTargetPage = (path: string): void => {
  window.localStorage.setItem(loginTargetPageLocalstorageKey, path);
};

export const getLoginTargetPage = (): string => {
  const path = window.localStorage.getItem(loginTargetPageLocalstorageKey);
  window.localStorage.removeItem(loginTargetPageLocalstorageKey);
  return sanitizeRedirectPath(path ?? "/");
};

// github.com keeps the routes its OAuth app has always called back to; every
// other forge is under its slug.
const loginRoute = (forge: string): string =>
  forge === defaultForgeSlug
    ? "login"
    : `auth/${encodeURIComponent(forge)}/login`;

// The link is under `github` whichever forge it points to.
const loginLinkSchema = z.object({ github: z.string() });

export const getLoginLink = async (
  page: string | null,
  forge: string = defaultForgeSlug,
): Promise<APIResult<string>> => {
  setLoginTargetPage(page ?? "/");
  const response = await fetchFromAPI(
    loginLinkSchema,
    "GET",
    loginRoute(forge),
  );
  if (!response.ok) return response;
  return Ok(response.data.github);
};

export type LoginResult = {
  user: User;
  // Another account has the email of the account this login just created.
  emailAlreadyUsed: boolean;
};

// Finishes a login, or a connect: the forge calls back to the same route.
export const finishLogin = async (
  query: URLSearchParams,
  forge: string = defaultForgeSlug,
): Promise<APIResult<LoginResult>> => {
  const response = await fetchFromAPI(
    z.object({
      username: z.string(),
      emailAlreadyUsed: z.boolean(),
    }),
    "GET",
    `${loginRoute(forge)}/cb`,
    { query },
  );
  if (!response.ok) return response;
  return Ok({
    user: { name: response.data.username },
    emailAlreadyUsed: response.data.emailAlreadyUsed,
  });
};

export const logout = async (): Promise<APIResult<void>> => {
  return fetchFromAPI(z.void(), "DELETE", "login");
};
