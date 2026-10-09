import { URLSearchParams } from "url";
import { z } from "zod";
import { User } from "@/store/userContext";
import { sanitizeRedirectPath } from "@/utils";
import { Ok, fetchFromAPI } from ".";
import { APIResult } from "./index";

const loginTargetPageLocalstorageKey = "login-target-page";

export const getCurrentUser = async (): Promise<APIResult<User | null>> => {
  const response = await fetchFromAPI(
    z.nullable(z.object({ username: z.string(), email: z.string() })),
    "GET",
    "whoami",
  );
  if (!response.ok) return response;
  if (response.data == null) return Ok(null);
  return Ok({
    name: response.data.username,
    email: response.data.email,
  });
};

export const setLoginTargetPage = (path: string | null): void => {
  if (path != null) {
    window.localStorage.setItem(loginTargetPageLocalstorageKey, path);
  }
};

export const getLoginTargetPage = (): string => {
  const path = window.localStorage.getItem(loginTargetPageLocalstorageKey);
  window.localStorage.removeItem(loginTargetPageLocalstorageKey);
  return sanitizeRedirectPath(path ?? "/");
};

export const getLoginLink = async (
  page: string | null,
): Promise<APIResult<string>> => {
  setLoginTargetPage(page);
  const response = await fetchFromAPI(
    z.object({ github: z.string() }),
    "GET",
    "login",
  );
  if (!response.ok) return response;
  return Ok(response.data.github);
};

export const finishLogin = async (
  query: URLSearchParams,
): Promise<APIResult<User>> => {
  const response = await fetchFromAPI(z.string(), "GET", "login/cb", {
    query,
  });
  if (!response.ok) return response;
  return Ok({ name: response.data });
};

export const getSignupLink = async (): Promise<APIResult<string>> => {
  const response = await fetchFromAPI(
    z.object({ github: z.string() }),
    "GET",
    "signup",
  );
  if (!response.ok) return response;
  return Ok(response.data.github);
};

// The query is the forge's callback: its code and the state the backend
// checks against this browser's cookie.
export const getSignupData = async (
  query: URLSearchParams,
): Promise<
  APIResult<
    User & {
      exists: boolean;
    }
  >
> => {
  const response = await fetchFromAPI(
    z.object({
      exists: z.boolean(),
      email: z.string(),
      github_login: z.string(),
    }),
    "GET",
    "signup/fill",
    { query },
  );
  if (!response.ok) return response;
  return Ok({
    exists: response.data.exists,
    email: response.data.email,
    name: response.data.github_login,
  });
};

export const finishSignup = async (
  name: string,
  email: string,
  agreeEmail: boolean,
): Promise<APIResult<string>> => {
  const response = await fetchFromAPI(z.string(), "POST", "signup", {
    body: JSON.stringify({
      email,
      subscription_type: "free",
      agree_to_emails: agreeEmail,
      github_login: name,
    }),
  });
  if (!response.ok) return response;
  return Ok(getLoginTargetPage());
};

export const logout = async (): Promise<APIResult<void>> => {
  return fetchFromAPI(z.void(), "DELETE", "login");
};
