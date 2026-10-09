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

const setLoginTargetPage = (path: string | null): void => {
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
  const response = await fetchFromAPI(
    z.object({ username: z.string() }),
    "GET",
    "login/cb",
    { query },
  );
  if (!response.ok) return response;
  return Ok({ name: response.data.username });
};

export const logout = async (): Promise<APIResult<void>> => {
  return fetchFromAPI(z.void(), "DELETE", "login");
};
