import { z } from "zod";
import { APIError, APIResult, Ok, fetchFromAPI, userMessage } from ".";

const forgeSchema = z
  .object({
    slug: z.string(),
    kind: z.union([z.literal("github"), z.literal("gitea")]),
    web_url: z.string(),
    source: z.enum(["configured", "registered"]),
    name: z.string(),
    // Only a registered forge whoever asks may bring back is listed disabled.
    status: z.enum(["active", "disabled"]),
    can_manage: z.boolean(),
  })
  .transform((forge) => ({
    slug: forge.slug,
    kind: forge.kind,
    webUrl: forge.web_url.replace(/\/+$/, ""),
    source: forge.source,
    name: forge.name,
    status: forge.status,
    canManage: forge.can_manage,
  }));

export type Forge = z.infer<typeof forgeSchema>;

// Rows and URLs that predate multi-forge support name no forge: they are
// github.com's.
export const defaultForgeSlug = "github";

export const githubForge: Forge = {
  slug: defaultForgeSlug,
  kind: "github",
  webUrl: "https://github.com",
  source: "configured",
  name: "github.com",
  status: "active",
  canManage: false,
};

export const getForges = async (): Promise<APIResult<Array<Forge>>> =>
  await fetchFromAPI(z.array(forgeSchema), "GET", "forges");

export const findForge = (
  forges: Array<Forge>,
  slug: string,
): Forge | undefined =>
  forges.find((forge) => forge.slug === slug) ??
  (slug === defaultForgeSlug ? githubForge : undefined);

type RepoRef = { forge: string; repoUser: string; repoName: string };

export const repoPath = (repo: RepoRef): string =>
  `/repo/${repo.forge}/${repo.repoUser}/${repo.repoName}`;

// Branch names may contain '/', which both forges expect verbatim, but also
// '#', '?' or '%', which must not end the path.
const encodePath = (path: string): string =>
  path.split("/").map(encodeURIComponent).join("/");

// Where each kind serves a branch; a new kind must name its own.
const branchSegment: Record<Forge["kind"], string> = {
  github: "tree",
  gitea: "src/branch",
};

// Links into the forge's own web UI.
export const forgeLinks = (forge: Forge) => {
  const repo = (r: RepoRef) =>
    `${forge.webUrl}/${encodeURIComponent(r.repoUser)}/${encodeURIComponent(r.repoName)}`;
  return {
    repo,
    branch: (r: RepoRef, branch: string) =>
      `${repo(r)}/${branchSegment[forge.kind]}/${encodePath(branch)}`,
    commit: (r: RepoRef, commit: string) =>
      `${repo(r)}/commit/${encodeURIComponent(commit)}`,
    user: (login: string) => `${forge.webUrl}/${encodeURIComponent(login)}`,
  };
};

export const forgeHost = (forge: Forge): string => {
  try {
    return new URL(forge.webUrl).host;
  } catch {
    return forge.webUrl;
  }
};

// What to call a forge in a sentence or on a button.
export const forgeLabel = (forge: Forge): string =>
  forge.slug === defaultForgeSlug ? "GitHub" : forge.name;

// What `POST /api/auth/start` and `POST /api/forges` answer: log in through
// the forge's OAuth app at `login`, or register an OAuth app calling back to
// `callback` first.
const loginAnswerSchema = z.object({ login: z.string() }).transform((a) => ({
  t: "login" as const,
  link: a.login,
}));

const startAnswerSchema = z.union([
  loginAnswerSchema,
  z
    .object({ register: z.object({ slug: z.string(), callback: z.string() }) })
    .transform((a) => ({ t: "register" as const, ...a.register })),
]);

export type StartAnswer = z.infer<typeof startAnswerSchema>;

export const startAuth = (url: string): Promise<APIResult<StartAnswer>> =>
  fetchFromAPI(startAnswerSchema, "POST", "auth/start", {
    body: JSON.stringify({ url }),
  });

export type ForgeRegistration = {
  url: string;
  clientId: string;
  clientSecret: string;
};

// Stores the forge pending its first login, which only this browser may do,
// and answers the link to start it.
export const registerForge = async (
  registration: ForgeRegistration,
): Promise<APIResult<string>> => {
  const response = await fetchFromAPI(loginAnswerSchema, "POST", "forges", {
    body: JSON.stringify(registration),
  });
  if (!response.ok) return response;
  return Ok(response.data.link);
};

// What to tell someone whose registration was refused.
export const registrationError = (error: APIError): string => {
  switch (error.status) {
    case 409: {
      // "a registration of <host> is already in progress; try again after
      // <time>", or that it is registered already.
      const message = userMessage(error);
      return `${message.charAt(0).toUpperCase()}${message.slice(1)}.`;
    }
    case 429:
      return "Too many registrations from your address. Try again later.";
    default:
      return userMessage(error);
  }
};
