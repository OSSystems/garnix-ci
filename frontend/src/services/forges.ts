import { z } from "zod";
import { APIResult, fetchFromAPI } from ".";

const forgeSchema = z
  .object({
    slug: z.string(),
    kind: z.union([z.literal("github"), z.literal("gitea")]),
    web_url: z.string(),
  })
  .transform((forge) => ({
    slug: forge.slug,
    kind: forge.kind,
    webUrl: forge.web_url.replace(/\/+$/, ""),
  }));

export type Forge = z.infer<typeof forgeSchema>;

// Rows and URLs that predate multi-forge support name no forge: they are
// github.com's.
export const defaultForgeSlug = "github";

export const githubForge: Forge = {
  slug: defaultForgeSlug,
  kind: "github",
  webUrl: "https://github.com",
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
