import { z } from "zod";
import { APIResult, fetchFromAPI } from ".";

const kindSchema = z.enum(["user", "org", "instance"]);

export type InstallationKind = z.infer<typeof kindSchema>;

const installableSchema = z.object({
  kind: kindSchema,
  target: z.string(),
  installation: z.number().nullable(),
});

export type Installable = z.infer<typeof installableSchema>;

const installationSchema = z.object({
  id: z.number(),
  forge: z.string(),
  kind: kindSchema,
  target: z.string(),
  installer: z.string(),
  status: z.enum(["active", "needs_reinstall"]),
  reason: z.string().nullable(),
  createdAt: z.string(),
});

export type Installation = z.infer<typeof installationSchema>;

const installationRepoSchema = z.object({
  owner: z.string(),
  name: z.string(),
  private: z.boolean(),
  enabledIn: z.number().nullable(),
});

export type InstallationRepo = z.infer<typeof installationRepoSchema>;

const installationReposSchema = z.object({
  repos: z.array(installationRepoSchema),
  total: z.number(),
});

export type InstallationRepos = z.infer<typeof installationReposSchema>;

const base = (forge: string) => `forges/${encodeURIComponent(forge)}`;

const installationPath = (forge: string, id: number) =>
  `${base(forge)}/installations/${id}`;

const repoPath = (forge: string, id: number, owner: string, name: string) =>
  `${installationPath(forge, id)}/repos/${encodeURIComponent(owner)}/${encodeURIComponent(name)}`;

export const getInstallable = (
  forge: string,
): Promise<APIResult<Array<Installable>>> =>
  fetchFromAPI(z.array(installableSchema), "GET", `${base(forge)}/installable`);

export const getInstallations = (
  forge: string,
): Promise<APIResult<Array<Installation>>> =>
  fetchFromAPI(
    z.array(installationSchema),
    "GET",
    `${base(forge)}/installations`,
  );

export const install = (
  forge: string,
  kind: InstallationKind,
  target: string,
): Promise<APIResult<Installation>> =>
  fetchFromAPI(installationSchema, "POST", `${base(forge)}/installations`, {
    body: JSON.stringify({ kind, target }),
  });

export const reinstall = (
  forge: string,
  id: number,
): Promise<APIResult<Installation>> =>
  fetchFromAPI(
    installationSchema,
    "POST",
    `${installationPath(forge, id)}/reinstall`,
  );

export const uninstall = (
  forge: string,
  id: number,
): Promise<APIResult<unknown>> =>
  fetchFromAPI(z.unknown(), "DELETE", installationPath(forge, id));

export const getInstallationRepos = (
  forge: string,
  id: number,
  page: number,
  limit: number,
): Promise<APIResult<InstallationRepos>> =>
  fetchFromAPI(
    installationReposSchema,
    "GET",
    `${installationPath(forge, id)}/repos`,
    { query: { page: String(page), limit: String(limit) } },
  );

export const enableRepo = (
  forge: string,
  id: number,
  owner: string,
  name: string,
): Promise<APIResult<unknown>> =>
  fetchFromAPI(z.unknown(), "PUT", repoPath(forge, id, owner, name));

export const disableRepo = (
  forge: string,
  id: number,
  owner: string,
  name: string,
): Promise<APIResult<unknown>> =>
  fetchFromAPI(z.unknown(), "DELETE", repoPath(forge, id, owner, name));
