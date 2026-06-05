import { z } from "zod";
import { fromSecs, toSecs } from "@/utils/duration";
import { mapCollectResult } from "@/utils";
import { APIResult, Err, Ok, fetchFromAPI } from ".";

type Usage = {
  byOrg: Record<string, OrgUsage>;
};

export type Plan = z.infer<typeof planSchema>;
const planSchema = z.object({
  display_name: z.string(),
  description: z.optional(z.string()),
  base_ci_time: z.number().transform(fromSecs),
  maximum_pr_deployment_time: z.number().transform(fromSecs),
  included_branch_deployment_hosts: z.number(),
  extra_usage: z.object({
    ciTime: z.number().transform(fromSecs),
    prDeployTime: z.number().transform(fromSecs),
    hostingSpend: z.number().transform((cents) => Math.floor(cents / 100)),
  }),
  is_paid: z.boolean(),
});

export type OrgUsage = z.infer<typeof orgUsageSchema>;
const orgUsageSchema = z.object({
  ci_time: z.number().transform(fromSecs),
  pr_deployment_time: z.number().transform(fromSecs),
  branch_deployment_hosts: z.number(),
  plan: planSchema,
  installation_status: z.discriminatedUnion("tag", [
    z.object({ tag: z.literal("NoActiveInstallation") }),
    z.object({
      tag: z.literal("InstallationRenewing"),
      contents: z.coerce.date(),
    }),
    z.object({
      tag: z.literal("InstallationCancelling"),
      contents: z.coerce.date(),
    }),
  ]),
});
const usageResponseSchema = z.object({
  by_org: z.record(z.string(), orgUsageSchema),
});

export const getAccountUsage = async (): Promise<APIResult<Usage>> => {
  const res = await fetchFromAPI(usageResponseSchema, "GET", "account/usage");
  if (!res.ok) return res;
  return Ok({ byOrg: res.data.by_org });
};

export const getOrgUsage = (org: string): Promise<APIResult<OrgUsage>> => {
  return fetchFromAPI(orgUsageSchema, "GET", `account/usage/${org}`);
};

export const setUsageLimits = (
  org: string,
  newUsageLimits: Plan["extra_usage"],
): Promise<APIResult<unknown>> => {
  return fetchFromAPI(z.unknown(), "PUT", `account/usage/${org}`, {
    body: JSON.stringify({
      ciTime: toSecs(newUsageLimits.ciTime),
      prDeployTime: toSecs(newUsageLimits.prDeployTime),
      hostingSpend: newUsageLimits.hostingSpend * 100,
    }),
  });
};

export type AccountTokenScopes = z.infer<typeof accountTokenScopes>;
const accountTokenScopes = z.object({
  cache: z.boolean(),
  api: z.boolean(),
});

const accessTokenMetadata = z.object({
  id: z.number(),
  name: z.string(),
  created: z.coerce.date(),
  last_used: z.coerce.date().optional(),
  scopes: accountTokenScopes,
});

export const getAccessTokens = () => {
  return fetchFromAPI(
    z.object({ tokens: z.array(accessTokenMetadata) }),
    "GET",
    "account/tokens",
  );
};

type AccountTokensConfig = {
  name: string;
  scopes: AccountTokenScopes;
};

export const generateAccessToken = (body: AccountTokensConfig) => {
  return fetchFromAPI(
    z.object({ token: z.string() }),
    "POST",
    "account/tokens",
    { body: JSON.stringify(body) },
  );
};

export const revokeAccessToken = (tokenId: number) => {
  return fetchFromAPI(z.unknown(), "DELETE", `account/tokens/${tokenId}`);
};

export const getRepos = async () => {
  const result = await fetchFromAPI(
    z.object({ repos: z.array(z.string()) }),
    "GET",
    "account/repos",
  );
  if (!result.ok) return result;
  return mapCollectResult((repo) => {
    const [repoUser, repoName] = repo.split("/");
    if (!repoUser || !repoName) {
      return Err({ message: `Unable to parse repo: ${repo}` });
    }
    return Ok({ repoUser, repoName });
  }, result.data.repos);
};
