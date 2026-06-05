import { z } from "zod";
import { fromSecs } from "@/utils/duration";
import { mapCollectResult } from "@/utils";
import { Err, Ok, fetchFromAPI } from ".";

export type OrgUsage = z.infer<typeof orgUsageSchema>;
const orgUsageSchema = z.object({
  ci_time: z.number().transform(fromSecs),
  pr_deployment_time: z.number().transform(fromSecs),
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

export const getAccountUsage = () => {
  return fetchFromAPI(
    z.object({ by_org: z.record(z.string(), orgUsageSchema) }),
    "GET",
    "account/usage",
  );
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
