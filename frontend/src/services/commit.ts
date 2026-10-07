import { z } from "zod";
import { Build, buildSchema } from "./build";
import { Run, runSchema } from "./run";
import { defaultForgeSlug } from "./forges";
import { APIResult, Ok, fetchFromAPI } from ".";

export const commitSummarySchema = z
  .object({
    forge: z.string().optional(),
    repo_owner: z.string(),
    repo_name: z.string(),
    git_commit: z.string(),
    branch: z.string().optional(),
    req_user: z.string(),
    start_time: z.coerce.date(),
    succeeded: z.number(),
    failed: z.number(),
    pending: z.number(),
    cancelled: z.number(),
  })
  .transform((commit) => ({
    ...commit,
    forge: commit.forge ?? defaultForgeSlug,
    repoUser: commit.repo_owner,
    repoName: commit.repo_name,
    gitCommit: commit.git_commit,
    reqUser: commit.req_user,
    startTime: commit.start_time,
  }));

export type CommitSummary = z.infer<typeof commitSummarySchema>;

export const getCommits = async (): Promise<
  APIResult<Array<CommitSummary>>
> => {
  const response = await fetchFromAPI(
    z.object({ commits: z.array(commitSummarySchema) }),
    "GET",
    "commits",
  );
  if (!response.ok) return response;
  return Ok(response.data.commits);
};

export const getCommitsForRepo = async (
  forge: string,
  repoOwner: string,
  repoName: string,
): Promise<APIResult<Array<CommitSummary>>> => {
  const response = await fetchFromAPI(
    z.object({ commits: z.array(commitSummarySchema) }),
    "GET",
    `commits/repo/${forge}/${repoOwner}/${repoName}`,
  );
  if (!response.ok) return response;
  return Ok(response.data.commits);
};

export const getCommit = async (
  commit: string,
): Promise<
  APIResult<{ summary: CommitSummary; builds: Array<Build>; runs: Array<Run> }>
> => {
  return await fetchFromAPI(
    z.object({
      summary: commitSummarySchema,
      builds: z.array(buildSchema),
      runs: z.array(runSchema),
    }),
    "GET",
    `commits/${commit}`,
  );
};
