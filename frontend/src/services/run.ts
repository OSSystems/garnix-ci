import { z } from "zod";
import { statusSchema } from "./build";
import { defaultForgeSlug } from "./forges";
import { APIResult, fetchFromAPI } from ".";

export const runSchema = z
  .object({
    id: z.string(),
    name: z.string(),
    forge: z.string().optional(),
    repo_user: z.string(),
    repo_name: z.string(),
    git_commit: z.string(),
    branch: z.string().optional(),
    status: statusSchema,
    start_time: z.coerce.date(),
    end_time: z.coerce.date().optional(),
  })
  .transform((run) => ({
    ...run,
    tag: "Run" as const,
    forge: run.forge ?? defaultForgeSlug,
    repoUser: run.repo_user,
    repoName: run.repo_name,
    gitCommit: run.git_commit,
    startTime: run.start_time,
    endTime: run.end_time ?? null,
  }));

export type Run = z.infer<typeof runSchema>;

export const getRun = async (id: string): Promise<APIResult<Run>> => {
  return await fetchFromAPI(runSchema, "GET", `run/${id}`);
};
