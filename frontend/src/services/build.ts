import { z } from "zod";
import { defaultForgeSlug } from "./forges";
import { APIResult, fetchFromAPI } from ".";

export const statusSchema = z
  .union([
    z.literal("Failure"),
    z.literal("Success"),
    z.literal("Timeout"),
    z.literal("Cancelled"),
  ])
  .optional()
  .transform((s) => s ?? ("Pending" as const));

export const buildSchema = z
  .object({
    id: z.string(),
    branch: z.string().optional(),
    forge: z.string().optional(),
    repo_user: z.string(),
    repo_name: z.string(),
    req_user: z.string(),
    git_commit: z.string(),
    start_time: z.coerce.date(),
    package_type: z.string(),
    system: z.union([z.string(), z.null()]),
    package: z.string(),
    end_time: z.coerce.date().optional(),
    status: statusSchema,
  })
  .transform((build) => ({
    ...build,
    tag: "Build" as const,
    forge: build.forge ?? defaultForgeSlug,
    repoUser: build.repo_user,
    repoName: build.repo_name,
    reqUser: build.req_user,
    gitCommit: build.git_commit,
    startTime: build.start_time,
    packageType: build.package_type,
    endTime: build.end_time ?? null,
  }));

export type Build = z.infer<typeof buildSchema>;
export type BuildStatus = Build["status"];

const buildsWithRelatedBuilds = z.intersection(
  buildSchema,
  z.object({
    original_build: z
      .object({
        id: z.string(),
        git_commit: z.string(),
        status: statusSchema,
      })
      .transform((b) => ({ ...b, gitCommit: b.git_commit }))
      .optional(),
  }),
);

export type BuildWithRelatedBuilds = z.infer<typeof buildsWithRelatedBuilds>;

export const getBuild = async (
  id: string,
): Promise<APIResult<BuildWithRelatedBuilds>> => {
  return await fetchFromAPI(buildsWithRelatedBuilds, "GET", `build/${id}`);
};

export const cancelBuild = async (
  buildId: string,
): Promise<APIResult<null>> => {
  return await fetchFromAPI(z.null(), "PUT", `build/${buildId}`, {
    body: JSON.stringify({ status: "Cancelled" }),
  });
};
