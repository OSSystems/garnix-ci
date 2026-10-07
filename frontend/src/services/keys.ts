import { Repo } from "./modules";
import { defaultForgeSlug } from "./forges";

// Modules only know github.com repositories.

export const getRepoKey = async (repo: Repo) => {
  const response = await fetch(
    `/api/keys/${defaultForgeSlug}/${repo.repoUser}/${repo.repoName}/repo-key.public`,
  );
  return await response.text();
};
