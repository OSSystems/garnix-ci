import { getPathMatch } from "next/dist/shared/lib/router/utils/path-match";
import { compileNonPath } from "next/dist/shared/lib/router/utils/prepare-destination";

// eslint-disable-next-line @typescript-eslint/no-require-imports
const nextConfig = require("../../../next.config.js");

type Redirect = { source: string; destination: string; permanent: boolean };

// Resolves a path the way Next's server applies `redirects`: the first rule
// whose source matches wins, its params substituted into the destination.
const redirectFor = async (
  path: string,
): Promise<{ location: string; permanent: boolean } | null> => {
  const redirects: Array<Redirect> = await nextConfig.redirects();
  for (const redirect of redirects) {
    const params = getPathMatch(redirect.source)(path);
    if (params)
      return {
        location: compileNonPath(redirect.destination, params),
        permanent: redirect.permanent,
      };
  }
  return null;
};

describe("forge-less repository URLs", () => {
  it("redirect temporarily to github.com's, so a rollback is not cached away", async () => {
    expect(await redirectFor("/repo/alice/proj")).toEqual({
      location: "/repo/github/alice/proj",
      permanent: false,
    });
  });

  it("leave forge-qualified URLs alone", async () => {
    expect(await redirectFor("/repo/git.example/alice/proj")).toBeNull();
  });

  it("are served by a Next server, which is what applies redirects", () => {
    // A static export would silently drop `redirects`.
    expect(nextConfig.output).toBe("standalone");
  });
});
