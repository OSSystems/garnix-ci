import "@testing-library/jest-dom";
import { render, screen } from "@testing-library/react";
import { commitSummarySchema } from "@/services/commit";
import { Forge, githubForge } from "@/services/forges";
import { ConfigContext } from "@/store/configContext";
import { CommitBuildsSummary } from ".";

const gitea: Forge = {
  slug: "git.example",
  kind: "gitea",
  webUrl: "https://git.example.com",
};

const commitOn = (forge: string | undefined, branch = "main") =>
  commitSummarySchema.parse({
    forge,
    repo_owner: "owner",
    repo_name: "repo",
    git_commit: "0123456789abcdef",
    branch,
    req_user: "alice",
    start_time: "2026-10-07T12:00:00Z",
    succeeded: 1,
    failed: 0,
    pending: 0,
    cancelled: 0,
  });

const renderWithForges = (commit: ReturnType<typeof commitOn>) =>
  render(
    <ConfigContext.Provider
      value={{ githubAppName: "app", forges: [githubForge, gitea] }}
    >
      <CommitBuildsSummary commit={commit} />
    </ConfigContext.Provider>,
  );

const hrefOf = (name: string) =>
  screen.getByRole("link", { name }).getAttribute("href");

const isLink = (name: string) => screen.queryByRole("link", { name }) !== null;

describe("CommitBuildsSummary", () => {
  it("links a Gitea repo to its instance's webUrl", () => {
    renderWithForges(commitOn("git.example"));
    expect(hrefOf("owner/repo")).toBe("/repo/git.example/owner/repo");
    expect(hrefOf("main")).toBe(
      "https://git.example.com/owner/repo/src/branch/main",
    );
    expect(hrefOf("01234567")).toBe(
      "https://git.example.com/owner/repo/commit/0123456789abcdef",
    );
    expect(hrefOf("@alice")).toBe("https://git.example.com/alice");
    expect(hrefOf("view on git.example.com")).toBe(
      "https://git.example.com/owner/repo",
    );
  });

  it("keeps a branch's slashes but escapes what would end the path", () => {
    renderWithForges(commitOn("git.example", "feature/fix#12?x%"));
    expect(hrefOf("feature/fix#12?x%")).toBe(
      "https://git.example.com/owner/repo/src/branch/feature/fix%2312%3Fx%25",
    );
  });

  it("treats a commit without a forge as github.com's", () => {
    renderWithForges(commitOn(undefined));
    expect(hrefOf("owner/repo")).toBe("/repo/github/owner/repo");
    expect(hrefOf("main")).toBe("https://github.com/owner/repo/tree/main");
    expect(hrefOf("01234567")).toBe(
      "https://github.com/owner/repo/commit/0123456789abcdef",
    );
    expect(hrefOf("@alice")).toBe("https://github.com/alice");
    expect(hrefOf("view on github.com")).toBe("https://github.com/owner/repo");
  });

  it("does not link out to a forge the server did not list", () => {
    renderWithForges(commitOn("unknown.example"));
    expect(hrefOf("owner/repo")).toBe("/repo/unknown.example/owner/repo");
    expect(isLink("main")).toBe(false);
    expect(isLink("01234567")).toBe(false);
    expect(isLink("@alice")).toBe(false);
    expect(screen.queryByText(/view on/)).toBeNull();
  });
});
