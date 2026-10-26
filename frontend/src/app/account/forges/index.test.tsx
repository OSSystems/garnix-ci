import { enableFetchMocks, MockResponseInit } from "jest-fetch-mock";
enableFetchMocks();

import userEvent from "@testing-library/user-event";
import "@testing-library/jest-dom";
import { render, screen, waitFor, within } from "@testing-library/react";
import { goTo } from "@/utils/navigate";
import { UserProvider, useUser } from "@/store/userContext";
import { ConfigContext } from "@/store/configContext";
import { ForgesComponent } from ".";

jest.mock("../../../utils/navigate", () => ({ goTo: jest.fn() }));
const push = jest.fn();
jest.mock("next/navigation", () => ({
  useRouter: () => ({ replace: jest.fn(), push }),
}));

type Forge = {
  slug: string;
  kind: "github" | "gitea";
  web_url: string;
  source: "configured" | "registered";
  name: string;
  status: "active" | "disabled";
  can_manage: boolean;
};

const github: Forge = {
  slug: "github",
  kind: "github",
  web_url: "https://github.com",
  source: "configured",
  name: "github.com",
  status: "active",
  can_manage: false,
};

const acme = (overrides: Partial<Forge> = {}): Forge => ({
  slug: "git.acme.com",
  kind: "gitea",
  web_url: "https://git.acme.com",
  source: "registered",
  name: "git.acme.com",
  status: "active",
  can_manage: false,
  ...overrides,
});

let forges: Array<Forge>;
let identities: Array<{ forge: string; login: string }>;
let holdsModuleSettings: boolean;
let started: MockResponseInit;

type Kind = "user" | "org" | "instance";
type InstallationJSON = {
  id: number;
  forge: string;
  kind: Kind;
  target: string;
  installer: string;
  status: "active" | "needs_reinstall";
  reason: string | null;
  createdAt: string;
};
type RepoJSON = { owner: string; name: string; private: boolean };

let installable: Array<{ kind: Kind; target: string }>;
let installations: Array<InstallationJSON>;
let reposOf: Record<number, Array<RepoJSON>>;
let enabled: Record<string, number>;
let nextId: number;

const installation = (
  overrides: Partial<InstallationJSON> & { id: number; kind: Kind },
): InstallationJSON => ({
  forge: "git.acme.com",
  target: "",
  installer: "alice-acme",
  status: "active",
  reason: null,
  createdAt: "2026-10-26T12:00:00Z",
  ...overrides,
});

const repo = (owner: string, name: string, priv = false): RepoJSON => ({
  owner,
  name,
  private: priv,
});

const sameTarget = (
  a: { kind: Kind; target: string },
  b: { kind: Kind; target: string },
) => a.kind === b.kind && a.target.toLowerCase() === b.target.toLowerCase();

const installationsAPI = (
  method: string,
  url: URL,
  body: string,
): MockResponseInit | undefined => {
  const prefix = "/api/forges/git.acme.com/";
  if (!url.pathname.startsWith(prefix)) return undefined;
  const path = url.pathname.slice(prefix.length).split("/");
  const json = (status: number, value: unknown) => ({
    status,
    body: JSON.stringify(value),
  });
  if (method === "GET" && path[0] === "installable")
    return json(
      200,
      installable.map((target) => ({
        ...target,
        installation:
          installations.find((i) => sameTarget(i, target))?.id ?? null,
      })),
    );
  if (path[0] !== "installations") return undefined;
  if (path.length === 1 && method === "GET") return json(200, installations);
  if (path.length === 1 && method === "POST") {
    const { kind, target } = JSON.parse(body);
    const created = installation({ id: nextId++, kind, target });
    installations.push(created);
    return json(200, created);
  }
  const id = Number(path[1]);
  const found = installations.find((i) => i.id === id);
  if (!found) return json(404, { message: "Not Found" });
  if (path.length === 3 && path[2] === "reinstall" && method === "POST") {
    found.status = "active";
    found.reason = null;
    return json(200, found);
  }
  if (path.length === 2 && method === "DELETE") {
    installations = installations.filter((i) => i.id !== id);
    return json(200, []);
  }
  if (path.length === 3 && path[2] === "repos" && method === "GET") {
    const page = Number(url.searchParams.get("page"));
    const limit = Number(url.searchParams.get("limit"));
    const all = reposOf[id] ?? [];
    return json(200, {
      repos: all.slice((page - 1) * limit, page * limit).map((r) => ({
        ...r,
        enabledIn: enabled[`${r.owner}/${r.name}`] ?? null,
      })),
      total: all.length,
    });
  }
  if (path.length === 5 && path[2] === "repos") {
    const full = path.slice(3).map(decodeURIComponent).join("/");
    if (method === "PUT") {
      const other = enabled[full];
      if (other != null && other !== id)
        return json(409, {
          status: "error",
          message: `${full} is already enabled in the installation ${other} (on the organisation acme); disable it there first`,
        });
      enabled[full] = id;
      return json(200, []);
    }
    if (method === "DELETE") {
      if (enabled[full] === id) delete enabled[full];
      return json(200, []);
    }
  }
  return undefined;
};
const requests: Array<{
  method: string;
  url: string;
  body: string;
  contentType: string | null;
}> = [];

beforeEach(() => {
  fetchMock.resetMocks();
  jest.clearAllMocks();
  requests.length = 0;
  holdsModuleSettings = false;
  installable = [];
  installations = [];
  reposOf = {};
  enabled = {};
  nextId = 100;
  window.localStorage.clear();
  started = {
    status: 200,
    body: JSON.stringify({
      register: {
        slug: "git.new.com",
        callback: "https://garnix.example/auth/git.new.com/login/cb",
      },
    }),
  };
  fetchMock.doMock(async (req): Promise<MockResponseInit> => {
    const body = req.body ? req.body.toString() : "";
    requests.push({
      method: req.method,
      url: req.url,
      body,
      contentType: req.headers.get("Content-Type"),
    });
    const ok = { status: 200, body: "[]" };
    const answered = installationsAPI(
      req.method,
      new URL(req.url, "https://garnix.example"),
      body,
    );
    if (answered) return answered;
    if (req.method === "GET" && req.url === "/api/forges")
      return { status: 200, body: JSON.stringify(forges) };
    if (req.method === "GET" && req.url === "/api/whoami")
      return {
        status: 200,
        body: JSON.stringify({
          // The main identity's login.
          username: identities[0]?.login,
          forge: identities[0]?.forge,
          email: "alice@example.com",
          is_admin: false,
          identities,
        }),
      };
    if (req.method === "POST" && req.url === "/api/auth/start") return started;
    if (req.method === "GET" && req.url === "/api/auth/git.acme.com/connect")
      return {
        status: 200,
        body: JSON.stringify({ github: "https://git.acme.com/authorize" }),
      };
    const disconnected = req.url.match(/^\/api\/auth\/([^/]+)\/identity$/);
    if (req.method === "DELETE" && disconnected) {
      if (holdsModuleSettings && !body.includes("confirmDeleteModuleSettings"))
        return {
          status: 409,
          body: JSON.stringify({
            status: "error",
            reason: "has_module_settings",
            // The UI acts on the reason, whatever the message says.
            message: "Module settings were saved through git.acme.com.",
          }),
        };
      identities = identities.filter((i) => i.forge !== disconnected[1]);
      return ok;
    }
    if (
      ["PUT", "DELETE"].includes(req.method) &&
      req.url.startsWith("/api/forges/git.acme.com")
    )
      return ok;
    throw Error(`unmocked path: ${req.method} ${req.url}`);
  });
});

const blockOf = (slug: string) => screen.findByTestId(`forge-${slug}`);

const rowOf = (kind: Kind, target: string) =>
  screen.findByTestId(`installation-${kind}-${target}`);

const openSettings = async (user: ReturnType<typeof userEvent.setup>) => {
  await user.click(
    within(await blockOf("git.acme.com")).getByRole("button", {
      name: "Settings",
    }),
  );
  return screen.getByTestId("managed-forge-git.acme.com");
};

describe("forges", () => {
  it("lists the account's identities, and connects the other forges", async () => {
    const user = userEvent.setup();
    forges = [github, acme()];
    identities = [{ forge: "github", login: "alice" }];
    render(<ForgesComponent />);
    expect(within(await blockOf("github")).getByText("alice")).toBeVisible();
    const acmeRow = await blockOf("git.acme.com");
    expect(within(acmeRow).getByText("Not connected")).toBeVisible();
    await user.click(within(acmeRow).getByText("Connect"));
    expect(goTo).toHaveBeenCalledWith("https://git.acme.com/authorize");
    expect(window.localStorage.getItem("login-target-page")).toBe("/account");
  });

  it("never disconnects the last identity", async () => {
    forges = [github, acme()];
    identities = [{ forge: "github", login: "alice" }];
    render(<ForgesComponent />);
    const button = within(await blockOf("github")).getByText("Disconnect");
    expect(button).toBeDisabled();
    expect(button).toHaveAttribute(
      "title",
      expect.stringContaining("only forge you log in with"),
    );
  });

  it("disconnects an identity without module settings at once", async () => {
    const user = userEvent.setup();
    forges = [github, acme()];
    identities = [
      { forge: "github", login: "alice" },
      { forge: "git.acme.com", login: "alice-acme" },
    ];
    render(<ForgesComponent />);
    await user.click(
      within(await blockOf("git.acme.com")).getByText("Disconnect"),
    );
    expect(
      await within(await blockOf("git.acme.com")).findByText("Not connected"),
    ).toBeVisible();
    const deletes = requests.filter((r) => r.method === "DELETE");
    expect(deletes.map((r) => r.body)).toEqual(["{}"]);
    expect(screen.queryByText(/deletes them/)).toBeNull();
  });

  it("asks before deleting the module settings saved through an identity", async () => {
    const user = userEvent.setup();
    holdsModuleSettings = true;
    forges = [github, acme()];
    identities = [
      { forge: "github", login: "alice" },
      { forge: "git.acme.com", login: "alice-acme" },
    ];
    render(<ForgesComponent />);
    await user.click(
      within(await blockOf("git.acme.com")).getByText("Disconnect"),
    );
    expect(await screen.findByText(/deletes them with it/)).toBeVisible();
    expect(identities).toHaveLength(2);
    await user.click(
      screen.getByText("Disconnect and delete the configurations"),
    );
    expect(
      await within(await blockOf("git.acme.com")).findByText("Not connected"),
    ).toBeVisible();
    const deletes = requests.filter((r) => r.method === "DELETE");
    expect(deletes.map((r) => JSON.parse(r.body))).toEqual([
      {},
      { confirmDeleteModuleSettings: true },
    ]);
    // Servant refuses a body it is not told is JSON.
    expect(deletes.map((r) => r.contentType)).toEqual([
      "application/json",
      "application/json",
    ]);
  });

  it("renames the account after disconnecting the identity it was named after", async () => {
    const user = userEvent.setup();
    forges = [github, acme()];
    identities = [
      { forge: "github", login: "alice" },
      { forge: "git.acme.com", login: "alice-acme" },
    ];
    const Name = () => {
      const { user } = useUser();
      return (
        <span data-testid="user-name">
          {user.state === "logged-in" ? user.user.name : ""}
        </span>
      );
    };
    render(
      <UserProvider>
        <Name />
        <ForgesComponent />
      </UserProvider>,
    );
    const name = screen.getByTestId("user-name");
    await waitFor(() => expect(name).toHaveTextContent(/^alice$/));
    await user.click(within(await blockOf("github")).getByText("Disconnect"));
    expect(
      await within(await blockOf("github")).findByText("Not connected"),
    ).toBeVisible();
    await waitFor(() => expect(name).toHaveTextContent(/^alice-acme$/));
  });
});

describe("forge settings", () => {
  it("warn a registrant who logs in only through the forge that disabling it logs them out", async () => {
    const user = userEvent.setup();
    forges = [github, acme({ can_manage: true })];
    identities = [{ forge: "git.acme.com", login: "alice-acme" }];
    render(<ForgesComponent />);
    const card = await openSettings(user);
    await user.click(within(card).getByText("Disable"));
    expect(screen.getByTestId("disable-logs-out")).toHaveTextContent(
      "You log in only through git.acme.com: disabling it logs you out, and this account cannot re-enable it. Registering git.acme.com again within 30 days brings it back with everyone who logged in through it.",
    );
  });

  it("do not warn a manager who logs in through another forge too", async () => {
    const user = userEvent.setup();
    forges = [github, acme({ can_manage: true })];
    identities = [
      { forge: "github", login: "alice" },
      { forge: "git.acme.com", login: "alice-acme" },
    ];
    render(<ForgesComponent />);
    const card = await openSettings(user);
    await user.click(within(card).getByText("Disable"));
    expect(screen.queryByTestId("disable-logs-out")).toBeNull();
  });

  it("are only offered to whoever may manage the forge, and folded at first", async () => {
    const user = userEvent.setup();
    forges = [github, acme({ can_manage: false })];
    identities = [{ forge: "github", login: "alice" }];
    const { unmount } = render(<ForgesComponent />);
    const block = await blockOf("git.acme.com");
    expect(within(block).getByText("git.acme.com")).toBeVisible();
    expect(within(block).queryByText("Settings")).toBeNull();
    expect(within(await blockOf("github")).queryByText("Settings")).toBeNull();
    unmount();
    forges = [github, acme({ can_manage: true })];
    render(<ForgesComponent />);
    await blockOf("git.acme.com");
    expect(screen.queryByTestId("managed-forge-git.acme.com")).toBeNull();
    expect(await openSettings(user)).toBeVisible();
  });

  it("replace the secret, which is never shown", async () => {
    const user = userEvent.setup();
    forges = [github, acme({ can_manage: true })];
    identities = [{ forge: "github", login: "alice" }];
    render(<ForgesComponent />);
    const card = await openSettings(user);
    expect(within(card).getByText("Client secret: configured ✓")).toBeVisible();
    await user.click(within(card).getByText("Replace"));
    await user.type(within(card).getByLabelText("New client secret"), "s3cr3t");
    await user.click(within(card).getByRole("button", { name: "Replace" }));
    const put = requests.find((r) => r.method === "PUT");
    expect(put?.url).toBe("/api/forges/git.acme.com/secret");
    expect(JSON.parse(put!.body)).toEqual({ clientSecret: "s3cr3t" });
  });

  it("are disabled only once confirmed", async () => {
    const user = userEvent.setup();
    forges = [github, acme({ can_manage: true })];
    identities = [{ forge: "github", login: "alice" }];
    render(<ForgesComponent />);
    const card = await openSettings(user);
    expect(within(card).getByText(/stays disabled for 30 days/)).toBeVisible();
    await user.click(within(card).getByText("Disable"));
    expect(
      screen.getByText(/ends all logins and sessions through git\.acme\.com/),
    ).toBeVisible();
    expect(requests.filter((r) => r.method === "DELETE")).toEqual([]);
    forges = [github, acme({ can_manage: true, status: "disabled" })];
    await user.click(screen.getAllByText("Disable").at(-1)!);
    expect(
      requests.filter((r) => r.method === "DELETE").map((r) => r.url),
    ).toEqual(["/api/forges/git.acme.com"]);
    expect(
      await within(card).findByText("Re-enable with a new secret"),
    ).toBeVisible();
  });

  it("come back with a new secret once disabled, and are no forge to log in with", async () => {
    const user = userEvent.setup();
    forges = [github, acme({ can_manage: true, status: "disabled" })];
    identities = [{ forge: "github", login: "alice" }];
    render(<ForgesComponent />);
    const card = await screen.findByTestId("managed-forge-git.acme.com");
    expect(screen.queryByText("Not connected")).toBeNull();
    await user.click(within(card).getByText("Re-enable with a new secret"));
    await user.type(within(card).getByLabelText("New client secret"), "new");
    await user.click(within(card).getByText("Re-enable"));
    expect(requests.find((r) => r.method === "PUT")?.url).toBe(
      "/api/forges/git.acme.com/secret",
    );
  });
});

describe("adding a forge from the account", () => {
  it("is offered while only GitHub is active", async () => {
    forges = [github];
    identities = [{ forge: "github", login: "alice" }];
    render(<ForgesComponent />);
    expect(
      await screen.findByText("Add a Gitea/Forgejo instance"),
    ).toBeVisible();
    expect(screen.getByLabelText("Gitea/Forgejo URL")).toBeVisible();
    expect(
      screen.getByText(/Connect or register another forge first/),
    ).toBeVisible();
  });

  it("opens the registration page, coming back to the account", async () => {
    const user = userEvent.setup();
    forges = [github];
    identities = [{ forge: "github", login: "alice" }];
    render(<ForgesComponent />);
    await user.type(
      await screen.findByLabelText("Gitea/Forgejo URL"),
      "https://git.new.com",
    );
    await user.click(screen.getByText("Add"));
    await waitFor(() => expect(push).toHaveBeenCalled());
    const target = new URL(push.mock.calls[0][0], "https://garnix.example");
    expect(target.pathname).toBe("/login/register");
    expect(target.searchParams.get("url")).toBe("https://git.new.com");
    expect(target.searchParams.get("from")).toBe("account");
    const start = requests.find((r) => r.url === "/api/auth/start");
    expect(JSON.parse(start!.body)).toEqual({ url: "https://git.new.com" });
    expect(window.localStorage.getItem("login-target-page")).toBe("/account");
    expect(goTo).not.toHaveBeenCalled();
  });

  it("connects a forge garnix already knows at once", async () => {
    const user = userEvent.setup();
    started = {
      status: 200,
      body: JSON.stringify({ login: "https://git.acme.com/authorize" }),
    };
    forges = [github, acme()];
    identities = [{ forge: "github", login: "alice" }];
    render(<ForgesComponent />);
    await user.type(
      await screen.findByLabelText("Gitea/Forgejo URL"),
      "https://git.acme.com",
    );
    await user.click(screen.getByText("Add"));
    await waitFor(() =>
      expect(goTo).toHaveBeenCalledWith("https://git.acme.com/authorize"),
    );
    expect(window.localStorage.getItem("login-target-page")).toBe("/account");
    expect(push).not.toHaveBeenCalled();
  });

  it("shows why the backend refuses the URL", async () => {
    const user = userEvent.setup();
    started = {
      status: 400,
      body: JSON.stringify({
        message: "Bad Request: only https URLs can be registered",
      }),
    };
    forges = [github];
    identities = [{ forge: "github", login: "alice" }];
    render(<ForgesComponent />);
    await user.type(
      await screen.findByLabelText("Gitea/Forgejo URL"),
      "https://git.new.com",
    );
    await user.click(screen.getByText("Add"));
    expect(await screen.findByTestId("register-error")).toHaveTextContent(
      /^only https URLs can be registered$/,
    );
    expect(push).not.toHaveBeenCalled();
  });
});

describe("GitHub", () => {
  it("links to the GitHub App for its installations", async () => {
    forges = [github, acme()];
    identities = [{ forge: "github", login: "alice" }];
    render(
      <ConfigContext.Provider
        value={{ githubAppName: "garnix-ci", forges: [] }}
      >
        <ForgesComponent />
      </ConfigContext.Provider>,
    );
    const block = await blockOf("github");
    expect(
      within(block).getByText("Manage installations").closest("a"),
    ).toHaveAttribute(
      "href",
      "https://github.com/apps/garnix-ci/installations/new",
    );
    expect(within(block).queryByText("Installations")).toBeNull();
    expect(requests.map((r) => r.url)).not.toContain(
      "/api/forges/github/installable",
    );
  });
});

describe("installations", () => {
  const connected = () => {
    forges = [github, acme()];
    identities = [
      { forge: "github", login: "alice" },
      { forge: "git.acme.com", login: "alice-acme" },
    ];
  };

  const expand = async (
    user: ReturnType<typeof userEvent.setup>,
    row: HTMLElement,
  ) => {
    await user.click(within(row).getByRole("button", { expanded: false }));
    return within(row);
  };

  it("lists only what the caller may install on, and installs", async () => {
    const user = userEvent.setup();
    connected();
    installable = [
      { kind: "user", target: "alice-acme" },
      { kind: "org", target: "acme" },
      { kind: "instance", target: "" },
    ];
    installations = [installation({ id: 1, kind: "instance" })];
    render(<ForgesComponent />);
    const own = await rowOf("user", "alice-acme");
    expect(within(own).getByText("Your account")).toBeVisible();
    const instance = await rowOf("instance", "");
    expect(within(instance).getByText("Whole instance")).toBeVisible();
    expect(within(instance).getByText("Active")).toBeVisible();
    expect(within(instance).queryByText("Install")).toBeNull();
    const org = await rowOf("org", "acme");
    expect(within(org).getByText("Organisation")).toBeVisible();
    await user.click(within(org).getByText("Install"));
    expect(await within(org).findByText("Active")).toBeVisible();
    const post = requests.find(
      (r) => r.method === "POST" && r.url.endsWith("/installations"),
    );
    expect(JSON.parse(post!.body)).toEqual({ kind: "org", target: "acme" });
    expect(within(org).queryByText("Install")).toBeNull();
  });

  it("shows why an installation needs a reinstall, and reinstalls it", async () => {
    const user = userEvent.setup();
    connected();
    installable = [{ kind: "org", target: "acme" }];
    installations = [
      installation({
        id: 7,
        kind: "org",
        target: "acme",
        status: "needs_reinstall",
        reason: "git.acme.com refused the token and would not renew it.",
      }),
    ];
    render(<ForgesComponent />);
    const org = await rowOf("org", "acme");
    expect(within(org).getByText("Needs reinstall")).toBeVisible();
    expect(
      within(org).getByText(
        "git.acme.com refused the token and would not renew it.",
      ),
    ).toBeVisible();
    await user.click(within(org).getByText("Reinstall"));
    expect(await within(org).findByText("Active")).toBeVisible();
    expect(within(org).queryByText(/refused the token/)).toBeNull();
    expect(
      requests.filter((r) => r.method === "POST").map((r) => r.url),
    ).toEqual(["/api/forges/git.acme.com/installations/7/reinstall"]);
  });

  it("shows why installing is refused", async () => {
    const user = userEvent.setup();
    connected();
    installable = [{ kind: "org", target: "acme" }];
    render(<ForgesComponent />);
    const org = await rowOf("org", "acme");
    installations = [installation({ id: 3, kind: "org", target: "acme" })];
    fetchMock.mockResponseOnce(
      JSON.stringify({
        message:
          "garnix is already installed on the organisation acme (installation 3); reinstall it instead",
      }),
      { status: 409 },
    );
    await user.click(within(org).getByText("Install"));
    expect(
      await within(org).findByText(
        /already installed on the organisation acme/,
      ),
    ).toBeVisible();
  });

  it("lists the repositories of an installation, and enables and disables them", async () => {
    const user = userEvent.setup();
    connected();
    installable = [{ kind: "org", target: "acme" }];
    installations = [installation({ id: 5, kind: "org", target: "acme" })];
    reposOf = {
      5: [repo("acme", "firmware", true), repo("acme", "docs")],
    };
    enabled = { "acme/docs": 5 };
    render(<ForgesComponent />);
    const org = await expand(user, await rowOf("org", "acme"));
    const firmware = await org.findByTestId("repo-acme/firmware");
    expect(within(firmware).getByAltText("private")).toBeVisible();
    expect(
      within(org.getByTestId("repo-acme/docs")).queryByAltText("private"),
    ).toBeNull();
    const firmwareToggle = within(firmware).getByLabelText(
      "Build acme/firmware",
    );
    expect(firmwareToggle).not.toBeChecked();
    expect(org.getByLabelText("Build acme/docs")).toBeChecked();
    await user.click(firmwareToggle);
    await waitFor(() => expect(firmwareToggle).toBeChecked());
    await user.click(org.getByLabelText("Build acme/docs"));
    await waitFor(() =>
      expect(org.getByLabelText("Build acme/docs")).not.toBeChecked(),
    );
    expect(
      requests
        .filter((r) => ["PUT", "DELETE"].includes(r.method))
        .map((r) => `${r.method} ${r.url}`),
    ).toEqual([
      "PUT /api/forges/git.acme.com/installations/5/repos/acme/firmware",
      "DELETE /api/forges/git.acme.com/installations/5/repos/acme/docs",
    ]);
    expect(enabled).toEqual({ "acme/firmware": 5 });
    expect(
      org.getByText("Installed by alice-acme on 26 Oct 2026"),
    ).toBeVisible();
  });

  it("says where a repository already builds, and does not offer it", async () => {
    const user = userEvent.setup();
    connected();
    installable = [
      { kind: "org", target: "acme" },
      { kind: "instance", target: "" },
    ];
    installations = [
      installation({ id: 5, kind: "org", target: "acme" }),
      installation({ id: 6, kind: "instance" }),
    ];
    reposOf = { 5: [repo("acme", "infra")] };
    enabled = { "acme/infra": 6 };
    render(<ForgesComponent />);
    const org = await expand(user, await rowOf("org", "acme"));
    const infra = await org.findByTestId("repo-acme/infra");
    expect(
      within(infra).getByText("Builds through the whole instance"),
    ).toBeVisible();
    expect(within(infra).getByLabelText("Build acme/infra")).toBeDisabled();
  });

  it("shows why a repository is not enabled when another installation took it meanwhile", async () => {
    const user = userEvent.setup();
    connected();
    installable = [{ kind: "org", target: "acme" }];
    installations = [installation({ id: 5, kind: "org", target: "acme" })];
    reposOf = { 5: [repo("acme", "infra")] };
    render(<ForgesComponent />);
    const org = await expand(user, await rowOf("org", "acme"));
    const toggle = await org.findByLabelText("Build acme/infra");
    enabled = { "acme/infra": 9 };
    await user.click(toggle);
    expect(
      await org.findByText(
        "acme/infra is already enabled in the installation 9 (on the organisation acme); disable it there first",
      ),
    ).toBeVisible();
    expect(toggle).not.toBeChecked();
  });

  it("pages through the repositories", async () => {
    const user = userEvent.setup();
    connected();
    installable = [{ kind: "org", target: "acme" }];
    installations = [installation({ id: 5, kind: "org", target: "acme" })];
    reposOf = {
      5: Array.from({ length: 25 }, (_, i) =>
        repo("acme", `repo-${String(i + 1).padStart(2, "0")}`),
      ),
    };
    render(<ForgesComponent />);
    const org = await expand(user, await rowOf("org", "acme"));
    expect(await org.findByText("1–20 of 25")).toBeVisible();
    expect(org.getByText("Previous")).toBeDisabled();
    expect(org.getByTestId("repo-acme/repo-20")).toBeVisible();
    await user.click(org.getByText("Next"));
    expect(await org.findByText("21–25 of 25")).toBeVisible();
    expect(org.queryByTestId("repo-acme/repo-20")).toBeNull();
    expect(org.getByTestId("repo-acme/repo-25")).toBeVisible();
    expect(org.getByText("Next")).toBeDisabled();
    expect(
      requests
        .filter((r) => r.url.includes("/repos?"))
        .map((r) => new URL(r.url, "https://garnix.example").search),
    ).toEqual(["?page=1&limit=20", "?page=2&limit=20"]);
    await user.click(org.getByText("Previous"));
    expect(await org.findByText("1–20 of 25")).toBeVisible();
  });

  it("filters the page by name and by whether it builds", async () => {
    const user = userEvent.setup();
    connected();
    installable = [{ kind: "org", target: "acme" }];
    installations = [installation({ id: 5, kind: "org", target: "acme" })];
    reposOf = {
      5: [repo("acme", "firmware"), repo("acme", "docs"), repo("acme", "dash")],
    };
    enabled = { "acme/docs": 5 };
    render(<ForgesComponent />);
    const org = await expand(user, await rowOf("org", "acme"));
    await org.findByTestId("repo-acme/firmware");
    await user.type(org.getByLabelText("Filter repositories"), "D");
    expect(org.queryByTestId("repo-acme/firmware")).toBeNull();
    expect(org.getByTestId("repo-acme/docs")).toBeVisible();
    expect(org.getByTestId("repo-acme/dash")).toBeVisible();
    await user.click(org.getByText("Building"));
    expect(org.queryByTestId("repo-acme/dash")).toBeNull();
    expect(org.getByTestId("repo-acme/docs")).toBeVisible();
    await user.click(org.getByText("All"));
    expect(org.getByTestId("repo-acme/dash")).toBeVisible();
  });

  it("uninstalls only once confirmed", async () => {
    const user = userEvent.setup();
    connected();
    installable = [{ kind: "org", target: "acme" }];
    installations = [installation({ id: 5, kind: "org", target: "acme" })];
    reposOf = { 5: [] };
    render(<ForgesComponent />);
    const org = await expand(user, await rowOf("org", "acme"));
    await user.click(await org.findByText("Uninstall"));
    expect(
      screen.getByText("Uninstall garnix from the organisation acme?"),
    ).toBeVisible();
    expect(requests.filter((r) => r.method === "DELETE")).toEqual([]);
    await user.click(screen.getByText("Cancel"));
    await user.click(org.getByText("Uninstall"));
    await user.click(screen.getAllByText("Uninstall").at(-1)!);
    expect(await org.findByText("Install")).toBeVisible();
    expect(
      requests.filter((r) => r.method === "DELETE").map((r) => r.url),
    ).toEqual(["/api/forges/git.acme.com/installations/5"]);
  });

  it("are not shown on a forge the account is not connected to", async () => {
    forges = [github, acme()];
    identities = [{ forge: "github", login: "alice" }];
    render(<ForgesComponent />);
    const block = await blockOf("git.acme.com");
    expect(within(block).getByText("Not connected")).toBeVisible();
    expect(within(block).queryByText("Installations")).toBeNull();
    expect(requests.some((r) => r.url.includes("installable"))).toBe(false);
  });
});
