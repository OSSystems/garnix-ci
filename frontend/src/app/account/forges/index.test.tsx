import { enableFetchMocks, MockResponseInit } from "jest-fetch-mock";
enableFetchMocks();

import userEvent from "@testing-library/user-event";
import "@testing-library/jest-dom";
import { render, screen, waitFor, within } from "@testing-library/react";
import { goTo } from "@/utils/navigate";
import { UserProvider, useUser } from "@/store/userContext";
import { ForgesComponent } from ".";

jest.mock("../../../utils/navigate", () => ({ goTo: jest.fn() }));
jest.mock("next/navigation", () => ({
  useRouter: () => ({ replace: jest.fn(), push: jest.fn() }),
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
  fetchMock.doMock(async (req): Promise<MockResponseInit> => {
    const body = req.body ? req.body.toString() : "";
    requests.push({
      method: req.method,
      url: req.url,
      body,
      contentType: req.headers.get("Content-Type"),
    });
    const ok = { status: 200, body: "[]" };
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

const rowOf = async (forge: string) =>
  (await screen.findByText(forge)).closest("tr")!;

describe("connected forges", () => {
  it("lists the account's identities, and connects the other forges", async () => {
    const user = userEvent.setup();
    forges = [github, acme()];
    identities = [{ forge: "github", login: "alice" }];
    render(<ForgesComponent />);
    expect(within(await rowOf("GitHub")).getByText("alice")).toBeVisible();
    const acmeRow = await rowOf("git.acme.com");
    expect(within(acmeRow).getByText("Not connected")).toBeVisible();
    await user.click(within(acmeRow).getByText("Connect"));
    expect(goTo).toHaveBeenCalledWith("https://git.acme.com/authorize");
    expect(window.localStorage.getItem("login-target-page")).toBe("/account");
  });

  it("never disconnects the last identity", async () => {
    forges = [github, acme()];
    identities = [{ forge: "github", login: "alice" }];
    render(<ForgesComponent />);
    const button = within(await rowOf("GitHub")).getByText("Disconnect");
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
      within(await rowOf("git.acme.com")).getByText("Disconnect"),
    );
    expect(
      await within(await rowOf("git.acme.com")).findByText("Not connected"),
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
      within(await rowOf("git.acme.com")).getByText("Disconnect"),
    );
    expect(await screen.findByText(/deletes them with it/)).toBeVisible();
    expect(identities).toHaveLength(2);
    await user.click(
      screen.getByText("Disconnect and delete the configurations"),
    );
    expect(
      await within(await rowOf("git.acme.com")).findByText("Not connected"),
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
    await user.click(within(await rowOf("GitHub")).getByText("Disconnect"));
    expect(
      await within(await rowOf("GitHub")).findByText("Not connected"),
    ).toBeVisible();
    await waitFor(() => expect(name).toHaveTextContent(/^alice-acme$/));
  });
});

describe("registered forges", () => {
  it("warn a registrant who logs in only through the forge that disabling it logs them out", async () => {
    const user = userEvent.setup();
    forges = [github, acme({ can_manage: true })];
    identities = [{ forge: "git.acme.com", login: "alice-acme" }];
    render(<ForgesComponent />);
    const card = await screen.findByTestId("managed-forge-git.acme.com");
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
    const card = await screen.findByTestId("managed-forge-git.acme.com");
    await user.click(within(card).getByText("Disable"));
    expect(screen.queryByTestId("disable-logs-out")).toBeNull();
  });

  it("are only shown to whoever may manage them", async () => {
    forges = [github, acme({ can_manage: false })];
    identities = [{ forge: "github", login: "alice" }];
    render(<ForgesComponent />);
    await screen.findByText("Connected forges");
    expect(screen.queryByText("Registered forges")).toBeNull();
  });

  it("replace the secret, which is never shown", async () => {
    const user = userEvent.setup();
    forges = [github, acme({ can_manage: true })];
    identities = [{ forge: "github", login: "alice" }];
    render(<ForgesComponent />);
    const card = await screen.findByTestId("managed-forge-git.acme.com");
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
    const card = await screen.findByTestId("managed-forge-git.acme.com");
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
