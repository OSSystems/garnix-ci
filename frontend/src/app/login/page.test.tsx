import { enableFetchMocks, MockResponseInit } from "jest-fetch-mock";
enableFetchMocks();

import userEvent from "@testing-library/user-event";
import "@testing-library/jest-dom";
import { render, screen } from "@testing-library/react";
import { goTo } from "@/utils/navigate";
import Page from "./page";

const push = jest.fn();
jest.mock("next/navigation", () => ({
  useRouter: () => ({ push }),
}));
jest.mock("../../utils/navigate", () => ({ goTo: jest.fn() }));

const forges = [
  {
    slug: "github",
    kind: "github",
    web_url: "https://github.com",
    source: "configured",
    name: "github.com",
    status: "active",
    can_manage: false,
  },
  {
    slug: "gitea.com",
    kind: "gitea",
    web_url: "https://gitea.com",
    source: "configured",
    name: "gitea.com",
    status: "active",
    can_manage: false,
  },
  {
    slug: "git.old",
    kind: "gitea",
    web_url: "https://git.old",
    source: "registered",
    name: "git.old",
    status: "disabled",
    can_manage: true,
  },
];

let startAnswer: MockResponseInit;
const requests: Array<{ method: string; url: string; body: string }> = [];

beforeEach(() => {
  fetchMock.resetMocks();
  jest.clearAllMocks();
  requests.length = 0;
  window.localStorage.clear();
  fetchMock.doMock(async (req): Promise<MockResponseInit> => {
    requests.push({
      method: req.method,
      url: req.url,
      body: req.body ? req.body.toString() : "",
    });
    if (req.method === "GET" && req.url === "/api/forges")
      return { status: 200, body: JSON.stringify(forges) };
    if (req.method === "GET" && req.url === "/api/auth/gitea.com/login")
      return {
        status: 200,
        body: JSON.stringify({ github: "https://gitea.com/authorize" }),
      };
    if (req.method === "POST" && req.url === "/api/auth/start")
      return startAnswer;
    throw Error(`unmocked path: ${req.method} ${req.url}`);
  });
});

const renderPage = () => render(<Page searchParams={{ page: "/account" }} />);

describe("login page", () => {
  it("offers every active forge, and logs in through the chosen one", async () => {
    const user = userEvent.setup();
    renderPage();
    expect(await screen.findByText("Log in with GitHub")).toBeInTheDocument();
    expect(screen.queryByText("Log in with git.old")).toBeNull();
    await user.click(screen.getByText("Log in with gitea.com"));
    expect(goTo).toHaveBeenCalledWith("https://gitea.com/authorize");
    expect(window.localStorage.getItem("login-target-page")).toBe("/account");
  });

  it("logs in through a forge named by its URL", async () => {
    const user = userEvent.setup();
    startAnswer = {
      status: 200,
      body: JSON.stringify({ login: "https://git.acme.com/authorize" }),
    };
    renderPage();
    await user.type(
      screen.getByLabelText("Gitea/Forgejo URL"),
      "https://git.acme.com",
    );
    await user.click(screen.getByRole("button", { name: "Log in" }));
    expect(requests.find((r) => r.url === "/api/auth/start")?.body).toBe(
      JSON.stringify({ url: "https://git.acme.com" }),
    );
    expect(goTo).toHaveBeenCalledWith("https://git.acme.com/authorize");
  });

  it("goes to the registration form for a forge garnix does not know", async () => {
    const user = userEvent.setup();
    startAnswer = {
      status: 200,
      body: JSON.stringify({
        register: {
          slug: "git.acme.com",
          callback: "https://garnix.example/auth/git.acme.com/login/cb",
        },
      }),
    };
    renderPage();
    await user.type(
      screen.getByLabelText("Gitea/Forgejo URL"),
      "https://git.acme.com",
    );
    await user.click(screen.getByRole("button", { name: "Log in" }));
    expect(goTo).not.toHaveBeenCalled();
    const target = new URL(push.mock.calls[0][0], "https://garnix.example");
    expect(target.pathname).toBe("/login/register");
    // The registration page asks the backend again for the rest.
    expect(Object.fromEntries(target.searchParams)).toEqual({
      url: "https://git.acme.com",
    });
  });

  it("comes back to the home page after a login that names no page", async () => {
    const user = userEvent.setup();
    // Left by a connect that never came back.
    window.localStorage.setItem("login-target-page", "/account");
    render(<Page searchParams={{}} />);
    await user.click(await screen.findByText("Log in with gitea.com"));
    expect(goTo).toHaveBeenCalledWith("https://gitea.com/authorize");
    expect(window.localStorage.getItem("login-target-page")).toBe("/");
  });

  it("shows why the backend refused a URL", async () => {
    const user = userEvent.setup();
    startAnswer = {
      status: 400,
      body: JSON.stringify({
        message: "Bad Request: only https URLs can be registered",
      }),
    };
    renderPage();
    await user.type(
      screen.getByLabelText("Gitea/Forgejo URL"),
      "http://git.acme.com",
    );
    await user.click(screen.getByRole("button", { name: "Log in" }));
    expect(await screen.findByTestId("login-error")).toHaveTextContent(
      /^only https URLs can be registered$/,
    );
    expect(push).not.toHaveBeenCalled();
  });
});
