import { enableFetchMocks, MockResponseInit } from "jest-fetch-mock";
enableFetchMocks();

import userEvent from "@testing-library/user-event";
import "@testing-library/jest-dom";
import { render, screen, waitFor } from "@testing-library/react";
import { goTo } from "@/utils/navigate";
import { Err, Ok } from "@/services";
import Page from "./page";
import { registerView } from "./register";

jest.mock("../../../utils/navigate", () => ({ goTo: jest.fn() }));
const replace = jest.fn();
jest.mock("next/navigation", () => ({ useRouter: () => ({ replace }) }));

const callback = "https://garnix.example/auth/git.acme.com/login/cb";

let answer: MockResponseInit;
let started: MockResponseInit;
let sent: string | undefined;
let startedWith: string | undefined;

beforeEach(() => {
  fetchMock.resetMocks();
  jest.clearAllMocks();
  started = {
    status: 200,
    body: JSON.stringify({ register: { slug: "git.acme.com", callback } }),
  };
  fetchMock.doMock(async (req): Promise<MockResponseInit> => {
    if (req.method === "POST" && req.url === "/api/auth/start") {
      startedWith = req.body?.toString();
      return started;
    }
    if (req.method === "POST" && req.url === "/api/forges") {
      sent = req.body?.toString();
      return answer;
    }
    throw Error(`unmocked path: ${req.method} ${req.url}`);
  });
});

const renderForm = async (searchParams: Record<string, string> = {}) => {
  render(
    <Page searchParams={{ url: "https://git.acme.com", ...searchParams }} />,
  );
  expect(await screen.findByTestId("redirect-uri")).toHaveTextContent(callback);
};

const fillAndSubmit = async () => {
  const user = userEvent.setup();
  await renderForm();
  await user.type(screen.getByLabelText("Client ID"), "the-id");
  await user.type(screen.getByLabelText("Client Secret"), "the-secret");
  await user.click(screen.getByText("Register and log in"));
};

describe("registering a forge", () => {
  it("submits it, then logs in through it", async () => {
    answer = {
      status: 200,
      body: JSON.stringify({ login: "https://git.acme.com/authorize" }),
    };
    await fillAndSubmit();
    expect(JSON.parse(sent!)).toEqual({
      url: "https://git.acme.com",
      clientId: "the-id",
      clientSecret: "the-secret",
    });
    expect(goTo).toHaveBeenCalledWith("https://git.acme.com/authorize");
  });

  it("says a registration of the host is already in progress", async () => {
    answer = {
      status: 409,
      body: JSON.stringify({
        message:
          "a registration of git.acme.com is already in progress; try again after 12:34 UTC",
      }),
    };
    await fillAndSubmit();
    expect(await screen.findByTestId("register-error")).toHaveTextContent(
      "A registration of git.acme.com is already in progress; try again after 12:34 UTC.",
    );
    expect(goTo).not.toHaveBeenCalled();
  });

  it("shows the backend's text for any other refusal", async () => {
    answer = {
      status: 403,
      body: JSON.stringify({
        message:
          "Forbidden: This endpoint is not available through the programmatic api.",
      }),
    };
    await fillAndSubmit();
    expect(await screen.findByTestId("register-error")).toHaveTextContent(
      /^This endpoint is not available through the programmatic api\.$/,
    );
  });

  it("sends whoever opens it without a forge to the login page", () => {
    render(<Page searchParams={{}} />);
    expect(replace).toHaveBeenCalledWith("/login");
    expect(screen.queryByText("Register and log in")).toBeNull();
  });

  it("takes the forge and its redirect URI from the backend, never from its URL", async () => {
    await renderForm({ slug: "x", callback: "https://wrong.example/cb" });
    expect(JSON.parse(startedWith!)).toEqual({ url: "https://git.acme.com" });
    expect(screen.getByText("Register git.acme.com")).toBeVisible();
    expect(screen.queryByText(/wrong\.example/)).toBeNull();
  });

  it("sends a forge garnix already knows to the login page", async () => {
    started = {
      status: 200,
      body: JSON.stringify({ login: "https://git.acme.com/authorize" }),
    };
    render(<Page searchParams={{ url: "https://git.acme.com" }} />);
    await waitFor(() => expect(replace).toHaveBeenCalledWith("/login"));
    expect(goTo).not.toHaveBeenCalled();
  });

  it("shows why the backend refuses the forge", async () => {
    started = {
      status: 400,
      body: JSON.stringify({
        message: "Bad Request: only https URLs can be registered",
      }),
    };
    render(<Page searchParams={{ url: "http://git.acme.com" }} />);
    expect(await screen.findByTestId("register-error")).toHaveTextContent(
      /^only https URLs can be registered$/,
    );
    expect(screen.queryByText("Register and log in")).toBeNull();
  });

  it("says so when the redirect URI cannot be copied", async () => {
    const user = userEvent.setup();
    Object.defineProperty(navigator, "clipboard", {
      value: { writeText: () => Promise.reject(new Error("denied")) },
      configurable: true,
    });
    await renderForm();
    await user.click(screen.getByText("Copy"));
    expect(
      await screen.findByText(/Could not copy: select the redirect URI/),
    ).toBeVisible();
  });
});

describe("registerView", () => {
  const callback = "https://garnix.example/auth/git.acme.com/login/cb";

  it("shows the form only for a forge to register, with the backend's slug and redirect URI", () => {
    expect(
      registerView({
        loading: false,
        data: Ok({ t: "register", slug: "git.acme.com", callback }),
      }),
    ).toEqual({ t: "form", slug: "git.acme.com", callback });
    expect(
      registerView({
        loading: false,
        data: Ok({ t: "login", link: "https://git.acme.com/authorize" }),
      }),
    ).toEqual({ t: "leave" });
    expect(registerView({ loading: true })).toEqual({ t: "loading" });
  });

  it("says why the backend refuses the forge", () => {
    expect(
      registerView({
        loading: false,
        data: Err({
          path: "/api/auth/start",
          reason: "not-ok",
          message: "Bad Request: only https URLs can be registered",
          status: 400,
        }),
      }),
    ).toEqual({ t: "refused", message: "only https URLs can be registered" });
  });
});
