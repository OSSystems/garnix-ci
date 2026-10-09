import { enableFetchMocks, MockResponseInit } from "jest-fetch-mock";
enableFetchMocks();

import userEvent from "@testing-library/user-event";
import "@testing-library/jest-dom";
import { act, render, screen } from "@testing-library/react";
import { useEffect } from "react";
import { UserProvider } from "@/store/userContext";
import { Err, Ok } from "@/services";
import { LoginCallback, afterFinish } from ".";

const replace = jest.fn();
const push = jest.fn();
const searchParams = new URLSearchParams({ code: "c", state: "s" });
jest.mock("next/navigation", () => ({
  useRouter: () => ({ replace, push }),
  useSearchParams: () => searchParams,
}));
// The animation tells when it is done at once.
jest.mock("../loginAnimation", () => ({
  LoginAnimation: (props: { onAnimationDone: () => void }) => {
    useEffect(() => props.onAnimationDone());
    return null;
  },
}));

let callback: MockResponseInit;
let loggedIn: boolean;
const urls: Array<string> = [];

beforeEach(() => {
  fetchMock.resetMocks();
  jest.clearAllMocks();
  urls.length = 0;
  loggedIn = true;
  window.localStorage.setItem("login-target-page", "/account");
  fetchMock.doMock(async (req): Promise<MockResponseInit> => {
    urls.push(req.url);
    // The session the callback just set.
    if (req.url === "/api/whoami")
      return {
        status: 200,
        body: JSON.stringify(
          loggedIn
            ? {
                username: "alice",
                email: "a@example.com",
                identities: [{ forge: "github", login: "alice" }],
              }
            : null,
        ),
      };
    if (req.url.includes("/cb?")) return callback;
    throw Error(`unmocked path: ${req.method} ${req.url}`);
  });
});

const renderCallback = async (forge: string) => {
  await act(async () => {
    render(
      <UserProvider>
        <LoginCallback forge={forge} />
      </UserProvider>,
    );
  });
};

describe("the login callback", () => {
  it("finishes a login through a forge under its slug", async () => {
    callback = {
      status: 200,
      body: JSON.stringify({ username: "alice", emailAlreadyUsed: false }),
    };
    await renderCallback("git.acme.com");
    expect(urls).toContain("/api/auth/git.acme.com/login/cb?code=c&state=s");
    expect(replace).toHaveBeenCalledWith("/account");
  });

  it("finishes a github.com login where it always has", async () => {
    callback = {
      status: 200,
      body: JSON.stringify({ username: "alice", emailAlreadyUsed: false }),
    };
    await renderCallback("github");
    expect(urls).toContain("/api/login/cb?code=c&state=s");
    expect(replace).toHaveBeenCalledWith("/account");
  });

  it("says once that the person may already have an account, never which", async () => {
    const user = userEvent.setup();
    callback = {
      status: 200,
      body: JSON.stringify({ username: "alice", emailAlreadyUsed: true }),
    };
    await renderCallback("git.acme.com");
    expect(
      screen.getByText(
        "An account with this email already exists. If it is yours, log in with it and connect this forge from settings.",
      ),
    ).toBeVisible();
    expect(replace).not.toHaveBeenCalled();
    await user.click(screen.getByText("Continue"));
    expect(replace).toHaveBeenCalledWith("/account");
  });

  it("shows why a login through a pending forge was refused", async () => {
    callback = {
      status: 403,
      body: JSON.stringify({
        message:
          "Forbidden: git.acme.com is still waiting for whoever registered it to log in through it. Try again once they have.",
      }),
    };
    await renderCallback("git.acme.com");
    expect(
      screen.getByText(
        /^git\.acme\.com is still waiting for whoever registered it/,
      ),
    ).toBeVisible();
    expect(replace).not.toHaveBeenCalled();
  });

  it("goes back to the account page when a connect fails", async () => {
    const user = userEvent.setup();
    callback = {
      status: 409,
      body: JSON.stringify({
        message: "Your account is already connected to git.acme.com as carol.",
      }),
    };
    await renderCallback("git.acme.com");
    await user.click(screen.getByText("Back"));
    expect(push).toHaveBeenCalledWith("/account");
  });

  it("goes back to the forge chooser when a login fails", async () => {
    const user = userEvent.setup();
    loggedIn = false;
    callback = {
      status: 403,
      body: JSON.stringify({ message: "Forbidden: no" }),
    };
    await renderCallback("git.acme.com");
    await user.click(screen.getByText("Back"));
    expect(push).toHaveBeenCalledWith("/login");
  });
});

describe("afterFinish", () => {
  const refused = (status: number) =>
    Err({
      path: "/api/auth/git.acme.com/login/cb",
      reason: "not-ok" as const,
      message: "Forbidden: no",
      status,
    });
  const loggedInAs = (emailAlreadyUsed: boolean) =>
    Ok({ user: { name: "alice" }, emailAlreadyUsed });

  it("leaves after a login, saying first when the email was in use", () => {
    expect(afterFinish(loggedInAs(false), "git.acme.com")).toEqual({
      t: "leaving",
    });
    expect(afterFinish(loggedInAs(true), "git.acme.com")).toEqual({
      t: "notice",
    });
  });

  it("says a refused login through another forge cannot go on", () => {
    expect(afterFinish(refused(403), "git.acme.com")).toEqual({
      t: "failed",
      title: "This login cannot go on.",
      message: "no",
    });
    expect(afterFinish(refused(403), "github")).toMatchObject({
      title: "Something went wrong.",
    });
    expect(afterFinish(refused(500), "git.acme.com")).toMatchObject({
      title: "Something went wrong.",
    });
  });
});
