import { enableFetchMocks } from "jest-fetch-mock";
enableFetchMocks();

import { getForges, registerForge } from "./forges";

const github = {
  slug: "github",
  kind: "github",
  web_url: "https://github.com",
  source: "configured",
  name: "github.com",
  status: "active",
  can_manage: false,
};

beforeEach(() => fetchMock.resetMocks());

describe("getForges", () => {
  it("reads what the backend says of each forge", async () => {
    fetchMock.mockResponseOnce(JSON.stringify([github]));
    expect(await getForges()).toEqual({
      ok: true,
      data: [
        {
          slug: "github",
          kind: "github",
          webUrl: "https://github.com",
          source: "configured",
          name: "github.com",
          status: "active",
          canManage: false,
        },
      ],
    });
  });

  it("refuses a forge whose management the backend no longer tells", async () => {
    const withoutCanManage: Partial<typeof github> = { ...github };
    delete withoutCanManage.can_manage;
    fetchMock.mockResponseOnce(JSON.stringify([withoutCanManage]));
    const forges = await getForges();
    expect(forges.ok).toBe(false);
    expect(!forges.ok && forges.error.reason).toBe("schema-invalid");
  });
});

describe("registerForge", () => {
  it("answers the link to log in through the registered forge", async () => {
    fetchMock.mockResponseOnce(JSON.stringify({ login: "https://git/authz" }));
    expect(
      await registerForge({ url: "u", clientId: "i", clientSecret: "s" }),
    ).toEqual({ ok: true, data: "https://git/authz" });
  });

  it("reads anything but a login link as a malformed answer", async () => {
    fetchMock.mockResponseOnce(
      JSON.stringify({ register: { slug: "git", callback: "cb" } }),
    );
    const answer = await registerForge({
      url: "u",
      clientId: "i",
      clientSecret: "s",
    });
    expect(!answer.ok && answer.error.reason).toBe("schema-invalid");
    expect(!answer.ok && answer.error.path).toBe("forges");
  });
});
