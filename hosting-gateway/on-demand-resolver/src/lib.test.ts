import assert from "node:assert";
import { beforeEach, describe, it } from "node:test";
import { FETCH_INTERVAL, mkOnDemandResolver } from "./lib";

const mockCnames: Record<string, Array<string>> = {
  "points-to-garnix.com": ["package.branch.repo.valid-owner.garnix.me"],
  "points-to-invalid-garnix.com": [
    "package.branch.repo.invalid-owner.garnix.me",
  ],
  "points-to-something-else.com": ["example.org"],
  "has-no-cname.com": [],
};

const testOnDemandResolver = mkOnDemandResolver({
  getTimestamp: Date.now,
  resolveCname: async (hostName) => mockCnames[hostName] ?? [],
  fetchGarnixDomains: async () => [
    "package.branch.repo.valid-owner.garnix.me",
    "package.branch.repo.validCamelCase.garnix.me",
  ],
  hostingDomain: "garnix.me",
});

describe("onDemandResolver", () => {
  describe("isValid", () => {
    it("returns true if it is a valid garnix domain", async () => {
      assert.equal(
        await testOnDemandResolver.isValid(
          "package.branch.repo.valid-owner.garnix.me",
        ),
        true,
      );
    });

    it("returns false if passed a non-existing garnix domain", async () => {
      assert.equal(
        await testOnDemandResolver.isValid(
          "package.branch.repo.invalid-owner.garnix.me",
        ),
        false,
      );
    });

    it("returns true if passed a custom domain that CNAMEs to a valid garnix domain", async () => {
      assert.equal(
        await testOnDemandResolver.isValid("points-to-garnix.com"),
        true,
      );
    });

    it("returns false if passed a custom domain that CNAMEs to an invalid garnix domain", async () => {
      assert.equal(
        await testOnDemandResolver.isValid("has-no-cname.com"),
        false,
      );
      assert.equal(
        await testOnDemandResolver.isValid("points-to-something-else.com"),
        false,
      );
    });

    it("returns false if passed a custom domain that CNAMEs to a non-existing garnix domain", async () => {
      assert.equal(
        await testOnDemandResolver.isValid("points-to-invalid-garnix.com"),
        false,
      );
    });

    it("compares domains case-insensitively", async () => {
      assert.equal(
        await testOnDemandResolver.isValid(
          "package.branch.repo.validcamelcase.garnix.me",
        ),
        true,
      );
      assert.equal(
        await testOnDemandResolver.isValid(
          "package.branch.repo.valid-OWNER.garnix.me",
        ),
        true,
      );
    });

    it("doesn't look up a CNAME for a name already under the hosting domain", async () => {
      let resolveCnameCalled = false;
      const testOnDemandResolver = mkOnDemandResolver({
        getTimestamp: Date.now,
        resolveCname: async (_) => {
          resolveCnameCalled = true;
          return [];
        },
        fetchGarnixDomains: async () => [],
        hostingDomain: "garnix.me",
      });
      assert.equal(
        await testOnDemandResolver.isValid(
          "package.branch.repo.invalid-owner.garnix.me",
        ),
        false,
      );
      assert.equal(await testOnDemandResolver.isValid("garnix.me"), false);
      assert.equal(resolveCnameCalled, false);
    });

    it("treats the configured hosting domain, not garnix.me, as its own", async () => {
      let resolveCnameCalled = false;
      const selfHosted = mkOnDemandResolver({
        getTimestamp: Date.now,
        resolveCname: async (_) => {
          resolveCnameCalled = true;
          return [];
        },
        fetchGarnixDomains: async () => ["app.main.repo.owner.example.test"],
        hostingDomain: "example.test",
      });
      assert.equal(
        await selfHosted.isValid("app.main.repo.owner.example.test"),
        true,
      );
      // Under the hosting domain but not routable: rejected without a lookup.
      assert.equal(
        await selfHosted.isValid("gone.main.repo.owner.example.test"),
        false,
      );
      assert.equal(resolveCnameCalled, false);
      // A garnix.me name is now just another custom domain here, so it does
      // get a CNAME lookup.
      assert.equal(await selfHosted.isValid("something.garnix.me"), false);
      assert.equal(resolveCnameCalled, true);
    });

    it("doesn't look up a CNAME for IP addresses", async () => {
      let resolveCnameCalled = false;
      const testOnDemandResolver = mkOnDemandResolver({
        getTimestamp: Date.now,
        resolveCname: async (_) => {
          resolveCnameCalled = true;
          return [];
        },
        fetchGarnixDomains: async () => [],
        hostingDomain: "garnix.me",
      });
      assert.equal(await testOnDemandResolver.isValid("1.2.3.4"), false);
      assert.equal(resolveCnameCalled, false);
    });

    describe("garnix-server response caching", () => {
      let now: number = null as any;
      let fetchGarnixDomainsCallCount: number = null as any;
      let mockReturnValue: Promise<Array<string>> = null as any;
      let testOnDemandResolver: ReturnType<typeof mkOnDemandResolver> =
        null as any;

      beforeEach(() => {
        now = Date.now();
        fetchGarnixDomainsCallCount = 0;
        mockReturnValue = Promise.resolve([
          "package.branch.repo.owner.garnix.me",
        ]);
        testOnDemandResolver = mkOnDemandResolver({
          getTimestamp: () => now,
          resolveCname: async (hostName) => mockCnames[hostName] ?? [],
          fetchGarnixDomains: async () => {
            fetchGarnixDomainsCallCount++;
            return mockReturnValue;
          },
          hostingDomain: "garnix.me",
        });
      });

      it("caches garnix-server responses", async () => {
        await Promise.all([
          testOnDemandResolver.isValid("package.branch.repo.owner.garnix.me"),
          testOnDemandResolver.isValid("package.branch.repo.owner.garnix.me"),
          testOnDemandResolver.isValid("package.branch.repo.owner.garnix.me"),
          testOnDemandResolver.isValid("package.branch.repo.owner.garnix.me"),
          testOnDemandResolver.isValid("package.branch.repo.owner.garnix.me"),
        ]);
        assert.equal(fetchGarnixDomainsCallCount, 1);
      });

      it("fetches again if FETCH_INTERVAL has passed", async () => {
        await testOnDemandResolver.isValid(
          "package.branch.repo.owner.garnix.me",
        );
        now += FETCH_INTERVAL;
        assert.equal(
          await testOnDemandResolver.isValid(
            "package.branch.repo.owner.garnix.me",
          ),
          true,
        );
        assert.equal(fetchGarnixDomainsCallCount, 2);
      });

      it("returns the last successful fetch if there is an error fetching domains from garnix-server", async () => {
        mockReturnValue = Promise.resolve([
          "package.branch.repo.owner.garnix.me",
        ]);
        await testOnDemandResolver.isValid(
          "package.branch.repo.owner.garnix.me",
        );
        now += FETCH_INTERVAL;
        mockReturnValue = Promise.reject(new Error("boo"));
        assert.equal(
          await testOnDemandResolver.isValid(
            "package.branch.repo.owner.garnix.me",
          ),
          true,
        );
        assert.equal(fetchGarnixDomainsCallCount, 2);
        await testOnDemandResolver.isValid(
          "package.branch.repo.owner.garnix.me",
        );
        assert.equal(fetchGarnixDomainsCallCount, 2);
        now += FETCH_INTERVAL;
        mockReturnValue = Promise.resolve([
          "package.branch.repo.owner.garnix.me",
        ]);
        assert.equal(
          await testOnDemandResolver.isValid(
            "package.branch.repo.owner.garnix.me",
          ),
          true,
        );
        assert.equal(fetchGarnixDomainsCallCount, 3);
      });
    });
  });
});
