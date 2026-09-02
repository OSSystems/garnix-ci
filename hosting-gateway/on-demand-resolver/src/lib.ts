import dns from "node:dns/promises";

// A name we would never have to follow a CNAME for: one already under our own
// hosting domain (if it were routable, the domain list would have said so), or
// a bare IP address.
const isCustomDomain = (domain: string, hostingDomain: string) => {
  if (domain === hostingDomain || domain.endsWith(`.${hostingDomain}`)) {
    return false;
  }
  if (domain.match(/^(\d+\.){3}\d+$/)) return false;
  return true;
};

export const FETCH_INTERVAL = 10_000;

export const mkOnDemandResolver = (dependencies: {
  getTimestamp: () => number;
  resolveCname: typeof dns.resolveCname;
  fetchGarnixDomains: () => Promise<Array<string>>;
  // The base domain deployed servers live under. Upstream hardcoded
  // garnix.me; a self-hosted instance picks its own.
  hostingDomain: string;
}) => {
  let fetchedAt = 0;
  let lastSuccessfulFetch: Array<string> = [];
  let domainCache: Promise<Array<string>> | null = null;

  const fetchGarnixDomainsWithCache = async (): Promise<Array<string>> => {
    const now = dependencies.getTimestamp();
    if (domainCache == null || now - fetchedAt >= FETCH_INTERVAL) {
      domainCache = dependencies.fetchGarnixDomains();
      fetchedAt = now;
    }
    try {
      lastSuccessfulFetch = (await domainCache).map((s) => s.toLowerCase());
      return lastSuccessfulFetch;
    } catch (err) {
      console.log(
        `Failed to fetch domains from garnix-server: ${(err as any).message}`,
      );
      return lastSuccessfulFetch;
    }
  };

  return {
    async isValid(input: string) {
      const domain = input.toLowerCase();
      const routableDomains = await fetchGarnixDomainsWithCache();
      if (routableDomains.includes(domain)) return true;
      if (!isCustomDomain(domain, dependencies.hostingDomain.toLowerCase())) {
        return false;
      }
      console.log(`Checking CNAME of ${domain}`);
      let resolvedDomains;
      try {
        resolvedDomains = await dependencies.resolveCname(domain);
      } catch (err) {
        console.log(
          `Failed to lookup CNAME of ${domain}: ${(err as any).message}`,
        );
        return false;
      }
      console.log(`${domain} resolved to ${JSON.stringify(resolvedDomains)}`);
      if (resolvedDomains.length !== 1) return false;
      return routableDomains.includes(resolvedDomains[0]!);
    },
  };
};
