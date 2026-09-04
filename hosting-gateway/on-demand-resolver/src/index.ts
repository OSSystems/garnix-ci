// This service should respond with 200 if the passed domain should request a
// TLS certificate, non-200 otherwise.
//
// See documentation here:
// https://caddyserver.com/docs/json/apps/tls/automation/on_demand/permission/http/

import http from "node:http";
import dns from "node:dns/promises";
import { mkOnDemandResolver } from "./lib";
import z from "zod";

if (process.env.GARNIX_ORIGIN == null) {
  throw new Error("GARNIX_ORIGIN env var not set");
}
const GARNIX_ORIGIN = process.env.GARNIX_ORIGIN;

if (process.env.GARNIX_HOSTING_DOMAIN == null) {
  throw new Error("GARNIX_HOSTING_DOMAIN env var not set");
}
const GARNIX_HOSTING_DOMAIN = process.env.GARNIX_HOSTING_DOMAIN;

const prodFetchGarnixDomains = async (): Promise<Array<string>> => {
  const schema = z.object({ domains: z.array(z.string()) });
  const json = await (
    await fetch(`${GARNIX_ORIGIN}/api/hosts/on-demand-resolver`)
  ).json();
  return schema.parse(json).domains;
};

const productionOnDemandResolver = mkOnDemandResolver({
  getTimestamp: Date.now,
  resolveCname: dns.resolveCname,
  fetchGarnixDomains: prodFetchGarnixDomains,
  hostingDomain: GARNIX_HOSTING_DOMAIN,
});

if (process.env.PORT == null) {
  throw new Error("PORT env var not set");
}
const PORT: number = parseInt(process.env.PORT);

http
  .createServer(async (req, res) => {
    const domain = new URL(`http://x${req.url}`).searchParams.get("domain");
    console.log(`Request ${domain}`);
    res.statusCode =
      domain != null && (await productionOnDemandResolver.isValid(domain))
        ? 200
        : 400;
    console.log(`Response ${domain}: ${res.statusCode}`);
    res.end();
  })
  .listen(PORT, () => {
    console.log(`Listening on ${PORT}`);
  });
