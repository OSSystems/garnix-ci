# Monitoring a self-hosted garnix

The monitoring modules come with `services.garnixServer`, so importing
`nixosModules.garnix` is enough. You only need to turn the pieces on.

There are three modules:

- `garnix.monitoring`: shared state. The list of hosts to scrape, the domain
  their FQDNs are derived from, and the basic auth credentials.
- `garnix.monitoring-server`: Prometheus and Grafana.
- `garnix.monitoring-client`: the exporters on a scraped machine, and
  optionally an nginx vhost in front of them.

Scrape jobs are derived from `garnix.monitoring.monitoredHosts`. A host with
`proxied = true` (the default) is scraped over https with basic auth at
the paths the client's vhost serves: `/metrics`, `/nginx`, `/nginxlog` and
`/server-metrics`. A host with `proxied = false` is scraped over plain http,
directly on each exporter's own port.

Set `garnix.monitoring.basicAuth.passwordFile = null` (the default) to turn
basic auth off on both sides.

## One machine, scraped over loopback

Nothing is exposed: the exporters are reached on `127.0.0.1`, and Grafana
listens on `garnix.monitoring-server.listenAddress` for you to tunnel to.

```nix
{
  services.nginx.enable = true; # the nginxlog exporter runs in the nginx group

  garnix.monitoring.monitoredHosts.myhost = {
    fqdn = "127.0.0.1";
    proxied = false;
    scrapeNginx = true;
    scrapeNginxLog = true;
    scrapeGarnixServer = true;
  };

  garnix.monitoring-server = {
    enable = true;
    fqdn = "monitoring.example.com";
    grafana.secretKeyFile = "/var/lib/garnix/secrets/grafana-secret-key";
    nginx.enable = false;
  };

  garnix.monitoring-client = {
    enable = true;
    nginx.enable = false;
  };
}
```

`monitoring-client.nodeId` defaults to `networking.hostName`, so the key in
`monitoredHosts` above has to match this machine's hostname.

## Server and client on separate machines

`garnix.monitoring.domain` gives every host a default FQDN of
`prometheus-node-exporter.<name>.<domain>`, which is what the client's vhost is
served under. Both machines need the same `basicAuth.passwordFile` contents.

On the monitoring server:

```nix
{
  garnix.monitoring = {
    domain = "example.com";
    basicAuth.passwordFile = "/var/lib/garnix/secrets/prometheus-basic-auth";
    monitoredHosts.web1 = {
      scrapeNginx = true;
      scrapeNginxLog = true;
      scrapeGarnixServer = true;
    };
  };

  garnix.monitoring-server = {
    enable = true;
    fqdn = "monitoring.example.com";
    grafana.secretKeyFile = "/var/lib/garnix/secrets/grafana-secret-key";
  };
}
```

On `web1`, repeat the `garnix.monitoring` block and add:

```nix
{
  garnix.monitoring-client.enable = true;
}
```

## Grafana behind an ingress you already run

Set `garnix.monitoring-server.nginx.enable = false` and point your ingress at
`listenAddress:grafana.port`. No vhost and no `security.acme.certs` entry are
created. Set `grafana.rootUrl` if the public URL is not `https://<fqdn>`.

To keep the local vhost but bring your own certificate, leave
`nginx.enable = true` and set `nginx.acme.enable = false`.

## Other options

- `garnix.monitoring-server.sqlExporter.enable` adds the `sql` job. Its `target`
  defaults to `garnix.database.exporter.fqdn` when the garnix database module is
  part of the configuration; otherwise set it explicitly.
- `garnix.monitoring-server.extraScrapeConfigs` is appended to the derived jobs.
- `garnix.monitoring-server.watchdog.enable` is a no-op unless
  `watchdog/nixos-module.nix` is in your module closure.
- `garnix.monitoring-client.openFirewall` defaults to the client's
  `nginx.enable` and controls ports 80 and 443.

Working configurations for the first two scenarios live in
`examples/example-selfhost.nix` as `exampleMonitoringLoopback`,
`exampleMonitoringSplit` and `exampleMonitoringSplitClient`.
