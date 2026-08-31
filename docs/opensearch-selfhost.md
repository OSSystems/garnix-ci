# Self-hosting OpenSearch

OpenSearch is a machine role of its own, so `nixosModules.garnix` does not pull
it in. Import `opensearch/nixos-module.nix` on the host that runs it, and point
`services.garnixServer.opensearch.url` at that host from the garnix server.

The module needs `sops-nix` in the module list: it declares
`sops.secrets.opensearch-garnix`, which is also what
`garnix.opensearch.basicAuths` defaults to. Set `basicAuths` yourself to serve
other credentials; the passwords are still read from the paths you give it.

## The vhost

OpenSearch listens on `[::1]:9200` and its dashboards on `[::1]:5601`. In front
of them the module runs an nginx vhost named after `garnix.opensearch.fqdn`,
which proxies `/` to the former and `/dashboards` to the latter, both behind
the basic auth from `basicAuths`. By default that vhost terminates TLS with an
ACME certificate and redirects plain http to it.

## TLS terminated in front of this machine

Keep the vhost, drop the certificate:

```nix
{
  garnix.opensearch.nginx.acme.enable = false;
}
```

The vhost is then served over plain http, with the basic auth and both
locations unchanged. `forceSSL` follows `acme.enable`, because redirecting to
https is only worth doing when something on this machine has a certificate to
answer with.

## An ingress you already run

```nix
{
  garnix.opensearch.nginx.enable = false;
}
```

No vhost and no nginx. Ports 80 and 443 stay closed, since
`nginx.openFirewall` defaults to `nginx.enable`; set it to `true` to open them
anyway. Point your ingress at `[::1]:9200` and `[::1]:5601`, and note that
`basicAuths` is enforced by the vhost this module writes — with the vhost gone,
authenticating users is yours to do.

## Dev VMs

`nginx.acme.enable` defaults to `nginx.enable && !garnix.devMode.enable`, so a
VM running in dev mode serves plain http without any of the above being set.
