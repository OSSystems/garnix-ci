# Hosting deployed servers on a self-hosted garnix

Garnix can run the NixOS configurations a repo builds, as microVM guests on the
garnix host, reachable over HTTPS at a name derived from the repo.

What gets deployed is declared in `garnix.yaml`, under `servers:` — one entry
per configuration, naming the branch (or pull request) that triggers it. That
is the whole declaration: reading the yaml tells you what a push does.

```yaml
# garnix.yaml, in the repo being built:
servers:
  - configuration: web
    deployment:
      type: on-branch
      branch: main
      machine: i2x4   # 2 vCPU, 4 GiB; the default is i1x2
```

The configuration it names is ordinary NixOS, plus one import: a guest is a
microVM, so its root filesystem and its nix store come from the guest profile
rather than from the configuration itself.

```nix
# flake.nix, in the same repo:
{
  inputs.garnix-guest.url = "github:OSSystems/garnix-guest-lib";

  outputs = { nixpkgs, garnix-guest, ... }: {
    nixosConfigurations.web = nixpkgs.lib.nixosSystem {
      modules = [
        garnix-guest.nixosModules.garnix-guest
        {
          services.nginx.enable = true;
          services.nginx.virtualHosts."_".root = ./site;
          system.stateVersion = "25.11";
        }
      ];
    };
  };
}
```

A push to `main` then deploys it at
`web.main.<repo>.<owner>.<hostingDomain>`, with a certificate issued on first
request.

Nothing garnix-specific is required beyond that import: no hostnames, no keys.
The hosting SSH key belongs to the garnix instance doing the hosting, not to
the repository being deployed, so it never appears in the repo — the
provisioner puts it on the guest it creates, and the guest keeps it on its own
disk from then on.

## The three pieces

Hosting needs all three, and they can live on one machine:

- `nixosModules.garnix-provisioner` — the daemon that creates, exposes and
  destroys guests.
- `nixosModules.garnix` — the backend, told where the provisioner's socket is
  and what its hosting domain is.
- `nixosModules.garnix-hosting-gateway` — Caddy and Traefik in front of the
  guests.

Without the provisioner socket, hosting is off: a deploy fails immediately
saying no provisioner is configured, rather than failing later in a way that is
harder to read.

## DNS

`*.<hostingDomain>` must resolve to the machine running the gateway. Every
deployed server's name is under it, and they are not known in advance.

A repo can also declare extra hostnames with `garnix.server.domains`. Those are
routed verbatim, so they are the user's own names, pointed at a garnix name
with a CNAME.

> **On a shared instance, `garnix.server.domains` is not ownership-checked.**
> Upstream validates a declared hostname against a registry of verified
> domains; that registry is not part of this fork. Any repo that can deploy can
> therefore claim any hostname. On a single-team instance that is harmless. If
> it is not, do not enable hosting for repos you do not control.

## Configuration

```nix
{
  imports = [
    garnix.nixosModules.garnix
    garnix.nixosModules.garnix-provisioner
    garnix.nixosModules.garnix-hosting-gateway
  ];

  garnix.local-provisioner = {
    enable = true;
    uplinkInterface = "eno1";
    # Pass store-path flakerefs so guest builds need no network fetch.
    nixpkgsFlake = "path:${inputs.nixpkgs}";
    microvmFlake = "path:${inputs.microvm}";
    # The private half of every guest's garnix.guest.sshPublicKey.
    sshPrivateKeyPath = "/run/secrets/garnix_hosting_ssh";
  };

  services.garnixServer = {
    enable = true;
    url = "https://garnix.example.com";

    provisionerSocket = config.garnix.local-provisioner.socketPath;

    hosting = {
      domain = "apps.example.com";
      statsReportUrl = "https://garnix.example.com/api/hosts/stats";
      sshKeys = [ config.garnix.local-provisioner.sshPrivateKeyPath ];
      # Must match local-provisioner.hostAddress.
      guestSubnetPrefix = "10.111.0.";
      # Leave the host 2 cores and 4 GiB for itself.
      vcpuBudget = "reserve:2";
      memoryBudget = "reserve:4096";
      # The largest machine any repo's garnix.yaml may ask for.
      maxTier = "i4x8";
      # Keep that much of the budget out of pull requests' reach.
      branchReserve = "i2x4";
    };
  };

  garnix.hosting-gateway = {
    enable = true;
    hostingDomain = "apps.example.com";
    garnixOrigin = "https://garnix.example.com";
    serverMappingEndpoint = "https://garnix.example.com/api/hosts/traefik";
  };

  security.acme.acceptTerms = true;
  security.acme.defaults.email = "ops@example.com";
}
```

The backend's user must be in `garnix.local-provisioner.backendGroup` (default
`garnix`) to reach the daemon socket.

The public key the provisioner derives from `sshPrivateKeyPath` is written to
`/var/lib/garnix-provisioner/hosting.pub` and set on every base guest it
creates. Repositories never see it: on first boot the guest copies it to
`/var/lib/garnix/hosting_authorized_keys`, a path on its own disk that sshd
reads for `root` and for the `garnix` user, and that survives activating a
configuration which never mentions the key.

## What a repo can ask for

Whether a configuration is deployed, from which branch, and at what size are
`garnix.yaml`'s business. Everything a deployed server exposes is the
configuration's own, set on `garnix.server`:

- `garnix.server.ports` — named ports. `type = "http"` ones are routed at
  `<name>.<server-name>`; `type = "tcp"` ones get a host port forwarded to
  them.
- `garnix.server.exposeSSH` — a public SSH port for the guest.
- `garnix.server.authorizeDeployerGithubKeys` — let whoever pushed log in as
  the guest's `garnix` user, using their GitHub keys.
- `garnix.server.authorizedSSHKeys` — the same, for keys given explicitly.
- `garnix.server.domains` — extra hostnames, subject to the caveat above.
- `garnix.server.persistence` — keep the guest's disk across pushes and
  redeploy in place, instead of replacing it.

Revoking any of these takes effect on the next deploy: a redeploy converges the
guest rather than adding to it.

A `servers:` entry naming a configuration the commit does not build fails the
deploy with that name, rather than silently deploying nothing.

## Limits

Three separate things, easy to confuse:

- `vcpuBudget` / `memoryBudget` cap what **every guest together** may hold, live
  and pooled. They are the instance's ceiling, checked when a guest is acquired.
- `maxTier` caps what a **single `servers:` entry** may ask for. Checked while
  planning, before any build is waited on, so a repo asking for `i16x32` is told
  which entry is at fault instead of being told the instance is out of capacity.
  It applies to branch and pull request deploys alike.
- `branchReserve` is a slice of the budget above that **pull requests may not
  take**. A pull request deploy is refused when taking it would leave less than
  the reserve free — including when the guest it would take is already warm in
  the pool — so a push to a deployed branch always has room to land, however
  many pull requests are open.

All three default to unset, which is no limit at all beyond the host's own
hardware.

## Lifecycle

A branch deploy is replaced on every push, unless it declares persistence, in
which case the same guest is redeployed in place.

A pull request gets its own `pull-<n>` deployment. Those are also reaped when
idle: the gateway reports which hostnames it served, and a PR guest that has
been up more than 12 hours without appearing in a report is torn down. The
reaper does nothing at all when no heartbeats have arrived, since that means
the gateway is down rather than that everything is idle.

A pull request from a fork is never deployed. Its code would run on a guest
holding the repo's deploy key.

garnix comments on the pull request with the addresses its servers landed on,
once per pull request, and comments again when a deploy fails, once per commit.
Neither is gated on `commentOnFailure`: declaring an `on-pull-request` server is
itself the opt-in, and a deploy whose address nobody is told is not much of a
deploy. Both need the garnix app to have `pull_requests: write`; without it the
comment is skipped and the deploy is unaffected.

## Checking it works

```
# The routing table the gateway polls:
curl https://garnix.example.com/api/hosts/traefik

# The names Caddy is allowed to get certificates for:
curl https://garnix.example.com/api/hosts/on-demand-resolver
```

If a deploy fails, its run's logs carry the guest's failed units and journal:
`switch-to-configuration` reports which units failed but not why, so the
backend reads the rest off the guest before tearing it down.
