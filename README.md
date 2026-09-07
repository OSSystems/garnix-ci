# garnix

CI for nixified, flake-based GitHub repos: it builds every flake output on push,
reports per-output checks back to GitHub, and can deploy the NixOS
configurations a repo declares.

This is a fork of [garnix-io/garnix-ci](https://github.com/garnix-io/garnix-ci)
focused on running the whole stack on your own infrastructure.

## Quickstart

The server is a NixOS module. It needs `flakeInputs` and `flakePackages` in
`specialArgs`, since it resolves its own binaries from this flake:

```nix
{
  inputs.garnix.url = "github:OSSystems/garnix-ci";

  outputs = { nixpkgs, garnix, ... }: {
    nixosConfigurations.ci = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        flakeInputs = garnix.inputs;
        flakePackages = garnix.packages.x86_64-linux;
      };
      modules = [ garnix.nixosModules.garnix ./configuration.nix ];
    };
  };
}
```

```nix
# configuration.nix
services.garnixServer = {
  enable = true;
  hostname = "garnix.example.com";
  url = "https://garnix.example.com";
  adminGithubLogin = "your-github-login";
  githubAppName = "your-github-app-slug";
  acmeEmail = "ops@example.com";

  database = { host = "localhost"; port = 5432; user = "garnix"; name = "garnix"; };
  opensearch = { url = "https://os.example.com/_msearch"; host = "os.example.com"; username = "garnix"; };

  # One path per secret, staged out-of-band (sops-nix, Vault, Ansible...).
  # Eleven are required; the module asserts on any you leave out.
  secrets.databasePasswordPath = "/var/lib/garnix/secrets/database-password";
  # ...
};
```

[`examples/example-selfhost.nix`](examples/example-selfhost.nix) is the complete,
evaluated reference: every secret path, the OpenSearch host, and the monitoring
variants.

## Modules

| Output | Role |
| --- | --- |
| `nixosModules.garnix` | The server: backend, frontend, nginx, migrations, monitoring |
| `nixosModules.garnix-provisioner` | microVM host for deployed servers |
| `nixosModules.garnix-hosting-gateway` | HTTPS routing to those guests |
| `nixosModules.garnix-guest` | Profile applied inside each guest |
| `opensearch/nixos-module.nix` | OpenSearch, as a machine role of its own |

## Documentation

- [Hosting deployed servers](docs/hosting-selfhost.md) — `servers:` in
  `garnix.yaml`, DNS, the gateway, guest lifecycle and limits
- [Monitoring](docs/monitoring-selfhost.md) — Prometheus, Grafana, loopback and
  split-machine setups
- [OpenSearch](docs/opensearch-selfhost.md) — build logs storage
- [Development](docs/development.md) — GitHub app, submitting test builds,
  running the frontend, the (currently disabled) per-pull-request demo deploy

## License

See [LICENSE](LICENSE).

## Acknowledgments

We erased git history when open sourcing, so we'll be explicit here about our
debt to everyone who contributed before the project became open source:

- Alex David
- Evie Ciobanu
- Greg Pfeil
- Jean-François Roche
- Julian Kirsten Arni
- Ramses de Norre
- Sönke Hahn

Thanks very very much!
