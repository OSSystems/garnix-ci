{ pkgs
, lib
, system
, self
, ...
}:
let
  onDemandResolver =
    let
      nodejs = pkgs.nodejs;
      npmRoot = ./on-demand-resolver;
      node_modules = pkgs.importNpmLock.buildNodeModules { inherit nodejs npmRoot; };
      bundle = pkgs.runCommand "on-demand-resolver" { buildInputs = [ nodejs ]; } ''
        cp -r ${npmRoot} on-demand-resolver
        cd on-demand-resolver
        chmod -R +w .
        cp -r ${node_modules}/node_modules .
        npm run build
        mkdir -p $out
        cp -r node_modules dist $out
      '';
    in
    {
      package = pkgs.writeShellApplication {
        name = "on-demand-resolver";
        runtimeInputs = [ nodejs ];
        text = "node ${bundle}/dist";
      };
      check = pkgs.runCommand "test-on-demand-resolver" { buildInputs = [ nodejs ]; } ''
        cp -r ${npmRoot} on-demand-resolver
        cd on-demand-resolver
        chmod -R +w .
        cp -r ${node_modules}/node_modules .
        npm test
        touch $out
      '';
    };
in
{
  devShellInputs = with pkgs; [
    go
    gopls
  ];

  packages = {
    onDemandResolver = onDemandResolver.package;
  };

  checks = {
    testOnDemandResolver = onDemandResolver.check;

    # The middleware is a Traefik local plugin, loaded from source at runtime
    # rather than built here, so its own tests are the only thing that catches
    # a break before a deploy does.
    testHeartbeatMiddleware =
      pkgs.runCommand "test-heartbeat-middleware" { buildInputs = [ pkgs.go ]; } ''
        cp -r ${./heartbeatmiddleware} heartbeatmiddleware
        cd heartbeatmiddleware
        chmod -R +w .
        export HOME=$TMPDIR
        export GOFLAGS=-mod=mod
        export GOCACHE=$TMPDIR/go-cache
        # Pure Go: the plugin has no cgo dependency, and there is no C
        # toolchain in this build environment.
        export CGO_ENABLED=0
        go test ./...
        touch $out
      '';

    # The module's monitoring hookup is conditional here, unlike upstream, so
    # both shapes have to evaluate.
    gatewayModuleEval =
      let
        evalGateway = extra: (lib.nixosSystem {
          inherit system;
          modules = [
            ./nixos-module.nix
            ../nix/modules/monitoring-client.nix
            ({ ... }: {
              _module.args.flakePackages = {
                "hosting-gateway/onDemandResolver" = onDemandResolver.package;
              };
              boot.loader.grub.enable = false;
              fileSystems."/" = { device = "/dev/null"; fsType = "ext4"; };
              system.stateVersion = "25.11";
              security.acme.defaults.email = "ops@example.test";
              garnix.hosting-gateway = {
                enable = true;
                serverMappingEndpoint = "http://127.0.0.1:8000/api/hosts/traefik";
                hostingDomain = "example.test";
                garnixOrigin = "http://127.0.0.1:8000";
              };
            })
            extra
          ];
        }).config;

        withoutMonitoring = evalGateway { };
        withMonitoring = evalGateway {
          garnix.monitoring-client.enable = true;
          garnix.monitoring-client.nodeId = "gateway.example.test";
        };
      in
      # Traefik is what routes, and it must poll the backend for its table.
      assert withoutMonitoring.services.traefik.enable;
      assert withoutMonitoring.services.traefik.staticConfigOptions.providers.http.endpoint
        == "http://127.0.0.1:8000/api/hosts/traefik";
      # Caddy terminates TLS in front of it and asks the resolver about names
      # it has never seen.
      assert withoutMonitoring.services.caddy.enable;
      assert withoutMonitoring.services.caddy.settings.apps.tls.automation.on_demand.permission.endpoint
        == "http://localhost:8081/";
      assert withoutMonitoring.systemd.services.caddyOnDemandResolver.environment.GARNIX_HOSTING_DOMAIN
        == "example.test";
      # With monitoring off there is no node-exporter route to add, and no
      # separate nginx vhost to disable.
      assert !(withoutMonitoring.services.traefik.dynamicConfigOptions ? http);
      # With it on, Traefik takes over the node-exporter route so nginx does
      # not contend for :443.
      assert withMonitoring.services.traefik.dynamicConfigOptions.http.routers ? node-exporter-router;
      assert !withMonitoring.garnix.monitoring-client.nginx.enable;
      pkgs.runCommand "hosting-gateway-module-eval" { } ''
        touch "$out"
      '';
  };

  nixosModule = ./nixos-module.nix;
}
