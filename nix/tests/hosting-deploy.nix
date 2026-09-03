# End-to-end test of the hosting deploy layer's exposed surface: the backend
# publishes a routing table, the gateway consumes it, traffic reaches a guest,
# the heartbeat middleware reports back, Caddy's on-demand resolver agrees
# about which names are certifiable, and a guest's stats push is accepted from
# the bridge and refused from anywhere else.
#
# What this does NOT cover: provisioning a real microVM. Building a guest
# closure needs a network-connected nix inside the test VM, so the "deployed"
# server here is a seeded database row pointing at a plain web server. The
# provisioning path itself (acquireServer, copyClosure, switch-to-configuration)
# is still only exercised on a real host.
{ pkgs, lib, system, self, flakeInputs, ... }:
let
  hostingDomain = "hosting.test";
  backendHostname = "garnix.test";

  # The canonical name of the server the test seeds, and the extra names it is
  # expected to also answer on.
  canonicalName = "web.main.widgets.acme";
  primaryName = "widgets.acme";
  portName = "api.${canonicalName}";
  declaredDomain = "custom.example";

  guestBody = "hello from the guest";
  apiBody = "hello from the api port";

  # Dummy credentials, materialised into the store. This is a throwaway test
  # VM, so the store leak does not matter.
  mkSecret = name: content: pkgs.writeText "garnix-hosting-test-${name}" content;

  jwtKeyFile = mkSecret "jwt-key" "ZGV2LWp3dC1rZXktMzItYnl0ZXMtcGFkZGluZyEhIQ==";

  githubAppPkFile = pkgs.runCommand "garnix-hosting-test-github-app-pk"
    { nativeBuildInputs = [ pkgs.openssl ]; } ''
    openssl genrsa -out $out 2048
  '';

  ageKeyPair = pkgs.runCommand "garnix-hosting-test-age-keypair"
    { nativeBuildInputs = [ pkgs.age ]; } ''
    mkdir -p $out
    age-keygen -o $out/key 2>$out/pub.raw
    grep -oE 'age1[a-z0-9]+' $out/pub.raw > $out/pub
  '';
in
pkgs.testers.runNixOSTest {
  name = "garnix-hosting-deploy";

  node.specialArgs = {
    inherit flakeInputs;
    flakePackages = self.packages.${system};
  };

  nodes = {
    backend = { config, lib, pkgs, ... }: {
      imports = [ self.nixosModules.garnix ];

      networking.extraHosts = ''
        127.0.0.1 ${backendHostname}
      '';

      garnix.devMode.enable = true;
      garnix.monitoring-client.enable = false;

      virtualisation.memorySize = 3072;
      virtualisation.diskSize = 4096;

      services.garnixServer = {
        enable = true;
        hostname = backendHostname;
        url = "http://${backendHostname}";
        adminGithubLogin = "test";
        githubAppName = "test";
        acmeEmail = null;
        # Nothing here drives the web UI, and building it would dominate the
        # test's build time.
        frontend.enable = false;

        hosting = {
          domain = hostingDomain;
          statsReportUrl = "http://${backendHostname}/api/hosts/stats";
          # Every node in a NixOS test shares 192.168.1.0/24, so this is the
          # "bridge" as far as the stats guard is concerned.
          guestSubnetPrefix = "192.168.1.";
        };

        database = {
          host = "127.0.0.1";
          port = 5432;
          user = "garnix";
          name = "garnix";
          ssl.mode = "disable";
        };

        opensearch = {
          url = "http://127.0.0.1:9999/_msearch";
          host = "127.0.0.1";
          username = "garnix";
        };

        s3Cache.enable = false;
        remoteBuilders.hosts = [ ];

        secrets = {
          databasePasswordPath = toString (mkSecret "database-password" "postgres");
          githubWebhookSecretPath = toString (mkSecret "github-webhook-secret" "dev");
          githubClientSecretPath = toString (mkSecret "github-client-secret" "dev");
          githubClientIdPath = toString (mkSecret "github-client-id" "dev");
          githubAppIdPath = toString (mkSecret "github-app-id" "12345");
          githubAppPkPath = toString githubAppPkFile;
          opensearchCredentialPath = toString (mkSecret "opensearch-credential" "dev");
          jwtKeyPath = toString jwtKeyFile;
          repoSecretsKeyPath = "${ageKeyPair}/key";
          repoSecretsPubKeyPath = "${ageKeyPair}/pub";
        };
      };

      services.postgresql = {
        enable = true;
        ensureDatabases = [ "garnix" ];
        ensureUsers = [{
          name = "garnix";
          ensureDBOwnership = true;
        }];
        authentication = lib.mkForce ''
          local   all       all                      trust
          host    all       all      127.0.0.1/32    md5
          host    all       all      ::1/128         md5
        '';
      };

      # postgresql's initialScript runs before ensureUsers, so the password
      # has to be set afterwards.
      systemd.services.garnix-test-pg-password = {
        wantedBy = [ "multi-user.target" "garnixServer.service" ];
        before = [ "garnixServer.service" ];
        after = [ "postgresql.service" "postgresql-setup.service" ];
        requires = [ "postgresql.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          User = "postgres";
        };
        script = ''
          ${config.services.postgresql.package}/bin/psql -tAc \
            "ALTER USER garnix WITH PASSWORD 'postgres';"
        '';
      };
    };

    gateway = { nodes, lib, ... }: {
      imports = [
        self.nixosModules.garnix-hosting-gateway
        ../modules/monitoring-client.nix
        ../modules/monitoring.nix
      ];

      networking.extraHosts = ''
        ${nodes.backend.networking.primaryIPAddress} ${backendHostname}
      '';

      garnix.monitoring-client.enable = false;
      security.acme.defaults.email = "ops@example.test";
      security.acme.acceptTerms = true;

      garnix.hosting-gateway = {
        enable = true;
        serverMappingEndpoint = "http://${backendHostname}/api/hosts/traefik";
        hostingDomain = hostingDomain;
        garnixOrigin = "http://${backendHostname}";
        # The test would otherwise spend most of its time waiting for the
        # default poll interval.
        pollInterval = 2;
      };
    };

    # Stands in for a deployed microVM: something at an address, answering on
    # the guest port and on a declared named port.
    guest = { nodes, ... }: {
      networking.extraHosts = ''
        ${nodes.backend.networking.primaryIPAddress} ${backendHostname}
      '';
      networking.firewall.allowedTCPPorts = [ 80 8080 ];
      services.nginx = {
        enable = true;
        virtualHosts."guest" = {
          default = true;
          listen = [{ addr = "0.0.0.0"; port = 80; }];
          locations."/".return = "200 '${guestBody}'";
        };
        virtualHosts."guest-api" = {
          listen = [{ addr = "0.0.0.0"; port = 8080; }];
          locations."/".return = "200 '${apiBody}'";
        };
      };
    };
  };

  testScript = { nodes, ... }:
    let
      guestIp = nodes.guest.networking.primaryIPAddress;
      gatewayIp = nodes.gateway.networking.primaryIPAddress;
    in
    ''
      import json

      def psql(query):
          return backend.succeed(
              "sudo -u postgres psql garnix -tAX -v ON_ERROR_STOP=1 -c "
              + "\"" + query.replace('"', '\\"') + "\""
          ).strip()

      def through_traefik(host):
          return gateway.succeed(
              f"curl --fail --silent -H 'Host: {host}' http://localhost:8080/"
          )

      def resolver_status(domain):
          return gateway.succeed(
              "curl --silent -o /dev/null -w '%{http_code}' "
              + f"'http://localhost:8081/?domain={domain}'"
          ).strip()

      start_all()

      guest.wait_for_unit("nginx.service")
      backend.wait_for_unit("garnixServer.service", timeout=300)
      backend.wait_until_succeeds(
          "curl --fail http://${backendHostname}/api/health/check", timeout=180
      )
      gateway.wait_for_unit("traefik.service")
      gateway.wait_for_unit("caddy.service")
      gateway.wait_for_unit("caddyOnDemandResolver.service")

      with subtest("the gateway starts with an empty routing table"):
          # Traefik rejects a malformed provider response wholesale, so a
          # backend with nothing deployed still has to answer with a valid
          # configuration.
          empty = json.loads(
              backend.succeed("curl --fail http://${backendHostname}/api/hosts/traefik")
          )
          assert empty["http"]["routers"] == {}, empty
          assert "heartbeatmiddleware" in empty["http"]["middlewares"]

      with subtest("seeding a deployed server"):
          psql(
              "INSERT INTO builds "
              "(repo_user, repo_name, git_commit, package, status, package_type, "
              " req_user, repo_is_public, branch, drv_path, uploaded_to_cache) "
              "VALUES ('acme', 'widgets', 'deadbeef', 'web', 'success', "
              "'nixosConfiguration', 'acme', true, 'main', "
              "'/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-web.drv', true)"
          )
          psql(
              "INSERT INTO servers "
              "(configuration_build_id, provider, instance_id, ipv4, ready_at, "
              " server_tier, is_primary, exposed, domains) "
              "SELECT id, 'microvm', 'guest-1', '${guestIp}', now(), 'i1x2', true, "
              "'{\\\"ssh_port\\\": null, \\\"tcp\\\": [], "
              "\\\"http\\\": [{\\\"name\\\": \\\"api\\\", \\\"port\\\": 8080}]}'::json, "
              "'[\\\"${declaredDomain}\\\"]'::json "
              "FROM builds WHERE package = 'web'"
          )

      with subtest("the backend publishes a router for every name the server answers on"):
          config = json.loads(
              backend.succeed("curl --fail http://${backendHostname}/api/hosts/traefik")
          )
          routers = config["http"]["routers"]
          services = config["http"]["services"]

          # The canonical name, the short name a primary deploy also answers
          # on, the declared http port, and the repo's own hostname.
          assert "${canonicalName}" in routers, routers
          assert "${primaryName}" in routers, routers
          assert "${portName}" in routers, routers
          assert "${declaredDomain}" in routers, routers

          # A declared domain is matched verbatim; everything else is
          # suffixed with the hosting domain.
          assert routers["${canonicalName}"]["rule"] == (
              "Host(`${canonicalName}.${hostingDomain}`)"
          )
          assert routers["${declaredDomain}"]["rule"] == "Host(`${declaredDomain}`)"

          # The named port is its own service on the guest's port; the rest
          # share the guest's port 80.
          assert services["${canonicalName}"]["loadBalancer"]["servers"] == [
              {"url": "http://${guestIp}"}
          ], services
          assert services["${portName}"]["loadBalancer"]["servers"] == [
              {"url": "http://${guestIp}:8080"}
          ], services

          # Every router goes through the plugin, or a served request would
          # never be reported and the reaper would tear the server down.
          for name, router in routers.items():
              assert router["middlewares"] == ["heartbeatmiddleware"], (name, router)

      with subtest("traefik picks the table up and routes to the guest"):
          gateway.wait_until_succeeds(
              "curl --fail --silent -H 'Host: ${canonicalName}.${hostingDomain}' "
              "http://localhost:8080/",
              timeout=60,
          )
          assert "${guestBody}" in through_traefik("${canonicalName}.${hostingDomain}")

      with subtest("a primary deploy also answers on the repo's own name"):
          assert "${guestBody}" in through_traefik("${primaryName}.${hostingDomain}")

      with subtest("a declared http port routes to that port on the guest"):
          assert "${apiBody}" in through_traefik("${portName}.${hostingDomain}")

      with subtest("a declared domain is routed verbatim"):
          assert "${guestBody}" in through_traefik("${declaredDomain}")

      with subtest("an unknown hostname is not routed anywhere"):
          gateway.fail(
              "curl --fail --silent -H 'Host: nope.${hostingDomain}' http://localhost:8080/"
          )

      with subtest("the heartbeat middleware reports what it served"):
          # This is what keeps stopUnusedServers from reaping a PR deploy that
          # is still being used, so the whole chain -- plugin, backend, table
          # -- has to work, not just the routing.
          backend.wait_until_succeeds(
              "sudo -u postgres psql garnix -tAX -c "
              "\"SELECT count(*) FROM heartbeat "
              "WHERE hostname = '${canonicalName}.${hostingDomain}'\" | grep -qx 1",
              timeout=60,
          )

      with subtest("caddy is allowed a certificate for names we actually serve"):
          assert resolver_status("${canonicalName}.${hostingDomain}") == "200"
          assert resolver_status("${primaryName}.${hostingDomain}") == "200"
          assert resolver_status("${portName}.${hostingDomain}") == "200"
          assert resolver_status("${declaredDomain}") == "200"

      with subtest("caddy is refused a certificate for a name we do not serve"):
          # Without this, any SNI pointed at the gateway would make it ask a
          # CA for a certificate it has no business holding.
          assert resolver_status("nope.${hostingDomain}") == "400"
          assert resolver_status("") == "400"

      with subtest("the backend's own on-demand check agrees with the resolver"):
          backend.succeed(
              "curl --fail 'http://${backendHostname}/api/hosts/on-demand-check"
              "?domain=${canonicalName}.${hostingDomain}'"
          )
          backend.fail(
              "curl --fail 'http://${backendHostname}/api/hosts/on-demand-check"
              "?domain=nope.${hostingDomain}'"
          )

      with subtest("a guest's stats push is accepted from the guest itself"):
          guest.succeed(
              "curl --fail -X POST -H 'Content-Type: application/json' "
              "-d '{\"provisioner_id\": \"guest-1\", \"cpu_pct\": 12.5, "
              "\"mem_used_kb\": 100000, \"mem_total_kb\": 2000000}' "
              "http://${backendHostname}/api/hosts/stats"
          )
          assert psql(
              "SELECT count(*) FROM server_stats WHERE cpu_pct = 12.5"
          ) == "1"

      with subtest("a guest cannot push stats in another guest's name"):
          # Everything on the bridge passes the subnet check, so the only
          # thing stopping a guest fabricating its neighbour's samples is the
          # instance-id-to-address match.
          gateway.fail(
              "curl --fail -X POST -H 'Content-Type: application/json' "
              "-d '{\"provisioner_id\": \"guest-1\", \"cpu_pct\": 99.0, "
              "\"mem_used_kb\": 1, \"mem_total_kb\": 2}' "
              "http://${backendHostname}/api/hosts/stats"
          )
          assert psql("SELECT count(*) FROM server_stats WHERE cpu_pct = 99.0") == "0"

      with subtest("a sample for a server that is not running is rejected"):
          guest.fail(
              "curl --fail -X POST -H 'Content-Type: application/json' "
              "-d '{\"provisioner_id\": \"no-such-guest\", \"cpu_pct\": 1.0, "
              "\"mem_used_kb\": 1, \"mem_total_kb\": 2}' "
              "http://${backendHostname}/api/hosts/stats"
          )

      with subtest("tearing the server down takes its routes with it"):
          psql("UPDATE servers SET ended_at = now() WHERE instance_id = 'guest-1'")
          gateway.wait_until_fails(
              "curl --fail --silent -H 'Host: ${canonicalName}.${hostingDomain}' "
              "http://localhost:8080/",
              timeout=60,
          )
          # And Caddy must stop being willing to hold a certificate for it.
          # The backend memoises the routable set for 10s.
          gateway.wait_until_succeeds(
              "test $(curl --silent -o /dev/null -w '%{http_code}' "
              "'http://localhost:8081/?domain=${canonicalName}.${hostingDomain}') = 400",
              timeout=60,
          )
    '';
}
