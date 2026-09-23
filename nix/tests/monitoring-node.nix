{ pkgs, ... }:

let
  basicAuth = {
    username = "prometheus";
    password = "test";
  };
in

{
  name = "test monitoring node";

  nodes.server = { config, lib, ... }: {
    networking.extraHosts = "127.0.0.1 ${config.garnix.monitoring-client.fqdn}";
    garnix = {
      monitoring = {
        monitoredHosts.${config.networking.hostName} = {
          scrapeNginx = true;
          scrapeGarnixServer = true;
        };
      };
      monitoring-client = {
        enable = lib.mkForce true;
        nodeId = config.networking.hostName;
        fqdn = config.garnix.devMode.certificates.domain;
        basicAuth = {
          inherit (basicAuth) username;
          passwordFile = "${pkgs.writeText "passwd-file" basicAuth.password}";
        };
      };
    };
    services.garnixServer.metricsPort = 80;
  };

  testScript = { nodes, ... }: ''
    start_all()
    server.wait_for_unit("multi-user.target")
    server.wait_for_unit("nginx.service")
    server.wait_for_unit("prometheus-node-exporter.service")
    server.wait_for_unit("prometheus-nginx-exporter.service")

    with subtest("Access node exporter through nginx"):
      server.fail(
        "curl --fail https://${nodes.server.garnix.monitoring-client.fqdn}"
      )
      server.succeed(
        "curl --fail -u ${basicAuth.username}:${basicAuth.password} https://${nodes.server.garnix.monitoring-client.fqdn}/metrics"
      )

    with subtest("Access nginx exporter through nginx"):
       server.fail(
         "curl --fail https://${nodes.server.garnix.monitoring-client.fqdn}/nginx"
       )
       server.succeed(
         "curl --fail -u ${basicAuth.username}:${basicAuth.password} https://${nodes.server.garnix.monitoring-client.fqdn}/nginx"
       )

    with subtest("Access garnix server metrics through nginx"):
       server.fail(
         "curl --fail https://${nodes.server.garnix.monitoring-client.fqdn}/server-metrics"
       )
       server.succeed(
         "curl --fail -u ${basicAuth.username}:${basicAuth.password} https://${nodes.server.garnix.monitoring-client.fqdn}/server-metrics"
       )
  '';
}
