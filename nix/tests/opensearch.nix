{ ... }:
{
  name = "test opensearch node";

  nodes = {
    server1 =
      {
        config,
        lib,
        nodes,
        ...
      }:
      {
        garnix.opensearch = {
          enable = true;
          fqdn = config.garnix.devMode.certificates.domain;
          dashboards.enable = true;
          bindIP = nodes.server1.networking.primaryIPAddress;
          isSingleNode = true;
        };
        networking.extraHosts = "127.0.0.1 ${config.garnix.opensearch.fqdn}";
        virtualisation = {
          memorySize = 2048;
          forwardPorts = [
            {
              from = "host";
              host.port = 4443;
              guest.port = 443;
            }
          ];
        };
      };

    client =
      {
        config,
        lib,
        nodes,
        ...
      }:
      {
        garnix.fluent-bit = {
          enable = lib.mkForce true;
          devModeOutputsToFile = false;
          opensearch.fqdn = nodes.server1.garnix.opensearch.fqdn;
        };
        networking.extraHosts = "${nodes.server1.networking.primaryIPAddress} ${nodes.server1.garnix.opensearch.fqdn}";
        virtualisation.memorySize = 2048;
      };
  };

  testScript =
    { nodes, ... }:
    ''
      start_all()
      server1.wait_for_unit("multi-user.target")
      client.wait_for_unit("multi-user.target")
      server1.wait_for_unit("nginx.service")
      server1.wait_for_unit("opensearch.service")
      server1.wait_for_unit("opensearch-dashboards.service")
      client.wait_for_unit("fluent-bit.service")

      def getOpensearch(url, username="garnix", passwordFile="/run/secrets/opensearch-garnix"):
        return f'curl -v --max-time 5 --fail -u "{username}:$(cat {passwordFile})" {url}'

      with subtest("Access opensearch through nginx"):
        server1.fail(
          "curl --fail https://${nodes.server1.garnix.opensearch.fqdn}"
        )
        server1.succeed(
          getOpensearch("https://${nodes.server1.garnix.opensearch.fqdn}")
        )
        server1.wait_until_succeeds(
          getOpensearch("https://${nodes.server1.garnix.opensearch.fqdn}/_cat/indices/garnix-system-$(date --utc '+%Y.%m.%d')")
        )
      with subtest("Access opensearch dashboards through nginx"):
        server1.fail(
          "curl --fail https://${nodes.server1.garnix.opensearch.fqdn}/dashboards"
        )
        server1.wait_until_succeeds(
          getOpensearch(
            "https://${nodes.server1.garnix.opensearch.fqdn}/dashboards")
        )
      with subtest("Search for logs in opensearch"):
        client.succeed(
          'logger "Hello, world!"'
        )
        # wait 5s as fluent-bit has a flush timeout of 5s
        import time
        time.sleep(5)
        client.wait_until_succeeds(
          getOpensearch(
            "https://${nodes.server1.garnix.opensearch.fqdn}/garnix-system-$(date --utc '+%Y.%m.%d')/_search?q=message:Hello",
            username="garnix",
            passwordFile="/run/secrets/opensearch-garnix"
          ) + """ | jq --exit-status '.hits.hits | first | debug | ._source.message == "Hello, world!"'""",
          timeout=30
        )
    '';
}
