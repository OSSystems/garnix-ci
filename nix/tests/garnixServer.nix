{ ... }:
{
  name = "garnix server (with separate database) tests";

  nodes = {
    garnixServer =
      {
        config,
        lib,
        nodes,
        pkgs,
        ...
      }:
      {
        networking.extraHosts = ''
          ${nodes.garnixServer.networking.primaryIPAddress} garnix.io
          ${nodes.garnixServer.networking.primaryIPAddress} app.garnix.io
          ${nodes.db.networking.primaryIPAddress} ${nodes.db.garnix.database.fqdn}
        '';

        environment.systemPackages = [ pkgs.httpie ];

        services.garnixServer = {
          enable = true;
          testFeatures = [ "DevApi" ];
          provisionServerPool = false;
        };
        garnix.database = {
          enable = false;
        };

      };
    db =
      {
        config,
        lib,
        nodes,
        pkgs,
        ...
      }:
      {
        garnix.database = {
          enable = true;
          allowedIPs = [
            "${nodes.db.networking.primaryIPAddress}/32"
            "${nodes.garnixServer.networking.primaryIPAddress}/32"
            "${nodes.garnixServer.networking.primaryIPv6Address}/128"
            "fe80::/10"
          ];
          exporter.enable = true;
        };
      };
  };

  testScript = { nodes, ... }: ''
    import json
    start_all()
    db.wait_for_unit("multi-user.target")
    db.wait_for_unit("postgresql.service")
    db.wait_for_unit("prometheus-sql-exporter.service")
    db.wait_for_unit("nginx.service")

    garnixServer.wait_for_unit("multi-user.target")
    garnixServer.wait_for_unit("garnixServer.service", timeout=60)

    with subtest("nginx is running"):
      user = garnixServer.succeed("curl --fail http://app.garnix.io/api/whoami")
      assert user == "null"
      health = garnixServer.succeed("curl --fail http://app.garnix.io/api/health/check")
      assert health == "[]"

    with subtest("the metrics endpoint is protected and available"):
      garnixServer.wait_until_succeeds("nc localhost 8323 -z")
      metrics = garnixServer.succeed("curl --fail -iv http://127.0.0.1:8323/", timeout=30)

    with subtest("login and list commits"):
      garnixServer.wait_until_succeeds("http --check-status --session=./session.json http://app.garnix.io/api/dev/log-me-in")
      zzz = garnixServer.succeed("cat session.json")
      jwtToken = garnixServer.succeed("cat session.json | jq -r '.cookies.[] | select(.name==\"JWT-Cookie\") | .value'").strip()
      builds = garnixServer.succeed(f"http --check-status -A bearer -a {jwtToken} 127.0.0.1:8321/api/commits")
      assert json.loads(builds) == {"commits":[]}
  '';
}
