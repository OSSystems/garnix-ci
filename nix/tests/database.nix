{ ... }:
{
  name = "test database module";

  nodes = {
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
            "${nodes.db.networking.primaryIPv6Address}/32"
            "fe80::/10"
          ];
          exporter = {
            enable = true;
          };
        };

        networking.extraHosts = ''
          ${nodes.db.networking.primaryIPAddress} ${nodes.db.garnix.database.fqdn}
        '';

        services.prometheus.exporters.sql = {
          configuration.jobs.counts.queries = {
            garnix_users = {
              labels = [ "garnix_users" ];
              help = "Total users";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM pg_catalog.pg_user WHERE usename = 'garnix'";
            };
          };
        };
      };
  };

  testScript = { nodes, ... }: ''
    start_all()
    db.wait_for_unit("multi-user.target")
    db.wait_for_unit("postgresql.service")
    db.wait_for_unit("prometheus-sql-exporter.service")
    db.wait_for_unit("nginx.service")

    with subtest("Test sql exporter"):
      garnixUserCount = db.wait_until_succeeds("curl --fail http://127.0.0.1:${toString nodes.db.services.prometheus.exporters.sql.port}/metrics | grep '^sql_garnix_users' | awk '{print $2}'", 10).strip()
      assert garnixUserCount == '1', "garnix database user not found"

    with subtest("Connect on database with psql using garnix user on garnix database through TCP/IP"):
      db.wait_until_succeeds("PGPASSWORD=$(cat ${nodes.db.sops.secrets.database-password.path}) psql -U ${nodes.db.garnix.database.dbUser} -d ${nodes.db.garnix.database.dbName} -p ${toString nodes.db.garnix.database.dbPort} -h ${nodes.db.networking.primaryIPAddress} -w -c 'SELECT 1'", 5)

    with subtest("Check firewall rules"):
      assert db.succeed("iptables -L -n | grep ${nodes.db.networking.primaryIPAddress} | grep 'ACCEPT' | grep 'tcp dpt:${toString nodes.db.garnix.database.dbPort}'"), "iptables rule not found"
      assert db.succeed("ip6tables -L -n | grep fe80 | grep 'ACCEPT' | grep 'tcp dpt:${toString nodes.db.garnix.database.dbPort}'"), "iptables rule not found"

    with subtest("Check that the garnix user can connect locally using the unix socket"):
      db.wait_until_succeeds("sudo -u garnix psql -p ${toString nodes.db.garnix.database.dbPort} -w -c 'SELECT 1'", 5)
  '';
}
