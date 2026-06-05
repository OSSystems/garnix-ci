{ lib, config, ... }:
let
  monitoredHosts = {
    garnix5 = { fqdn = "prometheus-node-exporter.5.garnix.io"; };
    garnix6 = { fqdn = "prometheus-node-exporter.6.garnix.io"; };
    garnix7 = { fqdn = "prometheus-node-exporter.7.garnix.io"; };
    garnix8 = { fqdn = "prometheus-node-exporter.8.garnix.io"; };
    garnix9 = { fqdn = "prometheus-node-exporter.9.garnix.io"; };
    arm-server-0 = { };
    arm-1 = { };
    opensearch1 = { };
    opensearch2 = { };
    opensearch3 = { };
    monitoring = { };
    db1 = { };
    garnix-server1 = { scrapeNginx = true; scrapeNginxLog = true; scrapeGarnixServer = true; };
    garnix-server2 = { scrapeNginx = true; scrapeNginxLog = true; scrapeGarnixServer = true; };
    action-runner2 = { };
    macMini1 = {
      fqdn = "macMini1.garnix.io";
      proxied = false;
      port = 9100;
    };
    macMini2 = {
      fqdn = "macMini2.garnix.io";
      proxied = false;
      port = 9100;
    };
  };

  monitoredHostsType = lib.types.submodule ({ name, config, ... }: {
    options = {
      fqdn = lib.mkOption {
        type = lib.types.str;
        default = "prometheus-node-exporter.${name}.garnix.io";
        description = "The FQDN to reach the monitoring service";
      };
      proxied = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether the prometheus node exporter is proxied by nginx (and needs https and basic auth)";
      };
      port = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
        description = "The FQDN to reach the monitoring service";
      };
      scrapeNginx = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether to scrape the nginx exporter";
      };
      scrapeNginxLog = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether to scrape the nginx log exporter";
      };
      scrapeGarnixServer = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether to scrape the garnix server exporter";
      };
    };
  });
in
{
  options.garnix.monitoring = {
    monitoredHosts = lib.mkOption {
      type = lib.types.attrsOf monitoredHostsType;
      readOnly = ! config.garnix.devMode.enable;
      default = monitoredHosts;
    };
  };
}
