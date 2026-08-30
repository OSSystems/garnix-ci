{ lib, config, ... }:
let
  cfg = config.garnix.monitoring;

  monitoredHostType = lib.types.submodule ({ name, ... }: {
    options = {
      fqdn = lib.mkOption {
        type = lib.types.str;
        default =
          if cfg.domain == null
          then name
          else "prometheus-node-exporter.${name}.${cfg.domain}";
        description = "The FQDN to reach the monitoring service";
      };
      proxied = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          Whether the exporters on this host are proxied by that host's nginx,
          and so need https, basic auth, and the per-exporter metrics paths.
          When false, the exporters are scraped directly over http.
        '';
      };
      port = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
        description = ''
          Port the node exporter is reachable on. When null, the node
          exporter's own port is used.
        '';
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
    domain = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "example.com";
      description = ''
        Domain the default fqdn of each monitored host is derived from. When
        null, a host's attribute name is used as its fqdn verbatim, which is
        what a single-machine deployment scraping over loopback wants.
      '';
    };

    monitoredHosts = lib.mkOption {
      type = lib.types.attrsOf monitoredHostType;
      default = { };
      description = "The hosts the monitoring server scrapes.";
    };

    basicAuth = {
      username = lib.mkOption {
        type = lib.types.str;
        default = "prometheus";
        description = "Basic auth user the server presents and the clients accept.";
      };
      passwordFile = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = ''
          File holding the basic auth password shared by the monitoring server
          and its clients. When null, neither side sets up basic auth.
        '';
      };
    };
  };
}
