{ config
, options
, lib
, pkgs
, ...
}:

let
  cfg = config.garnix.monitoring-server;
  monitoring = config.garnix.monitoring;
  exporters = config.services.prometheus.exporters;

  prometheus-basic-auth = "/run/prometheus/prometheus-basic-auth";
  basicAuthEnabled = monitoring.basicAuth.passwordFile != null;

  basicAuth = lib.optionalAttrs basicAuthEnabled {
    basic_auth = {
      inherit (monitoring.basicAuth) username;
      password_file = prometheus-basic-auth;
    };
  };

  grafanaUrl = "http://${cfg.listenAddress}:${toString cfg.grafana.port}";

  jobs = [
    {
      name = "node";
      hosts = monitoring.monitoredHosts;
      port = host: if host.port != null then host.port else exporters.node.port;
    }
    {
      name = "nginx";
      hosts = lib.filterAttrs (_: h: h.scrapeNginx) monitoring.monitoredHosts;
      proxiedPath = "/nginx";
      port = _: exporters.nginx.port;
    }
    {
      name = "nginxlog";
      hosts = lib.filterAttrs (_: h: h.scrapeNginxLog) monitoring.monitoredHosts;
      proxiedPath = "/nginxlog";
      port = _: exporters.nginxlog.port;
    }
    {
      name = "server-metrics";
      hosts = lib.filterAttrs (_: h: h.scrapeGarnixServer) monitoring.monitoredHosts;
      proxiedPath = "/server-metrics";
      directPath = "/";
      port = _: config.services.garnixServer.metricsPort;
    }
  ];

  proxiedJob = job: hosts: {
    job_name = job.name;
    scheme = "https";
    static_configs = [{
      targets = lib.mapAttrsToList
        (_: h: h.fqdn + lib.optionalString (h.port != null) ":${toString h.port}")
        hosts;
    }];
  } // basicAuth // lib.optionalAttrs (job ? proxiedPath) { metrics_path = job.proxiedPath; };

  directJob = job: hosts: suffix: {
    job_name = job.name + suffix;
    scheme = "http";
    static_configs = [{
      targets = lib.mapAttrsToList (_: h: "${h.fqdn}:${toString (job.port h)}") hosts;
    }];
  } // lib.optionalAttrs (job ? directPath) { metrics_path = job.directPath; };

  scrapeConfigsFor = job:
    let
      proxied = lib.filterAttrs (_: h: h.proxied) job.hosts;
      direct = lib.filterAttrs (_: h: ! h.proxied) job.hosts;
    in
    lib.optional (proxied != { }) (proxiedJob job proxied)
    ++ lib.optional (direct != { })
      (directJob job direct (lib.optionalString (proxied != { }) "_unproxied"));

  sqlJob = {
    job_name = "sql";
    scheme = "https";
    static_configs = [{ targets = [ cfg.sqlExporter.target ]; }];
  } // basicAuth;
in
{
  imports = [ ./monitoring.nix ];

  options.garnix.monitoring-server = {
    enable = lib.mkEnableOption "garnix monitoring server";

    fqdn = lib.mkOption {
      type = lib.types.str;
      example = "monitoring.example.com";
      description = "The FQDN Grafana is served under.";
    };

    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "Address Prometheus and Grafana bind to.";
    };

    prometheus.port = lib.mkOption {
      type = lib.types.port;
      default = 2433;
    };

    grafana = {
      port = lib.mkOption {
        type = lib.types.port;
        default = 2432;
      };

      rootUrl = lib.mkOption {
        type = lib.types.str;
        default = "https://${cfg.fqdn}";
        defaultText = lib.literalExpression ''"https://''${config.garnix.monitoring-server.fqdn}"'';
        description = "Public URL Grafana generates links against.";
      };

      secretKeyFile = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = ''
          File holding Grafana's secret_key. When null, Grafana's built-in
          default is used, which is fine only for a throwaway instance.
        '';
      };
    };

    nginx = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          Whether to serve Grafana through a local nginx vhost. Set to false to
          put Grafana behind an ingress you already run, which then proxies to
          listenAddress:grafana.port.
        '';
      };

      acme.enable = lib.mkOption {
        type = lib.types.bool;
        default = cfg.nginx.enable;
        defaultText = lib.literalExpression "config.garnix.monitoring-server.nginx.enable";
        description = "Whether to request an ACME certificate for the Grafana vhost.";
      };
    };

    watchdog.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Whether to run the watchdog daemon alongside the monitoring server.
        Has no effect unless the watchdog module is in the module closure.
      '';
    };

    sqlExporter = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether to scrape a prometheus-sql-exporter.";
      };

      target = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = config.garnix.database.exporter.fqdn or null;
        defaultText = lib.literalExpression "config.garnix.database.exporter.fqdn";
        description = "Target of the sql exporter job.";
      };
    };

    extraScrapeConfigs = lib.mkOption {
      type = lib.types.listOf (lib.types.attrsOf lib.types.anything);
      default = [ ];
      description = "Scrape configs appended to the derived ones.";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      assertions = [
        {
          assertion = cfg.grafana.secretKeyFile != null;
          message = ''
            garnix.monitoring-server.grafana.secretKeyFile must be set: grafana
            no longer ships a default secret_key.
          '';
        }
        {
          assertion = cfg.sqlExporter.enable -> cfg.sqlExporter.target != null;
          message = ''
            garnix.monitoring-server.sqlExporter.target must be set when
            sqlExporter.enable = true and the garnix database module is not
            part of this configuration.
          '';
        }
      ];

      services.grafana = {
        enable = true;
        settings = {
          date_formats.default_timezone = "utc";
          server = {
            http_addr = cfg.listenAddress;
            http_port = cfg.grafana.port;
            domain = cfg.fqdn;
            root_url = cfg.grafana.rootUrl;
          };
        } // lib.optionalAttrs (cfg.grafana.secretKeyFile != null) {
          security.secret_key = "$__file{${cfg.grafana.secretKeyFile}}";
        };
        provision = {
          enable = true;

          dashboards.settings.providers = [
            {
              name = "garnixServer";
              options.path = pkgs.linkFarm "garnix-grafana-dashboards" [
                {
                  name = "node-exporter-full.json";
                  path = ../data/grafana-node-exporter-full.json;
                }
              ];
            }
          ];

          datasources.settings.datasources = [
            {
              name = "Prometheus";
              type = "prometheus";
              url = "http://${cfg.listenAddress}:${toString config.services.prometheus.port}";
              jsonData = {
                timeInterval = config.services.prometheus.globalConfig.scrape_interval;
              };
            }
          ];
        };
      };

      services.prometheus = {
        enable = true;
        inherit (cfg.prometheus) port;
        inherit (cfg) listenAddress;
        globalConfig = {
          scrape_interval = "30s";
          scrape_timeout = "10s";
        };
        retentionTime = "90d";
        scrapeConfigs =
          lib.concatMap scrapeConfigsFor jobs
          ++ lib.optional cfg.sqlExporter.enable sqlJob
          ++ cfg.extraScrapeConfigs;
      };
    }

    (lib.optionalAttrs (options.garnix ? watchdog) {
      garnix.watchdog.enable = cfg.watchdog.enable;
    })

    (lib.mkIf basicAuthEnabled {
      systemd.services.prometheus = {
        serviceConfig = {
          PrivateTmp = true;
          LoadCredential = [
            "basicAuthPassword:${monitoring.basicAuth.passwordFile}"
          ];
        };
        unitConfig.RequiresMountsFor = [ config.systemd.services.prometheus.serviceConfig.WorkingDirectory ];
        preStart = ''
          cp "$CREDENTIALS_DIRECTORY/basicAuthPassword" ${prometheus-basic-auth}
        '';
      };
    })

    (lib.mkIf cfg.nginx.enable {
      services.nginx = {
        enable = true;
        recommendedProxySettings = true;
        recommendedOptimisation = true;
        proxyTimeout = "600s";
        virtualHosts.${cfg.fqdn} = config.garnix.devMode.withDevCerts {
          addSSL = true;
          enableACME = cfg.nginx.acme.enable;
          locations."/".proxyPass = grafanaUrl;
          locations."/api/live" = {
            proxyPass = grafanaUrl;
            proxyWebsockets = true;
          };
        };
      };
    })

    (lib.mkIf cfg.nginx.acme.enable {
      security.acme.certs.${cfg.fqdn} = { };
    })
  ]);
}
