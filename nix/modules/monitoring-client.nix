{ config
, options
, lib
, pkgs
, ...
}:

let
  cfg = config.garnix.monitoring-client;
  monitoring = config.garnix.monitoring;
  basicAuthEnabled = monitoring.basicAuth.passwordFile != null;

  node = monitoring.monitoredHosts.${cfg.nodeId} or {
    fqdn = cfg.nodeId;
    scrapeNginx = false;
    scrapeNginxLog = false;
    scrapeGarnixServer = false;
  };

  protected = attrs: attrs // lib.optionalAttrs basicAuthEnabled {
    inherit (cfg.nginx) basicAuthFile;
  };
in

{
  imports = [ ./monitoring.nix ];

  options.garnix.monitoring-client = {
    enable = lib.mkEnableOption "garnix client monitoring";

    nodeId = lib.mkOption {
      type = lib.types.str;
      default = config.networking.hostName;
      defaultText = lib.literalExpression "config.networking.hostName";
      description = "This host's key in `garnix.monitoring.monitoredHosts`.";
    };

    nginx = {
      enable = lib.mkOption {
        type = lib.types.bool;
        description = "Whether to also setup nginx to forward to the node exporter";
        default = true;
      };

      basicAuthFile = lib.mkOption {
        type = lib.types.str;
        default = "/run/nginx/htpasswd-monitoring";
        description = "The file to store the basic auth credentials";
      };
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = cfg.nginx.enable;
      defaultText = lib.literalExpression "config.garnix.monitoring-client.nginx.enable";
      description = "Whether to open ports 80 and 443.";
    };

    fqdn = lib.mkOption {
      type = lib.types.str;
      default = if config.garnix.devMode.enable then "test" else node.fqdn;
      defaultText = lib.literalExpression "the fqdn of this host in garnix.monitoring.monitoredHosts";
      description = "The FQDN for this host's prometheus instance";
    };
  };

  config = lib.mkIf cfg.enable (
    if (builtins.hasAttr "launchd" options) then
      {
        services.prometheus.exporters.node.enable = true;
      }
    else
      lib.mkMerge [
        {
          assertions = [
            {
              assertion = monitoring.monitoredHosts ? ${cfg.nodeId};
              message = ''
                garnix.monitoring-client.nodeId is "${cfg.nodeId}", which is not a
                key of garnix.monitoring.monitoredHosts. Add it there, or set
                nodeId to one of: ${lib.concatStringsSep ", " (lib.attrNames monitoring.monitoredHosts)}
              '';
            }
          ];

          networking.firewall.allowedTCPPorts = lib.optionals cfg.openFirewall [ 80 443 ];

          services.prometheus.exporters = {
            node = {
              enable = true;
              enabledCollectors = [ "systemd" "processes" ];
            };
            nginx.enable = node.scrapeNginx;
            nginxlog = {
              enable = node.scrapeNginxLog;
              group = "nginx";
              settings.namespaces = [
                {
                  name = "nginx";
                  source.files = [ "/var/log/nginx/json_access.log" ];
                  parser = "json";
                }
              ];
            };
          };
        }
        (lib.mkIf cfg.nginx.enable {
          security.acme = {
            acceptTerms = true;
            certs.${cfg.fqdn} = { };
          };

          services.nginx = {
            enable = true;
            recommendedProxySettings = true;
            recommendedOptimisation = true;
            serverNamesHashBucketSize = 128;
            proxyTimeout = "600s";
            virtualHosts.${cfg.fqdn} = config.garnix.devMode.withDevCerts {
              forceSSL = true;
              enableACME = true;
              locations = {
                "/" = protected {
                  proxyPass = "http://[::1]:${toString config.services.prometheus.exporters.node.port}";
                };
                "/nginx" = lib.mkIf node.scrapeNginx (protected {
                  proxyPass = "http://[::1]:${toString config.services.prometheus.exporters.nginx.port}/metrics";
                });
                "/nginxlog" = lib.mkIf node.scrapeNginxLog (protected {
                  proxyPass = "http://[::1]:${toString config.services.prometheus.exporters.nginxlog.port}/metrics";
                });
                "/server-metrics" = lib.mkIf node.scrapeGarnixServer (protected {
                  proxyPass = "http://127.0.0.1:${toString config.services.garnixServer.metricsPort}/";
                });
              };
            };
          };
        })
        (lib.mkIf (cfg.nginx.enable && basicAuthEnabled) {
          systemd.services.nginx = {
            serviceConfig.LoadCredential = [
              "basicAuthPassword:${monitoring.basicAuth.passwordFile}"
            ];

            preStart = ''
              ${pkgs.apacheHttpd}/bin/htpasswd -icm ${cfg.nginx.basicAuthFile} ${monitoring.basicAuth.username} < "$CREDENTIALS_DIRECTORY/basicAuthPassword"
            '';
          };
        })
      ]
  );
}
