{ config
, lib
, pkgs
, flakePackages
, ...
}:
let
  cfg = config.garnix.hosting-gateway;
  basicAuthFile = "/run/traefik/htpasswd-monitoring";

  traefikPort = 8080;
  onDemandResolverPort = 8081;

  # Upstream assumes monitoring is always on and always uses basic auth. Here
  # both are optional, so the node-exporter route and its auth are added only
  # when they are actually configured.
  monitoringClient = config.garnix.monitoring-client;
  monitoringAuth = config.garnix.monitoring.basicAuth;
  routeNodeExporter = monitoringClient.enable;
  useBasicAuth = routeNodeExporter && monitoringAuth.passwordFile != null;
in
{
  options.garnix.hosting-gateway = {
    enable = lib.mkEnableOption "the garnix hosting gateway";

    serverMappingEndpoint = lib.mkOption {
      type = lib.types.str;
      description = ''
        URL of the backend's `GET /api/hosts/traefik` endpoint, which returns
        the current routing table.
      '';
    };

    pollInterval = lib.mkOption {
      type = lib.types.int;
      default = 5;
      description = "How often, in seconds, to re-read `serverMappingEndpoint`.";
    };

    extraCaddyAcmeConfig = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = { };
      description = ''
        Extra options merged into each ACME issuer in `services.caddy.settings`.
        Use this to point Caddy at a staging CA, or to configure a DNS-01
        challenge.
      '';
    };

    hostingDomain = lib.mkOption {
      type = lib.types.str;
      description = ''
        Base domain deployed servers live under, matching the backend's
        `GARNIX_HOSTING_DOMAIN`. The resolver uses it to tell a name it already
        serves from a customer's own domain that needs a CNAME lookup.
      '';
    };

    garnixOrigin = lib.mkOption {
      type = lib.types.str;
      default = "https://garnix.io";
      description = ''
        Origin of the garnix backend the on-demand resolver asks which domains
        are currently routable.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ 80 443 ];

    # Traefik forwards to the node exporter itself (below), so the separate
    # nginx vhost the monitoring client would otherwise set up would fight it
    # for port 443.
    garnix.monitoring-client.nginx.enable = lib.mkIf routeNodeExporter false;

    # Caddy asks this service whether a domain it has been asked for a
    # certificate for is one we actually serve. See
    # https://caddyserver.com/docs/json/apps/tls/automation/on_demand/permission/http/
    systemd.services.caddyOnDemandResolver = {
      description = "Answers Caddy's on-demand TLS certificate questions";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = lib.getExe flakePackages."hosting-gateway/onDemandResolver";
      };
      environment.PORT = toString onDemandResolverPort;
      environment.GARNIX_ORIGIN = cfg.garnixOrigin;
      environment.GARNIX_HOSTING_DOMAIN = cfg.hostingDomain;
    };

    services.caddy = {
      enable = true;
      settings.apps = {
        tls.automation = {
          policies = [
            # The monitoring host's own name is known up front, so it gets a
            # certificate without going through the on-demand path.
            (lib.mkIf routeNodeExporter {
              subjects = [ monitoringClient.fqdn ];
              issuers = [
                ({
                  module = "acme";
                  email = config.security.acme.defaults.email;
                } // cfg.extraCaddyAcmeConfig)
              ];
              storage = {
                module = "file_system";
                root = "/var/lib/caddy";
              };
              on_demand = false;
            })
            # Everything else is a deployed server, whose name is not known
            # until it is deployed.
            {
              issuers = [
                ({
                  module = "acme";
                  email = config.security.acme.defaults.email;
                } // cfg.extraCaddyAcmeConfig)
              ];
              storage = {
                module = "file_system";
                root = "/var/lib/caddy";
              };
              on_demand = true;
            }
          ];
          on_demand.permission = {
            module = "http";
            endpoint = "http://localhost:${toString onDemandResolverPort}/";
          };
        };

        # Caddy terminates TLS and hands everything to Traefik, which owns the
        # actual routing.
        http.servers."tlstermination" = {
          listen = [ ":443" ];
          routes = [{
            handle = [{
              handler = "reverse_proxy";
              upstreams = [{ dial = "localhost:${toString traefikPort}"; }];
            }];
          }];
        };
      };
    };

    services.traefik = {
      enable = true;
      staticConfigOptions = {
        entryPoints.http = {
          address = ":${toString traefikPort}";
          # Caddy is the only thing that can reach this port, so its
          # X-Forwarded-* headers are the trustworthy ones.
          forwardedHeaders.insecure = true;
        };
        providers.http = {
          endpoint = cfg.serverMappingEndpoint;
          pollInterval = "${toString cfg.pollInterval}s";
        };
        # Declared custom domains are CNAMEs pointing at a garnix name, so the
        # router has to follow one hop to match them.
        hostResolver = {
          cnameFlattening = true;
          resolvDepth = 2;
        };
        experimental.localPlugins.heartbeatmiddleware = {
          moduleName = "github.com/garnix-io/garnix/heartbeatmiddleware";
        };
      };

      dynamicConfigOptions = lib.mkIf routeNodeExporter {
        http = {
          middlewares = lib.mkIf useBasicAuth {
            node-exporter-basic-auth.basicAuth.usersFile = basicAuthFile;
          };
          routers.node-exporter-router = {
            rule = "Host(`${monitoringClient.fqdn}`)";
            service = "node-exporter";
            middlewares = lib.optional useBasicAuth "node-exporter-basic-auth";
          };
          services.node-exporter.loadBalancer.servers = [{
            url = "http://localhost:${toString config.services.prometheus.exporters.node.port}";
          }];
        };
      };
    };

    systemd.services.traefik = {
      serviceConfig = {
        RuntimeDirectoryMode = "0750";
        # Traefik only loads plugins from its own state directory, so the
        # middleware source has to be copied in before it starts.
        ExecStartPre = [
          (lib.getExe (pkgs.writeShellApplication {
            name = "init-traefik-plugins";
            runtimeInputs = [ pkgs.coreutils ];
            text = ''
              PLUGINS_DIR=/var/lib/traefik/plugins-local/src/github.com/garnix-io/garnix
              mkdir -p "$PLUGINS_DIR"
              rm -rf "$PLUGINS_DIR/heartbeatmiddleware"
              cp -r ${./heartbeatmiddleware} "$PLUGINS_DIR/heartbeatmiddleware"
              chmod -R u+w "$PLUGINS_DIR/heartbeatmiddleware"
            '';
          }))
        ];
        LoadCredential =
          lib.optional useBasicAuth "basicAuthPassword:${monitoringAuth.passwordFile}";
      };

      preStart = lib.mkIf useBasicAuth ''
        ${pkgs.apacheHttpd}/bin/htpasswd -icm ${basicAuthFile} ${monitoringAuth.username} \
          < "$CREDENTIALS_DIRECTORY/basicAuthPassword"
      '';
    };
  };
}
