{ config
, lib
, pkgs
, ...
}:

let
  cfg = config.garnix.opensearch;
  basicAuthFile = "/var/lib/nginx/htpasswd";
in

{
  options.garnix = {
    opensearch = {
      enable = lib.mkEnableOption "Enable OpenSearch service";

      heapSize = lib.mkOption {
        type = lib.types.int;
        default = 1024;
        description = "The heap size in MB for the OpenSearch JVM";
      };

      dashboards = {
        enable = lib.mkEnableOption "Enable OpenSearch dashboards service";
        package = lib.mkPackageOption pkgs "opensearch-dashboards" { };
      };

      fqdn = lib.mkOption {
        type = lib.types.str;
        description = "The FQDN to reach the opensearch service";
      };

      basicAuths = lib.mkOption {
        description =
          "List of authentication credentials for the OpenSearch service";
        type = lib.types.listOf (lib.types.submodule {
          options = {
            username = lib.mkOption { type = lib.types.str; };
            passwordFile = lib.mkOption { type = lib.types.str; };
          };
        });
        default = [
          {
            username = "garnix";
            passwordFile = config.sops.secrets.opensearch-garnix.path;
          }
        ];
      };

      isSingleNode = lib.mkOption {
        type = lib.types.bool;
        description = "Is this a single node cluster";
        default = false;
      };

      bindIP = lib.mkOption {
        type = lib.types.str;
        description = "The IP address to use for internal and external communication";
      };

      nodesIPs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "The list of IP addresses of the nodes in the cluster";
        default = [ ];
      };

      initialClusterManagerNodes = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "The list of IP addresses of the nodes in the cluster that should be cluster managers";
        default = [ ];
      };

      lockMemory = lib.mkOption {
        type = lib.types.bool;
        description = "Lock the memory of the OpenSearch process";
        default = true;
      };

      roles = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "The list of node roles";
        default = [ ];
      };
    };
  };

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ 80 443 ];

    sops.secrets = {
      opensearch-garnix = { };
    };

    systemd.tmpfiles.rules = [
      "d /opensearch/node 0700 opensearch - - -"
      "d /opensearch/dashboards 0700 opensearch - - -"
      "d /opensearch/snapshots 0700 opensearch - - -"
    ];

    users = {
      groups.opensearch.gid = 3000;
      users.opensearch = {
        uid = 3000;
        home = "/opensearch/node";
        group = "opensearch";
        isSystemUser = true;
      };
    };

    services = {
      opensearch = {
        enable = true;
        dataDir = "/opensearch/node";
        package = pkgs.opensearch.overrideAttrs (finalAttrs: prevAttrs: {
          version = "2.12.0";
          src = pkgs.fetchurl {
            url = "https://artifacts.opensearch.org/releases/bundle/opensearch/${finalAttrs.version}/opensearch-${finalAttrs.version}-linux-x64.tar.gz";
            hash = "sha256-t9s633qDzxvG1x+VVATpczzvD+ojnfTiwB/EambMKtA=";
          };
          # nixpkgs' installPhase targets OpenSearch 3.x, which ships a top-level
          # `agent` directory. The 2.12.0 tarball has no such directory, so the
          # `cp -R ... agent $out` fails. Provide an empty one to keep the copy
          # happy; nothing in 2.12.0 reads it.
          preInstall = (prevAttrs.preInstall or "") + ''
            mkdir -p agent
          '';
        });

        settings = {
          "network.host" = "[::1]";
          "bootstrap.memory_lock" = cfg.lockMemory;
          "cluster.name" = "garnix";
          "node.name" = config.networking.hostName;
          "cluster.max_shards_per_node" = 2000;
          "path.repo" = [ "/opensearch/snapshots" ];
        } // lib.optionalAttrs (cfg.initialClusterManagerNodes != [ ] && !cfg.isSingleNode) {
          "cluster.initial_cluster_manager_nodes" = cfg.initialClusterManagerNodes;
        } // lib.optionalAttrs (cfg.isSingleNode) {
          "discovery.type" = "single-node";
        } // lib.optionalAttrs (!cfg.isSingleNode) {
          "discovery.type" = "";
          "network.bind_host" = [ cfg.bindIP "::1" ];
          "network.publish_host" = cfg.bindIP;
          "discovery.seed_hosts" = cfg.nodesIPs;
        } // lib.optionalAttrs (cfg.roles != [ ]) {
          "node.roles" = cfg.roles;
        };

        extraJavaOptions = [
          # Xms and Xmx are already defined as cmdline args by config/jvm.options.
          # Appending the next two lines overrides the former.
          "-Xms${toString cfg.heapSize}m"
          "-Xmx${toString cfg.heapSize}m"
        ];
        extraCmdLineOptions = [
          "-Eplugins.security.disabled=true"
        ];
      };

      nginx = {
        enable = true;
        recommendedProxySettings = true;
        recommendedOptimisation = true;
        # This is needed for long domain names
        serverNamesHashBucketSize = 128;
        proxyTimeout = "600s";
        virtualHosts."opensearch.garnix.io" = config.garnix.devMode.withDevCerts {
          forceSSL = !config.garnix.devMode.enable;
          enableACME = !config.garnix.devMode.enable;
          locations."/" = {
            inherit basicAuthFile;
            proxyPass = "http://[::1]:9200";
          };
          locations."/dashboards" = {
            inherit basicAuthFile;
            proxyPass = "http://[::1]:5601";
          };
        };
      };
    };

    systemd.services = {
      opensearch = {
        serviceConfig = {
          LimitMEMLOCK = "infinity";
          LimitMEMLOCKSoft = "infinity";
        };
      };
      opensearch-dashboards =
        let
          opensearchDashboardConfig = (pkgs.formats.json { }).generate "opensearch-dashboards.json" (
            (lib.filterAttrsRecursive (_: value: value != null && value != [ ]) ({
              server = {
                host = "::";
                port = 5601;
                basePath = "/dashboards";
                rewriteBasePath = true;
              };

              opensearchDashboards = {
                index = ".opensearch_dashboard";
                defaultAppId = "discover";
              };

              opensearch = {
                hosts = [ "http://[::1]:9200" ];
                ssl.verificationMode = "none";
                requestHeadersWhitelist = [ "authorization" "securitytenant" ];
              };
            }))
          );
        in
        lib.mkIf cfg.dashboards.enable {
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" "opensearch.service" ];
          description = "OpenSearch Dashboards";
          serviceConfig = {
            DynamicUser = false;
            StateDirectory = "opensearch-dashboards";
            User = "opensearch";
            Group = "opensearch";
            Environment = [
              "DISABLE_SECURITY_DASHBOARDS_PLUGIN=true"
            ];
          };
          script = ''
            ${lib.getExe cfg.dashboards.package} \
              --config ${opensearchDashboardConfig} \
              --path.data "/opensearch/dashboards" \
          '';
        };

      nginx = {
        serviceConfig = {
          StateDirectory = "nginx";
          LoadCredential = builtins.map
            (auth: "opensearch_${auth.username}_password:${auth.passwordFile}")
            cfg.basicAuths;
        };

        # Create the htpasswd file for basic auths from the password files that's stored in SOPS.
        # The nginx service runs with PrivateTmp set to true, so this file will only
        # be accessible to nginx.
        preStart = ''
          : > ${basicAuthFile}
          ${lib.concatLines (builtins.map (auth: "${pkgs.apacheHttpd}/bin/htpasswd -im ${basicAuthFile} ${auth.username} < $CREDENTIALS_DIRECTORY/opensearch_${auth.username}_password") cfg.basicAuths)}
        '';
      };
    };

    networking.firewall.extraCommands = lib.concatLines (map (ip: "iptables -I INPUT -p tcp -s ${ip} -j ACCEPT") cfg.nodesIPs);

    virtualisation.vmVariant = {
      networking.extraHosts = "127.0.0.1 ${cfg.fqdn}";
      virtualisation = {
        cores = lib.mkForce 4;
        memorySize = lib.mkForce (8 * 1024);
        fileSystems."/nix/.rw-store".options = lib.mkForce [ "size=16G" ];
      };
    };
  };
}
