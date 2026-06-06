{ config
, lib
, pkgs
, flakePackages
, flakeInputs
, ...
}:
let
  buildLogsFluentBitPort = 8888;

  logsDir = pkgs.writeShellScriptBin "logsDir" ''
    if [ -d /var/lib/garnix/logs ]; then
      echo "Logs dir exists. Not creating"
    else
      mkdir /var/lib/garnix/logs
    fi
  '';

  migrateScript = with pkgs;
    writeShellScriptBin "migrate" ''
      set -euo pipefail

      export SQITCH_PASSWORD=$(cat /run/secrets/database-password)
      ${lib.getBin flakePackages."backend_migrate"}/bin/sqitch deploy \
          "db:pg://${config.garnix.database.dbUser}:$(cat /run/secrets/database-password)@${config.garnix.database.fqdn}:${toString config.garnix.database.dbPort}/${config.garnix.database.dbName}?sslmode=${config.garnix.database.ssl.mode}&sslrootcert=${config.garnix.database.ssl.rootCert}"
    '';
  dbCheckIsReady = with pkgs;
    writeShellScriptBin "dbCheckIsReady" ''
      #!/usr/bin/env bash
      set -uo pipefail

      max_retries=10
      retry_interval=2
      retries=0
      start_time=$(date +%s)

      export PGPASSWORD=$(cat /run/secrets/database-password)
      echo $PGHOST

      while true; do
        end_time=$(date +%s)
        ${flakePackages."backend_postgres"}/bin/psql -c "SELECT 1;" >/dev/null
        #shellcheck disable=SC2181
        if [ $? -eq 0 ]; then
          elapsed_time=$((end_time - start_time))
          echo "Postgresql is ready to handle queries after $elapsed_time seconds."
          break
        fi

        retries=$((retries + 1))

        if [ $retries -ge $max_retries ]; then
          echo "Maximum retries reached. Readiness condition not met."
          exit 1
        fi

        echo "Readiness condition not met. Retrying in $retry_interval seconds..."
        sleep $retry_interval
      done
    '';
in
{
  options = {
    services = {
      garnixServer = {
        enable = lib.mkEnableOption "garnix server";
        url = lib.mkOption {
          type = lib.types.str;
          default = "https://app.garnix.io";
        };
        githubAppName = lib.mkOption {
          type = lib.types.str;
          default = "garnix-ci";
        };
        adminGithubLogin = lib.mkOption {
          type = lib.types.str;
          description = ''
            GitHub login of the self-host operator. The user that signs up
            with this exact login receives subscription_type = 'admin' and
            gains full administrative powers. Compared case-sensitively
            against the login GitHub returns at OAuth callback.
          '';
          example = "alice";
        };
        port = lib.mkOption {
          type = lib.types.int;
          default = 8321;
        };
        monitoringPort = lib.mkOption {
          type = lib.types.int;
          default = 8322;
        };
        metricsPort = lib.mkOption {
          type = lib.types.int;
          default = 8323;
        };
        opensearchUrl = lib.mkOption {
          type = lib.types.str;
          example = "https://<opensearch-ip>/_msearch";
        };
        testFeatures = lib.mkOption {
          type = lib.types.listOf (lib.types.enum [ "DevApi" "OpenSearchMocks" "CacheUploadMocks" ]);
          default = [ ];
        };
        s3Cache = {
          publicBucket = lib.mkOption {
            type = lib.types.str;
          };
          publicBaseUrl = lib.mkOption {
            type = lib.types.str;
          };
          privateBucket = lib.mkOption {
            type = lib.types.str;
          };
          host = lib.mkOption {
            type = lib.types.str;
          };
          region = lib.mkOption {
            type = lib.types.str;
            default = "auto";
          };
        };
      };

      frontend = {
        port = lib.mkOption {
          type = lib.types.int;
          default = 3000;
        };
      };
    };
  };

  config = lib.mkIf config.services.garnixServer.enable {
    assertions = [{
      assertion =
        !config.garnix.devMode.enable -> (config.services.garnixServer.testFeatures == [ ]);
      message = ''
        Test features cannot be enabled in production.
        If you want to enable test features, set garnix.devMode.enable to true.
      '';
    }];

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    environment.systemPackages = [ pkgs.postgresql_18 ];

    programs.ssh = {
      startAgent = true;
      extraConfig = ''
        AddKeysToAgent yes
        Host macMini1
           Hostname 142.132.141.88
           User nix-ssh
           IdentityFile /run/secrets/garnix_server_remote_builder_ssh
        Host macMini2
           Hostname 142.132.141.89
           User nix-ssh
           IdentityFile /run/secrets/garnix_server_remote_builder_ssh
        Host garnix5
           Hostname 65.108.28.108
           User nix-ssh
           IdentityFile /run/secrets/garnix_server_remote_builder_ssh
        Host garnix6
           Hostname 65.108.28.106
           User nix-ssh
           IdentityFile /run/secrets/garnix_server_remote_builder_ssh
        Host garnix7
           Hostname 65.108.28.107
           User nix-ssh
           IdentityFile /run/secrets/garnix_server_remote_builder_ssh
        Host garnix8
           Hostname 88.99.75.150
           User nix-ssh
           IdentityFile /run/secrets/garnix_server_remote_builder_ssh
        Host garnix9
           Hostname 157.90.140.190
           User nix-ssh
           IdentityFile /run/secrets/garnix_server_remote_builder_ssh
        Host arm-server-0
           Hostname 65.109.75.126
           User nix-ssh
           IdentityFile /run/secrets/garnix_server_remote_builder_ssh
        Host arm-server-1
           Hostname 91.107.205.127
           User nix-ssh
           IdentityFile /run/secrets/garnix_server_remote_builder_ssh
        Host cache.garnix.io
          Hostname 37.27.121.36
          User cache-uploader
      '';
    };

    nix = {
      settings = {
        cores = 4;
      };
      extraOptions = ''
        max-jobs = ${if config.garnix.devMode.enable then "auto" else "0"}
        keep-build-log = true
      '';
      buildMachines =
        let
          sshUser = "nix-ssh";
          sshKey = "/run/secrets/garnix_server_remote_builder_ssh";
          protocol = "ssh-ng";
        in
        [
          {
            hostName = "macMini1";
            inherit sshUser sshKey protocol;
            systems = [ "aarch64-darwin" "x86_64-darwin" ];
            maxJobs = 4;
            speedFactor = 1;
            supportedFeatures = [
              "big-parallel"
              "recursive-nix"
            ];
            mandatoryFeatures = [ ];
          }
          {
            hostName = "macMini2";
            inherit sshUser sshKey protocol;
            systems = [ "aarch64-darwin" "x86_64-darwin" ];
            maxJobs = 4;
            speedFactor = 1;
            supportedFeatures = [
              "big-parallel"
              "recursive-nix"
            ];
            mandatoryFeatures = [ ];
          }
          {
            hostName = "garnix5";
            inherit sshUser sshKey protocol;
            systems = [ "x86_64-linux" "i686-linux" ];
            maxJobs = 28;
            speedFactor = 4;
            supportedFeatures = [
              "nixos-test"
              "kvm"
              "big-parallel"
              "recursive-nix"
            ];
            mandatoryFeatures = [ ];
          }
          {
            hostName = "garnix6";
            inherit sshUser sshKey protocol;
            systems = [ "x86_64-linux" "i686-linux" ];
            maxJobs = 28;
            speedFactor = 4;
            supportedFeatures = [
              "nixos-test"
              "kvm"
              "big-parallel"
              "recursive-nix"
            ];
            mandatoryFeatures = [ ];
          }
          {
            hostName = "garnix7";
            inherit sshUser sshKey protocol;
            systems = [ "x86_64-linux" "i686-linux" ];
            maxJobs = 28;
            speedFactor = 4;
            supportedFeatures = [
              "nixos-test"
              "kvm"
              "big-parallel"
              "recursive-nix"
            ];
            mandatoryFeatures = [ ];
          }
          {
            hostName = "garnix8";
            inherit sshUser sshKey protocol;
            systems = [ "x86_64-linux" "i686-linux" ];
            maxJobs = 28;
            speedFactor = 4;
            supportedFeatures = [
              "nixos-test"
              "kvm"
              "big-parallel"
              "recursive-nix"
            ];
            mandatoryFeatures = [ ];
          }
          {
            hostName = "garnix9";
            inherit sshUser sshKey protocol;
            systems = [ "x86_64-linux" "i686-linux" ];
            maxJobs = 28;
            speedFactor = 4;
            supportedFeatures = [
              "nixos-test"
              "kvm"
              "big-parallel"
              "recursive-nix"
            ];
            mandatoryFeatures = [ ];
          }
          {
            hostName = "arm-server-0";
            inherit sshUser sshKey protocol;
            systems = [ "aarch64-linux" ];
            maxJobs = 60;
            speedFactor = 4;
            supportedFeatures = [
              "nixos-test"
              "kvm"
              "big-parallel"
              "recursive-nix"
            ];
            mandatoryFeatures = [ ];
          }
          {
            hostName = "arm-server-1";
            inherit sshUser sshKey protocol;
            systems = [ "aarch64-linux" ];
            maxJobs = 8;
            speedFactor = 1;
            supportedFeatures = [
              "nixos-test"
              "kvm"
              "big-parallel"
              "recursive-nix"
            ];
            mandatoryFeatures = [ ];
          }
        ];
      distributedBuilds = !config.garnix.devMode.enable;
      daemonIOSchedPriority = 4;
    };

    virtualisation.vmVariant = { lib, ... }: {
      services.garnixServer = {
        url = "https://testing.garnix.io";
        githubAppName = "test-app-jkarni";
        adminGithubLogin = "jkarni";
      };
    };

    garnix = {
      custom-gc = {
        enable = true;
        enableTimer = true;
        useNixHeuristicGc = false;
        upperLimitPercent = null;
      };
      fluent-bit.enableNginxLogParsing = true;
      fluent-bit.configuration.pipelines.build-logs =
        let
          tag = "build-logs";
        in
        {
          input = {
            Name = "http";
            Tag = tag;
            listen = "::1";
            port = buildLogsFluentBitPort;
          };
          output = {
            Name = "opensearch";
            Match = tag;
            Host = config.garnix.fluent-bit.opensearch.fqdn;
            Port = 443;
            Tls = "On";
            "Tls.verify" = if config.garnix.devMode.enable then "Off" else "On";
            HTTP_User = config.garnix.fluent-bit.opensearch.basicAuth.username;
            HTTP_Passwd = ''''${OPENSEARCH_PASSWORD}'';
            Logstash_Format = "On";
            Logstash_Prefix = "garnix-build-logs";
            Logstash_DateFormat = "%Y.%m.%d";
            Time_Key = "@timestamp";
            Time_Key_Nanos = "On";
            Replace_Dots = "On";
            Suppress_Type_Name = "On";
            Index = "fluent-bit";
          };
        };
    };

    services.garnixServer = {
      s3Cache = lib.mkMerge [
        (lib.mkIf config.garnix.devMode.enable {
          publicBucket = "test-public";
          publicBaseUrl = "https://pub-aed3ff3b65d444b3aeee39d6ea1767b0.r2.dev";
          privateBucket = "test-private";
          host = "79e0f6a031ca6d9650034b607922ba45.r2.cloudflarestorage.com";
        })
        (lib.mkIf (!config.garnix.devMode.enable) {
          publicBucket = "prod-public";
          publicBaseUrl = "https://garnix-cache.com";
          privateBucket = "prod-private";
          host = "79e0f6a031ca6d9650034b607922ba45.r2.cloudflarestorage.com";
        })
      ];
    };

    systemd.services.garnixServer = {
      description = "The garnix server";
      # Coreutils is needed so we have the right version of 'tail', allowing
      # use of the --pid option
      path = with pkgs; [
        git
        config.nix.package
        coreutils
        util-linux
        bzip2
        openssh
        age
        flakeInputs.comment.packages.${stdenv.hostPlatform.system}.default
      ];
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        Type = "notify";
        User = config.garnix.database.dbUser;
        RestartSec = 1;
        Restart = "always";
        StartLimitBurst = 3;
        TimeoutStopSec = 5;
        KillSignal = "SIGINT";
        # If you change this, also change the build-logs-dir option
        StateDirectory = "garnix";
        PrivateTmp = false;
        LimitNOFILE = 1048576;
        Environment = [
          "PGPORT=${toString config.garnix.database.dbPort}"
          "PGUSER=${config.garnix.database.dbUser}"
          "PGDATABASE=${config.garnix.database.dbName}"
          "PGHOST=${config.garnix.database.fqdn}"
          "PGSSLMODE=${config.garnix.database.ssl.mode}"
          "PGSSLROOTCERT=${config.garnix.database.ssl.rootCert}"
          "TPG_TLS=true"
          "TPG_TLS_MODE=full"
          "TPG_HOST=${toString config.garnix.database.fqdn}"
          "TPG_PORT=${toString config.garnix.database.dbPort}"
          "TPG_USER=${config.garnix.database.dbUser}"
          "TPG_DB=${config.garnix.database.dbName}"
          "GARNIX_URL=${config.services.garnixServer.url}"
          "GITHUB_APP_NAME=${config.services.garnixServer.githubAppName}"
          "GARNIX_ADMIN_GITHUB_LOGIN=${config.services.garnixServer.adminGithubLogin}"
          "OPENSEARCH_URL=${config.services.garnixServer.opensearchUrl}"
          "S3_CACHE_REGION=${config.services.garnixServer.s3Cache.region}"
          "S3_CACHE_HOST=${config.services.garnixServer.s3Cache.host}"
          "S3_CACHE_PUBLIC_BUCKET=${config.services.garnixServer.s3Cache.publicBucket}"
          "S3_CACHE_PUBLIC_BASE_URL=${config.services.garnixServer.s3Cache.publicBaseUrl}"
          "S3_CACHE_PRIVATE_BUCKET=${config.services.garnixServer.s3Cache.privateBucket}"
        ];
        SupplementaryGroups = [ config.users.groups.keys.name ];
        ExecStartPre = [
          "${dbCheckIsReady}/bin/dbCheckIsReady"
          "${migrateScript}/bin/migrate"
          "${logsDir}/bin/logsDir"
        ];
        ExecStart = ''
          ${lib.getBin flakePackages."backend_garnix"}/bin/server \
              ${lib.concatStringsSep " " (builtins.map (testFeature: "--enable ${testFeature}") config.services.garnixServer.testFeatures)} \
              --port ${toString config.services.garnixServer.port} \
              --monitoring-port ${toString config.services.garnixServer.monitoringPort} \
              --metrics-port ${toString config.services.garnixServer.metricsPort} \
              --build-logs-reporting-port ${toString buildLogsFluentBitPort} \
              --build-logs-dir /var/lib/garnix/logs
        '';
      };
      unitConfig = {
        StartLimitIntervalSec = 10;
      };
    };

    systemd.services.frontend = {
      description = "Garnix NextJS webapp";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        Type = "simple";
        User = config.users.users.garnix.name;
        RestartSec = 1;
        Restart = "always";
        KillSignal = "SIGINT";
        Environment = [
          "PORT=${toString config.services.frontend.port}"
          "HOSTNAME=127.0.0.1"
        ];
        ExecStart = ''
          ${flakePackages.frontend_default}/bin/garnix-frontend
        '';
      };
    };

    services.nginx = {
      enable = true;
      recommendedProxySettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      proxyTimeout = "600s";
      appendConfig = ''
        worker_processes auto;
        worker_rlimit_nofile 2048;
      '';
      eventsConfig = ''
        worker_connections 1024;
      '';
      virtualHosts."garnix.io" = {
        default = true;
        forceSSL = ! config.garnix.devMode.enable;
        enableACME = ! config.garnix.devMode.enable;
        locations."/api".proxyPass = "http://127.0.0.1:${toString config.services.garnixServer.port}";
        locations."@frontend".proxyPass = "http://127.0.0.1:${toString config.services.frontend.port}";
        locations."/" = {
          root = "${flakePackages.frontend_default}/public";
          tryFiles = "$uri @frontend";
        };
      };
      virtualHosts."api.garnix.io" = {
        default = false;
        forceSSL = ! config.garnix.devMode.enable;
        enableACME = ! config.garnix.devMode.enable;
        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString config.services.garnixServer.port}/api/";
          extraConfig = ''
            if ($http_origin ~* "^https://(app\.)?garnix\.io$") {
              add_header Access-Control-Allow-Origin "$http_origin";
            }
          '';
        };
      };
    };

    services.journald.extraConfig = ''
      SystemMaxUse=100G
      SystemMaxFiles=1000
    '';

    sops.secrets = {
      database-password = {
        mode = "0440";
        group = config.users.users.garnix.name;
      };
      github_webhook_secret = {
        mode = "0440";
        group = config.users.users.garnix.name;
      };
      github_client_secret = {
        mode = "0440";
        group = config.users.users.garnix.name;
      };
      github_client_id = {
        mode = "0440";
        group = config.users.users.garnix.name;
      };
      github_app_id = {
        mode = "0440";
        group = config.users.users.garnix.name;
      };
      github_app_pk = {
        mode = "0440";
        group = config.users.users.garnix.name;
      };
      garnix_server_remote_builder_ssh = {
        mode = "0400";
        group = config.users.users.garnix.name;
      };
      garnix_server_remote_builder_ssh_garnix = {
        mode = "0400";
        owner = config.users.users.garnix.name;
        key = "garnix_server_remote_builder_ssh";
      };
      opensearch-garnix = {
        mode = "0400";
        owner = config.users.users.garnix.name;
      };
      repo-secrets-key = {
        mode = "0400";
        owner = config.users.users.garnix.name;
      };
      repo-secrets-key-pub = {
        mode = "0400";
        owner = config.users.users.garnix.name;
      };
      prometheus-node-exporter-1 = {
        mode = "0400";
        owner = config.users.users.garnix.name;
      };
      # This key was generated by `Servant.Auth.Server.writeKey`.
      "garnix-jwt-key" = {
        mode = "0400";
        owner = config.users.users.garnix.name;
      };
      garnix_action_runner_ssh = {
        mode = "0400";
        owner = config.users.users.garnix.name;
      };
      "s3-cache-access-key-id" = {
        mode = "0400";
        owner = config.users.users.garnix.name;
      };
      "s3-cache-secret-access-key" = {
        mode = "0400";
        owner = config.users.users.garnix.name;
      };
      "cache-priv-key" = {
        mode = "0400";
        owner = config.users.users.garnix.name;
      };
    };

    users.users.garnix = {
      group = "garnix";
      extraGroups = [ config.users.groups.keys.name ];
      isNormalUser = true;
      description = "Garnix server user";
    };
    users.groups.garnix = { };

    garnix.killRogueNixProcesses = false;
  };
}
