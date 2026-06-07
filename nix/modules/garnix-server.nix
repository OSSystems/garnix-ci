{ config
, lib
, pkgs
, flakePackages
, flakeInputs
, ...
}:

let
  cfg = config.services.garnixServer;
  buildLogsFluentBitPort = 8888;

  remoteBuilderType = lib.types.submodule ({ ... }: {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        description = "SSH alias for the builder (used in /etc/ssh/ssh_config).";
      };
      hostname = lib.mkOption {
        type = lib.types.str;
        description = "DNS name or IP of the builder.";
      };
      user = lib.mkOption {
        type = lib.types.str;
        default = "nix-ssh";
        description = "SSH user on the remote builder.";
      };
      sshKeyPath = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Per-host override for the SSH private key.
          Falls back to remoteBuilders.sshKeyPath.
          The file must exist at this path on the host running garnix.
        '';
      };
      systems = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "Nix system tuples this builder handles (e.g. [\"x86_64-linux\"]).";
      };
      maxJobs = lib.mkOption {
        type = lib.types.int;
        default = 1;
      };
      speedFactor = lib.mkOption {
        type = lib.types.int;
        default = 1;
      };
      supportedFeatures = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
      };
      mandatoryFeatures = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
      };
    };
  });

  logsDir = pkgs.writeShellScriptBin "logsDir" ''
    if [ -d /var/lib/garnix/logs ]; then
      echo "Logs dir exists. Not creating"
    else
      mkdir /var/lib/garnix/logs
    fi
  '';

  pgConnString = "db:pg://${cfg.database.user}:$(cat /run/secrets/database-password)@${cfg.database.host}:${toString cfg.database.port}/${cfg.database.name}?sslmode=${cfg.database.ssl.mode}&sslrootcert=${cfg.database.ssl.rootCert}";

  migrateScript = pkgs.writeShellScriptBin "migrate" ''
    set -euo pipefail
    export SQITCH_PASSWORD=$(cat /run/secrets/database-password)
    ${lib.getBin flakePackages."backend_migrate"}/bin/sqitch deploy \
        "${pgConnString}"
  '';

  dbCheckIsReady = pkgs.writeShellScriptBin "dbCheckIsReady" ''
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

  s3CacheEnv = lib.optionals cfg.s3Cache.enable [
    "S3_CACHE_REGION=${cfg.s3Cache.region}"
    "S3_CACHE_HOST=${cfg.s3Cache.host}"
    "S3_CACHE_PUBLIC_BUCKET=${cfg.s3Cache.publicBucket}"
    "S3_CACHE_PUBLIC_BASE_URL=${cfg.s3Cache.publicBaseUrl}"
    "S3_CACHE_PRIVATE_BUCKET=${cfg.s3Cache.privateBucket}"
  ];

  remoteBuilderSshConfig = lib.concatMapStringsSep "\n"
    (h: ''
      Host ${h.name}
         Hostname ${h.hostname}
         User ${h.user}
         IdentityFile ${if h.sshKeyPath != null then h.sshKeyPath else cfg.remoteBuilders.sshKeyPath}
    '')
    cfg.remoteBuilders.hosts;
in
{
  imports = [
    ./garnix-frontend.nix
    ./garnix-nginx.nix
    ./garnix-secrets.nix
    ./custom-gc.nix
    ./fluent-bit.nix
    ./dev-mode.nix
    ./action-runner.nix
  ];

  options.services.garnixServer = {
    enable = lib.mkEnableOption "garnix server";

    hostname = lib.mkOption {
      type = lib.types.str;
      description = "Public hostname for the garnix vhost (single-origin self-host).";
      example = "garnix.example.com";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "garnix";
      description = "System user the backend, frontend, and migrations run as.";
    };

    url = lib.mkOption {
      type = lib.types.str;
      default = "https://app.garnix.io";
      description = "Public-facing base URL of the garnix instance.";
    };

    githubAppName = lib.mkOption {
      type = lib.types.str;
      default = "garnix-ci";
      description = "GitHub App slug used for install-flow links.";
    };

    adminGithubLogin = lib.mkOption {
      type = lib.types.str;
      description = ''
        GitHub login of the self-host operator. The user that signs up
        with this exact login receives subscription_type = 'admin'.
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

    acmeEmail = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "ACME account email. When null, no security.acme.defaults.email is set.";
    };

    testFeatures = lib.mkOption {
      type = lib.types.listOf (lib.types.enum [ "DevApi" "OpenSearchMocks" "CacheUploadMocks" ]);
      default = [ ];
    };

    trustedUsers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Extra entries appended to nix.settings.trusted-users.";
    };

    database = {
      host = lib.mkOption {
        type = lib.types.str;
        description = "Hostname or IP of the Postgres database.";
      };
      port = lib.mkOption {
        type = lib.types.port;
        default = 5432;
      };
      user = lib.mkOption {
        type = lib.types.str;
        default = "garnix";
      };
      name = lib.mkOption {
        type = lib.types.str;
        default = "garnix";
      };
      ssl = {
        mode = lib.mkOption {
          type = lib.types.str;
          default = "verify-full";
          description = "libpq sslmode (disable, allow, prefer, require, verify-ca, verify-full).";
        };
        rootCert = lib.mkOption {
          type = lib.types.str;
          default = "/etc/ssl/certs/ca-certificates.crt";
        };
      };
    };

    opensearch = {
      url = lib.mkOption {
        type = lib.types.str;
        description = "OpenSearch endpoint (typically https://host/_msearch).";
        example = "https://opensearch.example.com/_msearch";
      };
      host = lib.mkOption {
        type = lib.types.str;
        description = "OpenSearch hostname used by fluent-bit for log shipping.";
        example = "opensearch.example.com";
      };
      username = lib.mkOption {
        type = lib.types.str;
        default = "garnix";
        description = "OpenSearch basic-auth username for fluent-bit log shipping.";
      };
    };

    s3Cache = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Enable S3-backed Nix binary cache (upload + /api/cache/<hash>.narinfo).
          When false, the backend skips reading S3 secrets and the /api/cache narinfo
          endpoint short-circuits to 404.
        '';
      };
      region = lib.mkOption {
        type = lib.types.str;
        default = "auto";
      };
      host = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "S3 endpoint host (used iff s3Cache.enable).";
      };
      publicBucket = lib.mkOption {
        type = lib.types.str;
        default = "";
      };
      publicBaseUrl = lib.mkOption {
        type = lib.types.str;
        default = "";
      };
      privateBucket = lib.mkOption {
        type = lib.types.str;
        default = "";
      };
    };

    remoteBuilders = {
      sshKeyPath = lib.mkOption {
        type = lib.types.str;
        default = "/run/secrets/garnix_server_remote_builder_ssh";
        description = "Default SSH private key used to reach remote builders.";
      };
      hosts = lib.mkOption {
        type = lib.types.listOf remoteBuilderType;
        default = [ ];
        description = ''
          Remote Nix builders. When empty, builds run locally
          (`max-jobs = auto`). When non-empty, `max-jobs = 0` and
          nix.buildMachines is populated from this list.
        '';
      };
    };

    devDefaults = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = {
        url = "https://testing.garnix.io";
        githubAppName = "test-app-jkarni";
        adminGithubLogin = "jkarni";
      };
      description = ''
        Defaults applied to vmVariant builds. Override per option to point
        the dev VM at a different mock GitHub App.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !config.garnix.devMode.enable -> (cfg.testFeatures == [ ]);
        message = ''
          Test features cannot be enabled in production.
          Set garnix.devMode.enable = true if you want to enable test features.
        '';
      }
      {
        assertion = cfg.opensearch.url != "";
        message = "services.garnixServer.opensearch.url must be set.";
      }
      {
        assertion = cfg.opensearch.host != "";
        message = "services.garnixServer.opensearch.host must be set (used by fluent-bit).";
      }
      {
        assertion = !cfg.s3Cache.enable || (cfg.s3Cache.host != "" && cfg.s3Cache.publicBucket != "" && cfg.s3Cache.privateBucket != "" && cfg.s3Cache.publicBaseUrl != "");
        message = "services.garnixServer.s3Cache.{host,publicBucket,privateBucket,publicBaseUrl} must be set when s3Cache.enable = true.";
      }
    ];

    users = {
      users.${cfg.user} = lib.mkDefault {
        group = cfg.user;
        extraGroups = [ config.users.groups.keys.name ];
        isNormalUser = true;
        description = "Garnix server user";
      };
      groups.${cfg.user} = lib.mkDefault { };
    };

    environment.systemPackages = [ pkgs.postgresql_18 ];

    programs.ssh = {
      startAgent = true;
      extraConfig = remoteBuilderSshConfig;
    };

    nix = {
      settings = {
        cores = 4;
        trusted-users = cfg.trustedUsers;
      };
      extraOptions = ''
        max-jobs = ${if cfg.remoteBuilders.hosts == [ ] || config.garnix.devMode.enable then "auto" else "0"}
        keep-build-log = true
      '';
      buildMachines = lib.map
        (h: {
          hostName = h.name;
          sshUser = h.user;
          sshKey = if h.sshKeyPath != null then h.sshKeyPath else cfg.remoteBuilders.sshKeyPath;
          protocol = "ssh-ng";
          systems = h.systems;
          maxJobs = h.maxJobs;
          speedFactor = h.speedFactor;
          supportedFeatures = h.supportedFeatures;
          mandatoryFeatures = h.mandatoryFeatures;
        })
        cfg.remoteBuilders.hosts;
      distributedBuilds = cfg.remoteBuilders.hosts != [ ] && !config.garnix.devMode.enable;
      daemonIOSchedPriority = 4;
    };

    security.acme = lib.mkIf (cfg.acmeEmail != null) {
      acceptTerms = true;
      defaults.email = cfg.acmeEmail;
    };

    virtualisation.vmVariant = { lib, ... }: {
      services.garnixServer = lib.mapAttrs (_: v: lib.mkDefault v) cfg.devDefaults;
    };

    garnix = {
      custom-gc = {
        enable = true;
        enableTimer = true;
        useNixHeuristicGc = false;
        upperLimitPercent = null;
      };
      actionRunner.enable = lib.mkDefault true;
      fluent-bit.enable = lib.mkDefault true;
      fluent-bit.enableNginxLogParsing = true;
      fluent-bit.opensearch.fqdn = lib.mkDefault cfg.opensearch.host;
      fluent-bit.opensearch.basicAuth.username = lib.mkDefault cfg.opensearch.username;
      fluent-bit.opensearch.basicAuth.passwordFile = lib.mkDefault "/run/secrets/opensearch-garnix";
      fluent-bit.buildLogsPipeline = {
        enable = lib.mkDefault true;
        port = buildLogsFluentBitPort;
      };
    };

    systemd.services.garnixServer = {
      description = "The garnix server";
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
      after = [ "network-online.target" "garnix-secrets-stage.service" ];
      requires = [ "garnix-secrets-stage.service" ];
      serviceConfig = {
        Type = "notify";
        User = cfg.user;
        RestartSec = 1;
        Restart = "always";
        StartLimitBurst = 3;
        TimeoutStopSec = 5;
        KillSignal = "SIGINT";
        StateDirectory = "garnix";
        PrivateTmp = false;
        LimitNOFILE = 1048576;
        Environment = [
          "PGPORT=${toString cfg.database.port}"
          "PGUSER=${cfg.database.user}"
          "PGDATABASE=${cfg.database.name}"
          "PGHOST=${cfg.database.host}"
          "PGSSLMODE=${cfg.database.ssl.mode}"
          "PGSSLROOTCERT=${cfg.database.ssl.rootCert}"
          "TPG_HOST=${cfg.database.host}"
          "TPG_PORT=${toString cfg.database.port}"
          "TPG_USER=${cfg.database.user}"
          "TPG_DB=${cfg.database.name}"
          "GARNIX_URL=${cfg.url}"
          "GITHUB_APP_NAME=${cfg.githubAppName}"
          "GARNIX_ADMIN_GITHUB_LOGIN=${cfg.adminGithubLogin}"
          "OPENSEARCH_URL=${cfg.opensearch.url}"
          "S3_CACHE_ENABLED=${if cfg.s3Cache.enable then "true" else "false"}"
        ] ++ lib.optionals (cfg.database.ssl.mode != "disable") [
          # postgresql-typed reads TPG_TLS via lookupEnv — presence enables
          # TLS regardless of value. Only emit when ssl is actually on.
          "TPG_TLS=true"
          "TPG_TLS_MODE=full"
        ] ++ s3CacheEnv;
        SupplementaryGroups = [ config.users.groups.keys.name ];
        ExecStartPre = [
          "${dbCheckIsReady}/bin/dbCheckIsReady"
          "${migrateScript}/bin/migrate"
          "${logsDir}/bin/logsDir"
        ];
        ExecStart = ''
          ${lib.getBin flakePackages."backend_garnix"}/bin/server \
              ${lib.concatStringsSep " " (builtins.map (testFeature: "--enable ${testFeature}") cfg.testFeatures)} \
              --port ${toString cfg.port} \
              --monitoring-port ${toString cfg.monitoringPort} \
              --metrics-port ${toString cfg.metricsPort} \
              --build-logs-reporting-port ${toString buildLogsFluentBitPort} \
              --build-logs-dir /var/lib/garnix/logs
        '';
      };
      unitConfig = {
        StartLimitIntervalSec = 10;
      };
    };

    services.journald.extraConfig = ''
      SystemMaxUse=100G
      SystemMaxFiles=1000
    '';
  };
}
