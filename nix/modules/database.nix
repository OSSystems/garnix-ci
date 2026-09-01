{ config
, lib
, pkgs
, ...
}:

let
  cfg = config.garnix.database;
  isIPv6 = lib.hasInfix ":";
  psqlSslPrivateKeyPath = "/run/postgresql/psql-ssl-private-key.pem";
  devCerts =
    let
      caCerts = config.garnix.devMode.certificates.ca;
    in
    pkgs.runCommand "generate-db-certs"
      {
        nativeBuildInputs = [ pkgs.minica ];
      } ''
      minica -ca-cert ${caCerts.cert} -ca-key ${caCerts.key} \
        -domains ${cfg.fqdn}
      install -Dm444 -t $out ${cfg.fqdn}/{key,cert}.pem
    '';
  setUpSslForPsql = certDir: lib.getExe (pkgs.writeScriptBin "set-up-ssl-for-psql" ''
    cp ${certDir}/key.pem ${psqlSslPrivateKeyPath}
    chown postgres:postgres ${psqlSslPrivateKeyPath}
    chmod 600 ${psqlSslPrivateKeyPath}
  '');
in

{
  options.garnix = {
    database = {
      enable = lib.mkEnableOption "the database service";

      dbUser = lib.mkOption {
        type = lib.types.str;
        readOnly = true;
        default = "garnix";
        description = "The main user for the database";
      };

      dbMonitoringUser = lib.mkOption {
        type = lib.types.str;
        readOnly = true;
        default = "monitoring";
        description = "The user for monitoring the database";
      };

      dbName = lib.mkOption {
        type = lib.types.str;
        readOnly = true;
        default = "garnix";
        description = "The main database name";
      };

      dbPort = lib.mkOption {
        type = lib.types.port;
        readOnly = true;
        default = 9178;
        description = "The database port";
      };

      fqdn = lib.mkOption {
        type = lib.types.str;
        description = "The FQDN of the database service";
      };

      allowedIPs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "The list of IPs ranges that are allowed to connect to the database";
      };

      ssl = {
        mode = lib.mkOption {
          type = lib.types.str;
          default = "verify-full";
          readOnly = true;
        };
        rootCert = lib.mkOption {
          type = lib.types.str;
          default = "/etc/ssl/certs/ca-certificates.crt";
          readOnly = true;
        };
      };

      exporter = {
        enable = lib.mkEnableOption "the exporter service";
        fqdn = lib.mkOption {
          type = lib.types.str;
          description = "The FQDN of the database exporter service";
          default = "prometheus-sql-exporter.garnix.io";
        };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.postgresql = {
      enable = true;
      package = pkgs.postgresql_18;
      enableTCPIP = true;
      settings = {
        port = cfg.dbPort;
        ssl = true;
        ssl_cert_file =
          if config.garnix.devMode.enable then
            "${devCerts}/cert.pem"
          else
            "${config.security.acme.certs."${cfg.fqdn}".directory}/cert.pem";
        ssl_key_file = psqlSslPrivateKeyPath;
      };
      initialScript = pkgs.writeText "psql-initscript" ''
        CREATE USER ${cfg.dbUser};
        CREATE DATABASE ${cfg.dbName} OWNER ${cfg.dbUser};
        GRANT ALL PRIVILEGES ON DATABASE ${cfg.dbName} TO ${cfg.dbUser};
        CREATE USER ${cfg.dbMonitoringUser};
      '';
      authentication = lib.concatMapStringsSep "\n"
        (ip: ''
          hostssl ${cfg.dbName} ${cfg.dbUser} ${ip} md5
          hostssl ${cfg.dbName} ${cfg.dbMonitoringUser} ${ip} md5
        '')
        cfg.allowedIPs;
    };

    networking.firewall.extraCommands = lib.concatMapStringsSep "\n"
      (ip:
        if isIPv6 ip then ''
          ip6tables -I INPUT -p tcp --dport ${toString cfg.dbPort} -s ${ip} -j ACCEPT
        '' else ''
          iptables -I INPUT -p tcp --dport ${toString cfg.dbPort} -s ${ip} -j ACCEPT
        '')
      cfg.allowedIPs;

    sops = {
      secrets = {
        database-password = {
          mode = "0440";
        };
        database-monitoring-pgpass = {
          mode = "0440";
        };
      };
    };

    systemd.services.postgresql-setup = {
      serviceConfig = {
        LoadCredential = [
          "database-password:${config.sops.secrets.database-password.path}"
          "database-monitoring-pgpass:${config.sops.secrets.database-monitoring-pgpass.path}"
        ];
      };

      script = lib.mkAfter ''
        set -euo pipefail

        # Create garnix user if it doesn't exist

        export PGPASSWORD=$(cat $CREDENTIALS_DIRECTORY/database-password)

        MD5PASSWORD="md5$(printf "%s%s" "$PGPASSWORD" ${lib.escapeShellArg cfg.dbUser} | md5sum | ${lib.getExe pkgs.gawk} '{print $1}')"

        psql \
          "${cfg.dbName}" \
          -c "ALTER ROLE ${cfg.dbUser} WITH LOGIN PASSWORD '$MD5PASSWORD'" \
          -p ${toString cfg.dbPort} || echo "Not updating password for ${cfg.dbUser}"

        # Create monitoring user if it doesn't exist

        psql -tAc "SELECT 1 FROM pg_roles WHERE rolname='${cfg.dbMonitoringUser}'" | grep -q 1 || psql -tAc 'CREATE USER "${cfg.dbMonitoringUser}"'

        export PG_MONITORING_PASSWORD=$(cat $CREDENTIALS_DIRECTORY/database-monitoring-pgpass | ${pkgs.gawk}/bin/awk -F':' '{print $NF}')

        psql \
          "${cfg.dbName}" \
          -c "ALTER ROLE ${cfg.dbMonitoringUser} WITH LOGIN PASSWORD '$PG_MONITORING_PASSWORD'" \
          -p ${toString cfg.dbPort} || echo "Not updating password for ${cfg.dbMonitoringUser}"
      '';
    };

    systemd.services.postgresql = {
      preStart = ''
        ${setUpSslForPsql (
          if config.garnix.devMode.enable
            then devCerts
            else config.security.acme.certs.${cfg.fqdn}.directory
        )}
      '';
    } // lib.optionalAttrs (!config.garnix.devMode.enable) {
      after = [ "acme-selfsigned-${cfg.fqdn}.service" ];
      before = [ "acme-${cfg.fqdn}.service" ];
      wants = [ "acme-finished-${cfg.fqdn}.target" ];
    };

    # Required for ACME challenge
    networking.firewall.allowedTCPPorts = [ 80 443 ];

    # This is backed up by borg
    services.postgresqlBackup = {
      enable = true;
      databases = [ cfg.dbName ];
      pgdumpOptions = "-C -p ${toString cfg.dbPort}";
      compression = "zstd";
      startAt = "*-*-* 0,6,12,18:00:00";
    };

    services.zfs.autoSnapshot = {
      hourly = 6;
      daily = 3;
      weekly = 1;
      monthly = 0;
      enable = true;
    };

    services.prometheus.exporters.sql = lib.mkIf cfg.exporter.enable (
      let dbConnectionString = "postgres://${cfg.dbMonitoringUser}@${cfg.fqdn}/${cfg.dbName}?port=${toString cfg.dbPort}&sslmode=${cfg.ssl.mode}&sslrootcert=${cfg.ssl.rootCert}";
      in
      {
        listenAddress = "127.0.0.1";
        configuration.jobs.counts = {
          interval = "1m";
          connections = [ dbConnectionString ];
          queries = {
            build_total = {
              labels = [ "build_total" ];
              help = "Total amount of builds";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM builds";
            };
            build_by_status = {
              labels = [ "status" ];
              help = "Total amount of failed builds";
              values = [ "count" ];
              query = "SELECT COALESCE(status::text, 'null') as status, count(*) as count FROM builds GROUP BY status";
            };
            build_running = {
              labels = [ "build_running" ];
              help = "Running builds count";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM builds WHERE status is null AND start_time >= (now() - interval '2 hours')";
            };
            build_stalled = {
              labels = [ "build_stalled" ];
              help = "Stalled builds count";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM builds WHERE status is null AND start_time < (now() - interval '2 hours')";
            };
          };
        };
        configuration.jobs.pkis = {
          interval = "12h";
          connections = [ dbConnectionString ];
          queries = {
            users = {
              labels = [ "users" ];
              help = "Total users signed up";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM users";
            };
            active_repos_per_day = {
              labels = [ "active_repos_per_day" ];
              help = "Number of repos with at least one commit each day";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM (SELECT repo_user, repo_name FROM builds WHERE start_time > now() - interval '1 day' GROUP BY repo_user, repo_name) as foo";
            };
            active_repos_per_week = {
              labels = [ "active_repos_per_week" ];
              help = "Number of repos with at least one commit each week";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM (SELECT repo_user, repo_name FROM builds WHERE start_time > now() - interval '1 week' GROUP BY repo_user, repo_name) as foo";
            };
            active_repos_per_month = {
              labels = [ "active_repos_per_month" ];
              help = "Number of repos with at least one commit each month";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM (SELECT repo_user, repo_name FROM builds WHERE start_time > now() - interval '1 month' GROUP BY repo_user, repo_name) as foo";
            };
            active_orgs_per_day = {
              labels = [ "active_orgs_per_day" ];
              help = "Number of orgs with some repo with at least one commit each day";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM (SELECT repo_user FROM builds WHERE start_time > now() - interval '1 day' GROUP BY repo_user) as foo";
            };
            active_orgs_per_week = {
              labels = [ "active_orgs_per_week" ];
              help = "Number of orgs with some repo with at least one commit each week";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM (SELECT repo_user FROM builds WHERE start_time > now() - interval '1 week' GROUP BY repo_user) as foo";
            };
            active_orgs_per_month = {
              labels = [ "active_orgs_per_month" ];
              help = "Number of orgs with some repo with at least one commit each month";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM (SELECT repo_user FROM builds WHERE start_time > now() - interval '1 month' GROUP BY repo_user) as foo";
            };
            active_req_user_per_day = {
              labels = [ "active_req_user_per_day" ];
              help = "Number of people commiting to garnix-enabled repos per day";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM (SELECT req_user FROM builds WHERE start_time > now() - interval '1 day' GROUP BY req_user) as foo";
            };
            active_req_user_per_week = {
              labels = [ "active_req_user_per_week" ];
              help = "Number of people commiting to garnix-enabled repos per week";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM (SELECT req_user FROM builds WHERE start_time > now() - interval '1 week' GROUP BY req_user) as foo";
            };
            active_req_user_per_month = {
              labels = [ "active_req_user_per_month" ];
              help = "Number of people commiting to garnix-enabled repos per month";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM (SELECT req_user FROM builds WHERE start_time > now() - interval '1 month' GROUP BY req_user) as foo";
            };
            orgs_deploying_per_day = {
              labels = [ "orgs_deploying_per_day" ];
              help = "Number of different orgs deploying per day";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM (SELECT distinct builds.repo_user FROM servers INNER JOIN builds ON servers.configuration_build_id = builds.id WHERE created_at > now() - interval '1 day') as foo";
            };
            orgs_deploying_per_week = {
              labels = [ "orgs_deploying_per_week" ];
              help = "Number of different orgs deploying per week";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM (SELECT distinct builds.repo_user FROM servers INNER JOIN builds ON servers.configuration_build_id = builds.id WHERE created_at > now() - interval '1 week') as foo";
            };
            orgs_deploying_per_month = {
              labels = [ "orgs_deploying_per_month" ];
              help = "Number of different orgs deploying per month";
              values = [ "count" ];
              query = "SELECT count(*) as count FROM (SELECT distinct builds.repo_user FROM servers INNER JOIN builds ON servers.configuration_build_id = builds.id WHERE created_at > now() - interval '1 month') as foo";
            };
          };
        };
        enable = true;
      }
    );

    systemd.services."prometheus-sql-exporter" = lib.mkIf cfg.exporter.enable {
      unitConfig = {
        Description = "Prometheus SQL exporter";
      };
      after = [ "postgresql.service" ];
      preStart = ''
        set -euo pipefail
        cp $CREDENTIALS_DIRECTORY/db_password /tmp
        chmod 400 db_password
      '';
      serviceConfig = {
        PrivateTmp = true;
        LoadCredential = [ "db_password:${config.sops.secrets.database-monitoring-pgpass.path}" ];
        Environment = [
          "PGPASSFILE=/tmp/db_password"
        ];
      };
    };

    security.acme.certs = {
      "${cfg.exporter.fqdn}" = lib.mkIf cfg.exporter.enable {
        webroot = "/var/lib/acme/acme-challenge";
      };
      "${cfg.fqdn}" = {
        webroot = "/var/lib/acme/acme-challenge";
        group = "postgres";
        postRun = ''
          ${setUpSslForPsql config.security.acme.certs.${cfg.fqdn}.directory}
          ${lib.getExe' pkgs.systemd "systemctl"} reload postgresql.service
        '';
      };
    };

    services.nginx = {
      enable = true;
      recommendedProxySettings = true;
      recommendedOptimisation = true;
      proxyTimeout = "600s";
      virtualHosts = {
        "${cfg.exporter.fqdn}" = lib.mkIf cfg.exporter.enable (config.garnix.devMode.withDevCerts {
          forceSSL = ! config.garnix.devMode.enable;
          enableACME = ! config.garnix.devMode.enable;
          inherit (config.garnix.monitoring-client.nginx) basicAuthFile;
          locations."/".proxyPass = "http://127.0.0.1:${toString config.services.prometheus.exporters.sql.port}";
        });
      } // lib.optionalAttrs (! config.garnix.devMode.enable) {
        "${cfg.fqdn}" = {
          locations."/.well-known/acme-challenge".root =
            config.security.acme.certs.${cfg.fqdn}.webroot;
        };
      };
    };

    users = {
      users.garnix = {
        group = "garnix";
        extraGroups = [ config.users.groups.keys.name ];
        isNormalUser = true;
        description = "Garnix server user";
      };
      groups.garnix = { };
      users.monitoring = {
        group = "monitoring";
        isNormalUser = true;
        description = "user to manually log in as the monitoring db user";
      };
      groups.monitoring = { };
    };
  };
}
