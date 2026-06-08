{ config
, lib
, pkgs
, ...
}:

let
  cfg = config.garnix.fluent-bit;
  settingsFormatIni = pkgs.formats.ini {
    mkKeyValue = k: v: "  ${k} ${toString v}";
  };

  pipelineFiles = pkgs.linkFarm "pipelines.conf"
    ((lib.mapAttrsToList
      (_: pipeline:
        let
          pipelineName = "${pipeline.name}.conf";
        in
        {
          name = pipelineName;
          path =
            settingsFormatIni.generate pipelineName (lib.attrsets.filterAttrs (_: value: value != { }) {
              INPUT = pipeline.input;
              FILTER = pipeline.filter;
              OUTPUT =
                # If we are in dev-mode, and we are set to output to a file in dev-mode,
                # then we replace all outputs to files under /tmp
                if config.garnix.devMode.enable && config.garnix.fluent-bit.devModeOutputsToFile then
                  {
                    inherit (pipeline.output) Match;
                    Name = "file";
                    # This ends up in /tmp/systemd-private-<hash>-garnixServer/tmp/fluent-bit-test-output/
                    Path = "/tmp/fluent-bit-test-output";
                    File = pipeline.output.Match;
                    Mkdir = "On";
                  }
                else
                  pipeline.output;
            });
        })
      (lib.filterAttrs (_: pipeline: pipeline.enable) cfg.configuration.pipelines)) ++
    (lib.map (f: { name = f.name; path = f; }) filterFiles));
  parserFiles = (lib.mapAttrsToList
    (name: values:
      (settingsFormatIni.generate "parser.conf"
        {
          PARSER = {
            inherit name;
          } // values;
        })
    )
    cfg.configuration.parsers);
  filterFiles = (lib.mapAttrsToList
    (name: values:
      (settingsFormatIni.generate "filter.conf"
        {
          FILTER = {
            inherit name;
          } // values;
        })
    )
    cfg.configuration.extraFilters);
in

{
  options.garnix = {
    fluent-bit = {
      enable = lib.mkEnableOption "the fluent-bit service";

      enableNginxLogParsing = lib.mkEnableOption "enable parsing of nginx logs";

      # Some tests need the real outputs, so we include an escape hatch
      devModeOutputsToFile =
        lib.mkEnableOption "replacing of fluent-bit outputs by file outputs in dev-mode" // {
          default = true;
        };

      package = lib.mkOption {
        type = lib.types.package;
        default = lib.addMetaAttrs { mainProgram = "fluent-bit"; } (
          pkgs.fluent-bit.overrideAttrs (prevAttrs: {
            patches = (prevAttrs.patches or [ ]) ++ [
              (pkgs.fetchpatch {
                name = "fix-fluentd-systemd-journal-cursor.diff";
                url = "https://github.com/fluent/fluent-bit/pull/8396.diff";
                hash = "sha256-CmDb+HwF3zJmOuV1AbWuCwnX0CNjB0a5QiexPpvp/RY=";
              })
            ];
          })
        );
      };

      extraGroups = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
      };

      configuration = {
        service = lib.mkOption {
          type = lib.types.attrs;
          default = {
            flush = 5;
            logLevel = "debug";
            daemon = "false";
          };
        };

        parsers = lib.mkOption {
          description = "Fluent-bit parsers";
          default = { };
          type = lib.types.attrsOf (lib.types.attrs);
        };

        extraFilters = lib.mkOption {
          description = "Fluent-bit extra filters";
          default = { };
          type = lib.types.attrsOf (lib.types.attrs);
        };

        pipelines =
          let
            singleIniAtom = with lib.types; nullOr (oneOf [ bool int float str ]) // {
              description = "INI atom (null, bool, int, float or string)";
            };
          in
          lib.mkOption {
            description = "Fluent-bit pipelines";
            default = { };
            type = lib.types.attrsOf (lib.types.submodule ({ name, lib, ... }: {
              options = {
                enable = lib.mkEnableOption "the pipeline" // {
                  default = true;
                };
                name = lib.mkOption {
                  type = lib.types.str;
                  default = name;
                };
                input = lib.mkOption {
                  type = lib.types.attrsOf singleIniAtom;
                  default = { };
                };
                filter = lib.mkOption {
                  type = lib.types.attrsOf singleIniAtom;
                  default = { };
                };
                output = lib.mkOption {
                  type = lib.types.attrsOf singleIniAtom;
                  default = { };
                };
              };
            }));
          };
      };

      opensearch = {
        fqdn = lib.mkOption {
          type = lib.types.str;
          description = "OpenSearch host for log shipping.";
        };
        port = lib.mkOption {
          type = lib.types.int;
          default = 443;
          description = "OpenSearch port for log shipping.";
        };
        tls = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Use TLS when shipping logs to OpenSearch. Disable for a plain-HTTP local OpenSearch.";
        };
        basicAuth = {
          username = lib.mkOption {
            type = lib.types.str;
            default = "garnix";
          };
          passwordFile = lib.mkOption {
            type = lib.types.str;
            description = "Path to the OpenSearch basic-auth password file.";
          };
        };
      };

      buildLogsPipeline = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Run the build-logs HTTP-in -> OpenSearch pipeline.";
        };
        port = lib.mkOption {
          type = lib.types.int;
          default = 8888;
          description = "Port the build-logs HTTP input listens on (::1).";
        };
      };
    };
  };

  config = lib.mkMerge [{
    garnix.fluent-bit.configuration =
      let
        journalTagPrefix = "systemd";
        garnixServerJsonParserName = "garnixServer-json-parser";
        nginxJsonParserName = "nginx-json-parser";
        defaultOutput = {
          Name = "opensearch";
          Host = cfg.opensearch.fqdn;
          Port = cfg.opensearch.port;
          Tls = if cfg.opensearch.tls then "On" else "Off";
          "Tls.verify" = if config.garnix.devMode.enable then "Off" else "On";
          HTTP_User = cfg.opensearch.basicAuth.username;
          HTTP_Passwd = ''''${OPENSEARCH_PASSWORD}'';
          Logstash_Format = "On";
          Logstash_Prefix = "garnix-system";
          Logstash_DateFormat = "%Y.%m.%d";
          Time_Key = "@timestamp";
          Time_Key_Nanos = "On";
          Replace_Dots = "On";
          Suppress_Type_Name = "On";
          Index = "fluent-bit";
        };
      in
      {
        parsers = {
          "${garnixServerJsonParserName}" = {
            Format = "json";
          };
          "${nginxJsonParserName}" = {
            Format = "json";
            Time_Key = "time_iso8601";
            Time_Format = "%Y-%m-%dT%H:%M:%S%z";
          };
        };
        extraFilters = {
          "lua" = {
            Match = "nginx";
            script = pkgs.writeText "nginx-lua-filter.lua" ''
              function filter(tag, timestamp, record)
                record["request_length"] = tonumber(record["request_length"])
                record["request_time"] = tonumber(record["request_time"])
                record["bytes_sent"] = tonumber(record["bytes_sent"])
                record["body_bytes_sent"] = tonumber(record["body_bytes_sent"])
                record["upstream_response_time"] = tonumber(record["upstream_response_time"])
                if record["status"] == "200" and record["request_time"] ~= nil and record["request_time"] > 0 then
                  record["throughput"] = record["bytes_sent"] / record["request_time"]
                end
                return 1, timestamp, record
              end'';
            call = "filter";
          };
        };
        pipelines = {
          journal = {
            input = {
              Name = "systemd";
              Tag = "${journalTagPrefix}.*";
              Read_From_Tail = "On";
              DB = "journald_cursor.sqlite";
              Lowercase = "On";
              Strip_Underscores = "On";
            };
            filter = {
              Name = "parser";
              Match = "${journalTagPrefix}.garnixServer.service";
              Parser = garnixServerJsonParserName;
              Key_Name = "message";
              Reserve_Data = "True";
            };
            output = defaultOutput // {
              Match = "${journalTagPrefix}.*";
              Logstash_Prefix = "garnix-system";
            };
          };
        } // lib.optionalAttrs cfg.enableNginxLogParsing {
          nginx = {
            input = {
              Name = "tail";
              Tag = "nginx";
              DB = "nginx_cursor.sqlite";
              Parser = nginxJsonParserName;
              Path = "/var/log/nginx/json_access.log";
              Refresh_Interval = "15";
            };
            filter = {
              Name = "record_modifier";
              Match = "nginx";
              Record = ''server ''${HOSTNAME}'';
            };
            output = defaultOutput // {
              Match = "nginx";
              Logstash_Prefix = "nginx";
            };
          };
        } // lib.optionalAttrs cfg.buildLogsPipeline.enable {
          build-logs =
            let
              tag = "build-logs";
            in
            {
              input = {
                Name = "http";
                Tag = tag;
                listen = "::1";
                port = cfg.buildLogsPipeline.port;
              };
              output = defaultOutput // {
                Match = tag;
                Logstash_Prefix = "garnix-build-logs";
              };
            };
        };
      };

    systemd.services = {
      fluent-bit =
        let
          fluentMainConfig = settingsFormatIni.generate "fluent-bit.conf"
            {
              SERVICE = cfg.configuration.service;
            };
          pipelineConfig = pkgs.writeText "pipelines.conf" "@INCLUDE ${pipelineFiles}/*.conf";
        in
        lib.mkIf cfg.enable {
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ] ++ lib.optionals (cfg.enableNginxLogParsing) [ "nginx.service" ];
          description = "Fluent Bit log processor and forwarder";
          serviceConfig = {
            Nice = 10;
            SupplementaryGroups = [
              # allow to read the systemd journal
              "systemd-journal"
            ] ++ lib.optionals (cfg.enableNginxLogParsing) [ "nginx" ] ++ cfg.extraGroups;
            StateDirectory = "fluent-bit";
            DynamicUser = true;
            LoadCredential = [ "opensearch_password:${cfg.opensearch.basicAuth.passwordFile}" ];
            Restart = "always";
            RestartSec = 10;
            RestartSteps = 20;
            RestartMaxDelaySec = "3min";
            RestartMode = "direct";
            LimitNOFILE = 8192;
          };
          script = ''
            export OPENSEARCH_PASSWORD=$(cat $CREDENTIALS_DIRECTORY/opensearch_password)
              ${lib.getExe cfg.package} --workdir ''${STATE_DIRECTORY} --config=${fluentMainConfig} --config=${pipelineConfig} --parser=${cfg.package}/etc/fluent-bit/parsers.conf''
          + " " + lib.concatMapStringsSep " " (p: "--parser=" + p) parserFiles;
          reload = ''
            ${pkgs.coreutils}/bin/kill -HUP $MAINPID
          '';
        };
    };
  }
    (lib.mkIf (cfg.enableNginxLogParsing) {
      services.nginx.commonHttpConfig = ''
        log_format json escape=json
          '{'
            '"connection": "$connection", '                             # connection serial number
            '"connection_requests": "$connection_requests", '           # number of requests made in connection
            '"pid": "$pid", '
            '"request_id": "$request_id", '                             # the unique request id
            '"request_length": "$request_length", '
            '"remote_addr": "$remote_addr", '
            '"remote_user": "$remote_user", '
            '"remote_port": "$remote_port", '
            '"time_iso8601": "$time_iso8601", '
            '"request": "$request", '                                   # full path and no arguments
            '"request_uri": "$request_uri", '                           # full path and arguments
            '"args": "$args", '                                         # request arguments in URL's query string
            '"status": "$status", '                                     # response status code
            '"body_bytes_sent": "$body_bytes_sent", '                   # the number of body bytes excluding headers sent to a client
            '"bytes_sent": "$bytes_sent", '                             # the number of bytes sent to a client
            '"http_referer": "$http_referer", '
            '"http_user_agent": "$http_user_agent", '
            '"http_x_forwarded_for": "$http_x_forwarded_for", '
            '"http_host": "$http_host", '                               # the request Host: header
            '"server_name": "$server_name", '                           # the name of the vhost serving the request
            '"request_time": "$request_time", '                         # request processing time in seconds with msec resolution
            '"upstream": "$upstream_addr", '                            # upstream backend server for proxied requests
            '"upstream_connect_time": "$upstream_connect_time", '       # upstream handshake time incl. TLS
            '"upstream_header_time": "$upstream_header_time", '
            '"upstream_response_time": "$upstream_response_time", '
            '"upstream_response_length": "$upstream_response_length", '
            '"ssl_protocol": "$ssl_protocol", '
            '"ssl_cipher": "$ssl_cipher", '
            '"scheme": "$scheme", '
            '"request_method": "$request_method", '
            '"server_protocol": "$server_protocol" '                    # request protocol, like HTTP/1.1 or HTTP/2.0
          '}';

        access_log /var/log/nginx/json_access.log json;
      '';
      services.logrotate.settings.nginx.rotate = 3;
    })];
}
