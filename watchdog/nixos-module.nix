{
  lib,
  config,
  flakePackages,
  ...
}:
{
  options.garnix.watchdog = {
    enable = lib.mkEnableOption "watchdog daemon";
    port = lib.mkOption {
      type = lib.types.int;
      default = 5555;
    };
  };
  config = lib.mkIf config.garnix.watchdog.enable {
    users = {
      users.watchdog = {
        group = "watchdog";
        description = "watchdog server user";
        isNormalUser = true;
      };
      groups.watchdog = { };
    };

    sops.secrets = {
      watchdog_ssh = {
        mode = "0400";
        owner = config.users.users.watchdog.name;
      };
    };

    systemd.services.watchdog = {
      description = "watchdog daemon";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      serviceConfig = {
        Type = "simple";
        User = "watchdog";
        ExecStart = lib.getExe flakePackages.watchdog;
        Environment = [
          "PORT=${toString config.garnix.watchdog.port}"
          "DATA_DIR=${./data}"
          "WATCHDOG_SSH_IDENTITY_FILE=${config.sops.secrets.watchdog_ssh.path}"
        ];
      };
    };

    services.prometheus = {
      scrapeConfigs = [
        {
          job_name = "watchdog";
          scheme = "http";
          metrics_path = "/";
          static_configs = [
            {
              targets = [ "localhost:${toString config.garnix.watchdog.port}" ];
            }
          ];
        }
      ];
    };
  };
}
