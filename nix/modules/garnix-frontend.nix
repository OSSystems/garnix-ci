{ config, lib, flakePackages, ... }:

let
  cfg = config.services.garnixServer;
in
{
  options.services.garnixServer.frontend = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Run the bundled NextJS frontend behind the garnix vhost.";
    };
    port = lib.mkOption {
      type = lib.types.int;
      default = 3000;
      description = "Port the frontend NextJS server listens on (localhost only).";
    };
  };

  config = lib.mkIf (cfg.enable && cfg.frontend.enable) {
    systemd.services.frontend = {
      description = "Garnix NextJS webapp";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        RestartSec = 1;
        Restart = "always";
        KillSignal = "SIGINT";
        Environment = [
          "PORT=${toString cfg.frontend.port}"
          "HOSTNAME=127.0.0.1"
        ];
        ExecStart = "${flakePackages.frontend_default}/bin/garnix-frontend";
      };
    };
  };
}
