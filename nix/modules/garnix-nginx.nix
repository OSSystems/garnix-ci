{ config, lib, flakePackages, ... }:

let
  cfg = config.services.garnixServer;
  devMode = config.garnix.devMode.enable;
  frontendLocations = lib.optionalAttrs cfg.frontend.enable {
    "@frontend".proxyPass = "http://127.0.0.1:${toString cfg.frontend.port}";
    "/" = {
      root = "${flakePackages.frontend_default}/public";
      tryFiles = "$uri @frontend";
    };
  };
in
{
  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ 80 443 ];

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
      virtualHosts.${cfg.hostname} = {
        default = true;
        forceSSL = !devMode;
        enableACME = !devMode;
        locations = {
          "/api".proxyPass = "http://127.0.0.1:${toString cfg.port}";
        } // frontendLocations;
      };
    };
  };
}
