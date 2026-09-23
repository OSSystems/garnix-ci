{
  outputs =
    { nixpkgs, ... }:
    {
      nixosConfigurations.watchdog-pr-deployment-server = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (
            { pkgs, ... }:
            {
              system.stateVersion = "24.11";
              fileSystems."/" = {
                device = "/dev/sda1";
                fsType = "ext4";
              };
              boot.loader.grub.device = "/dev/sda";
              services.nginx =
                let
                  root = pkgs.writeTextFile {
                    name = "index";
                    destination = "/index.html";
                    text = pkgs.lib.removeSuffix "\n" (builtins.readFile ./date);
                  };
                in
                {
                  enable = true;
                  virtualHosts.default = {
                    root = "${root}";
                  };
                };
              networking.firewall.allowedTCPPorts = [ 80 ];
              virtualisation.vmVariant.services.getty.autologinUser = "root";
            }
          )
        ];
      };
    };
}
