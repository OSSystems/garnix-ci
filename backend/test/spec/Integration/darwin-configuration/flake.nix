{
  inputs.home-manager.url = "github:nix-community/home-manager/release-24.11";
  outputs =
    {
      self,
      home-manager,
      nixpkgs,
    }:
    {

      # This is a fake test for darwin configurations, as we cannot run darwin
      # derivations locally on linux machines when running this integration test.
      # So instead we use nixosConfigurations with `system = "x86_64-linux"`. We
      # should still get some test coverage for the attribute and config
      # machinery through this.
      #
      # Note that, surprisingly, `darwinConfigurations` seem to be very similar
      # to `nixosConfigurations` and seem to have the same attributes as those
      # (instead of being similar to `homeConfigurations`).

      darwinConfigurations = {
        failing = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            {
              system.stateVersion = "24.11";
              fileSystems."/" = {
                device = "/dev/sda1";
                fsType = "ext4";
              };
              boot.loader.grub.device = "/dev/sda";
            }
            {
              boot.loader.grub.device = "/dev/sdb";
            }
          ];
        };
        succeeding = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            {
              system.stateVersion = "24.11";
              fileSystems."/" = {
                device = "/dev/sda1";
                fsType = "ext4";
              };
              boot.loader.grub.device = "/dev/sda";
            }
          ];
        };
      };
    };
}
