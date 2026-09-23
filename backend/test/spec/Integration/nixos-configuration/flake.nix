{
  description = "A very basic flake";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";

  outputs = { self, nixpkgs }: {

    nixosModules = {
      foo = {
        imports = [ ];
      };
    };

    nixosConfigurations = {
      failing = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          self.nixosModules.foo
          ({ pkgs, ... }: {
            boot.isContainer = 1 / 0;
          })
        ];
      };
      succeeding = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          self.nixosModules.foo
          ({ pkgs, ... }: {
            boot.isContainer = true;
          })
        ];
      };
    };

  };
}
