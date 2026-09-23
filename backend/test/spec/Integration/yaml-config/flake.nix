{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
  outputs =
    { self, nixpkgs }:
    let
      mk =
        sys:
        let
          pkgs = import nixpkgs {
            system = sys;
          };
        in
        pkgs.hello;
    in
    {
      packages = {
        x86_64-linux = {
          a = mk "x86_64-linux";
          b = mk "x86_64-linux";
        };
        # This is to check that errors in a system that shouldn't be checked
        # don't propagate.
        aarch64-darwin = 1 / 0;
      };
    };
}
