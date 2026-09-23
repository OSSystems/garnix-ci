{
  description = "A very basic flake";
  inputs.publicDep = {
    url = "github:garnix-io/test-repo";
    flake = false;
  };
  outputs =
    {
      self,
      nixpkgs,
      publicDep,
    }:
    {
      packages =
        let
          mk =
            sys:
            let
              pkgs = nixpkgs.legacyPackages.${sys};
            in
            {
              default = pkgs.stdenv.mkDerivation {
                name = "test-derivation";
                src = ./.;
                configurePhase = "";
                buildPhase = "";
                installPhase = ''
                  mkdir -p $out
                  cat ${publicDep}/hello.txt > $out/foo
                '';
              };
            };
        in
        {
          x86_64-linux = mk "x86_64-linux";
          x86_64-darwin = mk "x86_64-darwin";
          aarch64-darwin = mk "aarch64-darwin";
        };
    };
}
