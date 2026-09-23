{
  description = "A very basic flake";
  inputs.privateDep = {
    url = "github:garnix-testing-org/test-repo-private";
    flake = false;
  };
  outputs =
    {
      self,
      nixpkgs,
      privateDep,
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
                  content=$(cat ${privateDep}/private.txt)
                  echo accessed: $content
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
