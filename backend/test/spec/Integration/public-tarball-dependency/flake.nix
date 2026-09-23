{
  inputs.publicDep = {
    url = "tarball+https://github.com/garnix-io/test-repo/archive/0ec98785f89b03df7b1b3b6789e3a97dd22530dc.tar.gz";
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
