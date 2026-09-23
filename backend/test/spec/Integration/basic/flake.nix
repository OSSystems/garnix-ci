{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: {

    packages =
      let
        mk =
          sys:
          let
            pkgs = nixpkgs.legacyPackages.${sys};

            failing = pkgs.stdenv.mkDerivation {
              name = "failing-pkg";
              buildPhase = "";
              src = ./.;
              checkPhase = ''
                echo "Test failed!"
                exit 1
              '';
              doCheck = true;
            };

            succeeding = pkgs.stdenv.mkDerivation {
              name = "hello-garnix";
              src = ./.;
              configurePhase = "";
              buildPhase = "";
              installPhase = ''
                mkdir -p $out/bin
                echo hi > $out/bin/hi
              '';
            };

          in
          {
            inherit failing succeeding;
          };
      in
      {
        x86_64-linux = mk "x86_64-linux";
        x86_64-darwin = mk "x86_64-darwin";
        aarch64-darwin = mk "aarch64-darwin";
      };
  };
}
