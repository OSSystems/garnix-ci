{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux =
      let
        pkgs = import nixpkgs {
          system = "x86_64-linux";
        };
        dep = pkgs.stdenv.mkDerivation {
          name = "failing-dep";

          buildPhase = "";

          src = ./.;

          checkPhase = ''
            echo "Dependency test failed!"
            exit 1
          '';

          doCheck = true;
        };

        fixedOutputDep = pkgs.stdenv.mkDerivation {
          name = "failing-fixed-output-dep";
          buildPhase = "";
          src = ./.;
          outputHashAlgo = "sha256";
          outputHashMode = "recursive";
          outputHash = "07iv4jangqnzrvjr749vl3x31z7dxds51bq1bhz5acbjbwf25wjf";
          installPhase = "
              mkdir -p $out/bin
              echo hi > $out/bin/hi
            ";
        };
      in
      {
        test = pkgs.stdenv.mkDerivation {
          name = "hello-garnix";

          buildInputs = [ dep ];

          doCheck = true;

        };

        fixedOutputTest = pkgs.stdenv.mkDerivation {
          name = "hello-garnix";

          buildInputs = [ fixedOutputDep ];

          doCheck = true;
        };
      };

  };
}
