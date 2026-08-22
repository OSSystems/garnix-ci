{ pkgs
, lib
, self
, system
, flakeInputs
, ...
}:
let
  node_modules = pkgs.importNpmLock.buildNodeModules {
    nodejs = pkgs.nodejs;
    npmRoot = ./.;
  };
  # There are a few artifacts that are built from nix, but nextjs needs in
  # place in order to build/run:
  populateArtifacts = ''
    echo Populating artifacts
    ln -nvsf ${self.packages.${system}."frontend_ageWasm_default"} src/age-wasm-compiled
  '';
  src = with pkgs.lib.fileset;
    toSource {
      root = ./.;
      fileset = fileFilter (file: ! file.hasExt "nix") ./.;
    };
  nextApp = pkgs.stdenv.mkDerivation {
    name = "frontend";
    inherit src;

    NEXT_BUILD_ID = builtins.hashString "sha256" "${./.}";
    outputs = [ "out" "assets" ];

    buildInputs = [ pkgs.nodejs ];

    configurePhase = ''
      # Get the node_modules from its own derivation
      ln -sf ${node_modules}/node_modules node_modules
      ${populateArtifacts}
      export HOME=$TMP
    '';

    buildPhase = ''
      npm run build -- --no-lint
    '';

    installPhase = ''
      mv .next/standalone $out
      mkdir -p $assets/public/_next
      mv .next/static $assets/public/_next/static
    '';
  };
  startScript = pkgs.writeShellApplication {
    name = "garnix-frontend";
    runtimeInputs = [ pkgs.nodejs ];
    text = "node ${nextApp}/server.js";
  };
in
{
  packages = {
    default = pkgs.symlinkJoin {
      name = "frontend";
      paths = [
        startScript
        nextApp.assets
      ];
    };
  };
  checks = lib.listToAttrs (lib.map
    (name: {
      inherit name;
      value = pkgs.runCommand name
        { buildInputs = [ pkgs.nodejs ]; }
        ''
          cp -r ${src} src
          cd src
          chmod -R +w .
          ln -sf ${node_modules}/node_modules node_modules
          ${populateArtifacts}
          npm run ${name} --ci | tee /dev/null
          touch $out
        '';
    })
    [ "knip" "lint" "test" ]);
  shellHook = populateArtifacts;
  devShellInputs = [
    pkgs.nodejs
    pkgs.prettier
    pkgs.typescript
    pkgs.typescript-language-server
    pkgs.vscode-langservers-extracted
  ];
}
