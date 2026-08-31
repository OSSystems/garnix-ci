{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  inputs.sops-nix = {
    url = "github:Mic92/sops-nix";
    inputs = {
      nixpkgs.follows = "nixpkgs";
    };
  };

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.treetop = {
    url = "github:soenkehahn/treetop";
    inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-utils.follows = "flake-utils";
      crane.follows = "crane";
    };
  };

  inputs.crane = {
    url = "github:ipetkov/crane";
  };

  inputs.comment = {
    url = "github:garnix-io/comment";
    inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-utils.follows = "flake-utils";
      crane.follows = "crane";
    };
  };

  inputs.cradle = {
    url = "github:garnix-io/cradle";
    inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-utils.follows = "flake-utils";
    };
  };

  inputs.treefmt-nix = {
    url = "github:numtide/treefmt-nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };


  outputs =
    flakeInputs@{ self
    , nixpkgs
    , flake-utils
    , sops-nix
    , cradle
    , treefmt-nix
    , ...
    }:
    let
      overlays = [
        (outerFinal: outerPrev: {
          haskellPackages =
            with outerPrev.haskell.lib;
            outerPrev.haskell.packages.ghc967.override {
              overrides = final: prev: {
                hashids = prev.hashids.overrideAttrs (old: {
                  meta = old.meta // {
                    broken = false;
                  };
                });
                generic-random = prev.callPackage ./nix/packages/generic-random.nix { };
                HDBC = prev.callPackage ./nix/packages/HDBC.nix { };
                servant-github-webhook = prev.callPackage ./nix/packages/servant-github-webhook.nix { };
                generics-eot = prev.callPackage ./nix/packages/generics-eot.nix { };
                iso-deriving = prev.callPackage ./nix/packages/iso-deriving.nix { };
                github-app = prev.callPackage ./nix/packages/github-app.nix { };
                github-webhooks = prev.callPackage ./nix/packages/github-webhooks.nix { };
                oauth2-simple = prev.callPackage ./nix/packages/oauth2-simple.nix { };
                cradle = cradle.lib.${outerPrev.stdenv.hostPlatform.system}.mkCradle final;
                garnix =
                  (dontHaddock (
                    disableLibraryProfiling (
                      disableExecutableProfiling (final.callPackage ./nix/packages/garnix.nix { })
                    )
                  )).overrideAttrs
                    (old: { });
              };
            };
        })
        (final: prev: {
          opensearch-dashboards = final.callPackage ./nix/packages/opensearch-dashboards/default.nix { };

          # See https://github.com/NixOS/nixpkgs/issues/319323
          opensearch = prev.opensearch.overrideAttrs (old: {
            # Workaround for packaging bug (deleting opensearch-cli breaks
            # opensearch-plugin/opensearch-keystore command)
            installPhase = builtins.replaceStrings
              [ "rm $out/bin/opensearch-cli\n" ]
              [ "" ]
              old.installPhase;
          });
        })
      ];
    in
    flake-utils.lib.eachDefaultSystem
      (
        system:
        let
          pkgs = import nixpkgs { inherit system overlays; };
          lib = nixpkgs.lib;
          namespace = prefix: attrSet: lib.mapAttrs' (name: value: { name = "${prefix}_${name}";inherit value; }) attrSet;
          subDirInputs = {
            inherit system pkgs flakeInputs self;
            lib = nixpkgs.lib;
          };

          treefmt = import ./nix/treefmt.nix subDirInputs;
          backend = import ./backend subDirInputs;
          frontend = import ./frontend subDirInputs;
          frontend-age-wasm = import ./frontend/age-wasm subDirInputs;
          examples = import ./examples subDirInputs;
        in
        {
          apps = lib.mapAttrs
            (_: drv: {
              type = "app";
              program = lib.getExe drv;
              meta.description = drv.meta.description;
            })
            (namespace "backend" backend.commands //
            namespace "examples" examples.commands
            );

          checks =
            namespace "backend" backend.checks //
            namespace "frontend" frontend.checks //
            namespace "frontend" (namespace "ageWasm" frontend-age-wasm.checks);

          packages =
            namespace "backend" backend.packages //
            namespace "frontend" frontend.packages //
            namespace "frontend" (namespace "ageWasm" frontend-age-wasm.packages);

          formatter = treefmt.wrapper;

          devShells.default = pkgs.mkShell {
            shellHook = backend.shellHook;
            buildInputs = [
              pkgs.just
              pkgs.nil
              pkgs.nix
              (pkgs.callPackage ./nix/packages/withSecrets.nix { })
            ]
            ++ backend.devShellInputs
            ++ frontend.devShellInputs;
          };
        }
      )
    // {
      nixosModules = {
        garnix = ./nix/modules/garnix-server.nix;
        default = ./nix/modules/garnix-server.nix;
      };
      nixosConfigurations =
        (import ./examples/example-multi-server-deployment.nix {
          inherit self overlays flakeInputs;
        }).nixosConfigurations
        // (import ./examples/example-selfhost.nix {
          inherit self overlays flakeInputs;
        }).nixosConfigurations
        // (import ./examples/example-selfhost-vm.nix {
          inherit self overlays flakeInputs;
        }).nixosConfigurations;
    };
}
