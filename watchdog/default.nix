{ lib, pkgs, ... }:
let
  fs = lib.fileset;
  src = fs.toSource {
    root = ./.;
    fileset = fs.unions [
      ./package.yaml
      (fs.fileFilter (file: file.hasExt "hs") ./.)
    ];
  };
in
rec {
  package = (pkgs.haskellPackages.callCabal2nix "watchdog" src { }).overrideAttrs (attrs: {
    nativeBuildInputs = attrs.nativeBuildInputs ++ [ pkgs.makeWrapper ];
    postInstall = ''
      wrapProgram "$out/bin/watchdog" \
          --prefix PATH : ${
            pkgs.lib.makeBinPath [
              pkgs.git
              pkgs.nix
              pkgs.openssh
            ]
          }
    '';
  });
  nixosModule = import ./nixos-module.nix;
  commands = {
    watchdogSpec = pkgs.writeShellApplication {
      name = "watchdogSpec";
      runtimeInputs = [
        pkgs.ghcid
        pkgs.haskellPackages.cabal-install
        pkgs.haskellPackages.hpack
        (pkgs.haskellPackages.ghc.withPackages (p: package.buildInputs))
      ];
      text = ''
        cd watchdog
        hpack
        ghcid --warnings --allow-eval
      '';
    };
    watchdogRunLocally = pkgs.writeShellApplication {
      name = "watchdogRunLocally";
      runtimeInputs = [
        package
        pkgs.sops
      ];
      text = ''
        tempdir=$(mktemp -d)
        trap 'rm -rf "$tempdir"' EXIT
        export WATCHDOG_SSH_IDENTITY_FILE="$tempdir/watchdog_ssh_identity_file"
        sops --decrypt --extract "[\"watchdog_ssh\"]" ./secrets/dev.yaml > "$WATCHDOG_SSH_IDENTITY_FILE"
        chmod go-rwx "$WATCHDOG_SSH_IDENTITY_FILE"
        DATA_DIR=${./data}
        export DATA_DIR
        export PORT=5555
        watchdog --help
        watchdog "$@"
      '';
    };
  };
}
