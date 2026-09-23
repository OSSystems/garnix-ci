{ pkgs, flakeInputs, ... }:
let
  treefmt-config = {
    projectRootFile = "flake.nix";
    programs = {
      gofmt.enable = true;
      nixfmt.enable = true;
      shellcheck.enable = true;
      shfmt.enable = true;
      ormolu.enable = true;
      ormolu.package = pkgs.haskellPackages.ormolu;
      prettier.enable = true;
      deadnix = {
        enable = true;
        no-lambda-arg = true;
        no-lambda-pattern-names = true;
        no-underscore = true;
      };
    };
    settings.global.excludes = [
      "backend/test/spec/Integration/bad-flake-nix/flake.nix"
    ];
    settings.formatter.shellcheck = {
      excludes = [ ".envrc" ];
    };
    settings.formatter.prettier = {
      options = [
        "--trailing-comma"
        "all"
        "--no-error-on-unmatched-pattern"
      ];
      excludes = [
        "**/secrets/**"
        "**/*.md"
        "**/*.mdx"
        "**/*.json"
        # This file is intentionally invalid.
        "backend/test/spec/Integration/bad-yaml-config/garnix.yaml"
      ];
    };
  };
in
(flakeInputs.treefmt-nix.lib.evalModule pkgs treefmt-config).config.build
