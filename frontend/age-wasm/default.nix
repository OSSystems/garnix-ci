{ pkgs, lib, ... }: rec {
  packages =
    {
      default =
        let
          age-wasm = pkgs.buildGoModule.override
            { go = pkgs.go // { GOOS = "js"; GOARCH = "wasm"; }; }
            {
              name = "agewasm";
              src = builtins.filterSource
                (path: type:
                  lib.hasSuffix ".go" path ||
                  lib.hasSuffix "/go.mod" path ||
                  lib.hasSuffix "/go.sum" path
                )
                ./.;
              vendorHash = "sha256-icYRGdAd6G3yzBUyhlvIn/FvAC9+FHfX7g2DywYQJ8g=";
            };

          ts-bindings = pkgs.runCommand "age-ts-bindings"
            { buildInputs = [ pkgs.typescript ]; }
            "tsc --outDir $out --project ${
        builtins.filterSource
          (path: type:
            lib.hasSuffix ".ts" path ||
              lib.hasSuffix "/tsconfig.json" path
          ) ./.
      }";
        in
        pkgs.runCommand "age-wasm-and-ts-bindings" { buildInputs = [ pkgs.binaryen ]; } ''
          mkdir $out
          cp ${ts-bindings}/* $out
          cp ${pkgs.go}/share/go/lib/wasm/wasm_exec.js $out/wasm_exec.js
          wasm-opt --enable-bulk-memory --enable-nontrapping-float-to-int --enable-sign-ext -Oz ${age-wasm}/bin/js_wasm/age-wasm -o $out/age.wasm
        '';
    };

  checks.test-age-wasm-encrypt = pkgs.runCommand "test-age-wasm-encrypt"
    { buildInputs = [ pkgs.nodejs pkgs.age ]; }
    ''
      node ${./integration-test.mjs} ${lib.escapeShellArg (toString packages.default)}
      touch $out
    '';

  devShellInputs = with pkgs; [
    go
    gopls
  ];
}
