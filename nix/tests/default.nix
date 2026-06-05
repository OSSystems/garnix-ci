{ flake, pkgs }:
let
  inherit (pkgs) lib;

  runTest = { testModule, overlays ? [ ] }: flake.inputs.nixpkgs.lib.nixos.runTest rec {
    imports = [
      testModule
    ];

    hostPkgs = import flake.inputs.nixpkgs {
      inherit (pkgs) system;
      overlays = pkgs.overlays ++ overlays;
    };

    defaults = { nodes, lib, config, ... }: {
      imports = [ flake.nixosModules.nixos ];
      garnix.devMode.enable = true;

      networking.usePredictableInterfaceNames = lib.mkForce false;
      systemd.network.networks."10-uplink".matchConfig.Name = lib.mkForce "eth0";

      # Remove caches that are added by default but cause time-outs in the tests.
      # Since cache.nixos.org is added by nixpkgs at the default priority, we are
      # obliged to use mkForce here.
      # To add any caches in the tests itself, you should therefore use mkForce as well.
      nix.settings.substituters = lib.mkForce [ ];

      # All nodes get the exact same SSH host key, so let's generate
      # entries for all of them in their global known hosts files so that
      # they can all trust each other.
      programs.ssh.knownHosts = lib.mkForce (lib.mapAttrs
        (_: node: {
          hostNames = [
            node.networking.hostName
            node.networking.primaryIPAddress
          ];
          publicKeyFile = ../data/ssh-key-for-local-dev-secrets.pub;
        })
        nodes
      );
      garnix.monitoring-client.enable = false;
    };

    _module.args = {
      inherit (flake) nixosModules;
    };

    node.pkgs = hostPkgs;
  };

  generatePerMachineTest = pkgs: lib.mapAttrs' (name: { extraModules ? [ ], scriptFun ? _: "" }:
    let
      cleanName = lib.replaceStrings [ "-" ] [ "_" ] name;
    in
    lib.nameValuePair "perMachineTests-${cleanName}"
      (runTest {
        testModule = { lib, nixosModules, ... }: {
          name = cleanName;
          nodes.${cleanName} = { lib, ... }: {
            imports = [ nixosModules.${name} ] ++ extraModules;
            # The machine name is derived from the host name, and cannot contain dashes
            networking.hostName = lib.mkForce cleanName;
            virtualisation.memorySize = 2048;
            garnix.monitoring-client.enable = lib.mkForce true;
          };
          testScript = { nodes, ... }: ''
            import json

            start_all()
            ${cleanName}.wait_for_unit("multi-user.target")
            ${scriptFun cleanName}

            ${if cleanName == "garnix_server1"
              then
              ''
                with subtest("periodic garbage collection enabled"):
                  ${cleanName}.succeed("systemctl status 'custom-gc.timer'")
              ''
              else if nodes.${cleanName}.garnix.builder.enable
              then
              ''
              ''
              else
              ''
                with subtest("periodic garbage collection enabled"):
                  ${cleanName}.succeed("systemctl status 'nix-gc.timer'")
              ''
            }

            (_, failed_units_str) = ${cleanName}.systemctl("list-units --failed --output=json")
            failed_units = json.loads(failed_units_str)
            assert not failed_units, f"failed units: {', '.join([ unit['unit'] for unit in failed_units ])}"
          '';
        };
      })
  );

  x86MachineTests = {
    garnix-server1 = {
      scriptFun = name: ''
        ${name}.wait_for_unit("garnixServer.service")
        with subtest("sql exporter is running"):
          import re
          ${name}.wait_for_unit("prometheus-sql-exporter.service")
          sql_metrics = ${name}.wait_until_succeeds("curl -v http://127.0.01:9237/metrics | grep -E '^sql_build_.+'")
          assert re.search(re.compile(r'^sql_build_stalled\{.+\} 0', re.MULTILINE), sql_metrics)
          assert re.search(re.compile(r'^sql_build_running{.+} 0', re.MULTILINE), sql_metrics)
          assert re.search(re.compile(r'^sql_build_total{.+} 0', re.MULTILINE), sql_metrics)
      '';
      extraModules = [
        ({ config, ... }: {
          networking.extraHosts = ''
            127.0.0.1 ${config.garnix.database.fqdn}
            ::1 ${config.garnix.database.fqdn}
          '';
        })
      ];
    };
    garnix5 = { };
    monitoring = { };
    opensearch1 = {
      extraModules = [
        {
          garnix.opensearch = {
            bindIP = lib.mkForce "127.0.0.1";
            nodesIPs = lib.mkForce [ ];
            initialClusterManagerNodes = lib.mkForce [ ];
          };
        }
      ];
    };
    opensearch2 = {
      extraModules = [
        {
          garnix.opensearch = {
            bindIP = lib.mkForce "127.0.0.1";
            nodesIPs = lib.mkForce [ ];
            initialClusterManagerNodes = lib.mkForce [ ];
          };
        }
      ];
    };
    opensearch3 = {
      extraModules = [
        {
          garnix.opensearch = {
            bindIP = lib.mkForce "127.0.0.1";
            nodesIPs = lib.mkForce [ ];
            initialClusterManagerNodes = lib.mkForce [ ];
          };
        }
      ];
    };
    db1 = {
      scriptFun = name: ''
        ${name}.wait_for_unit("postgresql.service")
        ${name}.wait_for_unit("prometheus-sql-exporter.service")
      '';
    };
  };

  aarch64MachineTests = {
    arm-server-0 = { };
    arm-server-1 = { };
  };

  testModules = [
    "monitoring-node"
    "monitoring-server"
    "fail2ban"
    "opensearch"
    "fluent-bit"
    "database"
    "garnixServer"
  ];
in
(lib.genAttrs testModules (name:
  let testFile = import ./${name}.nix;
  in runTest (if testFile ? testModule then testFile else { testModule = testFile; })
))
//
lib.optionalAttrs (pkgs.stdenv.isx86_64) (generatePerMachineTest pkgs x86MachineTests)
  //
lib.optionalAttrs (pkgs.stdenv.isAarch64) (generatePerMachineTest pkgs aarch64MachineTests)
