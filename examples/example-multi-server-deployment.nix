{ self, overlays, flakeInputs }:
let
  fleet = {
    garnix5 = { fqdn = "prometheus-node-exporter.5.garnix.io"; };
    garnix6 = { fqdn = "prometheus-node-exporter.6.garnix.io"; };
    garnix7 = { fqdn = "prometheus-node-exporter.7.garnix.io"; };
    garnix8 = { fqdn = "prometheus-node-exporter.8.garnix.io"; };
    garnix9 = { fqdn = "prometheus-node-exporter.9.garnix.io"; };
    arm-server-0 = { };
    arm-1 = { };
    opensearch1 = { };
    opensearch2 = { };
    opensearch3 = { };
    monitoring = { };
    db1 = { };
    garnix-server1 = { scrapeNginx = true; scrapeNginxLog = true; scrapeGarnixServer = true; };
    garnix-server2 = { scrapeNginx = true; scrapeNginxLog = true; scrapeGarnixServer = true; };
    action-runner2 = { };
    macMini1 = {
      fqdn = "macMini1.garnix.io";
      proxied = false;
      port = 9100;
    };
    macMini2 = {
      fqdn = "macMini2.garnix.io";
      proxied = false;
      port = 9100;
    };
  };

  monitoring = {
    domain = "garnix.io";
    monitoredHosts = fleet;
  };

  # The 11 hard-coded remote builders previously baked into
  # backend/nixos-module.nix, kept here as dev tooling.
  bigBuilders = builtins.map
    (h: {
      inherit (h) name hostname systems maxJobs supportedFeatures;
      user = "nix-ssh";
      speedFactor = h.speedFactor or 1;
      mandatoryFeatures = [ ];
    }) [
    {
      name = "macMini1";
      hostname = "142.132.141.88";
      systems = [ "aarch64-darwin" "x86_64-darwin" ];
      maxJobs = 4;
      supportedFeatures = [ "big-parallel" "recursive-nix" ];
    }
    {
      name = "macMini2";
      hostname = "142.132.141.89";
      systems = [ "aarch64-darwin" "x86_64-darwin" ];
      maxJobs = 4;
      supportedFeatures = [ "big-parallel" "recursive-nix" ];
    }
    {
      name = "garnix5";
      hostname = "65.108.28.108";
      systems = [ "x86_64-linux" "i686-linux" ];
      maxJobs = 28;
      speedFactor = 4;
      supportedFeatures = [ "nixos-test" "kvm" "big-parallel" "recursive-nix" ];
    }
    {
      name = "garnix6";
      hostname = "65.108.28.106";
      systems = [ "x86_64-linux" "i686-linux" ];
      maxJobs = 28;
      speedFactor = 4;
      supportedFeatures = [ "nixos-test" "kvm" "big-parallel" "recursive-nix" ];
    }
    {
      name = "garnix7";
      hostname = "65.108.28.107";
      systems = [ "x86_64-linux" "i686-linux" ];
      maxJobs = 28;
      speedFactor = 4;
      supportedFeatures = [ "nixos-test" "kvm" "big-parallel" "recursive-nix" ];
    }
    {
      name = "garnix8";
      hostname = "88.99.75.150";
      systems = [ "x86_64-linux" "i686-linux" ];
      maxJobs = 28;
      speedFactor = 4;
      supportedFeatures = [ "nixos-test" "kvm" "big-parallel" "recursive-nix" ];
    }
    {
      name = "garnix9";
      hostname = "157.90.140.190";
      systems = [ "x86_64-linux" "i686-linux" ];
      maxJobs = 28;
      speedFactor = 4;
      supportedFeatures = [ "nixos-test" "kvm" "big-parallel" "recursive-nix" ];
    }
    {
      name = "arm-server-0";
      hostname = "65.109.75.126";
      systems = [ "aarch64-linux" ];
      maxJobs = 60;
      speedFactor = 4;
      supportedFeatures = [ "nixos-test" "kvm" "big-parallel" "recursive-nix" ];
    }
    {
      name = "arm-server-1";
      hostname = "91.107.205.127";
      systems = [ "aarch64-linux" ];
      maxJobs = 8;
      supportedFeatures = [ "nixos-test" "kvm" "big-parallel" "recursive-nix" ];
    }
  ];
in
{
  nixosConfigurations = {
    exampleGarnixServer = flakeInputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        inherit flakeInputs;
        flakePackages = self.packages.x86_64-linux;
      };
      modules = [
        { nixpkgs.overlays = overlays; }
        self.nixosModules.garnix
        {
          fileSystems."/".device = "foo";
          fileSystems."/".fsType = "ext4";
          boot.loader.systemd-boot.enable = true;
          system.stateVersion = "25.11";
          services.garnixServer = {
            enable = true;
            hostname = "garnix.example.dev";
            adminGithubLogin = "jkarni";
            githubAppName = "test-app-jkarni";
            opensearch = {
              url = "http://exampleOpenSearch/_msearch";
              host = "exampleOpenSearch";
            };
            database = {
              host = "exampleDb";
              port = 9178;
            };
            testFeatures = [ "DevApi" ];
            remoteBuilders.hosts = bigBuilders;
          };
          garnix = {
            devMode.enable = true;
            fluent-bit.enable = true;
            inherit monitoring;
          };
        }
      ];
    };

    exampleDb = flakeInputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        inherit flakeInputs;
        flakePackages = self.packages.x86_64-linux;
      };
      modules = [
        { nixpkgs.overlays = overlays; }
        flakeInputs.sops-nix.nixosModules.sops
        ../nix/modules/common.nix
        ../nix/modules/custom-gc.nix
        ../nix/modules/database.nix
        ../nix/modules/dev-mode.nix
        ../nix/modules/linux-common.nix
        ../nix/modules/monitoring-client.nix
        {
          fileSystems."/".device = "foo";
          fileSystems."/".fsType = "ext4";
          boot.loader.systemd-boot.enable = true;
          system.stateVersion = "25.11";
          sops.defaultSopsFile = ../secrets/dev.yaml;
          garnix = {
            devMode.enable = true;
            inherit monitoring;
            monitoring-client.nodeId = "db1";
            database = {
              enable = true;
              fqdn = "exampleDb";
              allowedIPs = [ "0.0.0.0/0" ];
            };
          };
        }
      ];
    };

    exampleOpenSearch = flakeInputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        inherit flakeInputs;
        flakePackages = self.packages.x86_64-linux;
      };
      modules = [
        { nixpkgs.overlays = overlays; }
        flakeInputs.sops-nix.nixosModules.sops
        ../nix/modules/common.nix
        ../nix/modules/custom-gc.nix
        ../nix/modules/dev-mode.nix
        ../nix/modules/linux-common.nix
        ../nix/modules/monitoring-client.nix
        ../opensearch/nixos-module.nix
        ({ lib, config, ... }: {
          fileSystems."/".device = "foo";
          fileSystems."/".fsType = "ext4";
          boot.loader.systemd-boot.enable = true;
          system.stateVersion = "25.11";
          sops.defaultSopsFile = ../secrets/dev.yaml;
          garnix = {
            devMode.enable = true;
            inherit monitoring;
            monitoring-client.nodeId = "opensearch1";
            ipv6Address = "TODO";
            opensearch = {
              enable = true;
              fqdn = "exampleOpenSearch";
              dashboards.enable = true;
              isSingleNode = true;
              bindIP = (lib.head config.networking.interfaces.eth1.ipv4.addresses).address;
            };
          };
        })
      ];
    };
  };
}
