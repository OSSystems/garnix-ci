{
  self,
  flakeInputs,
  overlays,
}:
let
  mkSystem =
    module:
    flakeInputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        inherit flakeInputs;
        flakePackages = self.packages.x86_64-linux;
      };
      modules = [
        { nixpkgs.overlays = overlays; }
        self.nixosModules.garnix
        module
      ];
    };

  # Minimum host plumbing. Replace with your real disk/boot setup.
  hostPlumbing = {
    fileSystems = {
      "/" = {
        device = "/dev/disk/by-label/nixos";
        fsType = "ext4";
      };
    };
    boot.loader.systemd-boot.enable = true;
    system.stateVersion = "25.11";
  };

  opensearchHost = {
    inherit (hostPlumbing) boot fileSystems system;

    imports = [
      flakeInputs.sops-nix.nixosModules.sops
      ../opensearch/nixos-module.nix
    ];

    sops.defaultSopsFile = ../secrets/dev.yaml;
    services.openssh.enable = true;

    garnix.opensearch = {
      enable = true;
      fqdn = "opensearch.internal.example.com";
      dashboards.enable = true;
      isSingleNode = true;
      bindIP = "127.0.0.1";
    };
  };
in
{
  nixosConfigurations = {
    exampleSelfhost = mkSystem {
      inherit (hostPlumbing) boot fileSystems system;
      networking.hostName = "garnix-selfhost";

      services.garnixServer = {
        enable = true;
        hostname = "garnix.example.com";
        url = "https://garnix.example.com";
        adminGithubLogin = "your-github-login";
        githubAppName = "your-github-app-slug";
        acmeEmail = "ops@example.com";

        database = {
          host = "postgres.internal.example.com";
          port = 5432;
          user = "garnix";
          name = "garnix";
          ssl.mode = "verify-full";
        };

        opensearch = {
          url = "https://opensearch.internal.example.com/_msearch";
          host = "opensearch.internal.example.com";
          username = "garnix";
        };

        # S3 cache disabled in this example. To enable, set s3Cache.enable = true
        # and fill in the bucket/host/baseUrl/access-key/secret-key paths.
        s3Cache.enable = false;

        # Build locally. Populate this list with SSH-reachable Nix builders
        # if you want to distribute builds.
        remoteBuilders.hosts = [ ];

        # Per-secret paths. With sops-nix you'd substitute these for
        # config.sops.secrets.<name>.path entries. Without sops-nix, point
        # them at plain files staged out-of-band (Vault, Ansible, etc.).
        secrets = {
          databasePasswordPath = "/var/lib/garnix/secrets/database-password";
          githubWebhookSecretPath = "/var/lib/garnix/secrets/github_webhook_secret";
          githubClientSecretPath = "/var/lib/garnix/secrets/github_client_secret";
          githubClientIdPath = "/var/lib/garnix/secrets/github_client_id";
          githubAppIdPath = "/var/lib/garnix/secrets/github_app_id";
          githubAppPkPath = "/var/lib/garnix/secrets/github_app_pk";
          opensearchCredentialPath = "/var/lib/garnix/secrets/opensearch-garnix";
          jwtKeyPath = "/var/lib/garnix/secrets/garnix-jwt-key";
          repoSecretsKeyPath = "/var/lib/garnix/secrets/repo-secrets-key";
          repoSecretsPubKeyPath = "/var/lib/garnix/secrets/repo-secrets-key-pub";
          # SSH private key the backend uses to reach the action-runner
          # (loopback by default). Its public half must be in
          # garnix.actionRunner.authorizedKey.
          actionRunnerSshPath = "/var/lib/garnix/secrets/garnix_action_runner_ssh";
        };
      };

      # Example sops-nix wiring (uncomment if you use sops). You still set the
      # *Path options above to config.sops.secrets.<name>.path so the
      # secrets-stage service knows where to find each file.
      #
      # imports = [ flakeInputs.sops-nix.nixosModules.sops ];
      # sops.defaultSopsFile = ./secrets.yaml;
      # sops.secrets = {
      #   database-password = { mode = "0400"; owner = "root"; };
      #   github_webhook_secret = { mode = "0400"; owner = "root"; };
      #   # ... etc
      # };
    };
    exampleMonitoringLoopback =
      let
        hostName = "garnix-monitoring";
      in
      mkSystem {
        inherit (hostPlumbing) boot fileSystems system;
        networking.hostName = hostName;
        services.nginx.enable = true;
        garnix = {
          monitoring.monitoredHosts.${hostName} = {
            fqdn = "127.0.0.1";
            proxied = false;
            scrapeNginx = true;
            scrapeNginxLog = true;
            scrapeGarnixServer = true;
          };
          monitoring-server = {
            enable = true;
            fqdn = "monitoring.example.com";
            grafana.secretKeyFile = "/var/lib/garnix/secrets/grafana-secret-key";
            nginx.enable = false;
            watchdog.enable = false;
          };
          monitoring-client = {
            enable = true;
            nginx.enable = false;
          };
        };
      };
    exampleMonitoringSplit = mkSystem {
      inherit (hostPlumbing) boot fileSystems system;
      networking.hostName = "monitoring";
      security.acme = {
        acceptTerms = true;
        defaults.email = "ops@example.com";
      };
      garnix = {
        monitoring = {
          domain = "example.com";
          basicAuth.passwordFile = "/var/lib/garnix/secrets/prometheus-basic-auth";
          monitoredHosts = {
            monitoring = { };
            web1 = {
              scrapeNginx = true;
              scrapeNginxLog = true;
              scrapeGarnixServer = true;
            };
          };
        };
        monitoring-server = {
          enable = true;
          fqdn = "monitoring.example.com";
          grafana.secretKeyFile = "/var/lib/garnix/secrets/grafana-secret-key";
          watchdog.enable = false;
        };
      };
    };
    exampleMonitoringSplitClient = mkSystem {
      inherit (hostPlumbing) boot fileSystems system;
      networking.hostName = "web1";
      security.acme = {
        acceptTerms = true;
        defaults.email = "ops@example.com";
      };
      garnix = {
        monitoring = {
          domain = "example.com";
          basicAuth.passwordFile = "/var/lib/garnix/secrets/prometheus-basic-auth";
          monitoredHosts.web1 = {
            scrapeNginx = true;
            scrapeNginxLog = true;
            scrapeGarnixServer = true;
          };
        };
        monitoring-client.enable = true;
      };
    };
    exampleOpenSearchExternalTls = mkSystem {
      imports = [ opensearchHost ];
      networking.hostName = "opensearch-tls-upstream";

      garnix.opensearch.nginx.acme.enable = false;
    };
    exampleOpenSearchExternalIngress = mkSystem {
      imports = [ opensearchHost ];
      networking.hostName = "opensearch-ingress";

      garnix.opensearch.nginx.enable = false;
    };
  };
}
