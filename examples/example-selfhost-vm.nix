{ self, overlays, flakeInputs }:
let
  # Public key for the VM-test SSH key. Pair is generated outside Nix
  # under /tmp/garnix-vm-test/. Replace if you regenerate the key.
  testSshPubKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEqFnxnhIIUixQpzrdtXLLe8yJPwuVLEbW+RxZITNx1p garnix-vm-test";
in
{
  nixosConfigurations.selfhostVm = flakeInputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = {
      inherit flakeInputs;
      flakePackages = self.packages.x86_64-linux;
    };
    modules = [
      { nixpkgs.overlays = overlays; }
      self.nixosModules.garnix
      ({ pkgs, lib, config, ... }:
        let
          # Dummy secret content materialised into /nix/store. This is a test
          # VM, so the store leak is acceptable.
          mkSecret = name: content: pkgs.writeText "garnix-vm-secret-${name}" content;

          databasePasswordFile = mkSecret "database-password" "postgres";
          githubWebhookSecretFile = mkSecret "github-webhook-secret" "dev-webhook-secret";
          githubClientSecretFile = mkSecret "github-client-secret" "dev-client-secret";
          githubClientIdFile = mkSecret "github-client-id" "dev-client-id";
          githubAppIdFile = mkSecret "github-app-id" "12345";
          opensearchCredentialFile = mkSecret "opensearch-credential" "dev-opensearch-password";

          # The JWT key path is base64-decoded then handed to
          # Servant.Auth.Server.fromSecret. 32 raw bytes base64-encoded works.
          # ("dev-jwt-key-32-bytes-padding!!!" is 32 chars -> base64.)
          jwtKeyFile = mkSecret "jwt-key" "ZGV2LWp3dC1rZXktMzItYnl0ZXMtcGFkZGluZyEhIQ==";

          # Real RSA PEM private key (parsed by readRsaPem on startup).
          githubAppPkFile = pkgs.runCommand "garnix-vm-secret-github-app-pk"
            { nativeBuildInputs = [ pkgs.openssl ]; } ''
            openssl genrsa -out $out 2048
          '';

          # Real age key pair (the backend may not read these during startup,
          # but generate real ones so any eager validation passes).
          ageKeyPair = pkgs.runCommand "garnix-vm-age-keypair"
            { nativeBuildInputs = [ pkgs.age ]; } ''
            mkdir -p $out
            age-keygen -o $out/key 2>$out/pub.raw
            # age-keygen prints "Public key: <pub>" to stderr.
            grep -oE 'age1[a-z0-9]+' $out/pub.raw > $out/pub
          '';
          repoSecretsKeyFile = "${ageKeyPair}/key";
          repoSecretsPubKeyFile = "${ageKeyPair}/pub";
        in
        {
          # VM disk plumbing (overridden in vmVariant anyway).
          fileSystems."/".device = "/dev/disk/by-label/nixos";
          fileSystems."/".fsType = "ext4";
          boot.loader.systemd-boot.enable = true;
          system.stateVersion = "25.11";
          networking.hostName = "garnix-selfhost-vm";

          # Allow VM SSH login as root.
          users.users.root.openssh.authorizedKeys.keys = [ testSshPubKey ];
          services.openssh.settings.PermitRootLogin = lib.mkForce "yes";

          garnix.devMode.enable = true;

          services.garnixServer = {
            enable = true;
            hostname = "garnix.test";
            url = "http://garnix.test";
            adminGithubLogin = "test";
            githubAppName = "test";
            acmeEmail = null;

            database = {
              host = "127.0.0.1";
              port = 5432;
              user = "garnix";
              name = "garnix";
              ssl.mode = "disable";
            };

            opensearch = {
              # Unreachable but populated (assertions require non-empty).
              url = "http://127.0.0.1:9999/_msearch";
              host = "127.0.0.1";
              username = "garnix";
            };

            s3Cache.enable = false;
            remoteBuilders.hosts = [ ];

            secrets = {
              databasePasswordPath = builtins.toString databasePasswordFile;
              githubWebhookSecretPath = builtins.toString githubWebhookSecretFile;
              githubClientSecretPath = builtins.toString githubClientSecretFile;
              githubClientIdPath = builtins.toString githubClientIdFile;
              githubAppIdPath = builtins.toString githubAppIdFile;
              githubAppPkPath = builtins.toString githubAppPkFile;
              opensearchCredentialPath = builtins.toString opensearchCredentialFile;
              jwtKeyPath = builtins.toString jwtKeyFile;
              repoSecretsKeyPath = repoSecretsKeyFile;
              repoSecretsPubKeyPath = repoSecretsPubKeyFile;
            };
          };

          # Local postgres for the test.
          # Note: initialScript runs *before* ensureUsers, so we cannot ALTER
          # garnix to set a password there (it does not exist yet). Instead
          # we set the password via a post-start hook on postgresql.service.
          services.postgresql = {
            enable = true;
            ensureDatabases = [ "garnix" ];
            ensureUsers = [{
              name = "garnix";
              ensureDBOwnership = true;
            }];
            authentication = lib.mkForce ''
              # TYPE  DATABASE  USER     ADDRESS         METHOD
              local   all       all                      trust
              host    all       all      127.0.0.1/32    md5
              host    all       all      ::1/128         md5
            '';
          };

          # postgresql `initialScript` runs before `ensureUsers`, so we can't
          # set the password there. Use a separate oneshot that fires after
          # postgresql has finished initialising the garnix role.
          systemd.services.garnix-vm-pg-password = {
            description = "Set garnix postgres user password for VM testing";
            wantedBy = [ "multi-user.target" "garnixServer.service" ];
            before = [ "garnixServer.service" ];
            after = [ "postgresql.service" "postgresql-setup.service" ];
            requires = [ "postgresql.service" ];
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              User = "postgres";
            };
            script = ''
              ${config.services.postgresql.package}/bin/psql -tAc \
                "ALTER USER garnix WITH PASSWORD 'postgres';"
            '';
          };

          # Forward host 2222 -> guest 22 for the SSH-in step.
          virtualisation.vmVariant = {
            virtualisation.forwardPorts = [
              {
                from = "host";
                host.port = 2222;
                guest.port = 22;
              }
            ];
          };
        })
    ];
  };
}
