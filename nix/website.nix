{ self, overlays, flakeInputs }:
let
  system = "x86_64-linux";
in
{
  nixosConfigurations.website = flakeInputs.nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = {
      inherit flakeInputs;
      flakePackages = self.packages.${system};
    };
    modules = [
      { nixpkgs.overlays = overlays; }
      self.nixosModules.garnix-guest
      self.nixosModules.garnix
      ({ pkgs, lib, config, ... }:
        let
          mkSecret = name: content: pkgs.writeText "garnix-website-secret-${name}" content;

          databasePassword = "postgres";
          databasePasswordFile = mkSecret "database-password" databasePassword;
          githubWebhookSecretFile = mkSecret "github-webhook-secret" "demo-webhook-secret";
          githubClientSecretFile = mkSecret "github-client-secret" "demo-client-secret";
          githubClientIdFile = mkSecret "github-client-id" "demo-client-id";
          githubAppIdFile = mkSecret "github-app-id" "12345";
          opensearchCredentialFile = mkSecret "opensearch-credential" "demo-opensearch-password";
          jwtKeyFile = mkSecret "jwt-key" "ZGV2LWp3dC1rZXktMzItYnl0ZXMtcGFkZGluZyEhIQ==";

          githubAppPkFile = pkgs.runCommand "garnix-website-secret-github-app-pk"
            { nativeBuildInputs = [ pkgs.openssl ]; } ''
            openssl genrsa -out $out 2048
          '';

          ageKeyPair = pkgs.runCommand "garnix-website-age-keypair"
            { nativeBuildInputs = [ pkgs.age ]; } ''
            mkdir -p $out
            age-keygen -o $out/key 2>$out/pub.raw
            grep -oE 'age1[a-z0-9]+' $out/pub.raw > $out/pub
          '';
        in
        {
          networking.hostName = "garnix-website";

          garnix.devMode.enable = true;
          garnix.actionRunner.enable = false;
          garnix.fluent-bit.enable = false;

          services.garnixServer = {
            enable = true;

            hostname = "website.garnix.local";
            url = "http://website.garnix.local";

            adminGithubLogin = "dev-user";
            githubAppName = "garnix-ci";
            acmeEmail = null;

            testFeatures = [ "DevApi" "OpenSearchMocks" "CacheUploadMocks" ];

            database = {
              host = "127.0.0.1";
              port = 5432;
              user = "garnix";
              name = "garnix";
              ssl.mode = "disable";
            };

            opensearch = {
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
              repoSecretsKeyPath = "${ageKeyPair}/key";
              repoSecretsPubKeyPath = "${ageKeyPair}/pub";
            };
          };

          services.postgresql = {
            enable = true;
            ensureDatabases = [ "garnix" ];
            ensureUsers = [{
              name = "garnix";
              ensureDBOwnership = true;
            }];
            authentication = lib.mkForce ''
              local   all       all                      trust
              host    all       all      127.0.0.1/32    md5
              host    all       all      ::1/128         md5
            '';
          };

          systemd.services.garnix-website-pg-password = {
            description = "Set the garnix postgres password";
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
                "ALTER USER garnix WITH PASSWORD '${databasePassword}';"
            '';
          };

          systemd.services.garnix-website-fixtures = {
            description = "Seed the demo database from sql/local-fixtures.sql";
            wantedBy = [ "multi-user.target" ];
            after = [ "garnixServer.service" ];
            requires = [ "garnixServer.service" ];
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              User = "postgres";
            };
            script = ''
              ${config.services.postgresql.package}/bin/psql \
                --dbname garnix --file ${../sql/local-fixtures.sql}
            '';
          };
        })
    ];
  };
}
