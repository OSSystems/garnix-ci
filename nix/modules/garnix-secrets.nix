{ config
, lib
, pkgs
, ...
}:

let
  cfg = config.services.garnixServer;
  secretsCfg = cfg.secrets;

  pathOption =
    description:
    lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description =
        description
        + " (string path. Do NOT use a Nix path literal or the secret will be copied into the world-readable /nix/store).";
    };

  installedSecrets = lib.filter (s: s.sourcePath != null) [
    {
      name = "database-password";
      sourcePath = secretsCfg.databasePasswordPath;
      required = true;
    }
    {
      name = "github_webhook_secret";
      sourcePath = secretsCfg.githubWebhookSecretPath;
      required = true;
    }
    {
      name = "github_client_secret";
      sourcePath = secretsCfg.githubClientSecretPath;
      required = true;
    }
    {
      name = "github_client_id";
      sourcePath = secretsCfg.githubClientIdPath;
      required = true;
    }
    {
      name = "github_app_id";
      sourcePath = secretsCfg.githubAppIdPath;
      required = true;
    }
    {
      name = "github_app_pk";
      sourcePath = secretsCfg.githubAppPkPath;
      required = true;
    }
    {
      name = "opensearch-garnix";
      sourcePath = secretsCfg.opensearchCredentialPath;
      required = true;
    }
    {
      name = "garnix-jwt-key";
      sourcePath = secretsCfg.jwtKeyPath;
      required = true;
    }
    {
      name = "repo-secrets-key";
      sourcePath = secretsCfg.repoSecretsKeyPath;
      required = true;
    }
    {
      name = "repo-secrets-key-pub";
      sourcePath = secretsCfg.repoSecretsPubKeyPath;
      required = true;
    }
    {
      name = "s3-cache-access-key-id";
      sourcePath = secretsCfg.s3CacheAccessKeyIdPath;
      required = cfg.s3Cache.enable;
    }
    {
      name = "s3-cache-secret-access-key";
      sourcePath = secretsCfg.s3CacheSecretAccessKeyPath;
      required = cfg.s3Cache.enable;
    }
    {
      name = "cache-priv-key";
      sourcePath = secretsCfg.cachePrivKeyPath;
      required = cfg.s3Cache.enable;
    }
    # SSH identity files must not be group/world-readable or ssh refuses them
    # (StrictModes). Stage 0400 owned by the backend user instead of the
    # default 0440 root:garnix.
    {
      name = "garnix_server_remote_builder_ssh";
      sourcePath = secretsCfg.remoteBuilderSshPath;
      required = cfg.remoteBuilders.hosts != [ ];
      mode = "0400";
      owner = cfg.user;
    }
    {
      name = "garnix_action_runner_ssh";
      sourcePath = secretsCfg.actionRunnerSshPath;
      required = config.garnix.actionRunner.enable;
      mode = "0400";
      owner = cfg.user;
    }
  ];

  requiredSecrets = [
    {
      option = "secrets.databasePasswordPath";
      value = secretsCfg.databasePasswordPath;
      required = true;
    }
    {
      option = "secrets.githubWebhookSecretPath";
      value = secretsCfg.githubWebhookSecretPath;
      required = true;
    }
    {
      option = "secrets.githubClientSecretPath";
      value = secretsCfg.githubClientSecretPath;
      required = true;
    }
    {
      option = "secrets.githubClientIdPath";
      value = secretsCfg.githubClientIdPath;
      required = true;
    }
    {
      option = "secrets.githubAppIdPath";
      value = secretsCfg.githubAppIdPath;
      required = true;
    }
    {
      option = "secrets.githubAppPkPath";
      value = secretsCfg.githubAppPkPath;
      required = true;
    }
    {
      option = "secrets.opensearchCredentialPath";
      value = secretsCfg.opensearchCredentialPath;
      required = true;
    }
    {
      option = "secrets.jwtKeyPath";
      value = secretsCfg.jwtKeyPath;
      required = true;
    }
    {
      option = "secrets.repoSecretsKeyPath";
      value = secretsCfg.repoSecretsKeyPath;
      required = true;
    }
    {
      option = "secrets.repoSecretsPubKeyPath";
      value = secretsCfg.repoSecretsPubKeyPath;
      required = true;
    }
    {
      option = "secrets.s3CacheAccessKeyIdPath";
      value = secretsCfg.s3CacheAccessKeyIdPath;
      required = cfg.s3Cache.enable;
    }
    {
      option = "secrets.s3CacheSecretAccessKeyPath";
      value = secretsCfg.s3CacheSecretAccessKeyPath;
      required = cfg.s3Cache.enable;
    }
    {
      option = "secrets.cachePrivKeyPath";
      value = secretsCfg.cachePrivKeyPath;
      required = cfg.s3Cache.enable;
    }
    {
      option = "secrets.remoteBuilderSshPath";
      value = secretsCfg.remoteBuilderSshPath;
      required = cfg.remoteBuilders.hosts != [ ];
    }
    {
      option = "secrets.actionRunnerSshPath";
      value = secretsCfg.actionRunnerSshPath;
      required = config.garnix.actionRunner.enable;
    }
  ];

  stageScript = pkgs.writeShellApplication {
    name = "garnix-stage-secrets";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      set -euo pipefail

      install -d -m 0750 -o root -g ${cfg.user} ${secretsCfg.dir}
    ''
    + lib.concatMapStringsSep "\n"
      (s: ''
        install -m ${s.mode or "0440"} -o ${s.owner or "root"} -g ${cfg.user} ${s.sourcePath} ${secretsCfg.dir}/${s.name}
      '')
      installedSecrets;
  };
in
{
  options.services.garnixServer.secrets = {
    dir = lib.mkOption {
      type = lib.types.str;
      default = "/run/garnix-secrets";
      description = ''
        Directory the module stages secrets into, and that the backend reads
        from (exported as GARNIX_SECRETS_DIR). Deliberately separate from
        /run/secrets so we never collide with sops-nix, which owns that
        directory and wipes entries it does not manage on every reconcile.
      '';
    };
    databasePasswordPath = pathOption "Path to the Postgres password file";
    githubWebhookSecretPath = pathOption "Path to the GitHub webhook secret file";
    githubClientSecretPath = pathOption "Path to the GitHub OAuth client secret file";
    githubClientIdPath = pathOption "Path to the GitHub OAuth client id file";
    githubAppIdPath = pathOption "Path to the GitHub App ID file";
    githubAppPkPath = pathOption "Path to the GitHub App private key (PEM) file";
    opensearchCredentialPath = pathOption "Path to the OpenSearch basic-auth password file";
    jwtKeyPath = pathOption "Path to the JWT signing key file (generated via Servant.Auth.Server.writeKey)";
    repoSecretsKeyPath = pathOption "Path to the repository secrets age private key";
    repoSecretsPubKeyPath = pathOption "Path to the repository secrets age public key";
    s3CacheAccessKeyIdPath = pathOption "Path to the S3 access-key-id file (required iff s3Cache.enable)";
    s3CacheSecretAccessKeyPath = pathOption "Path to the S3 secret-access-key file (required iff s3Cache.enable)";
    cachePrivKeyPath = pathOption "Path to the Nix cache signing private key (required iff s3Cache.enable)";
    remoteBuilderSshPath = pathOption "Path to the SSH key used to reach remote Nix builders (required iff remoteBuilders.hosts != [])";
    actionRunnerSshPath = pathOption "Path to the SSH private key the backend uses to reach the action-runner (required iff garnix.actionRunner.enable). Staged at <secrets.dir>/garnix_action_runner_ssh, the path the backend reads by default.";
  };

  config = lib.mkIf cfg.enable {
    assertions = lib.optionals (!config.garnix.devMode.enable) (
      lib.map
        (s: {
          assertion = !s.required || s.value != null;
          message = "services.garnixServer.${s.option} must be set.";
        })
        requiredSecrets
    );

    systemd.services.garnix-secrets-stage = {
      description = "Stage garnix secrets into ${secretsCfg.dir}";
      wantedBy = [ "multi-user.target" ];
      before = [
        "garnixServer.service"
        "frontend.service"
        "fluent-bit.service"
      ];
      after = [ "local-fs.target" ];
      unitConfig.RequiresMountsFor = "/run";
      restartTriggers = [ stageScript ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = lib.getExe stageScript;
      };
    };
  };
}
