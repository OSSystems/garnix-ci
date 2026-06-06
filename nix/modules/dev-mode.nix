{ config
, lib
, modulesPath
, ...
}:

let
  devCerts = config.garnix.devMode.certificates;
in

{
  options.garnix.devMode = {
    enable = lib.mkEnableOption "mode for testing and development VMs";

    disableACME = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };

    certificates = lib.mkOption {
      type = lib.types.raw;
      default = import "${modulesPath}/../tests/common/acme/server/snakeoil-certs.nix";
      readOnly = true;
    };

    withDevCerts = lib.mkOption {
      type = lib.types.functionTo lib.types.attrs;
      default = vhost: vhost // lib.optionalAttrs config.garnix.devMode.enable {
        enableACME = vhost.enableACME && ! config.garnix.devMode.enable;
        sslCertificate = devCerts.${devCerts.domain}.cert;
        sslCertificateKey = devCerts.${devCerts.domain}.key;
      };
      readOnly = true;
    };
  };

  config = lib.mkIf config.garnix.devMode.enable {
    users.users.root.password = lib.mkForce null;

    system.activationScripts =
      let
        nixos-vm-host-key = "nixos-vm-host-key";
      in
      {
        ${nixos-vm-host-key} = {
          text = ''
            echo "Copying the test VM host key in place"
            cp ${../data/ssh-key-for-local-dev-secrets} /etc/ssh/ssh_host_ed25519_key
            chmod 0400 /etc/ssh/ssh_host_ed25519_key
          '';
        };

        # Make sure that we decrypt the secrets only after having put in place the host key.
        setupSecrets = {
          text = lib.mkDefault "";
          deps = [ nixos-vm-host-key ];
        };
      };

    security.pki.certificateFiles = [
      devCerts.ca.cert
      ../tests/data/foo.garnix.me.cert.pem
    ];

    security.acme.certs = lib.mkIf config.garnix.devMode.disableACME (lib.mkForce { });
  };
}
