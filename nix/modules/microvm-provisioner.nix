{ config
, lib
, pkgs
, ...
}:
let
  cfg = config.garnix.local-provisioner;
  hostAddr = lib.elemAt (lib.splitString "/" cfg.hostAddress) 0;
  hostPrefixLength = lib.toInt (lib.elemAt (lib.splitString "/" cfg.hostAddress) 1);
  subnetPrefix = lib.concatStringsSep "." (lib.take 3 (lib.splitString "." hostAddr));
  stateDir = "/var/lib/garnix-provisioner";
  pubkeyPath = "${stateDir}/hosting.pub";
  terminalCaPubkeyPath = "${stateDir}/terminal-ca.pub";
  provisionerEnv = {
    PROVISIONER_SOCKET = cfg.socketPath;
    PROVISIONER_SOCKET_GROUP = cfg.backendGroup;
    PROVISIONER_STATE_DIR = stateDir;
    PROVISIONER_BRIDGE = cfg.bridge;
    PROVISIONER_SUBNET_PREFIX = subnetPrefix;
    PROVISIONER_NIXPKGS = cfg.nixpkgsFlake;
    PROVISIONER_MICROVM = cfg.microvmFlake;
    PROVISIONER_GUEST_PROFILE = "${cfg.guestProfile}";
    PROVISIONER_SSH_PUBKEY_FILE = pubkeyPath;
    PROVISIONER_TERMINAL_CA_PUBKEY_FILE = terminalCaPubkeyPath;
    PROVISIONER_PORT_RANGE_END = toString cfg.exposePortRange.to;
    PROVISIONER_GUEST_CPU = if cfg.guestCpuModel == null then "" else cfg.guestCpuModel;
    PROVISIONER_UPLINK = cfg.uplinkInterface;
    PROVISIONER_SSH_PORT_BASE = toString cfg.sshExposePortBase;
    PROVISIONER_TCP_PORT_BASE = toString cfg.tcpExposePortBase;
  };
in
{
  options.garnix.local-provisioner = {
    enable = lib.mkEnableOption "the garnix local microVM provisioner daemon";
    autostartGuests = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        microVMs are created imperatively by garnix-provisionerd and do NOT
        survive a host reboot on their own — without this, a deployed guest
        stays down until someone redeploys it by hand. When true, the
        garnix-guest-autostart.service oneshot queries the garnix DB on boot
        for every LIVE deployed server (servers.ready_at IS NOT NULL AND
        servers.ended_at IS NULL) and starts its microvm@garnix-<id>.service.
        The warm pool (server_pool table) is deliberately excluded — it
        refills itself. Set false to opt out.
      '';
    };
    socketPath = lib.mkOption {
      type = lib.types.str;
      default = "/run/garnix-provisioner/provisioner.sock";
      description = ''
        Unix socket the backend connects to (services.garnixServer.provisionerSocket).
        Must live under /run/garnix-provisioner (the service's RuntimeDirectory).
      '';
    };
    bridge = lib.mkOption {
      type = lib.types.str;
      default = "garnixbr0";
      description = "Name of the host-only bridge the guests attach to.";
    };
    hostAddress = lib.mkOption {
      type = lib.types.str;
      default = "10.111.0.1/24";
      description = "The host's CIDR address on the guest bridge (a /24).";
    };
    uplinkInterface = lib.mkOption {
      type = lib.types.str;
      example = "eno1";
      description = "External interface guests NAT out through.";
    };
    sshPrivateKeyPath = lib.mkOption {
      type = lib.types.str;
      default = "/run/secrets/garnix_server_ssh_hosting";
      description = ''
        The backend's hosting SSH private key (sshUserHostingKeys). The matching
        public key is derived at service start and baked into every guest.
      '';
    };
    terminalCaPrivateKeyPath = lib.mkOption {
      type = lib.types.str;
      default = "/run/secrets/garnix_terminal_ca";
      description = ''
        The dedicated web-terminal certificate-authority private key (finding
        H3), separate from the hosting/deploy key. The matching public key is
        derived at service start and baked into every guest as its
        TrustedUserCAKeys, so the backend's short-lived terminal-session certs
        are trusted WITHOUT the guest trusting the hosting key as a CA. If the
        secret is absent at start the derivation falls back to the hosting
        pubkey so the daemon still starts and guests stay evaluable.
      '';
    };
    guestCpuModel = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "IvyBridge";
      description = ''
        QEMU CPU model for guests (microvm.cpu). null (default) keeps
        microvm.nix's `-cpu host` passthrough. A fixed named model narrows
        the host-feature/side-channel surface a guest can see, at the cost
        of hiding newer ISA extensions from guest code. Must be a model the
        host CPU can satisfy (erdtree: dual E5-2667 v2 = IvyBridge;
        IvyBridge-IBRS if the microcode exposes spec-ctrl).
      '';
    };
    nixpkgsFlake = lib.mkOption {
      type = lib.types.str;
      example = "path:/nix/store/...-source";
      description = ''
        Flakeref the per-VM flakes pin nixpkgs to. Pass a store-path ref
        (e.g. "path:''${inputs.nixpkgs}") so guest builds need no network fetch.
      '';
    };
    microvmFlake = lib.mkOption {
      type = lib.types.str;
      example = "path:/nix/store/...-source";
      description = "Flakeref the per-VM flakes pin microvm.nix to (same shape as nixpkgsFlake).";
    };
    guestProfile = lib.mkOption {
      type = lib.types.path;
      example = "\${inputs.garnix-guest-lib}/guest-profile.nix";
      description = ''
        garnix-guest-lib's `guest-profile.nix`, copied into every generated
        guest spec directory. The flake's `garnix-provisioner` module sets
        this from its `garnix-guest-lib` input.
      '';
    };
    backendGroup = lib.mkOption {
      type = lib.types.str;
      default = "garnix";
      description = "Group granted write access to the daemon socket (the backend's group).";
    };
    sshExposePortBase = lib.mkOption {
      type = lib.types.int;
      default = 22000;
      description = ''
        Bottom of the host-port pool for per-VM SSH exposure (garnix.yaml
        sshExpose). Ports are allocated lowest-free from
        [sshExposePortBase, tcpExposePortBase) and recorded per guest.
      '';
    };
    tcpExposePortBase = lib.mkOption {
      type = lib.types.int;
      default = 32000;
      description = ''
        Bottom of the host-port pool for per-VM raw-tcp exposure (garnix.yaml
        ports type=tcp). Ports are allocated lowest-free from
        [tcpExposePortBase, exposePortRange.to] and recorded per guest.
      '';
    };
    exposePortRange = lib.mkOption {
      type = lib.types.submodule {
        options = {
          from = lib.mkOption {
            type = lib.types.port;
            default = 22000;
          };
          to = lib.mkOption {
            type = lib.types.port;
            default = 41999;
          };
        };
      };
      default = {
        from = 22000;
        to = 41999;
      };
      description = ''
        Host TCP port range opened on the uplink for DNAT'd SSH/tcp exposure.
        Must cover both sshExposePortBase (+1000) and tcpExposePortBase (+ 500*20).
      '';
    };
    guestEgressBlocklist = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "10.0.0.0/8"
        "172.16.0.0/12"
        "192.168.0.0/16"
        "169.254.0.0/16"
        "100.64.0.0/10"
      ];
      example = [
        "10.0.0.0/8"
        "192.168.0.0/16"
        "147.224.12.5/32"
      ];
      description = ''
        Destination CIDRs guests can never initiate connections to (FORWARD
        drop before NAT). The default covers RFC1918 + link-local + CGNAT,
        which includes the guest bridge subnet itself and the host LAN.
        NOTE: setting this option replaces the default — repeat the ranges
        you still want and append internal hosts that are NOT in private
        space (e.g. a remote builder's public address).
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    networking = {
      bridges.${cfg.bridge}.interfaces = [ ];
      interfaces.${cfg.bridge} = {
        useDHCP = false;
        ipv4.addresses = [
          {
            address = hostAddr;
            prefixLength = hostPrefixLength;
          }
        ];
      };
      networkmanager.unmanaged = [ "interface-name:${cfg.bridge}" ];

      nat = {
        enable = true;
        internalInterfaces = [ cfg.bridge ];
        externalInterface = cfg.uplinkInterface;
      };

      firewall = {
        interfaces.${cfg.bridge}.allowedUDPPorts = [ 67 ];

        interfaces.${cfg.uplinkInterface}.allowedTCPPortRanges = [
          {
            from = cfg.exposePortRange.from;
            to = cfg.exposePortRange.to;
          }
        ];

        extraCommands = ''
          iptables  -D FORWARD -i ${cfg.bridge} -o ${cfg.bridge} -j DROP 2>/dev/null || true
          iptables  -I FORWARD -i ${cfg.bridge} -o ${cfg.bridge} -j DROP
          ip6tables -D FORWARD -i ${cfg.bridge} -o ${cfg.bridge} -j DROP 2>/dev/null || true
          ip6tables -I FORWARD -i ${cfg.bridge} -o ${cfg.bridge} -j DROP 2>/dev/null || true

          iptables -C FORWARD -i ${cfg.bridge} -m comment --comment garnix-guest-egress-rebuild -j DROP 2>/dev/null || \
            iptables -I FORWARD 2 -i ${cfg.bridge} -m comment --comment garnix-guest-egress-rebuild -j DROP
          iptables -D FORWARD -i ${cfg.bridge} -j garnix-guest-egress 2>/dev/null || true
          iptables -F garnix-guest-egress 2>/dev/null || true
          iptables -X garnix-guest-egress 2>/dev/null || true
          iptables -N garnix-guest-egress
          iptables -A garnix-guest-egress -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
          ${lib.concatMapStrings (cidr: ''
            iptables -A garnix-guest-egress -d ${cidr} -j DROP
          '') cfg.guestEgressBlocklist}
          iptables -A garnix-guest-egress -j RETURN
          iptables -I FORWARD 2 -i ${cfg.bridge} -j garnix-guest-egress
          iptables -D FORWARD -i ${cfg.bridge} -m comment --comment garnix-guest-egress-rebuild -j DROP

          ip6tables -D FORWARD -i ${cfg.bridge} -j DROP 2>/dev/null || true
          ip6tables -I FORWARD -i ${cfg.bridge} -j DROP 2>/dev/null || true
        '';
        extraStopCommands = ''
          iptables  -D FORWARD -i ${cfg.bridge} -o ${cfg.bridge} -j DROP 2>/dev/null || true
          ip6tables -D FORWARD -i ${cfg.bridge} -o ${cfg.bridge} -j DROP 2>/dev/null || true
          iptables  -D FORWARD -i ${cfg.bridge} -j garnix-guest-egress 2>/dev/null || true
          iptables  -F garnix-guest-egress 2>/dev/null || true
          iptables  -X garnix-guest-egress 2>/dev/null || true
          iptables  -D FORWARD -i ${cfg.bridge} -m comment --comment garnix-guest-egress-rebuild -j DROP 2>/dev/null || true
          ip6tables -D FORWARD -i ${cfg.bridge} -j DROP 2>/dev/null || true
        '';
      };
    };

    boot.kernelModules = [ "br_netfilter" ];
    boot.kernel.sysctl = {
      "net.bridge.bridge-nf-call-iptables" = 1;
      "net.bridge.bridge-nf-call-ip6tables" = 1;
      "net.ipv6.conf.${cfg.bridge}.accept_ra" = 0;
      "net.ipv4.ip_local_port_range" = "${toString (cfg.exposePortRange.to + 1)} 60999";
    };
    assertions = [
      {
        assertion = cfg.exposePortRange.to < 60000;
        message = "garnix.local-provisioner.exposePortRange.to must stay below 60000 so an ephemeral port range remains above it.";
      }
    ];

    systemd = {
      services."microvm-tap-interfaces@".serviceConfig.ExecStartPost = [
        "${pkgs.writeShellScript "garnix-tap-isolate" ''
          set -euo pipefail
          case "$1" in
            garnix-*) ;;
            *) exit 0 ;;
          esac
          tap="gx''${1#garnix-}"
          ${pkgs.iproute2}/bin/ip link set dev "$tap" master ${cfg.bridge}
          ${pkgs.iproute2}/bin/ip link set dev "$tap" type bridge_slave isolated on
        ''} %i"
      ];

      tmpfiles.rules = [
        "d ${stateDir} 0755 root root -"
        "d ${stateDir}/specs 0755 root root -"
        "d ${stateDir}/exposed 0755 root root -"
        "f ${stateDir}/dnsmasq-hosts 0644 root root -"
      ];

      services.garnix-provisionerd = {
        description = "garnix local microVM provisioner";
        wantedBy = [ "multi-user.target" ];
        after = [
          "network.target"
          "dnsmasq.service"
          "firewall.service"
        ];
        path = [
          pkgs.nix
          pkgs.openssh
          pkgs.iptables
          "/run/current-system/sw"
        ];
        environment = provisionerEnv;
        serviceConfig = {
          ExecStartPre = pkgs.writeShellScript "garnix-provisioner-pubkey" ''
            set -euo pipefail
            ${pkgs.openssh}/bin/ssh-keygen -y -f ${cfg.sshPrivateKeyPath} > ${pubkeyPath}
            chmod 0644 ${pubkeyPath}
            if ${pkgs.openssh}/bin/ssh-keygen -y -f ${cfg.terminalCaPrivateKeyPath} > ${terminalCaPubkeyPath} 2>/dev/null; then
              :
            else
              cp ${pubkeyPath} ${terminalCaPubkeyPath}
            fi
            chmod 0644 ${terminalCaPubkeyPath}
          '';
          ExecStart = "${pkgs.python3}/bin/python3 ${../../provisioner/provisionerd.py}";
          RuntimeDirectory = "garnix-provisioner";
          Restart = "always";
          RestartSec = 5;
        };
        unitConfig.StartLimitIntervalSec = 0;
      };

      services.garnix-exposure-restore = {
        description = "Re-apply garnix guest port exposure after a firewall reload";
        wantedBy = [ "multi-user.target" ];
        after = [
          "firewall.service"
          "garnix-provisionerd.service"
        ];
        partOf = [ "firewall.service" ];
        path = [ pkgs.iptables ];
        environment = provisionerEnv;
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStart = "${pkgs.python3}/bin/python3 ${../../provisioner/provisionerd.py} --reconcile";
        };
      };

      services.garnix-guest-autostart = lib.mkIf cfg.autostartGuests {
        description = "Start live deployed garnix guests after a host reboot";
        wantedBy = [ "multi-user.target" ];
        wants = [
          "postgresql.service"
          "garnix-provisionerd.service"
          "network-online.target"
        ];
        after = [
          "postgresql.service"
          "garnix-provisionerd.service"
          "network-online.target"
        ];
        path = [ pkgs.postgresql_18 pkgs.util-linux "/run/current-system/sw" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = ''
          set -uo pipefail
          echo "garnix-guest-autostart: querying live deployed servers"
          ids="$(runuser -u postgres -- psql -X -q -t -A \
            -p ${toString config.garnix.database.dbPort} \
            -d ${config.garnix.database.dbName} \
            -c "SELECT provisioner_id FROM servers WHERE ready_at IS NOT NULL AND ended_at IS NULL;")"
          status=$?
          if [ "$status" -ne 0 ]; then
            echo "garnix-guest-autostart: psql query failed (exit $status); cannot determine which guests to start" >&2
            exit 1
          fi
          if [ -z "$ids" ]; then
            echo "garnix-guest-autostart: no live deployed servers in the DB; nothing to start"
            exit 0
          fi
          for id in $ids; do
            if systemctl start "microvm@garnix-$id.service"; then
              echo "garnix-guest-autostart: started microvm@garnix-$id.service"
            else
              echo "garnix-guest-autostart: microvm@garnix-$id.service failed to start (spec dir may be missing) - continuing" >&2
            fi
          done
        '';
      };
    };

    services.dnsmasq = {
      enable = true;
      resolveLocalQueries = false;
      settings = {
        port = 0;
        interface = cfg.bridge;
        bind-interfaces = true;
        dhcp-authoritative = true;
        dhcp-range = "${subnetPrefix}.10,${subnetPrefix}.250,12h";
        dhcp-hostsfile = "${stateDir}/dnsmasq-hosts";
        dhcp-option = [
          "option:router,${hostAddr}"
          "option:dns-server,9.9.9.9"
        ];
      };
    };
  };
}
