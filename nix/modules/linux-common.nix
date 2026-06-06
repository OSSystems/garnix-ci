# Things that we want on every (linux) server
{ config
, lib
, pkgs
, ...
}:
let
  ## Users that should be able to ssh into machines.
  kill-nix-daemon-process-without-client = pkgs.writeShellApplication {
    name = "kill-nix-daemon-process-without-client";
    runtimeInputs = [ pkgs.procps pkgs.coreutils-full ];
    text = ''
      set -x
      for PID in $(pgrep -f "nix-daemon [0-9]+" || true);
      do
        #shellcheck disable=SC2009
        PROCESS_CLIENT_PID="$(ps -o cmd= "$PID" | grep -oP 'nix-daemon \K\d+' || true)"
        if [[ -n "$PROCESS_CLIENT_PID" && "$PROCESS_CLIENT_PID" =~ ^[0-9]+$ ]]; then
          CLIENT_CMD=$(ps -o cmd= "$PROCESS_CLIENT_PID" || echo "missing-client")
          if [[ "$CLIENT_CMD" == "missing-client" ]]; then
            echo "Process $PID has no client anymore. Killing"
            kill -9 "$PID" || true
            continue
          fi
        fi
      done
    '';
  };
  kill-reparented-nix-daemon-clients = pkgs.writeShellApplication {
    name = "kill-reparented-nix-daemon-clients";
    runtimeInputs = [ pkgs.procps pkgs.coreutils-full ];
    text = ''
      set -x
      for PID in $(pgrep -f "nix-daemon [0-9]+" || true);
      do
        #shellcheck disable=SC2009
        PROCESS_CLIENT_PID="$(ps -o cmd= "$PID" | grep -oP 'nix-daemon \K\d+' || true)"
        if [[ -n "$PROCESS_CLIENT_PID" && "$PROCESS_CLIENT_PID" =~ ^[0-9]+$ ]]; then
          PARENT_PROCESS=$(ps -o ppid:1= "$PROCESS_CLIENT_PID" || true)
          if [[ "$PARENT_PROCESS" == "1" ]]; then
            echo "Process $PID was reparented to PID 1. Killing."
            kill -9 "$PID" || true
            continue
          fi
        fi
      done
    '';
  };
  check-long-running-nix-daemon-process = pkgs.writeShellApplication {
    name = "check-long-running-nix-daemon-process";
    runtimeInputs = [ pkgs.procps pkgs.coreutils-full ];
    text = ''
      NOW=$(date +%s)
      MAX_EXECUTION_TIME=$((3*60*60))
      for PID in $(pgrep -f "nix-daemon [0-9]+" || true);
      do
          #shellcheck disable=SC2009
          PROCESS_CLIENT_PID="$(ps -o cmd= "$PID" | grep -oP 'nix-daemon \K\d+' || true)"
          if [[ -n "$PROCESS_CLIENT_PID" && "$PROCESS_CLIENT_PID" =~ ^[0-9]+$ ]]; then
            CLIENT_CMD=$(ps -o cmd= "$PROCESS_CLIENT_PID" || true)
            if [[ "$CLIENT_CMD" == *"nix-serve"* ]]; then
              echo "Process $PID is started by nix-serve. Ignoring"
              continue
            fi
          fi
          PROCESS_START_DATETIME=$(ps -o lstart= "$PID" || echo "PROCESS_ALREADY_ENDED")
          if test "$PROCESS_START_DATETIME" = "PROCESS_ALREADY_ENDED"; then
            echo "Process $PID already stopped. Skipping."
            continue
          fi
          PROCESS_START_EPOCH=$(date -d "$PROCESS_START_DATETIME" +%s)
          DIFF=$((NOW - PROCESS_START_EPOCH))
          echo "Process $PID has been running for $DIFF seconds"
          if [ $DIFF -gt $MAX_EXECUTION_TIME ]; then
              echo "Process $PID has been running for more than 3 hours"
              exit 1
          fi
      done
    '';
  };
in
{
  options.garnix = {
    enableSoftwareRaid = lib.mkEnableOption "swraid";

    ipv4 = lib.mkOption {
      description = "Static ipv4 configuration. If set to null, then DHCP will be used which is not supported for hetzner dedicated machines";
      default = null;
      type = lib.types.nullOr (lib.types.submodule {
        options = {
          address = lib.mkOption {
            type = lib.types.str;
          };

          gateway = lib.mkOption {
            type = lib.types.str;
          };

          iface = lib.mkOption {
            type = lib.types.str;
          };
        };
      });
    };

    ipv6Address = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
    };

    killRogueNixProcesses = lib.mkEnableOption "kill-rogue-nix-processes";
  };

  config = lib.mkIf pkgs.stdenv.isLinux {
    # Limit the amount of generations kept in the bootloader config to avoid
    # filling up the boot partition.
    boot.loader = {
      grub.configurationLimit = 15;
      systemd-boot.configurationLimit = 15;
    };

    boot.swraid = lib.mkIf config.garnix.enableSoftwareRaid {
      enable = true;
      # The mdadm RAID1s were created with 'mdadm --create ... --homehost=hetzner',
      # but the hostname for each machine may be different, and mdadm's HOMEHOST
      # setting defaults to '<system>' (using the system hostname).
      # This results mdadm considering such disks as "foreign" as opposed to
      # "local", and showing them as e.g. '/dev/md/hetzner:root0'
      # instead of '/dev/md/root0'.
      # This is mdadm's protection against accidentally putting a RAID disk
      # into the wrong machine and corrupting data by accidental sync, see
      # https://bugzilla.redhat.com/show_bug.cgi?id=606481#c14 and onward.
      # We do not worry about plugging disks into the wrong machine because
      # we will never exchange disks between machines, so we tell mdadm to
      # ignore the homehost entirely.
      # We set PROGRAM to silence a warning about mdmonitor not being configured.
      # We cannot easily disable this service because it is hardwired into mdadm
      # and started by the upstream mdadm udev rules.
      mdadmConf = ''
        HOMEHOST <ignore>
        PROGRAM ${lib.getExe' pkgs.coreutils "true"}
      '';
    };
    # dirtyfrag mitigation:
    boot.blacklistedKernelModules = [
      "esp4"
      "esp6"
      "rxrpc"
    ];
    boot.extraModprobeConfig = ''
      install esp4 ${pkgs.coreutils}/bin/false
      install esp6 ${pkgs.coreutils}/bin/false
      install rxrpc ${pkgs.coreutils}/bin/false

      alias xfrm-type-2-50 off
      alias xfrm-type-10-50 off
      alias net-pf-33 off
    '';

    networking = {
      useNetworkd = true;
      useDHCP = false;
      usePredictableInterfaceNames = true;

      nameservers = [
        "2620:fe::11#dns11.quad9.net"
        "2620:fe::fe:11#dns11.quad9.net"

        "9.9.9.11#$dns11.quad9.net"
        "149.112.112.11#dns11.quad9.net"
      ];
    };

    systemd.network.wait-online.anyInterface = true;
    systemd.network.networks."10-uplink" = lib.mkMerge [
      (lib.mkIf (config.garnix.ipv4 == null) {
        matchConfig.Name = "en*";
        DHCP = "ipv4";
        dhcpV4Config = {
          # We configure DNS ourselves using resolved, so we don't want to use
          # the servers we get from DHCP.
          UseDNS = false;
        };
      })
      # ipv4
      (lib.mkIf (config.garnix.ipv4 != null) {
        matchConfig.Name = config.garnix.ipv4.iface;
        DHCP = "no";
        addresses = [
          {
            Address = config.garnix.ipv4.address;
            Peer = "${config.garnix.ipv4.gateway}/32";
          }
        ];
        gateway = [ config.garnix.ipv4.gateway ];
      })
      # ipv6
      (lib.mkIf (config.garnix.ipv6Address != null) {
        address = [ config.garnix.ipv6Address ];
        gateway = [ "fe80::1" ];
        networkConfig.IPv6AcceptRA = "no";
      })
    ];

    services.resolved = {
      enable = true;
      # Make the global config the preferred one for all domains.
      domains = [ "~." ];
      # Not all domains support DNSSEC yet.
      # Even when set to allow-downgrade, requests for such domains fail.
      dnssec = "false";
      dnsovertls =
        if config.garnix.devMode.enable
        then "false"
        else "true";
    };

    services.openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        AuthenticationMethods = "publickey";
        PermitRootLogin = "prohibit-password";

        # Lower this to reduce the pressure on MaxStartups
        LoginGraceTime = "20s";

        # Increase this over the default to avoid overly aggressive rate-limiting
        MaxStartups = "50:30:200";
      };
    };

    services.fail2ban = {
      enable = true;
      ignoreIP = [
        "10.0.0.0/8"
        "172.16.0.0/12"
        "192.168.0.0/16"
      ];
      bantime-increment = {
        enable = true;
        rndtime = "5m";
      };
    };

    services.nginx = {
      enableReload = true;
      logError = "stderr warn";
    };

    programs.mosh.enable = true;
    # For mosh
    networking.firewall.allowedUDPPorts = [ 60001 60002 60003 ];

    programs.zsh.enable = true;
    programs.fish.enable = true;

    documentation = {
      enable = true;
      info.enable = false;
      man = {
        enable = true;
        generateCaches = false;
      };
      doc.enable = false;
      nixos.enable = false;
    };

    time.timeZone = "UTC";

    security = {
      sudo = {
        enable = true;
        execWheelOnly = true;
        wheelNeedsPassword = false;
      };

      acme = {
        acceptTerms = true;
      };
    };

    nix = {
      settings = {
        trusted-users = [
          "@wheel"
          "garnix"
        ];
      };

      gc = {
        persistent = true;
        dates = "daily";
      };
    };

    systemd = {
      services = {
        nix-gc.serviceConfig.IOSchedulingPriority = 6;

        check-long-running-nix-daemon-process = {
          description = "Check for long-running nix-daemon processes";
          after = [ "nix-daemon.service" ];
          serviceConfig = {
            Type = "oneshot";
            User = "root";
          };
          script = lib.getExe check-long-running-nix-daemon-process;
        };

        kill-nix-daemon-without-client = lib.mkIf config.garnix.killRogueNixProcesses {
          description = "Kill nix-daemon processes without a client";
          after = [ "nix-daemon.service" ];
          serviceConfig = {
            Type = "oneshot";
            User = "root";
          };
          script = lib.getExe kill-nix-daemon-process-without-client;
        };

        kill-reparented-nix-daemon-clients = lib.mkIf config.garnix.killRogueNixProcesses {
          description = "Kill nix-daemon processes reparented to PID 1";
          after = [ "nix-daemon.service" ];
          serviceConfig = {
            Type = "oneshot";
            User = "root";
          };
          script = lib.getExe kill-reparented-nix-daemon-clients;
        };
      };
      timers = {
        check-long-running-nix-daemon-process = {
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnCalendar = "*:0/15";
          };
        };
        kill-nix-daemon-without-client = lib.mkIf config.garnix.killRogueNixProcesses {
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnCalendar = "*:0/15";
          };
        };
        kill-reparented-nix-daemon-clients = lib.mkIf config.garnix.killRogueNixProcesses {
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnCalendar = "*:0/15";
          };
        };
      };
    };

    system = {
      disableInstallerTools = true;
      build.nixos-rebuild = pkgs.nixos-rebuild.override { nix = config.nix.package.out; };
    };

    garnix = {
      useGarnixCache = true;
    };

    environment.systemPackages = with pkgs; [
      gdb
      iotop
      nethogs
      smartmontools
      traceroute
      check-long-running-nix-daemon-process
      kill-nix-daemon-process-without-client
      cryptsetup
    ];

    virtualisation.vmVariant = {
      virtualisation = {
        cores = 2;
        memorySize = 6 * 1024;
        graphics = false;
        diskSize = 2048;
        fileSystems."/nix/.rw-store".options = [ "size=6G" ];
        qemu = {
          options = [
            "-machine accel=kvm"
          ];
          guestAgent.enable = true;
        };
      };
      networking.usePredictableInterfaceNames = lib.mkForce false;
      systemd.network.networks."10-uplink".matchConfig.Name = lib.mkForce "eth0";
      services.getty.autologinUser = "root";
      users.users.root.password = "";
      garnix.devMode.enable = true;
    };

    system.activationScripts.diff = {
      supportsDryActivation = true;
      text = ''
        if [[ -e /run/current-system ]]; then
          echo "--- diff to current-system"
          export PATH=${config.nix.package}/bin:$PATH
          ${config.nix.package}/bin/nix --extra-experimental-features nix-command store diff-closures /run/current-system "$systemConfig"
          ${pkgs.nvd}/bin/nvd diff /run/current-system "$systemConfig"
          echo "---"
        fi
      '';
    };

    system.activationScripts.requires-reboot = {
      supportsDryActivation = true;
      text = ''
        if [[ -e /run/current-system ]]; then
          current=$(readlink -f /run/current-system/kernel)
          booted=$(readlink -f /run/booted-system/kernel)

          if [ "$current" != "$booted" ]; then
            uptime=$(${pkgs.coreutils-full}/bin/uptime | ${pkgs.gawk}/bin/awk -F'( |,|:)+' '{print $6,$7",",$8,"hours,",$9,"minutes."}')
            echo "--- reboot ?"
            echo "WARN: Kernel version has changed, system should be rebooted!"
            echo "Booted kernel: $booted"
            echo "Deployed kernel: $current"
            echo "Uptime: $uptime"
            echo "---"
          fi
        fi
      '';
    };
  };
}
