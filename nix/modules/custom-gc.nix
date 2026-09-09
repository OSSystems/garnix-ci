{ config, lib, pkgs, options, ... }:

let
  cfg = config.garnix.custom-gc;
  isDarwin = lib.attrsets.hasAttrByPath [ "environment" "darwinConfig" ] options;
  collectInLoop =
    pkgs.writeShellApplication {
      name = "custom-gc-script";
      runtimeInputs = [
        pkgs.coreutils
        pkgs.nix
        pkgs.nix-heuristic-gc
        pkgs.procps
        pkgs.util-linux
      ];
      text = ''
        ${lib.toShellVar "targetPercent" cfg.targetPercent}
        function prepare_builder {
          local MAX_WAIT=${builtins.toString (builtins.floor (2.1 * 3600))}
          i=0
          while pgrep -f 'nix-daemon --stdio' > /dev/null; do
              echo "Waiting to be ready to GC"
              sleep 10
              i=$((i+1))
              if [[ $i -ge $MAX_WAIT ]]; then
                echo " Timeout!"
                break
              fi
          done
          echo " READY!"
        }
        function should_run_gc {
          DISK_USAGE=$(df /nix/store --output=pcent | tr -dc '0-9')
          INODE_USAGE=$(df /nix/store --output=ipcent | tr -dc '0-9')
          [[ $DISK_USAGE -ge $targetPercent ]] || [[ $INODE_USAGE -ge $targetPercent && -z "$IS_ZFS" ]]
        }
        DISK_USAGE=$(df /nix/store --output=pcent | tr -dc '0-9')
        INODE_USAGE=$(df /nix/store --output=ipcent | tr -dc '0-9')
        set +e
        IS_ZFS=$(mount -t zfs | grep "on /nix")
        set -e
        echo "Disk usage at $DISK_USAGE %"
        echo "Inode usage at $INODE_USAGE %"
        if should_run_gc; then
          echo "Deleting generations"
          ${if cfg.isBuilder && !cfg.enableTimer then "prepare_builder" else ""}
          ${lib.getExe' config.nix.package "nix-env"} \
            --profile /nix/var/nix/profiles/system \
            --delete-generations +${toString cfg.numOfGenerationsToKeep}
          while should_run_gc; do
            echo "Running gc"
            if [[ $DISK_USAGE -ge $targetPercent ]]; then
              used=$(df /nix/store --output=used | tr -dc '0-9')
              avail=$(df /nix/store --output=avail | tr -dc '0-9')
              to_gc="$((used - (used + avail) * (targetPercent - 1) / 100))"
              # because of zfs compression we have to overcollect
              to_gc="$((to_gc * 2))"
              # avoid asymptotically approaching $targetPercent
              lower_limit=40000000
              to_gc="$((to_gc < lower_limit ? lower_limit : to_gc))"
              to_gc="''${to_gc}K"
            else
              to_gc="40G"
            fi
            echo "gc'ing $to_gc..."
            ${if cfg.useNixHeuristicGc
                then "nix-heuristic-gc \"$to_gc\""
                else "nix-collect-garbage --max-freed \"$to_gc\" --keep-going"}
          done
          echo "Done"
        else
          echo "Not running gc"
        fi
      '';
    };
  customGCScript =
    if cfg.upperLimitPercent == null
    then collectInLoop
    else
      pkgs.writeShellApplication {
        name = "custom-gc-script";
        runtimeInputs = [
          pkgs.coreutils
          pkgs.procps
          pkgs.util-linux
        ];
        text = ''
          ${lib.toShellVar "upperLimitPercent" cfg.upperLimitPercent}
          DISK_USAGE=$(df /nix/store --output=pcent | tr -dc '0-9')
          INODE_USAGE=$(df /nix/store --output=ipcent | tr -dc '0-9')
          echo "Disk usage at $DISK_USAGE %"
          echo "Inode usage at $INODE_USAGE %"
          if [[ $DISK_USAGE -ge $upperLimitPercent ]] || [[ $INODE_USAGE -ge $upperLimitPercent && -z "$IS_ZFS" ]]; then
            echo running gc
            ${lib.getExe collectInLoop}
          else
            echo not running gc
          fi
        '';
      };
in
{
  options.garnix.custom-gc = {
    enable = lib.mkEnableOption "the custom garbage collection service";
    isBuilder = lib.mkOption {
      type = lib.types.bool;
      description = "Takes the builder out of rotation and waits for builds to drain before performing gc";
      default = false;
    };
    enableTimer = lib.mkOption {
      type = lib.types.bool;
      description = "Runs custom-gc every hour";
    };
    numOfGenerationsToKeep = lib.mkOption {
      type = lib.types.ints.positive;
      description = "Number of old generations to keep in addition to the current one";
      default = 3;
    };
    useNixHeuristicGc = lib.mkOption {
      type = lib.types.bool;
      description = "Use https://github.com/risicle/nix-heuristic-gc for gc'ing";
      default = false;
    };
    targetPercent = lib.mkOption {
      type = lib.types.int;
      description = "Gc'ing will aim for having the disk usage be below this percentage";
      default = 75;
    };
    upperLimitPercent = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      description = "GC will not run unless the disk usage is above this percentage";
      default = null;
    };
  };
  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      nix.gc.automatic = lib.mkForce false;
      environment.systemPackages = [
        pkgs.nix-heuristic-gc
      ];
    }
    (lib.optionalAttrs (!isDarwin) {
      systemd.services.custom-gc = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
        serviceConfig = {
          IOSchedulingClass = "idle";
          Type = "oneshot";
          User = "root";
        };
        script = lib.getExe customGCScript;
      };
      systemd.timers.custom-gc = lib.mkIf cfg.enableTimer {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "hourly";
          Unit = "custom-gc.service";
        };
      };
    })
    (lib.optionalAttrs isDarwin {
      environment.systemPackages = [
        customGCScript
      ];
      launchd.daemons.custom-gc = lib.mkIf cfg.enableTimer {
        script = lib.getExe customGCScript;
        serviceConfig = {
          RunAtLoad = false;
          StartCalendarInterval = [{ Minute = 0; }];
          StandardOutPath = "/var/log/custom-gc.log";
          StandardErrorPath = "/var/log/custom-gc.log";
        };
      };
    })
  ]);
}
