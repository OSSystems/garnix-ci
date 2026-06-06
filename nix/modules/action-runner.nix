{ config
, pkgs
, flakeInputs
, lib
, ...
}:
let
  cfg = config.garnix.actionRunner;
  devModeSshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIsTYAj7lBPpDHSXA4kz07+PbvqElJhPG5bLbxYj255Z";
  pkgsUnstable = import flakeInputs.nixpkgsUnstable { system = pkgs.system; };
  libkrun = pkgsUnstable.libkrun.overrideAttrs (old: {
    src = flakeInputs.libkrun;
    cargoDeps = pkgsUnstable.rustPlatform.fetchCargoVendor {
      src = flakeInputs.libkrun;
      hash = "sha256-2ZjrOdrwnR1oaGmCZc/13LIlH3qPI7g9kBaYAEpwpSE=";
    };
  });
  crunWithLibkrun = pkgsUnstable.crun.overrideAttrs (old: {
    pname = "crun-libkrun";
    buildInputs = (old.buildInputs or [ ]) ++ [ libkrun pkgsUnstable.libkrunfw pkgsUnstable.pkg-config pkgsUnstable.makeWrapper ];
    configureFlags = (old.configureFlags or [ ]) ++ [ "--with-libkrun" ];
    postInstall = (old.postInstall or "") + ''
      # Ensure crun can dlopen libkrun*.so at runtime
      wrapProgram $out/bin/crun \
        --prefix LD_LIBRARY_PATH : ${pkgsUnstable.lib.makeLibraryPath [ pkgsUnstable.libkrun ]} \
        --set-default KRUNFW_PATH ${pkgsUnstable.libkrunfw}/share/libkrun
      wrapProgram $out/bin/krun \
        --prefix LD_LIBRARY_PATH : "$RUNTIME_LD_PATH" \
        --set-default KRUNFW_PATH ${pkgsUnstable.libkrunfw}/share/libkrun
    '';
  });

  bwrapRunner = pkgs.writeShellApplication {
    name = "bwrap-action-runner";
    text = ''
      TMP=$(mktemp -d)

      trap 'rm -rf $TMP' EXIT

      TEMP_SECRET="$TMP"/secret
      PIDFILE="$TMP"/pidfile
      touch "$PIDFILE"
      RESOLVCONF="$TMP"/resolv.conf
      SIGNAL="$TMP"/signalfile
      PASSWDFILE="$TMP"/passwd
      GROUPFILE="$TMP"/group
      HOSTNAMEFILE="$TMP"/hostname

      COMMAND=$1
      TIMEOUT_SECS=$2

      getent passwd "$UID" 65534 > "$PASSWDFILE"
      getent group "$(id -g)" 65534 > "$GROUPFILE"

      cat > "$TEMP_SECRET"
      mkfifo "$SIGNAL"
      # This is slirp4netns' DNS resolver
      echo "nameserver 10.0.2.3" > "$RESOLVCONF"

      echo "garnix-action-runner" > "$HOSTNAMEFILE"

      timeout "$TIMEOUT_SECS"s \
        bwrap \
           --unshare-net \
           --unshare-user \
           --unshare-uts \
           --hostname "garnix-action-runner" \
           --ro-bind /nix /nix \
           --bind /run /run \
           --bind /usr/bin/env /usr/bin/env \
           --tmpfs /etc \
           --ro-bind /etc/hosts /etc/hosts \
           --ro-bind "$HOSTNAMEFILE" /etc/hostname \
           --ro-bind /etc/nsswitch.conf /etc/nsswitch.conf \
           --symlink "$(readlink -f /etc/localtime)" /etc/localtime `# Some tools require this to be a symlink` \
           --ro-bind /etc/zoneinfo /etc/zoneinfo \
           --ro-bind /etc/ssl /etc/ssl \
           --ro-bind /etc/static /etc/static \
           --ro-bind /etc/locale.conf /etc/locale.conf \
           --ro-bind /etc/nscd.conf /etc/nscd.conf \
           --ro-bind /etc/man_db.conf /etc/man_db.conf \
           --ro-bind /etc/host.conf /etc/host.conf \
           --ro-bind /etc/protocols /etc/protocols \
           --ro-bind /etc/services /etc/services \
           --ro-bind "$RESOLVCONF" /etc/resolv.conf \
           --bind "$PASSWDFILE" /etc/passwd \
           --bind "$GROUPFILE" /etc/group \
           --dev-bind /dev/console /dev/console \
           --dev-bind /dev/core /dev/core \
           --dev-bind /dev/full /dev/full \
           --dev-bind /dev/null /dev/null \
           --dev-bind /dev/ptmx /dev/ptmx \
           --dev-bind /dev/pts /dev/pts \
           --dev-bind /dev/random /dev/random \
           --dev-bind /dev/shm /dev/shm \
           --dev-bind /dev/tty /dev/tty \
           --dev-bind /dev/urandom /dev/urandom \
           --dev-bind /dev/zero /dev/zero \
           --dev-bind /dev/net/tun /dev/net/tun \
           --dev-bind /dev/kvm /dev/kvm \
           --ro-bind /bin/sh /bin/sh \
           --proc /proc \
           --symlink /proc/self/fd /dev/fd \
           --tmpfs /tmp \
           --die-with-parent \
           --tmpfs /home/action-runner \
           --chdir /home/action-runner \
           --setenv HOME /home/action-runner \
           --bind "$TEMP_SECRET" "$TEMP_SECRET" \
           --bind "$SIGNAL" /syncfile \
           --bind "$PIDFILE" /pidfile \
           --setenv GARNIX_ACTION_PRIVATE_KEY_FILE "$TEMP_SECRET" \
           /bin/sh -c "echo \$\$ > /pidfile; read -n 1 -t 30 _ <> /syncfile; $COMMAND" &

      TIMEOUT_PID=$!

      exec {SIGNAL_FD_SLIRP}> "$SIGNAL"

      ${pkgs.slirp4netns}/bin/slirp4netns --configure --mtu=65520 \
          --disable-host-loopback \
          --ready-fd="$SIGNAL_FD_SLIRP" \
          "$(cat "$PIDFILE")" tap0 2>/dev/null 1>/dev/null &

      SLIRP_PID=$!
      wait "$TIMEOUT_PID"
      kill "$SLIRP_PID"
    '';
  };

  runner = pkgs.writeShellApplication {
    name = "action-runner";
    excludeShellChecks = [ "SC2102" "SC2016" ];
    runtimeInputs = [ pkgs.coreutils pkgs.podman ];
    text = ''
      COMMAND=$1
      TIMEOUT_SECS=$2
      SECRET_NAME=$(tr -dc A-Za-z0-9 </dev/urandom | head -c 20; echo)

      trap 'EXIT=$?; podman secret rm $SECRET_NAME; exit $EXIT' EXIT
      podman secret create "$SECRET_NAME" - > /dev/null

      mapfile -t CLOSURE_CMD  < <(nix-store --query --requisites "$COMMAND")

      # De-duplicate and resolve symlinks
      declare -A SEEN=()
      ALL_PATHS=()

      for p in "''${CLOSURE_CMD[@]}"; do
        if [[ -z "''${SEEN[$p]:-}" ]]; then
          SEEN[$p]=1
          ALL_PATHS+=("$p")
        fi
      done

      # Build podman --mount flags: bind each store path to the same path in the container, read-only
      MOUNTS=""
      for p in "''${ALL_PATHS[@]}"; do
        MOUNTS+="\"type=bind,src=$p,destination=$p,ro\", "
      done

      if [[ -s "''${ACTION_REPO_DIR:-}" ]]; then
        MOUNTS+="\"type=bind,src=$ACTION_REPO_DIR,destination=/tmp/base,rw\", "
      fi

      TEMP=$(mktemp -d)

      CONTAINERS_CONF="$TEMP"/containers.conf
      printf "[containers]\nmounts=[ %s ]\n" "$MOUNTS" > "$CONTAINERS_CONF"

      export CONTAINERS_CONF

      nix path-info --json --json-format 1 --recursive "$COMMAND" | jq -r 'to_entries | map([.key, .value.narHash, .value.narSize, "", (.value.references | length)] + .value.references) | add | map("\(.)\n") | add' | head -n -1 > "$TEMP"/registrations

      # TODO stop fetching the image directly, and instead have the fetch
      # happen in a separate derivation, so that our tests
      # are faster (but dockerTools.pullImage doesn't seem
      # to work for this)
      exec podman run \
        --quiet \
        --rm \
        --memory 4G \
        --timeout "$TIMEOUT_SECS" \
        --runtime "${crunWithLibkrun}/bin/krun" \
        --mount type=bind,src="$TEMP"/registrations,target=/tmp/registrations \
        --env [PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin] \
        --env GARNIX_ACTION_PRIVATE_KEY_FILE=/run/secrets/"$SECRET_NAME"  \
        --env GARNIX_CI \
        --env GARNIX_BRANCH \
        --env GARNIX_COMMIT_SHA \
        --secret "$SECRET_NAME" \
        docker.io/nixos/nix \
        bash -c "set -e; cat /tmp/registrations | nix-store --load-db; mkdir -p /tmp/base && cd /tmp/base && exec $COMMAND" \
        2> >(grep -v "Couldn't get terminal dimensions: ENOTTY")
    '';
  };
in
{
  options.garnix.actionRunner = {
    enable = lib.mkEnableOption "Enable Action Runner";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.bubblewrap
      runner
      bwrapRunner
    ];

    environment.etc."containers/policy.json" = {
      mode = "444";
      text = ''
        {
            "default": [
                {
                    "type": "insecureAcceptAnything"
                }
            ]
        }
      '';
    };

    users = {
      users.action-runner = {
        isSystemUser = true;
        shell = pkgs.bash;
        group = "action-runner";
        # subuid and subgids are needed for rootless podman
        autoSubUidGidRange = true;
        openssh.authorizedKeys.keys =
          if config.garnix.devMode.enable
          then [ devModeSshKey ]
          else [ (import ../data/keys.nix).actionRunnerKey ];
        # Needed for programs that look at `/etc/passwd` for the home directory
        # instead of `$HOME`. (E.g. `ssh` through `getpwuid`.) When running
        # actions, we mount in a fresh home directory here using `bubblewrap`.
        home = "/home/action-runner";
        createHome = true;
      };
      groups.action-runner = { };
    };

    nix.settings.trusted-users = [
      config.users.users.action-runner.name
    ];

    garnix.custom-gc = {
      enable = true;
      enableTimer = true;
    };

    services.logind.settings.Login = {
      KillUserProcesses = true;
      KillOnlyUsers = "nix-ssh";
    };

    virtualisation.vmVariant.virtualisation = {
      useNixStoreImage = true;
      writableStore = true;
    };
  };
}
