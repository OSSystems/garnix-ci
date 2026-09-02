{ lib
, pkgs
, self
, system
, flakeInputs
, ...
}:
let
  mkGuestProfileConfig =
    guestConfig:
    (lib.nixosSystem {
      inherit system;
      modules = [
        "${flakeInputs.garnix-guest-lib}/guest-profile.nix"
        ({ lib, ... }: {
          options.microvm = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
          config = lib.mkMerge [
            {
              garnix.guest.sshPublicKey = "ssh-ed25519 HOSTING hosting";
              garnix.guest.terminalCaPublicKey = "ssh-ed25519 TERMINAL terminal";
              system.stateVersion = "25.11";
            }
            guestConfig
          ];
        })
      ];
    }).config;
  guestProfileConfig = mkGuestProfileConfig { };
  compositeGuestProfileConfig =
    (lib.nixosSystem {
      inherit system;
      modules = [
        self.nixosModules.garnix-guest
        {
          garnix.guest.sshPublicKey = "ssh-ed25519 HOSTING hosting";
          services.nginx.enable = true;
          system.stateVersion = "25.11";
        }
      ];
    }).config;
  statsHttpStub = pkgs.writeText "garnix-stats-http-stub.py" ''
    import http.server
    import pathlib
    import sys

    status = int(sys.argv[1])
    port = int(sys.argv[2])
    count_path = pathlib.Path(sys.argv[3])
    ready_path = pathlib.Path(sys.argv[4])

    class Handler(http.server.BaseHTTPRequestHandler):
        def do_POST(self):
            count_path.write_text(str(int(count_path.read_text()) + 1))
            self.send_response(status)
            self.end_headers()

        def log_message(self, *_args):
            pass

    count_path.write_text("0")
    server = http.server.HTTPServer(("127.0.0.1", port), Handler)
    ready_path.touch()
    server.serve_forever()
  '';
in
{
  commands = { };
  checks = {
    provisionerdPortTests =
      pkgs.runCommand "provisionerd-port-tests" { nativeBuildInputs = [ pkgs.python3 ]; }
        ''
          cp ${./provisionerd.py} provisionerd.py
          cp ${./test_provisionerd_ports.py} test_provisionerd_ports.py
          python3 -m unittest test_provisionerd_ports -v
          touch "$out"
        '';
    guestProfileTerminalCaTests =
      assert lib.hasInfix "TrustedUserCAKeys /var/lib/garnix/terminal-ca.pub"
        guestProfileConfig.services.openssh.extraConfig;
      assert lib.hasInfix
        "AuthorizedKeysFile %h/.ssh/authorized_keys /etc/ssh/authorized_keys.d/%u /var/garnix/keys/authorized_keys /var/lib/garnix/hosting_authorized_keys"
        guestProfileConfig.services.openssh.extraConfig;
      assert lib.hasInfix
        "AuthorizedKeysFile %h/.ssh/authorized_keys /etc/ssh/authorized_keys.d/%u /var/lib/garnix/hosting_authorized_keys"
        guestProfileConfig.services.openssh.extraConfig;
      assert builtins.elem
        "C /var/lib/garnix/hosting_authorized_keys 0644 root root - /etc/ssh/garnix-hosting.pub"
        guestProfileConfig.systemd.tmpfiles.rules;
      assert builtins.elem "d /var/lib/garnix 0755 root root - -"
        guestProfileConfig.systemd.tmpfiles.rules;
      assert builtins.elem
        "C /var/lib/garnix/terminal-ca.pub 0644 root root - /etc/ssh/garnix-hosting-ca.pub"
        guestProfileConfig.systemd.tmpfiles.rules;
      pkgs.runCommand "guest-profile-terminal-ca-tests" { } ''
        touch "$out"
      '';
    guestProfileCompositeTests =
      assert compositeGuestProfileConfig.microvm.hypervisor == "qemu";
      assert
      compositeGuestProfileConfig.garnix.guest.sshPublicKey
      == "ssh-ed25519 HOSTING hosting";
      assert builtins.elem
        compositeGuestProfileConfig.garnix.guest.sshPublicKey
        compositeGuestProfileConfig.users.users.garnix.openssh.authorizedKeys.keys;
      assert builtins.elem
        "nginx.service"
        compositeGuestProfileConfig.systemd.services.logrotate-checkconf.after;
      pkgs.runCommand "guest-profile-composite-tests" { } ''
        touch "$out"
      '';
    # A repository's own configuration never mentions the hosting key: it
    # belongs to the garnix instance, not to the repo. So the profile has to
    # evaluate without it, and the guest has to stay reachable afterwards --
    # which is what /var/lib/garnix/hosting_authorized_keys is for. The
    # provisioner puts the key on the base guest; tmpfiles copies it to the
    # guest's own disk once; sshd reads it from there forever after.
    guestProfileWorksWithoutHostingKey =
      let
        keyless =
          (lib.nixosSystem {
            inherit system;
            modules = [
              self.nixosModules.garnix-guest
              { system.stateVersion = "25.11"; }
            ];
          }).config;
      in
      assert keyless.garnix.guest.sshPublicKey == "";
      assert keyless.users.users.garnix.openssh.authorizedKeys.keys == [ ];
      assert !(builtins.hasAttr "ssh/garnix-hosting.pub" keyless.environment.etc);
      assert builtins.elem
        "C /var/lib/garnix/hosting_authorized_keys 0644 root root - /etc/ssh/garnix-hosting.pub"
        keyless.systemd.tmpfiles.rules;
      assert lib.hasInfix "/var/lib/garnix/hosting_authorized_keys"
        keyless.services.openssh.extraConfig;
      pkgs.runCommand "guest-profile-works-without-hosting-key" { } ''
        touch "$out"
      '';
    guestProfileStatsTests =
      assert builtins.hasAttr "garnix-stats-reporter" guestProfileConfig.systemd.services;
      assert builtins.hasAttr "garnix-stats-reporter" guestProfileConfig.systemd.timers;
      assert !(builtins.hasAttr "statsReportUrl" guestProfileConfig.garnix.guest);
      assert !(builtins.hasAttr "provisionerId" guestProfileConfig.garnix.guest);
      assert
      guestProfileConfig.systemd.timers.garnix-stats-reporter.unitConfig.ConditionPathExists
      == "/var/lib/garnix/stats.env";
      assert
      guestProfileConfig.systemd.services.garnix-stats-reporter.unitConfig.ConditionPathExists
      == "/var/lib/garnix/stats.env";
      assert
      guestProfileConfig.systemd.services.garnix-stats-reporter.serviceConfig.EnvironmentFile
      == "/var/lib/garnix/stats.env";
      assert !(builtins.hasAttr "garnix/stats.env" guestProfileConfig.environment.etc);
      assert
      !(builtins.elem "C /var/lib/garnix/stats.env 0644 root root - /etc/garnix/stats.env" guestProfileConfig.systemd.tmpfiles.rules);
      pkgs.runCommand "guest-profile-stats-tests"
        {
          nativeBuildInputs = [
            pkgs.coreutils
            pkgs.curl
            pkgs.gawk
            pkgs.python3
          ];
        }
        ''
          set -eu
          reporter=${guestProfileConfig.systemd.services.garnix-stats-reporter.serviceConfig.ExecStart}

          run_http_case() {
            response_status=$1
            expected_exit=$2
            expected_attempts=$3
            port=$4
            count_file=$TMPDIR/count-$response_status
            ready_file=$TMPDIR/ready-$response_status
            python3 ${statsHttpStub} "$response_status" "$port" "$count_file" "$ready_file" &
            server_pid=$!
            while [ ! -e "$ready_file" ]; do sleep 0.01; done
            if GARNIX_STATS_URL="http://127.0.0.1:$port/api/hosts/stats" \
              GARNIX_PROVISIONER_ID=42 \
              GARNIX_STATS_CPU_SAMPLE_DELAY=0 \
              GARNIX_STATS_RETRY_DELAY=0 \
              NO_PROXY=127.0.0.1 \
              "$reporter" >"$TMPDIR/stdout-$response_status" 2>"$TMPDIR/stderr-$response_status"; then
              actual_exit=0
            else
              actual_exit=$?
            fi
            kill "$server_pid"
            wait "$server_pid" 2>/dev/null || true
            test "$actual_exit" -eq "$expected_exit"
            test "$(cat "$count_file")" -eq "$expected_attempts"
          }

          run_http_case 200 0 1 18180
          run_http_case 204 0 1 18181
          run_http_case 299 0 1 18182
          run_http_case 199 1 3 18183
          run_http_case 300 1 3 18184
          run_http_case 302 1 3 18185
          run_http_case 404 1 3 18186
          run_http_case 503 1 3 18187

          if GARNIX_STATS_URL=http://127.0.0.1:18188/api/hosts/stats \
            GARNIX_PROVISIONER_ID=42 \
            GARNIX_STATS_CPU_SAMPLE_DELAY=0 \
            GARNIX_STATS_RETRY_DELAY=0 \
            NO_PROXY=127.0.0.1 \
            "$reporter" >"$TMPDIR/stdout-connection" 2>"$TMPDIR/stderr-connection"; then
            echo "connection failure unexpectedly succeeded" >&2
            exit 1
          fi
          test "$(grep -c '^curl:' "$TMPDIR/stderr-connection")" -eq 3
          grep -F 'failed after 3 attempts' "$TMPDIR/stderr-connection"
          touch "$out"
        '';
  };
}
