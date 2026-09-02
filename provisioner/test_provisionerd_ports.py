"""Unit tests for provisionerd's host-port allocator + registry. provisionerd
reads PROVISIONER_* env at import time, so required vars are pinned first;
no iptables/network is touched (pure helpers only)."""

import contextlib
import json
import os
import shlex
import subprocess
import tempfile
import unittest
from types import SimpleNamespace
from unittest import mock

os.environ.setdefault("PROVISIONER_SOCKET", "/tmp/test-provisioner.sock")
os.environ.setdefault("PROVISIONER_NIXPKGS", "path:/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-src")
os.environ.setdefault("PROVISIONER_MICROVM", "path:/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-src")
os.environ.setdefault("PROVISIONER_GUEST_PROFILE", "/dev/null")
os.environ.setdefault("PROVISIONER_SSH_PUBKEY_FILE", "/dev/null")

import provisionerd as pd

_MISSING = object()

def required_callable(name):
    fn = getattr(pd, name, None)
    if not callable(fn):
        raise AssertionError(f"provisionerd.{name} is not implemented")
    return fn

@contextlib.contextmanager
def patched_pd(**values):
    old = {name: getattr(pd, name, _MISSING) for name in values}
    for name, value in values.items():
        setattr(pd, name, value)
    try:
        yield
    finally:
        for name, value in old.items():
            if value is _MISSING:
                delattr(pd, name)
            else:
                setattr(pd, name, value)

def write_state(directory, name, state):
    path = os.path.join(directory, f"{name}.json")
    with open(path, "w") as f:
        json.dump(state, f)
    return path

class FakeIptables:
    def __init__(self, nat=None, forward=None, fail_when=None):
        self.rules = {
            ("nat", "PREROUTING"): list(nat or []),
            (None, "FORWARD"): list(forward or []),
        }
        self.calls = []
        self.fail_when = fail_when or (lambda cmd: False)

    def run(self, cmd, check=True, timeout=None):
        self.calls.append((list(cmd), check, timeout))
        table = None
        index = 1
        if cmd[index : index + 1] == ["-w"]:
            index += 2
        if cmd[index : index + 2] == ["-t", "nat"]:
            table = "nat"
            index += 2
        op = cmd[index]
        chain = cmd[index + 1]
        args = list(cmd[index + 2 :])
        key = (table, chain)
        if op == "-S":
            return SimpleNamespace(
                stdout="".join(shlex.join(parts) + "\n" for parts in self.rules[key]),
                returncode=0,
            )
        if self.fail_when(cmd):
            if check:
                raise subprocess.CalledProcessError(1, cmd, output="injected failure")
            return SimpleNamespace(stdout="injected failure", returncode=1)
        if op == "-D":
            target = ["-A", chain] + args
            try:
                self.rules[key].remove(target)
            except ValueError:
                if check:
                    raise subprocess.CalledProcessError(1, cmd, output="rule absent")
                return SimpleNamespace(stdout="rule absent", returncode=1)
        elif op == "-A":
            self.rules[key].append(["-A", chain] + args)
        elif op == "-I":
            position = int(args[0])
            self.rules[key].insert(position - 1, ["-A", chain] + args[1:])
        else:
            raise AssertionError(f"unexpected iptables operation: {cmd}")
        return SimpleNamespace(stdout="", returncode=0)

class AllocTests(unittest.TestCase):
    def test_lowest_free(self):
        used = {22000, 22001}
        self.assertEqual(required_callable("alloc_host_port")(used, 22000, 31999), 22002)
        self.assertIn(22002, used)

    def test_preferred_wins(self):
        self.assertEqual(
            required_callable("alloc_host_port")(set(), 22000, 31999, preferred=22007),
            22007,
        )

    def test_preferred_taken_falls_back_to_lowest(self):
        self.assertEqual(
            required_callable("alloc_host_port")({22007}, 22000, 31999, preferred=22007),
            22000,
        )

    def test_exhausted_raises(self):
        alloc_host_port = required_callable("alloc_host_port")
        with self.assertRaises(RuntimeError):
            alloc_host_port({22000}, 22000, 22000)

class IpLedgerTests(unittest.TestCase):
    """The address a guest gets must be an explicit reservation, not a pure
    function of its id: the id-modulo mapping wrapped the /24 and handed two
    live guests one address."""

    @contextlib.contextmanager
    def ledger(self):
        with tempfile.TemporaryDirectory() as directory:
            with patched_pd(LEASES_FILE=os.path.join(directory, "ip-leases.json")):
                yield

    def test_preferred_matches_historical_mapping(self):
        preferred_ip = required_callable("preferred_ip")
        self.assertEqual(preferred_ip(0), f"{pd.SUBNET_PREFIX}.10")
        self.assertEqual(preferred_ip(7), f"{pd.SUBNET_PREFIX}.17")
        self.assertEqual(preferred_ip(239), f"{pd.SUBNET_PREFIX}.249")

    def test_allocation_is_stable_for_a_name(self):
        allocate_ip = required_callable("allocate_ip")
        with self.ledger():
            first = allocate_ip("garnix-7", 7)
            self.assertEqual(first, allocate_ip("garnix-7", 7))

    def test_wrapped_ids_do_not_collide(self):
        allocate_ip = required_callable("allocate_ip")
        with self.ledger():
            first = allocate_ip("garnix-7", 7)
            second = allocate_ip("garnix-247", 247)
            self.assertEqual(first, f"{pd.SUBNET_PREFIX}.17")
            self.assertNotEqual(first, second)

    def test_release_frees_the_address_for_reuse(self):
        allocate_ip = required_callable("allocate_ip")
        release_ip = required_callable("release_ip")
        with self.ledger():
            first = allocate_ip("garnix-7", 7)
            release_ip("garnix-7")
            release_ip("garnix-7")
            self.assertEqual(allocate_ip("garnix-247", 247), first)

    def test_exhausted_subnet_raises(self):
        allocate_ip = required_callable("allocate_ip")
        with self.ledger():
            for vm_id in range(pd.IP_HOST_MIN, pd.IP_HOST_MAX + 1):
                allocate_ip(f"garnix-{vm_id}", vm_id)
            with self.assertRaises(RuntimeError):
                allocate_ip("garnix-one-too-many", 9999)

    def test_lookup_by_name_prefers_the_ledger(self):
        allocate_ip = required_callable("allocate_ip")
        vm_ip_from_name = required_callable("vm_ip_from_name")
        with self.ledger():
            allocate_ip("garnix-7", 7)
            allocated = allocate_ip("garnix-247", 247)
            self.assertNotEqual(allocated, pd.preferred_ip(247))
            self.assertEqual(vm_ip_from_name("garnix-247"), allocated)

    def test_lookup_falls_back_for_pre_ledger_guests(self):
        vm_ip_from_name = required_callable("vm_ip_from_name")
        with self.ledger():
            self.assertEqual(vm_ip_from_name("garnix-7"), f"{pd.SUBNET_PREFIX}.17")
            self.assertEqual(vm_ip_from_name("not-a-guest"), "")

    def test_corrupt_ledger_refuses_rather_than_guessing(self):
        allocate_ip = required_callable("allocate_ip")
        with tempfile.TemporaryDirectory() as directory:
            path = os.path.join(directory, "ip-leases.json")
            with open(path, "w") as f:
                f.write("{not json")
            with patched_pd(LEASES_FILE=path):
                with self.assertRaises(RuntimeError):
                    allocate_ip("garnix-7", 7)

class RegistryTests(unittest.TestCase):
    def test_union_and_exclude(self):
        with tempfile.TemporaryDirectory() as d, patched_pd(EXPOSED_DIR=d):
            with open(os.path.join(d, "garnix-1.json"), "w") as f:
                json.dump({"ip": "10.111.0.11", "rules": [{"host": 22000, "guest": 22}]}, f)
            with open(os.path.join(d, "garnix-2.json"), "w") as f:
                json.dump({"ip": "10.111.0.12", "rules": [{"host": 32000, "guest": 80}]}, f)
            allocated_host_ports = required_callable("allocated_host_ports")
            self.assertEqual(allocated_host_ports(), {22000, 32000})
            self.assertEqual(allocated_host_ports(exclude="garnix-1"), {32000})

class StrictRegistryTests(unittest.TestCase):
    def test_read_exposure_accepts_valid_legacy_shape(self):
        state = {
            "ip": "10.111.0.11",
            "rules": [
                {"host": 22000, "guest": 22},
                {"host": 32000, "guest": 80},
            ],
        }
        with tempfile.TemporaryDirectory() as d, patched_pd(EXPOSED_DIR=d):
            write_state(d, "garnix-1", state)
            self.assertEqual(required_callable("read_exposure")("garnix-1"), state)

    def test_read_exposure_rejects_invalid_shapes(self):
        invalid = [
            [],
            {"rules": []},
            {"ip": 52, "rules": []},
            {"ip": "10.111.0.11", "rules": {}},
            {"ip": "10.111.0.11", "rules": [{"guest": 80}]},
            {"ip": "10.111.0.11", "rules": [{"host": "32000", "guest": 80}]},
            {"ip": "10.111.0.11", "rules": [{"host": 32000, "guest": True}]},
            {"ip": "10.111.0.11", "rules": [{"host": 70000, "guest": 80}]},
            {"ip": "10.111.0.11", "rules": [{"host": 32000, "guest": 0}]},
            {"ip": "10.111.0.11", "rules": [{"host": 22000, "guest": 80}]},
            {
                "ip": "10.111.0.11",
                "rules": [
                    {"host": 32000, "guest": 80},
                    {"host": 32000, "guest": 81},
                ],
            },
            {
                "ip": "10.111.0.11",
                "rules": [
                    {"host": 32000, "guest": 80},
                    {"host": 32001, "guest": 80},
                ],
            },
        ]
        with tempfile.TemporaryDirectory() as d, patched_pd(EXPOSED_DIR=d):
            for index, state in enumerate(invalid):
                name = f"garnix-{index}"
                write_state(d, name, state)
                with self.subTest(state=state), self.assertRaises(RuntimeError):
                    required_callable("read_exposure")(name)

    def test_read_exposure_rejects_truncated_and_unreadable_files(self):
        with tempfile.TemporaryDirectory() as d, patched_pd(EXPOSED_DIR=d):
            path = os.path.join(d, "garnix-1.json")
            with open(path, "w") as f:
                f.write('{"ip":')
            with self.assertRaisesRegex(RuntimeError, "garnix-1.json"):
                required_callable("read_exposure")("garnix-1")
            real_open = open

            def denied(filename, *args, **kwargs):
                if filename == path:
                    raise PermissionError("denied")
                return real_open(filename, *args, **kwargs)

            with mock.patch("builtins.open", side_effect=denied):
                with self.assertRaisesRegex(RuntimeError, "garnix-1.json"):
                    required_callable("read_exposure")("garnix-1")

    def test_allocated_ports_reject_cross_file_collision(self):
        with tempfile.TemporaryDirectory() as d, patched_pd(EXPOSED_DIR=d):
            write_state(
                d,
                "garnix-1",
                {"ip": "10.111.0.11", "rules": [{"host": 32000, "guest": 80}]},
            )
            write_state(
                d,
                "garnix-2",
                {"ip": "10.111.0.12", "rules": [{"host": 32000, "guest": 81}]},
            )
            with self.assertRaisesRegex(RuntimeError, "claimed by"):
                required_callable("allocated_host_ports")()

    def test_write_exposure_is_atomic_and_cleans_failed_temp(self):
        old = b'{"ip":"10.111.0.52","rules":[]}\n'
        with tempfile.TemporaryDirectory() as d, patched_pd(EXPOSED_DIR=d):
            path = os.path.join(d, "garnix-42.json")
            with open(path, "wb") as f:
                f.write(old)
            with mock.patch.object(pd.os, "replace", side_effect=OSError("replace failed")):
                with self.assertRaisesRegex(OSError, "replace failed"):
                    required_callable("write_exposure")(
                        "garnix-42",
                        "10.111.0.52",
                        [{"host": 32000, "guest": 80}],
                    )
            with open(path, "rb") as f:
                self.assertEqual(f.read(), old)
            self.assertEqual(os.listdir(d), ["garnix-42.json"])

class IptablesRuleTests(unittest.TestCase):
    def test_list_rules_preserves_quoted_arguments_and_checks_command(self):
        def fake_run(cmd, check=True, timeout=None):
            self.assertTrue(check)
            return SimpleNamespace(
                stdout=(
                    '-A PREROUTING -i eth0 -p tcp --dport 22000 '
                    '-m comment --comment "legacy tenant" -j DNAT '
                    '--to-destination 10.111.0.11:22\n'
                ),
                returncode=0,
            )

        with patched_pd(run=fake_run):
            rules = required_callable("_list_rules")("nat", "PREROUTING")
        self.assertIn("legacy tenant", rules[0])
        self.assertNotIn('"legacy', rules[0])

    def test_affected_rules_are_narrow_and_keep_original_positions(self):
        matching = [
            "-A",
            "PREROUTING",
            "-i",
            "eth0",
            "-p",
            "tcp",
            "--dport",
            "22001",
            "-j",
            "DNAT",
            "--to-destination",
            "10.111.0.52:80",
        ]
        stale_by_ip = [
            "-A",
            "PREROUTING",
            "-i",
            "eth0",
            "-p",
            "tcp",
            "--dport",
            "22999",
            "-j",
            "DNAT",
            "--to-destination",
            "10.111.0.52:81",
        ]
        udp = [
            "-A",
            "PREROUTING",
            "-i",
            "eth0",
            "-p",
            "udp",
            "--dport",
            "22001",
            "-j",
            "DNAT",
            "--to-destination",
            "10.111.0.52:80",
        ]
        non_dnat = [
            "-A",
            "PREROUTING",
            "-i",
            "eth0",
            "-p",
            "tcp",
            "--dport",
            "22001",
            "-j",
            "ACCEPT",
        ]
        other_interface = [
            "-A",
            "PREROUTING",
            "-i",
            "tailscale0",
            "-p",
            "tcp",
            "--dport",
            "22001",
            "-j",
            "DNAT",
            "--to-destination",
            "10.111.0.52:80",
        ]
        forward = [
            "-A",
            "FORWARD",
            "-p",
            "tcp",
            "-d",
            "10.111.0.52/32",
            "--dport",
            "80",
            "-j",
            "ACCEPT",
        ]
        unrelated_forward = [
            "-A",
            "FORWARD",
            "-p",
            "udp",
            "-d",
            "10.111.0.52/32",
            "--dport",
            "80",
            "-j",
            "ACCEPT",
        ]
        fake = FakeIptables(
            nat=[udp, matching, non_dnat, other_interface, stale_by_ip],
            forward=[unrelated_forward, forward],
        )
        with patched_pd(run=fake.run, UPLINK="eth0"):
            snapshots = required_callable("_affected_firewall_rules")(
                {22001}, "10.111.0.52"
            )
        self.assertEqual(
            snapshots,
            [
                ("nat", "PREROUTING", 2, matching[2:]),
                ("nat", "PREROUTING", 5, stale_by_ip[2:]),
                (None, "FORWARD", 2, forward[2:]),
            ],
        )

    def test_checked_delete_failure_does_not_hide_error(self):
        rule = [
            "-A",
            "PREROUTING",
            "-i",
            "eth0",
            "-p",
            "tcp",
            "--dport",
            "22001",
            "-j",
            "DNAT",
            "--to-destination",
            "10.111.0.52:80",
        ]
        fake = FakeIptables(nat=[rule], fail_when=lambda cmd: "-D" in cmd)
        journal = []
        with patched_pd(run=fake.run):
            with self.assertRaises(subprocess.CalledProcessError):
                required_callable("_delete_snapshots")(
                    [("nat", "PREROUTING", 1, rule[2:])], journal
                )
        self.assertEqual(journal, [])
        self.assertEqual(fake.rules[("nat", "PREROUTING")], [rule])

    def test_duplicate_matching_rules_delete_and_rollback_in_original_order(self):
        duplicate = [
            "-A",
            "PREROUTING",
            "-i",
            "eth0",
            "-p",
            "tcp",
            "--dport",
            "22001",
            "-j",
            "DNAT",
            "--to-destination",
            "10.111.0.52:80",
        ]
        unrelated = [
            "-A",
            "PREROUTING",
            "-p",
            "udp",
            "--dport",
            "22001",
            "-j",
            "ACCEPT",
        ]
        original = [duplicate, unrelated, duplicate]
        fake = FakeIptables(nat=original)
        with patched_pd(run=fake.run, UPLINK="eth0"):
            snapshots = required_callable("_affected_firewall_rules")(
                {22001}, "10.111.0.52"
            )
            journal = []
            required_callable("_delete_snapshots")(snapshots, journal)
            self.assertEqual(fake.rules[("nat", "PREROUTING")], [unrelated])
            required_callable("_rollback")(journal)
        self.assertEqual(fake.rules[("nat", "PREROUTING")], original)

class CleanupTests(unittest.TestCase):
    def test_cleanup_attempts_later_steps_after_exposure_failure(self):
        events = []

        def fail_exposure(_name):
            events.append("remove_exposure")
            raise RuntimeError("corrupt exposure registry")

        def fake_run(cmd, check=True, timeout=None):
            events.append(("run", list(cmd)))
            if cmd[0] == "iptables":
                raise RuntimeError("iptables unavailable")
            return SimpleNamespace(stdout="", returncode=0)

        with patched_pd(
            run=fake_run,
            remove_exposure=fail_exposure,
            dnsmasq_drop=lambda name: events.append(("dnsmasq_drop", name)),
            MICROVMS_DIR="/microvms",
            SPECS_DIR="/specs",
            GCROOTS_DIR="/gcroots",
        ):
            with mock.patch.object(pd.shutil, "rmtree"):
                with mock.patch.object(pd.os, "unlink", side_effect=FileNotFoundError):
                    with self.assertRaisesRegex(RuntimeError, "corrupt exposure registry"):
                        required_callable("cleanup_vm")("garnix-42")

        self.assertIn(
            ("run", ["systemctl", "stop", "microvm@garnix-42.service"]),
            events,
        )
        self.assertIn(("run", ["ip", "link", "delete", "gx42"]), events)
        self.assertIn(("dnsmasq_drop", "garnix-42"), events)
        self.assertIn(
            (
                "run",
                [
                    "systemctl",
                    "reset-failed",
                    "microvm@garnix-42.service",
                    "microvm-tap-interfaces@garnix-42.service",
                    "microvm-set-booted@garnix-42.service",
                ],
            ),
            events,
        )

    def test_cleanup_recovers_corrupt_registry_from_deterministic_guest_ip(self):
        old_nat = [
            [
                "-A",
                "PREROUTING",
                "-i",
                "eth0",
                "-p",
                "tcp",
                "--dport",
                "32009",
                "-j",
                "DNAT",
                "--to-destination",
                "10.111.0.52:80",
            ]
        ]
        old_forward = [
            [
                "-A",
                "FORWARD",
                "-p",
                "tcp",
                "-d",
                "10.111.0.52/32",
                "--dport",
                "80",
                "-j",
                "ACCEPT",
            ]
        ]
        firewall = FakeIptables(nat=old_nat, forward=old_forward)
        with tempfile.TemporaryDirectory() as d:
            exposed = os.path.join(d, "exposed")
            microvms = os.path.join(d, "microvms")
            specs = os.path.join(d, "specs")
            gcroots = os.path.join(d, "gcroots")
            for path in (exposed, microvms, specs, gcroots):
                os.makedirs(path)
            registry = os.path.join(exposed, "garnix-42.json")
            with open(registry, "w") as f:
                f.write('{"ip":')

            def fake_run(cmd, check=True, timeout=None):
                if cmd[0] == "iptables":
                    return firewall.run(cmd, check=check, timeout=timeout)
                return SimpleNamespace(stdout="", returncode=0)

            with patched_pd(
                EXPOSED_DIR=exposed,
                MICROVMS_DIR=microvms,
                SPECS_DIR=specs,
                GCROOTS_DIR=gcroots,
                DNSMASQ_HOSTS=os.path.join(d, "dnsmasq-hosts"),
                UPLINK="eth0",
                run=fake_run,
            ):
                with open(pd.DNSMASQ_HOSTS, "w") as f:
                    f.write("aa:bb:cc:dd:ee:ff,10.111.0.52,garnix-42\n")
                required_callable("cleanup_vm")("garnix-42")

            self.assertFalse(os.path.exists(registry))
            self.assertEqual(firewall.rules[("nat", "PREROUTING")], [])
            self.assertEqual(firewall.rules[(None, "FORWARD")], [])

    def test_cleanup_stops_path_dependent_units_before_removing_vm_directory(self):
        events = []

        def fake_run(cmd, check=True, timeout=None):
            events.append(("run", list(cmd), check))
            return SimpleNamespace(stdout="", returncode=0)

        def fake_rmtree(path, ignore_errors=False):
            events.append(("rmtree", path, ignore_errors))

        def fake_unlink(path):
            events.append(("unlink", path))

        with patched_pd(
            run=fake_run,
            remove_exposure=lambda name: events.append(("remove_exposure", name)),
            dnsmasq_drop=lambda name: events.append(("dnsmasq_drop", name)),
            MICROVMS_DIR="/microvms",
            SPECS_DIR="/specs",
            GCROOTS_DIR="/gcroots",
        ):
            with mock.patch.object(pd.shutil, "rmtree", side_effect=fake_rmtree):
                with mock.patch.object(pd.os, "unlink", side_effect=fake_unlink):
                    required_callable("cleanup_vm")("garnix-42")

        first_rmtree = next(i for i, event in enumerate(events) if event[0] == "rmtree")
        before_rmtree = events[:first_rmtree]
        self.assertIn(
            ("run", ["systemctl", "stop", "microvm@garnix-42.service"], False),
            before_rmtree,
        )
        self.assertIn(
            (
                "run",
                ["systemctl", "stop", "microvm-tap-interfaces@garnix-42.service"],
                False,
            ),
            before_rmtree,
        )
        self.assertIn(
            (
                "run",
                ["systemctl", "stop", "microvm-set-booted@garnix-42.service"],
                False,
            ),
            before_rmtree,
        )
        self.assertIn(
            ("run", ["ip", "link", "delete", "gx42"], False), before_rmtree
        )
        self.assertEqual(
            events[-1],
            (
                "run",
                [
                    "systemctl",
                    "reset-failed",
                    "microvm@garnix-42.service",
                    "microvm-tap-interfaces@garnix-42.service",
                    "microvm-set-booted@garnix-42.service",
                ],
                False,
            ),
        )

    def test_cleanup_repeats_all_idempotent_teardown_attempts(self):
        calls = []

        def fake_run(cmd, check=True, timeout=None):
            calls.append(list(cmd))
            return SimpleNamespace(stdout="", returncode=1)

        with patched_pd(
            run=fake_run,
            remove_exposure=lambda _name: None,
            dnsmasq_drop=lambda _name: None,
            MICROVMS_DIR="/missing-microvms",
            SPECS_DIR="/missing-specs",
            GCROOTS_DIR="/missing-gcroots",
        ):
            with mock.patch.object(pd.shutil, "rmtree"):
                with mock.patch.object(pd.os, "unlink", side_effect=FileNotFoundError):
                    required_callable("cleanup_vm")("garnix-42")
                    required_callable("cleanup_vm")("garnix-42")

        self.assertEqual(calls.count(["ip", "link", "delete", "gx42"]), 2)
        self.assertEqual(
            calls.count(
                [
                    "systemctl",
                    "reset-failed",
                    "microvm@garnix-42.service",
                    "microvm-tap-interfaces@garnix-42.service",
                    "microvm-set-booted@garnix-42.service",
                ]
            ),
            2,
        )

class GuestSpecTests(unittest.TestCase):
    def render_guest_spec(self, terminal_ca=None, terminal_ca_missing=False, guest_cpu=""):
        with tempfile.TemporaryDirectory() as d:
            profile = os.path.join(d, "guest-profile.nix")
            hosting_key = os.path.join(d, "hosting.pub")
            terminal_key = os.path.join(d, "terminal-ca.pub")
            with open(profile, "w") as f:
                f.write("{ ... }: {}\n")
            with open(hosting_key, "w") as f:
                f.write("ssh-ed25519 HOSTING hosting\n")
            terminal_path = ""
            if terminal_ca_missing:
                terminal_path = terminal_key
            elif terminal_ca is not None:
                terminal_path = terminal_key
                with open(terminal_key, "w") as f:
                    f.write(terminal_ca + "\n")
            with patched_pd(
                SPECS_DIR=os.path.join(d, "specs"),
                GUEST_PROFILE=profile,
                SSH_PUBKEY_FILE=hosting_key,
                TERMINAL_CA_PUBKEY_FILE=terminal_path,
                GUEST_CPU=guest_cpu,
            ):
                spec_dir = required_callable("write_spec")("garnix-42", 42, 4, 8192)
                with open(os.path.join(spec_dir, "guest.nix")) as f:
                    return f.read()

    def test_guest_spec_uses_named_tap_without_bridge_attribute(self):
        guest = self.render_guest_spec()
        self.assertIn('{ type = "tap"; id = "gx42"; mac = "02:67:78:00:00:2a"; }', guest)
        self.assertNotIn("bridge =", guest)

    def test_guest_spec_emits_dedicated_terminal_ca(self):
        guest = self.render_guest_spec(terminal_ca="ssh-ed25519 TERMINAL terminal")
        self.assertIn('garnix.guest.sshPublicKey = "ssh-ed25519 HOSTING hosting";', guest)
        self.assertIn(
            'garnix.guest.terminalCaPublicKey = "ssh-ed25519 TERMINAL terminal";',
            guest,
        )

    def test_guest_spec_does_not_embed_stats_configuration_before_claim(self):
        guest = self.render_guest_spec()
        self.assertNotIn("garnix.guest.statsReportUrl", guest)
        self.assertNotIn("garnix.guest.provisionerId", guest)

    def test_guest_spec_falls_back_to_hosting_key_for_terminal_ca(self):
        guest = self.render_guest_spec()
        self.assertIn(
            'garnix.guest.terminalCaPublicKey = "ssh-ed25519 HOSTING hosting";',
            guest,
        )

    def test_guest_spec_falls_back_when_terminal_ca_file_is_missing(self):
        guest = self.render_guest_spec(terminal_ca_missing=True)
        self.assertIn(
            'garnix.guest.terminalCaPublicKey = "ssh-ed25519 HOSTING hosting";',
            guest,
        )

    def test_guest_spec_falls_back_when_terminal_ca_file_is_empty(self):
        guest = self.render_guest_spec(terminal_ca="")
        self.assertIn(
            'garnix.guest.terminalCaPublicKey = "ssh-ed25519 HOSTING hosting";',
            guest,
        )

    def test_guest_spec_emits_configured_cpu(self):
        guest = self.render_guest_spec(guest_cpu="IvyBridge")
        self.assertIn('microvm.cpu = "IvyBridge";', guest)

    def test_guest_spec_omits_empty_cpu(self):
        guest = self.render_guest_spec(guest_cpu="")
        self.assertNotIn("microvm.cpu =", guest)

class ExposeTests(unittest.TestCase):
    def expose_with_firewall(self, exposed_dir, req, firewall=None, **settings):
        firewall = firewall or FakeIptables()
        values = {
            "EXPOSED_DIR": exposed_dir,
            "SSH_PORT_BASE": 22000,
            "TCP_PORT_BASE": 32000,
            "PORT_RANGE_END": 41999,
            "UPLINK": "eth0",
            "run": firewall.run,
        }
        values.update(settings)
        with patched_pd(**values):
            result = required_callable("do_expose")(req)
        return result, firewall

    def test_do_expose_allocates_lowest_ports_free_in_registry(self):
        with tempfile.TemporaryDirectory() as d:
            write_state(
                d,
                "garnix-other",
                {
                    "ip": "10.111.0.99",
                    "rules": [
                        {"host": 22000, "guest": 22},
                        {"host": 32000, "guest": 8080},
                    ],
                },
            )
            result, _ = self.expose_with_firewall(
                d, {"id": 42, "ssh_expose": True, "tcp_ports": [80, 443]}
            )
        self.assertEqual(
            result,
            {
                "ssh_port": 22001,
                "tcp_ports": [
                    {"guest": 80, "host": 32001},
                    {"guest": 443, "host": 32002},
                ],
            },
        )

    def test_do_expose_preserves_this_guests_previous_ports(self):
        old_nat = [
            [
                "-A",
                "PREROUTING",
                "-i",
                "eth0",
                "-p",
                "tcp",
                "--dport",
                "22007",
                "-j",
                "DNAT",
                "--to-destination",
                "10.111.0.52:22",
            ],
            [
                "-A",
                "PREROUTING",
                "-i",
                "eth0",
                "-p",
                "tcp",
                "--dport",
                "32009",
                "-j",
                "DNAT",
                "--to-destination",
                "10.111.0.52:80",
            ],
        ]
        old_forward = [
            [
                "-A",
                "FORWARD",
                "-p",
                "tcp",
                "-d",
                "10.111.0.52/32",
                "--dport",
                "22",
                "-j",
                "ACCEPT",
            ],
            [
                "-A",
                "FORWARD",
                "-p",
                "tcp",
                "-d",
                "10.111.0.52/32",
                "--dport",
                "80",
                "-j",
                "ACCEPT",
            ],
        ]
        with tempfile.TemporaryDirectory() as d:
            write_state(
                d,
                "garnix-42",
                {
                    "ip": "10.111.0.52",
                    "rules": [
                        {"host": 22007, "guest": 22},
                        {"host": 32009, "guest": 80},
                    ],
                },
            )
            result, _ = self.expose_with_firewall(
                d,
                {"id": 42, "ssh_expose": True, "tcp_ports": [80]},
                firewall=FakeIptables(nat=old_nat, forward=old_forward),
            )
        self.assertEqual(
            result,
            {"ssh_port": 22007, "tcp_ports": [{"guest": 80, "host": 32009}]},
        )

    def test_do_expose_replaces_stale_guest_rules_and_preserves_unrelated_rules(self):
        unrelated_nat = [
            "-A",
            "PREROUTING",
            "-i",
            "eth0",
            "-p",
            "udp",
            "--dport",
            "32000",
            "-j",
            "ACCEPT",
        ]
        stale_nat = [
            "-A",
            "PREROUTING",
            "-i",
            "eth0",
            "-p",
            "tcp",
            "--dport",
            "32999",
            "-j",
            "DNAT",
            "--to-destination",
            "10.111.0.52:9000",
        ]
        unrelated_forward = [
            "-A",
            "FORWARD",
            "-p",
            "udp",
            "-d",
            "10.111.0.52/32",
            "--dport",
            "9000",
            "-j",
            "ACCEPT",
        ]
        stale_forward = [
            "-A",
            "FORWARD",
            "-p",
            "tcp",
            "-d",
            "10.111.0.52/32",
            "--dport",
            "9000",
            "-j",
            "ACCEPT",
        ]
        firewall = FakeIptables(
            nat=[unrelated_nat, stale_nat],
            forward=[unrelated_forward, stale_forward],
        )
        with tempfile.TemporaryDirectory() as d:
            _, firewall = self.expose_with_firewall(
                d,
                {"id": 42, "ssh_expose": True, "tcp_ports": [80, 443]},
                firewall=firewall,
            )
        self.assertIn(unrelated_nat, firewall.rules[("nat", "PREROUTING")])
        self.assertNotIn(stale_nat, firewall.rules[("nat", "PREROUTING")])
        self.assertIn(unrelated_forward, firewall.rules[(None, "FORWARD")])
        self.assertNotIn(stale_forward, firewall.rules[(None, "FORWARD")])

    def test_exhaustion_preflight_preserves_old_state_and_makes_no_firewall_calls(self):
        with tempfile.TemporaryDirectory() as d:
            path = write_state(
                d,
                "garnix-42",
                {
                    "ip": "10.111.0.52",
                    "rules": [{"host": 32000, "guest": 80}],
                },
            )
            with open(path, "rb") as f:
                before = f.read()
            firewall = FakeIptables()
            with patched_pd(
                EXPOSED_DIR=d,
                SSH_PORT_BASE=22000,
                TCP_PORT_BASE=32000,
                PORT_RANGE_END=32000,
                UPLINK="eth0",
                run=firewall.run,
            ):
                with self.assertRaisesRegex(RuntimeError, "no free host port"):
                    required_callable("do_expose")(
                        {"id": 42, "ssh_expose": False, "tcp_ports": [80, 443]}
                    )
            with open(path, "rb") as f:
                self.assertEqual(f.read(), before)
            self.assertEqual(firewall.calls, [])

    def test_duplicate_tcp_ports_get_one_stable_mapping(self):
        with tempfile.TemporaryDirectory() as d:
            first, firewall = self.expose_with_firewall(
                d,
                {"id": 42, "ssh_expose": False, "tcp_ports": [80, 80, 443, 80]},
            )
            second, firewall = self.expose_with_firewall(
                d,
                {"id": 42, "ssh_expose": False, "tcp_ports": [80, 80, 443]},
                firewall=firewall,
            )
        self.assertEqual(
            first["tcp_ports"],
            [{"guest": 80, "host": 32000}, {"guest": 443, "host": 32001}],
        )
        self.assertEqual(second, first)

    def test_tcp_22_shares_ssh_mapping_when_ssh_is_exposed(self):
        with tempfile.TemporaryDirectory() as d:
            result, _ = self.expose_with_firewall(
                d, {"id": 42, "ssh_expose": True, "tcp_ports": [22, 22]}
            )
            with open(os.path.join(d, "garnix-42.json")) as f:
                state = json.load(f)
        self.assertEqual(
            result,
            {"ssh_port": 22000, "tcp_ports": [{"guest": 22, "host": 22000}]},
        )
        self.assertEqual(state["rules"], [{"host": 22000, "guest": 22}])

    def test_tcp_22_uses_tcp_range_without_ssh_exposure(self):
        with tempfile.TemporaryDirectory() as d:
            result, _ = self.expose_with_firewall(
                d, {"id": 42, "ssh_expose": False, "tcp_ports": [22]}
            )
        self.assertEqual(
            result,
            {"ssh_port": None, "tcp_ports": [{"guest": 22, "host": 32000}]},
        )

    def test_registry_write_failure_rolls_back_exact_live_rules(self):
        old_nat = [
            ["-A", "PREROUTING", "-p", "tcp", "--dport", "9999", "-j", "ACCEPT"],
            [
                "-A",
                "PREROUTING",
                "-i",
                "eth0",
                "-p",
                "tcp",
                "--dport",
                "32009",
                "-j",
                "DNAT",
                "--to-destination",
                "10.111.0.52:80",
            ],
            ["-A", "PREROUTING", "-p", "udp", "--dport", "32009", "-j", "ACCEPT"],
        ]
        old_forward = [
            [
                "-A",
                "FORWARD",
                "-p",
                "tcp",
                "-d",
                "10.111.0.52/32",
                "--dport",
                "80",
                "-j",
                "ACCEPT",
            ]
        ]
        firewall = FakeIptables(nat=old_nat, forward=old_forward)
        with tempfile.TemporaryDirectory() as d:
            path = write_state(
                d,
                "garnix-42",
                {
                    "ip": "10.111.0.52",
                    "rules": [{"host": 32009, "guest": 80}],
                },
            )
            with open(path, "rb") as f:
                before = f.read()
            with patched_pd(EXPOSED_DIR=d, run=firewall.run, UPLINK="eth0"):
                with mock.patch.object(pd, "write_exposure", side_effect=OSError("disk full")):
                    with self.assertRaisesRegex(OSError, "disk full"):
                        required_callable("do_expose")(
                            {"id": 42, "ssh_expose": False, "tcp_ports": [80]}
                        )
            with open(path, "rb") as f:
                self.assertEqual(f.read(), before)
        self.assertEqual(firewall.rules[("nat", "PREROUTING")], old_nat)
        self.assertEqual(firewall.rules[(None, "FORWARD")], old_forward)

    def test_add_failure_rolls_back_deleted_rules_in_original_order(self):
        old_nat = [
            ["-A", "PREROUTING", "-p", "tcp", "--dport", "9998", "-j", "ACCEPT"],
            [
                "-A",
                "PREROUTING",
                "-i",
                "eth0",
                "-p",
                "tcp",
                "--dport",
                "32009",
                "-j",
                "DNAT",
                "--to-destination",
                "10.111.0.52:80",
            ],
            ["-A", "PREROUTING", "-p", "tcp", "--dport", "9999", "-j", "ACCEPT"],
        ]
        firewall = FakeIptables(nat=old_nat, fail_when=lambda cmd: "-A" in cmd)
        with tempfile.TemporaryDirectory() as d:
            write_state(
                d,
                "garnix-42",
                {
                    "ip": "10.111.0.52",
                    "rules": [{"host": 32009, "guest": 80}],
                },
            )
            with patched_pd(EXPOSED_DIR=d, run=firewall.run, UPLINK="eth0"):
                with self.assertRaises(subprocess.CalledProcessError):
                    required_callable("do_expose")(
                        {"id": 42, "ssh_expose": False, "tcp_ports": [80]}
                    )
        self.assertEqual(firewall.rules[("nat", "PREROUTING")], old_nat)

    def test_invalid_registry_fails_before_firewall_side_effects(self):
        invalid = [
            '{"ip":',
            "[]",
            '{"ip":"10.111.0.52","rules":[{"guest":80}]}',
            '{"ip":"10.111.0.52","rules":[{"host":"32000","guest":80}]}',
            '{"ip":"10.111.0.99","rules":[]}',
        ]
        for body in invalid:
            with self.subTest(body=body), tempfile.TemporaryDirectory() as d:
                path = os.path.join(d, "garnix-42.json")
                with open(path, "w") as f:
                    f.write(body)
                firewall = FakeIptables()
                with patched_pd(EXPOSED_DIR=d, run=firewall.run):
                    with self.assertRaises(RuntimeError):
                        required_callable("do_expose")(
                            {"id": 42, "ssh_expose": False, "tcp_ports": [80]}
                        )
                self.assertEqual(firewall.calls, [])
                with open(path) as f:
                    self.assertEqual(f.read(), body)

    def test_delete_failure_aborts_before_new_rule_is_added(self):
        old = [
            "-A",
            "PREROUTING",
            "-i",
            "eth0",
            "-p",
            "tcp",
            "--dport",
            "32009",
            "-j",
            "DNAT",
            "--to-destination",
            "10.111.0.52:80",
        ]
        firewall = FakeIptables(nat=[old], fail_when=lambda cmd: "-D" in cmd)
        with tempfile.TemporaryDirectory() as d:
            write_state(
                d,
                "garnix-42",
                {
                    "ip": "10.111.0.52",
                    "rules": [{"host": 32009, "guest": 80}],
                },
            )
            with patched_pd(EXPOSED_DIR=d, run=firewall.run, UPLINK="eth0"):
                with self.assertRaises(subprocess.CalledProcessError):
                    required_callable("do_expose")(
                        {"id": 42, "ssh_expose": False, "tcp_ports": [80]}
                    )
        mutations = [call[0] for call in firewall.calls if "-S" not in call[0]]
        self.assertTrue(mutations)
        self.assertTrue(all("-A" not in cmd for cmd in mutations))

    def test_rollback_failure_reports_both_failures(self):
        old = [
            "-A",
            "PREROUTING",
            "-i",
            "eth0",
            "-p",
            "tcp",
            "--dport",
            "32009",
            "-j",
            "DNAT",
            "--to-destination",
            "10.111.0.52:80",
        ]
        failures = {"add": False}

        def fail_when(cmd):
            if "-A" in cmd:
                failures["add"] = True
                return True
            return failures["add"] and "-I" in cmd

        firewall = FakeIptables(nat=[old], fail_when=fail_when)
        with tempfile.TemporaryDirectory() as d:
            write_state(
                d,
                "garnix-42",
                {
                    "ip": "10.111.0.52",
                    "rules": [{"host": 32009, "guest": 80}],
                },
            )
            with patched_pd(EXPOSED_DIR=d, run=firewall.run, UPLINK="eth0"):
                with self.assertRaisesRegex(RuntimeError, "rollback also failed"):
                    required_callable("do_expose")(
                        {"id": 42, "ssh_expose": False, "tcp_ports": [80]}
                    )

    def test_remove_exposure_validates_before_mutating(self):
        firewall = FakeIptables()
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "garnix-42.json")
            with open(path, "w") as f:
                f.write('{"ip":')
            with patched_pd(EXPOSED_DIR=d, run=firewall.run):
                with self.assertRaises(RuntimeError):
                    required_callable("remove_exposure")("garnix-42")
            self.assertEqual(firewall.calls, [])
            self.assertTrue(os.path.exists(path))

    def test_remove_exposure_commits_rule_and_registry_removal(self):
        old_nat = [
            [
                "-A",
                "PREROUTING",
                "-i",
                "eth0",
                "-p",
                "tcp",
                "--dport",
                "32009",
                "-j",
                "DNAT",
                "--to-destination",
                "10.111.0.52:80",
            ]
        ]
        old_forward = [
            [
                "-A",
                "FORWARD",
                "-p",
                "tcp",
                "-d",
                "10.111.0.52/32",
                "--dport",
                "80",
                "-j",
                "ACCEPT",
            ]
        ]
        firewall = FakeIptables(nat=old_nat, forward=old_forward)
        with tempfile.TemporaryDirectory() as d:
            path = write_state(
                d,
                "garnix-42",
                {
                    "ip": "10.111.0.52",
                    "rules": [{"host": 32009, "guest": 80}],
                },
            )
            with patched_pd(EXPOSED_DIR=d, run=firewall.run, UPLINK="eth0"):
                required_callable("remove_exposure")("garnix-42")
            self.assertFalse(os.path.exists(path))
        self.assertEqual(firewall.rules[("nat", "PREROUTING")], [])
        self.assertEqual(firewall.rules[(None, "FORWARD")], [])

    def test_remove_exposure_unlink_failure_restores_live_rules(self):
        old_nat = [
            [
                "-A",
                "PREROUTING",
                "-i",
                "eth0",
                "-p",
                "tcp",
                "--dport",
                "32009",
                "-j",
                "DNAT",
                "--to-destination",
                "10.111.0.52:80",
            ]
        ]
        old_forward = [
            [
                "-A",
                "FORWARD",
                "-p",
                "tcp",
                "-d",
                "10.111.0.52/32",
                "--dport",
                "80",
                "-j",
                "ACCEPT",
            ]
        ]
        firewall = FakeIptables(nat=old_nat, forward=old_forward)
        with tempfile.TemporaryDirectory() as d:
            path = write_state(
                d,
                "garnix-42",
                {
                    "ip": "10.111.0.52",
                    "rules": [{"host": 32009, "guest": 80}],
                },
            )
            with patched_pd(EXPOSED_DIR=d, run=firewall.run, UPLINK="eth0"):
                with mock.patch.object(pd.os, "unlink", side_effect=OSError("unlink failed")):
                    with self.assertRaisesRegex(OSError, "unlink failed"):
                        required_callable("remove_exposure")("garnix-42")
            self.assertTrue(os.path.exists(path))
        self.assertEqual(firewall.rules[("nat", "PREROUTING")], old_nat)
        self.assertEqual(firewall.rules[(None, "FORWARD")], old_forward)

if __name__ == "__main__":
    unittest.main()
