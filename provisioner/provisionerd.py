"""garnix-provisionerd: create/destroy/status for local microvm.nix guests.

The garnix backend's LocalProvisioner (backend/src/Garnix/LocalProvisioner.hs)
speaks newline-delimited JSON over a unix socket, one request per connection:

  {"action": "create", "id": <int32>, "vcpu": N, "mem": MiB} -> {"ipv4": "10.111.0.X"}
  {"action": "destroy", "id": <int32>}                       -> {}
  {"action": "status", "id": <int32>}                        -> {"status": "running"|"off"}

Errors are returned as {"error": "..."}. The int32 id is the backend's
"hetzner id"; the guest is named garnix-<id> with a deterministic MAC/IP so
the dnsmasq DHCP reservation survives daemon restarts without extra state.

Runs as root (systemd unit garnix-provisionerd from provisioner/nixos-module.nix,
which supplies all PROVISIONER_* environment variables). Pure stdlib.
"""

import json
import logging
import os
import re
import shlex
import shutil
import socket
import socketserver
import subprocess
import sys
import tempfile
import threading
import time

log = logging.getLogger("garnix-provisionerd")

SOCKET_PATH = os.environ["PROVISIONER_SOCKET"]
SOCKET_GROUP = os.environ.get("PROVISIONER_SOCKET_GROUP", "garnix")
STATE_DIR = os.environ.get("PROVISIONER_STATE_DIR", "/var/lib/garnix-provisioner")
BRIDGE = os.environ.get("PROVISIONER_BRIDGE", "garnixbr0")
SUBNET_PREFIX = os.environ.get("PROVISIONER_SUBNET_PREFIX", "10.111.0")
NIXPKGS_FLAKE = os.environ["PROVISIONER_NIXPKGS"]
MICROVM_FLAKE = os.environ["PROVISIONER_MICROVM"]
GUEST_PROFILE = os.environ["PROVISIONER_GUEST_PROFILE"]
SSH_PUBKEY_FILE = os.environ["PROVISIONER_SSH_PUBKEY_FILE"]
TERMINAL_CA_PUBKEY_FILE = os.environ.get("PROVISIONER_TERMINAL_CA_PUBKEY_FILE", "")
GUEST_CPU = os.environ.get("PROVISIONER_GUEST_CPU", "")

DNSMASQ_HOSTS = os.path.join(STATE_DIR, "dnsmasq-hosts")
SPECS_DIR = os.path.join(STATE_DIR, "specs")
EXPOSED_DIR = os.path.join(STATE_DIR, "exposed")
LEASES_FILE = os.path.join(STATE_DIR, "ip-leases.json")
MICROVMS_DIR = "/var/lib/microvms"
GCROOTS_DIR = "/nix/var/nix/gcroots/microvm"

UPLINK = os.environ.get("PROVISIONER_UPLINK", "eth0")
SSH_PORT_BASE = int(os.environ.get("PROVISIONER_SSH_PORT_BASE", "22000"))
TCP_PORT_BASE = int(os.environ.get("PROVISIONER_TCP_PORT_BASE", "32000"))
TCP_PORTS_PER_VM = 20
PORT_RANGE_END = int(os.environ.get("PROVISIONER_PORT_RANGE_END", "41999"))

SSH_WAIT_SECONDS = int(os.environ.get("PROVISIONER_SSH_WAIT_SECONDS", "120"))

DHCP_LEASE_TIME = os.environ.get("PROVISIONER_DHCP_LEASE_TIME", "1h")

mutate_lock = threading.Lock()

def vm_name(vm_id: int) -> str:
    return f"garnix-{vm_id}"

def vm_mac(vm_id: int) -> str:
    b = vm_id & 0xFFFFFF
    return f"02:67:78:{(b >> 16) & 0xFF:02x}:{(b >> 8) & 0xFF:02x}:{b & 0xFF:02x}"

IP_HOST_MIN = 10
IP_HOST_MAX = 249

def preferred_ip(vm_id: int) -> str:
    """The address a guest gets when nothing else already holds it.

    Identical to the historical pure-function mapping, so an existing pool
    keeps every address it had; allocate_ip only diverges on collision.
    """
    span = IP_HOST_MAX - IP_HOST_MIN + 1
    return f"{SUBNET_PREFIX}.{IP_HOST_MIN + vm_id % span}"

def _read_leases() -> dict:
    try:
        with open(LEASES_FILE) as f:
            return json.load(f)
    except FileNotFoundError:
        return {}
    except (ValueError, OSError) as error:
        raise RuntimeError(f"cannot read IP ledger {LEASES_FILE}: {error}") from error

def _write_leases(leases: dict):
    tmp = LEASES_FILE + ".tmp"
    with open(tmp, "w") as f:
        json.dump(leases, f, indent=2, sort_keys=True)
        f.flush()
        os.fsync(f.fileno())
    os.replace(tmp, LEASES_FILE)

def allocate_ip(name: str, vm_id: int) -> str:
    """Reserve an address for @name and persist the assignment.

    The id-modulo mapping wrapped the /24, so two live guests whose ids are
    240 apart were handed the same address: the second guest's DHCP
    reservation and DNAT rules then pointed at the first guest's traffic.
    Id reuse is a matter of time once a pool churns, and on a multi-tenant
    pool the result is a cross-tenant leak rather than a glitch. The ledger
    makes the assignment explicit and makes exhaustion loud.

    Callers must hold mutate_lock.
    """
    leases = _read_leases()
    existing = leases.get(name)
    if existing:
        return existing
    taken = set(leases.values())
    preferred = preferred_ip(vm_id)
    chosen = preferred if preferred not in taken else None
    if chosen is None:
        for host in range(IP_HOST_MIN, IP_HOST_MAX + 1):
            candidate = f"{SUBNET_PREFIX}.{host}"
            if candidate not in taken:
                chosen = candidate
                break
    if chosen is None:
        raise RuntimeError(
            f"no free address in {SUBNET_PREFIX}.0/24: "
            f"{len(taken)} guests hold .{IP_HOST_MIN}-.{IP_HOST_MAX}"
        )
    if chosen != preferred:
        log.warning("%s: preferred %s is taken, allocated %s", name, preferred, chosen)
    leases[name] = chosen
    _write_leases(leases)
    return chosen

def release_ip(name: str):
    """Return @name's address to the pool. Idempotent."""
    leases = _read_leases()
    if leases.pop(name, None) is not None:
        _write_leases(leases)

def run(cmd, check=True, timeout=None):
    log.info("+ %s", " ".join(cmd))
    return subprocess.run(
        cmd, check=check, timeout=timeout, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True
    )

def nix_str(s: str) -> str:
    """Render a python string as a Nix double-quoted string literal."""
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"').replace("${", "\\${") + '"'

def write_spec(name: str, vm_id: int, vcpu: int, mem: int) -> str:
    """Write the per-VM flake (flake.nix + guest.nix) and return the spec dir."""
    with open(SSH_PUBKEY_FILE) as f:
        pubkey = f.read().strip()
    terminal_ca_pubkey = pubkey
    if TERMINAL_CA_PUBKEY_FILE:
        try:
            with open(TERMINAL_CA_PUBKEY_FILE) as f:
                _tca = f.read().strip()
            if _tca:
                terminal_ca_pubkey = _tca
        except FileNotFoundError:
            pass
    spec_dir = os.path.join(SPECS_DIR, name)
    os.makedirs(spec_dir, exist_ok=True)
    shutil.copyfile(GUEST_PROFILE, os.path.join(spec_dir, "guest-profile.nix"))
    with open(os.path.join(spec_dir, "flake.nix"), "w") as f:
        f.write(
            f"""{{
  description = "garnix-provisioned microVM {name}";
  inputs = {{
    nixpkgs.url = {nix_str(NIXPKGS_FLAKE)};
    microvm.url = {nix_str(MICROVM_FLAKE)};
    microvm.inputs.nixpkgs.follows = "nixpkgs";
  }};
  outputs = {{ self, nixpkgs, microvm }}: {{
    nixosConfigurations.{nix_str(name)} = nixpkgs.lib.nixosSystem {{
      system = "x86_64-linux";
      modules = [ microvm.nixosModules.microvm ./guest.nix ];
    }};
  }};
}}
"""
        )
    with open(os.path.join(spec_dir, "guest.nix"), "w") as f:
        cpu_line = f"  microvm.cpu = {nix_str(GUEST_CPU)};\n" if GUEST_CPU else ""
        f.write(
            f"""{{
  imports = [ ./guest-profile.nix ];
  networking.hostName = {nix_str(name)};
  microvm.vcpu = {vcpu};
  microvm.mem = {mem};
{cpu_line}  microvm.interfaces = [
    {{ type = "tap"; id = {nix_str(f"gx{vm_id}")}; mac = {nix_str(vm_mac(vm_id))}; }}
  ];
  garnix.guest.sshPublicKey = {nix_str(pubkey)};
  garnix.guest.terminalCaPublicKey = {nix_str(terminal_ca_pubkey)};
}}
"""
        )
    return spec_dir

def dnsmasq_reserve(name: str, mac: str, ip: str):
    dnsmasq_drop(name)
    with open(DNSMASQ_HOSTS, "a") as f:
        f.write(f"{mac},{ip},{name},{DHCP_LEASE_TIME}\n")
    run(["systemctl", "reload-or-restart", "dnsmasq.service"], check=False)

def dnsmasq_drop(name: str):
    try:
        with open(DNSMASQ_HOSTS) as f:
            lines = f.readlines()
    except FileNotFoundError:
        lines = []
    kept = [l for l in lines if not re.match(rf"^[^,]*,[^,]*,{re.escape(name)}(,|$)", l.strip())]
    if kept != lines:
        with open(DNSMASQ_HOSTS, "w") as f:
            f.writelines(kept)
        run(["systemctl", "reload-or-restart", "dnsmasq.service"], check=False)

def wait_for_ssh(ip: str, timeout: float):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        try:
            with socket.create_connection((ip, 22), timeout=2):
                return
        except OSError:
            time.sleep(2)
    raise RuntimeError(f"guest {ip} did not open tcp/22 within {int(timeout)}s")

def guest_console_tail(name: str, lines: int = 80) -> str:
    """Recent journal (serial console) of a guest's microvm unit, for failure
    diagnosis: an OOM during first-boot activation, a kernel panic, or a stuck
    unit shows up here instead of an opaque tcp/22 timeout."""
    try:
        out = run(
            ["journalctl", "-u", f"microvm@{name}.service", "--no-pager", "-n", str(lines)],
            check=False,
            timeout=15,
        )
        return out.stdout.strip()[-4000:]
    except Exception:
        return ""

def _dnat_specs(host_port: int, guest_ip: str, guest_port: int):
    """(table, chain, rest-args) for the PREROUTING DNAT + FORWARD ACCEPT."""
    return [
        (
            "nat",
            "PREROUTING",
            ["-i", UPLINK, "-p", "tcp", "--dport", str(host_port),
             "-j", "DNAT", "--to-destination", f"{guest_ip}:{guest_port}"],
        ),
        (
            None,
            "FORWARD",
            ["-p", "tcp", "-d", guest_ip, "--dport", str(guest_port), "-j", "ACCEPT"],
        ),
    ]

def _list_rules(table, chain: str) -> list:
    cmd = ["iptables", "-w", "5"]
    if table:
        cmd += ["-t", table]
    cmd += ["-S", chain]
    res = run(cmd, check=True)
    rules = []
    for line in (res.stdout or "").splitlines():
        try:
            parts = shlex.split(line)
        except ValueError as error:
            raise RuntimeError(f"cannot parse iptables rule in {chain}: {line!r}") from error
        if parts[:2] == ["-A", chain]:
            rules.append(parts)
    return rules

def _rule_value(parts: list, flag: str):
    try:
        index = parts.index(flag)
    except ValueError:
        return None
    return parts[index + 1] if index + 1 < len(parts) else None

def _is_affected_dnat(parts: list, host_ports: set, guest_ip: str) -> bool:
    destination = _rule_value(parts, "--to-destination") or ""
    destination_ip = destination.rsplit(":", 1)[0]
    dport = _rule_value(parts, "--dport")
    return (
        _rule_value(parts, "-i") == UPLINK
        and _rule_value(parts, "-p") == "tcp"
        and _rule_value(parts, "-j") == "DNAT"
        and dport is not None
        and (dport in {str(port) for port in host_ports} or destination_ip == guest_ip)
    )

def _is_affected_forward(parts: list, guest_ip: str) -> bool:
    return (
        _rule_value(parts, "-p") == "tcp"
        and _rule_value(parts, "-d") in {guest_ip, f"{guest_ip}/32"}
        and _rule_value(parts, "--dport") is not None
        and _rule_value(parts, "-j") == "ACCEPT"
    )

def _affected_firewall_rules(host_ports: set, guest_ip: str) -> list:
    snapshots = []
    for position, parts in enumerate(_list_rules("nat", "PREROUTING"), start=1):
        if _is_affected_dnat(parts, host_ports, guest_ip):
            snapshots.append(("nat", "PREROUTING", position, parts[2:]))
    for position, parts in enumerate(_list_rules(None, "FORWARD"), start=1):
        if _is_affected_forward(parts, guest_ip):
            snapshots.append((None, "FORWARD", position, parts[2:]))
    return snapshots

def _iptables_command(table, operation: str, chain: str, rest: list) -> list:
    cmd = ["iptables", "-w", "5"]
    if table:
        cmd += ["-t", table]
    return cmd + [operation, chain] + rest

def _delete_snapshots(snapshots: list, journal: list):
    ordered = sorted(snapshots, key=lambda item: (item[0] or "", item[1], -item[2]))
    for table, chain, position, rest in ordered:
        run(_iptables_command(table, "-D", chain, rest), check=True)
        journal.append(("insert", table, chain, position, rest))

def _append_rule(table, chain: str, rest: list, journal: list):
    run(_iptables_command(table, "-A", chain, rest), check=True)
    journal.append(("delete", table, chain, 0, rest))

def _rollback(journal: list):
    failures = []
    for operation, table, chain, position, rest in reversed(journal):
        inverse_rest = [str(position)] + rest if operation == "insert" else rest
        iptables_operation = "-I" if operation == "insert" else "-D"
        try:
            run(_iptables_command(table, iptables_operation, chain, inverse_rest), check=True)
        except Exception as error:
            failures.append(str(error))
    if failures:
        raise RuntimeError("firewall rollback failed: " + "; ".join(failures))

def _rollback_after_failure(journal: list, original: BaseException):
    try:
        _rollback(journal)
    except Exception as rollback_error:
        raise RuntimeError(
            f"exposure update failed: {original}; rollback also failed: {rollback_error}"
        ) from original

def _exposure_path(name: str) -> str:
    return os.path.join(EXPOSED_DIR, f"{name}.json")

def _is_port(value) -> bool:
    return isinstance(value, int) and not isinstance(value, bool) and 1 <= value <= 65535

def _exposure_rule_kind(host: int, guest: int, path: str) -> str:
    if SSH_PORT_BASE <= host < TCP_PORT_BASE:
        if guest != 22:
            raise RuntimeError(f"invalid SSH-range rule in {path}: guest port must be 22")
        return "ssh"
    if TCP_PORT_BASE <= host <= PORT_RANGE_END:
        return "tcp"
    raise RuntimeError(f"host port {host} in {path} is outside the configured exposure ranges")

def read_exposure(name: str):
    path = _exposure_path(name)
    try:
        with open(path) as f:
            state = json.load(f)
    except FileNotFoundError:
        return None
    except (OSError, ValueError) as error:
        raise RuntimeError(f"cannot read exposure registry {path}: {error}") from error
    if not isinstance(state, dict) or not isinstance(state.get("ip"), str):
        raise RuntimeError(f"invalid exposure registry {path}: expected object with string ip")
    raw_rules = state.get("rules")
    if not isinstance(raw_rules, list):
        raise RuntimeError(f"invalid exposure registry {path}: rules must be a list")
    rules = []
    seen_hosts = set()
    seen_tcp_guests = set()
    seen_ssh = False
    for raw in raw_rules:
        if not isinstance(raw, dict) or not _is_port(raw.get("host")) or not _is_port(raw.get("guest")):
            raise RuntimeError(f"invalid exposure rule in {path}: host and guest must be integer ports")
        host = raw["host"]
        guest = raw["guest"]
        if host in seen_hosts:
            raise RuntimeError(f"duplicate host port {host} in {path}")
        kind = _exposure_rule_kind(host, guest, path)
        if kind == "ssh":
            if seen_ssh:
                raise RuntimeError(f"duplicate SSH exposure in {path}")
            seen_ssh = True
        elif guest in seen_tcp_guests:
            raise RuntimeError(f"duplicate TCP guest port {guest} in {path}")
        else:
            seen_tcp_guests.add(guest)
        seen_hosts.add(host)
        rules.append({"host": host, "guest": guest})
    return {"ip": state["ip"], "rules": rules}

def allocated_host_ports(exclude: str = "") -> set:
    owners = {}
    try:
        entries = sorted(os.listdir(EXPOSED_DIR))
    except FileNotFoundError:
        return set()
    except OSError as error:
        raise RuntimeError(f"cannot list exposure registry {EXPOSED_DIR}: {error}") from error
    for filename in entries:
        if not filename.endswith(".json") or filename == f"{exclude}.json":
            continue
        name = filename.removesuffix(".json")
        state = read_exposure(name)
        if state is None:
            continue
        for rule in state["rules"]:
            host = rule["host"]
            if host in owners:
                raise RuntimeError(
                    f"host port {host} is claimed by both {owners[host]} and {filename}"
                )
            owners[host] = filename
    return set(owners)

def alloc_host_port(used: set, lo: int, hi: int, preferred=None) -> int:
    """Lowest free port in [lo, hi], preferring this VM's previous port so
    re-exposing stays stable. Marks the result used."""
    if preferred is not None and lo <= preferred <= hi and preferred not in used:
        used.add(preferred)
        return preferred
    for port in range(lo, hi + 1):
        if port not in used:
            used.add(port)
            return port
    raise RuntimeError(f"no free host port in {lo}-{hi}")

def _normalize_tcp_ports(values) -> list:
    ports = []
    seen = set()
    for raw in values:
        if isinstance(raw, bool):
            raise RuntimeError("TCP guest port must be an integer in 1-65535")
        try:
            port = int(raw)
        except (TypeError, ValueError) as error:
            raise RuntimeError("TCP guest port must be an integer in 1-65535") from error
        if not 1 <= port <= 65535:
            raise RuntimeError(f"TCP guest port {port} is outside 1-65535")
        if port not in seen:
            seen.add(port)
            ports.append(port)
    return ports[:TCP_PORTS_PER_VM]

def _plan_exposure(name: str, ip: str, ssh_expose: bool, tcp_values) -> dict:
    old_state = read_exposure(name)
    if old_state is not None and old_state["ip"] != ip:
        raise RuntimeError(
            f"exposure registry {_exposure_path(name)} has IP {old_state['ip']}, expected {ip}"
        )
    used = allocated_host_ports()
    preferred_ssh = None
    preferred_tcp = {}
    if old_state is not None:
        for rule in old_state["rules"]:
            used.remove(rule["host"])
            if _exposure_rule_kind(rule["host"], rule["guest"], _exposure_path(name)) == "ssh":
                preferred_ssh = rule["host"]
            else:
                preferred_tcp[rule["guest"]] = rule["host"]
    tcp_ports = _normalize_tcp_ports(tcp_values)
    new_rules = []
    tcp_result = []
    ssh_port = None
    if ssh_expose:
        ssh_port = alloc_host_port(
            used, SSH_PORT_BASE, TCP_PORT_BASE - 1, preferred=preferred_ssh
        )
        new_rules.append({"host": ssh_port, "guest": 22})
    for guest in tcp_ports:
        if guest == 22 and ssh_port is not None:
            tcp_result.append({"guest": 22, "host": ssh_port})
            continue
        host = alloc_host_port(
            used, TCP_PORT_BASE, PORT_RANGE_END, preferred=preferred_tcp.get(guest)
        )
        new_rules.append({"host": host, "guest": guest})
        tcp_result.append({"guest": guest, "host": host})
    return {
        "old_state": old_state,
        "new_rules": new_rules,
        "ssh_port": ssh_port,
        "tcp_result": tcp_result,
        "response": {"ssh_port": ssh_port, "tcp_ports": tcp_result},
        "ip": ip,
    }

def remove_exposure(name: str):
    state = read_exposure(name)
    ip = state["ip"] if state is not None else vm_ip_from_name(name)
    if not ip:
        return
    host_ports = set() if state is None else {rule["host"] for rule in state["rules"]}
    snapshots = _affected_firewall_rules(host_ports, ip)
    journal = []
    try:
        _delete_snapshots(snapshots, journal)
        if state is not None:
            os.unlink(_exposure_path(name))
    except BaseException as error:
        _rollback_after_failure(journal, error)
        raise

def remove_exposure_for_cleanup(name: str):
    """Remove exposure state during guest teardown.

    The normal mutation path validates the registry strictly and must keep
    doing so: accepting corrupt allocation state while assigning ports would
    be unsafe. Teardown has a stronger liveness requirement, though. If that
    registry is corrupt or unreadable, the guest's deterministic IP still lets
    us identify every DNAT/FORWARD rule aimed at it, remove those rules
    transactionally, and discard the unusable registry file.
    """
    registry_failure = None
    try:
        remove_exposure(name)
        return
    except Exception as error:
        registry_failure = error
        guest_ip = vm_ip_from_name(name)
        if not guest_ip:
            raise

    journal = []
    try:
        snapshots = _affected_firewall_rules(set(), guest_ip)
        _delete_snapshots(snapshots, journal)
        try:
            os.unlink(_exposure_path(name))
        except FileNotFoundError:
            pass
    except BaseException as fallback_error:
        try:
            _rollback(journal)
        except Exception as rollback_error:
            raise RuntimeError(
                "exposure cleanup failed: "
                f"{registry_failure}; deterministic-IP fallback also failed: "
                f"{fallback_error}; rollback also failed: {rollback_error}"
            ) from registry_failure
        raise RuntimeError(
            "exposure cleanup failed: "
            f"{registry_failure}; deterministic-IP fallback also failed: {fallback_error}"
        ) from registry_failure
    log.warning(
        "removed exposure for %s via deterministic-IP fallback after registry failure: %s",
        name,
        registry_failure,
    )

def write_exposure(name: str, ip: str, rules: list):
    os.makedirs(EXPOSED_DIR, exist_ok=True)
    path = _exposure_path(name)
    fd, temporary = tempfile.mkstemp(prefix=f".{name}.", suffix=".tmp", dir=EXPOSED_DIR)
    try:
        with os.fdopen(fd, "w") as f:
            json.dump({"ip": ip, "rules": rules}, f)
            f.write("\n")
            f.flush()
            os.fsync(f.fileno())
        os.replace(temporary, path)
    except BaseException:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass
        raise

def vm_ip_from_name(name: str) -> str:
    leases = _read_leases()
    if name in leases:
        return leases[name]
    m = re.match(r"^garnix-(\d+)$", name)
    return preferred_ip(int(m.group(1))) if m else ""

def cleanup_vm(name: str):
    """Best-effort teardown of every trace of a guest (idempotent)."""
    failures = []

    def attempt(label: str, action):
        try:
            action()
        except Exception as error:
            failures.append(f"{label}: {error}")
            log.warning("guest cleanup step failed for %s (%s): %s", name, label, error)

    attempt("exposure", lambda: remove_exposure_for_cleanup(name))
    units = [
        f"microvm@{name}.service",
        f"microvm-tap-interfaces@{name}.service",
        f"microvm-set-booted@{name}.service",
    ]
    for unit in units:
        attempt(f"stop {unit}", lambda unit=unit: run(["systemctl", "stop", unit], check=False))
    match = re.fullmatch(r"garnix-(\d+)", name)
    if match:
        attempt(
            "tap",
            lambda: run(["ip", "link", "delete", f"gx{match.group(1)}"], check=False),
        )

    def remove_tree(path: str):
        try:
            shutil.rmtree(path)
        except FileNotFoundError:
            pass

    attempt("microvm directory", lambda: remove_tree(os.path.join(MICROVMS_DIR, name)))
    attempt("spec directory", lambda: remove_tree(os.path.join(SPECS_DIR, name)))
    for root in (name, f"booted-{name}"):
        def remove_root(root=root):
            try:
                os.unlink(os.path.join(GCROOTS_DIR, root))
            except FileNotFoundError:
                pass

        attempt(f"gcroot {root}", remove_root)
    attempt("dnsmasq reservation", lambda: dnsmasq_drop(name))
    attempt("ip lease", lambda: release_ip(name))
    attempt(
        "reset failed units",
        lambda: run(["systemctl", "reset-failed", *units], check=False),
    )
    if failures:
        raise RuntimeError("guest cleanup incomplete: " + "; ".join(failures))

def do_create(req: dict) -> dict:
    vm_id = int(req["id"])
    vcpu = int(req["vcpu"])
    mem = int(req["mem"])
    name = vm_name(vm_id)
    mac = vm_mac(vm_id)
    with mutate_lock:
        cleanup_vm(name)
        ip = allocate_ip(name, vm_id)
        spec_dir = write_spec(name, vm_id, vcpu, mem)
        dnsmasq_reserve(name, mac, ip)
        try:
            create = run(["microvm", "-c", name, "-f", f"path:{spec_dir}"], check=False, timeout=1800)
            if create.returncode != 0:
                raise RuntimeError(f"microvm -c {name} failed: {create.stdout.strip()[-2000:]}")
            start = run(["systemctl", "start", f"microvm@{name}.service"], check=False)
            if start.returncode != 0:
                raise RuntimeError(f"systemctl start microvm@{name} failed: {start.stdout.strip()[-2000:]}")
            wait_for_ssh(ip, SSH_WAIT_SECONDS)
        except BaseException as exc:
            console = guest_console_tail(name) if isinstance(exc, Exception) else ""
            cleanup_vm(name)
            if console:
                raise RuntimeError(
                    f"{exc}\n--- guest {name} console (tail) ---\n{console}"
                ) from exc
            raise
    log.info("created %s (%s, %s vcpu, %s MiB) at %s", name, mac, vcpu, mem, ip)
    return {"ipv4": ip}

def do_destroy(req: dict) -> dict:
    name = vm_name(int(req["id"]))
    with mutate_lock:
        cleanup_vm(name)
    log.info("destroyed %s", name)
    return {}

def do_status(req: dict) -> dict:
    name = vm_name(int(req["id"]))
    is_active = subprocess.run(
        ["systemctl", "is-active", f"microvm@{name}.service"], stdout=subprocess.PIPE, text=True
    )
    state = is_active.stdout.strip()
    return {"status": "running" if state == "active" else "off"}

def do_expose(req: dict) -> dict:
    vm_id = int(req["id"])
    name = vm_name(vm_id)
    ip = vm_ip_from_name(name)
    ssh_expose = bool(req.get("ssh_expose", False))
    with mutate_lock:
        plan = _plan_exposure(name, ip, ssh_expose, req.get("tcp_ports", []))
        old_rules = [] if plan["old_state"] is None else plan["old_state"]["rules"]
        affected_ports = {rule["host"] for rule in old_rules + plan["new_rules"]}
        snapshots = _affected_firewall_rules(affected_ports, ip)
        journal = []
        try:
            _delete_snapshots(snapshots, journal)
            for rule in plan["new_rules"]:
                for table, chain, rest in _dnat_specs(rule["host"], ip, rule["guest"]):
                    _append_rule(table, chain, rest, journal)
            write_exposure(name, ip, plan["new_rules"])
        except BaseException as error:
            _rollback_after_failure(journal, error)
            raise
    log.info("exposed %s: ssh_port=%s tcp=%s", name, plan["ssh_port"], plan["tcp_result"])
    return plan["response"]

ACTIONS = {"create": do_create, "destroy": do_destroy, "status": do_status, "expose": do_expose}

class Handler(socketserver.StreamRequestHandler):
    def handle(self):
        try:
            line = self.rfile.readline()
            if not line.strip():
                return
            req = json.loads(line)
            action = ACTIONS.get(req.get("action"))
            if action is None:
                resp = {"error": f"unknown action: {req.get('action')!r}"}
            else:
                resp = action(req)
        except Exception as e:
            log.exception("request failed")
            resp = {"error": f"{type(e).__name__}: {e}"}
        try:
            self.wfile.write(json.dumps(resp).encode() + b"\n")
        except BrokenPipeError:
            pass

class Server(socketserver.ThreadingUnixStreamServer):
    daemon_threads = True
    request_queue_size = 64

def _rule_present(table, chain: str, rest: list) -> bool:
    return run(_iptables_command(table, "-C", chain, rest), check=False).returncode == 0

def reconcile_exposures() -> int:
    """Re-apply every recorded DNAT. Returns the number of guests restored.

    Exposure rules live only in the kernel's tables. A host reboot drops all
    of them while EXPOSED_DIR still says the guest is published, and
    garnix-guest-autostart revives the guests but not their exposure — so a
    deployed app came back up unreachable on its host port, with the registry
    claiming otherwise, until somebody redeployed it by hand. The registry is
    the source of truth for what should be published; replay it.

    Each rule is appended only when absent, so this is idempotent and safe on
    every daemon start rather than only after a reboot.

    A guest that cannot be restored is logged at ERROR and skipped: the
    daemon keeps serving so the rest of the pool still works and an operator
    can re-issue expose, which is strictly better than a restart loop that
    leaves every guest unreachable.
    """
    try:
        entries = sorted(os.listdir(EXPOSED_DIR))
    except FileNotFoundError:
        return 0
    restored = 0
    for filename in entries:
        if not filename.endswith(".json"):
            continue
        name = filename.removesuffix(".json")
        try:
            state = read_exposure(name)
            if state is None:
                continue
            added = 0
            for rule in state["rules"]:
                for table, chain, rest in _dnat_specs(rule["host"], state["ip"], rule["guest"]):
                    if not _rule_present(table, chain, rest):
                        run(_iptables_command(table, "-A", chain, rest), check=True)
                        added += 1
            if added:
                log.info("restored %s firewall rule(s) for %s (%s)", added, name, state["ip"])
            restored += 1
        except Exception as error:
            log.error("could not restore exposure for %s: %s", name, error)
    return restored

def main():
    logging.basicConfig(stream=sys.stderr, level=logging.INFO, format="%(levelname)s %(message)s")
    os.makedirs(SPECS_DIR, exist_ok=True)
    os.makedirs(EXPOSED_DIR, exist_ok=True)
    if "--reconcile" in sys.argv[1:]:
        count = reconcile_exposures()
        log.info("reconciled exposure for %s guest(s)", count)
        return
    if not os.path.exists(DNSMASQ_HOSTS):
        open(DNSMASQ_HOSTS, "a").close()
    try:
        os.unlink(SOCKET_PATH)
    except FileNotFoundError:
        pass
    with mutate_lock:
        reconcile_exposures()
    server = Server(SOCKET_PATH, Handler)
    os.chmod(SOCKET_PATH, 0o660)
    shutil.chown(SOCKET_PATH, user="root", group=SOCKET_GROUP)
    log.info("listening on %s (bridge %s, subnet %s.0/24)", SOCKET_PATH, BRIDGE, SUBNET_PREFIX)
    server.serve_forever()

if __name__ == "__main__":
    main()
