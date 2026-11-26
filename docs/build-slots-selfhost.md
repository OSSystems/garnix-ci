# Build slots

garnix builds up to `services.garnixServer.maxConcurrentBuilds` checks at
once. The rest wait in garnix's queue: a freed slot goes to the next
repository owner in turn, and within an owner to the check that has waited
longest. So a commit with one check left does not wait behind a newer commit
with many.

That only holds while the limit is no higher than what the builders can run.
Past it, the extra checks start their `nix build` and wait inside Nix, which
hands out the builders in no particular order. The default therefore matches
the builders: the sum of `maxJobs` over `remoteBuilders.hosts`.

```nix
services.garnixServer = {
  remoteBuilders.hosts = [
    { hostname = "builder-1"; systems = [ "x86_64-linux" ]; maxJobs = 8; }
    { hostname = "builder-2"; systems = [ "x86_64-linux" ]; maxJobs = 8; }
  ];
  # 16 by default; raise it if the builders sit idle while checks wait.
  maxConcurrentBuilds = 24;
};
```

A check holds its slot for its whole `nix build`: while Nix fetches from
caches, while it builds, and while it waits for another check that builds the
same dependency. That is why a limit far below what the builders can run
leaves them idle.

Without remote builders, or with builders for different systems, the default
is `null`, which keeps the server's own limit of 50. All systems share the
one queue, so with mixed builders the checks for a system with few builders
could take every slot while the other builders sit idle. Set the limit
yourself there, if at all.

Two metrics on the server's Prometheus endpoint show the queue:

- `garnix_server_nix_build_queue_len`: checks waiting for a slot.
- `garnix_server_nix_build_queue_wait_time`: how long checks waited, in
  seconds.
