# Development

## The per-pull-request demo deploy

Currently disabled. `garnix.yaml` no longer declares a `servers:` block, so a
pull request builds `nixosConfigurations.website` (it still matches the
`nixosConfigurations.*` include) but nothing is deployed from it.

To turn it back on, add the block back to `garnix.yaml`:

```yaml
servers:
  - configuration: website
    deployment:
      type: on-pull-request
      machine: i2x4
```

Every pull request then gets its own microVM guest running nginx, the Next.js
frontend, the Haskell backend and postgres together (see `nix/website.nix`), at
`website.pull-<n>.<repo>.<owner>.<hostingDomain>`. garnix comments the address
on the pull request once it is up.

That guest is a demonstration instance. It receives no secrets from garnix, so
its GitHub credentials are dummies generated at build time: no webhook arrives
and no build can be started on it. `sql/local-fixtures.sql` is seeded on boot so
the views have something in them, and `/api/dev/log-me-in` mints a session for
the fixture's `dev-user` without going through OAuth.

There's also an admin page on `/garnix-admin` that is useful for some
development tasks.

> The `nixos-compose` flow that used to live here was removed along with the
> example configurations it drove (`exampleGarnixServer`, `exampleDb`,
> `exampleOpenSearch`). `examples/example-selfhost.nix` remains as the
> reference for a real self-hosted deployment.

## Setting up a GitHub app

You _will_ need a GitHub app for garnix to work, both for production and for
testing. On the `/garnix-admin` page you can create one by pressing the 'Submit
to GitHub' button. That gives you a set of credentials to put into
`secrets/dev.yaml`:

```bash
sops edit secrets/dev.yaml
```

Then enable your new GitHub app on a repo you want to build, through the GitHub
UI.

The app manifest asks for `pull_requests: write`, which is used only by the
`commentOnFailure` option in `garnix.yaml`. If you are updating an app created
before that permission existed, every installation has to accept it; until then
the comment request 403s, which is logged but doesn't fail the build.

## Submitting a test build

```bash
curl -v \
  -XPOST \
  http://<your-instance>/api/build/submit \
  -H 'Content-Type: application/json' \
  -d '{ "owner": "garnix-io", "repo": "comment", "testCommit": "8b2b57d91dd1f4d094bb944a0a0ef65319a5663f" }'
```

The build then shows up under `/repo/garnix-io/comment`. This endpoint resolves
an installation token, so it only works against an instance with a real GitHub
App — not against a pull request's demonstration deploy.

## Running the frontend

Against a backend you already have running:

```bash
cd frontend
npm run dev
```

Then open [localhost:3000](http://localhost:3000).
