# Garnix

Garnix is a CI service for nixified, flake-based github repos.

## Seeing the stack run

Opening a pull request against this repo deploys it. `garnix.yaml` declares
`nixosConfigurations.website` (see `nix/website.nix`) as an `on-pull-request`
server, so every pull request gets its own microVM guest running nginx, the
Next.js frontend, the Haskell backend and postgres together, at
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

### Setting up a GitHub app

You _will_ need a github app for Garnix to work, both for production and for testing.
On the `/garnix-admin` page you can create one by pressing the 'Submit to GitHub' button.
That will give you a bunch of credentials that you'll have to put into the `/secrets/dev.yaml` file by running

```bash
sops edit secrets/dev.yaml
```

Then you have to enable your new GitHub app on a repo that you want to build through the GitHub ui.

The app manifest asks for `pull_requests: write`, which is used only by the
`commentOnFailure` option in `garnix.yaml`. If you are updating an app created
before that permission existed, every installation has to accept it; until then
the comment request 403s, which is logged but doesn't fail the build.

Finally, you can submit a test build against an instance that has the app
installed, with something like this:

```bash
curl -v \
  -XPOST \
  http://<your-instance>/api/build/submit \
  -H 'Content-Type: application/json' \
  -d '{ "owner": "garnix-io", "repo": "comment", "testCommit": "8b2b57d91dd1f4d094bb944a0a0ef65319a5663f" }'
```

And then you can see the build under `/repo/garnix-io/comment`, for example.
Note that this endpoint resolves an installation token, so it only works
against an instance with a real GitHub App -- not against a pull request's
demonstration deploy.

### Developing the frontend

You can run the frontend in development mode against a backend you already have
running:

```bash
cd frontend
npm run dev
```

Then point your browser to [localhost:3000](http://localhost:3000).

# Acknowledgments

We erased git history when open sourcing, so we'll be explicit here about our
debt to everyone who contributed before the project became open source:

- Alex David
- Evie Ciobanu
- Greg Pfeil
- Jean-François Roche
- Julian Kirsten Arni
- Ramses de Norre
- Sönke Hahn

Thanks very very much!
