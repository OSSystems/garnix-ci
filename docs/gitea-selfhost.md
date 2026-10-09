# Gitea and Forgejo

A self-hosted garnix can build repositories on Gitea or Forgejo instances
besides github.com. Forgejo speaks the same API, so both are configured as
`gitea`.

## Configuring an instance

Point `GARNIX_FORGES_FILE` at a JSON file with one entry per instance. The key
is the instance's slug, which names it in URLs and in messages:

```json
{
  "git.example": {
    "kind": "gitea",
    "webUrl": "https://git.example.com",
    "apiUrl": "https://git.example.com/api/v1",
    "webhookSecretFile": "/run/secrets/webhook_secret",
    "oauthClientId": "…",
    "oauthClientSecretFile": "/run/secrets/oauth_client_secret",
    "apiTokenFile": "/run/secrets/api_token",
    "admins": ["alice"]
  }
}
```

Secrets live in their own files, so the forges file itself can sit in the
Nix store. The slug `github` is reserved for github.com.

garnix acts on the instance as a bot user, with the token in `apiTokenFile`.
A repository counts as having garnix installed when that bot can push to it:
give the bot write access to the repositories garnix should build.

## Logging in

Register `https://<garnix>/auth/<slug>/login/cb` as the redirect URI of the
instance's OAuth application. The web login page offers GitHub only, so this
path has no page of its own in the frontend.

A garnix account holds at most one identity per forge. The first login
through an instance creates an account with the email the instance reports.
An email is contact data only: two accounts may share one, and it never links
them.

## Admins

Admin rights belong to one forge: the logins in an instance's `admins`
administer that instance's repositories and no other forge's. On github.com
the admin is the login in `GARNIX_ADMIN_GITHUB_LOGIN`. Nothing else grants
admin rights: accounts whose `subscription_type` is `admin` in the database
lose them on upgrade unless their login is listed there.

## Webhooks

Add a webhook to each repository (or organisation) with:

- URL: `https://<garnix>/api/forges/<slug>/webhook`
- Content type: `application/json`
- Secret: the content of `webhookSecretFile`
- Events: push and pull request

Deliveries without a valid signature are refused.

## Private flake inputs

A repository can use private repositories of the same owner on the same
instance as flake inputs, as `git+https://…`, `tarball+https://…` or
`file+https://…` URLs pinned in `flake.lock`. The rules of the
collaborator check are the same as on GitHub.

Every input URL on the instance's host must point into a single repository,
as `https://git.example.com/owner/repo` does. garnix refuses inputs on that
host that do not (paths outside the instance, `..` segments), and `github:`
or `gitlab:` inputs whose `host` is the instance.

The bot token reaches every repository the bot sees, so it is only used to
fetch the private inputs garnix checked, into the Nix store, before
evaluation. Evaluation and builds run without it. A consequence: fetching a
private repository of the instance from within `flake.nix`, with
`builtins.fetchGit`, `builtins.fetchTree` or `builtins.fetchurl`, fails.
Declare it as a flake input instead. Private inputs with `submodules = true`
or `lfs = true` are refused: `.gitmodules` and `.lfsconfig` could point at
any repository the bot sees.

`github:` inputs of a repository on an instance must be public: the server's
GitHub token, if any, could otherwise fetch private repositories for it.
garnix asks GitHub with that token (`GITHUB_ACCESS_TOKEN`), or anonymously
without one, and refuses inputs that are private or that GitHub does not
show. Answers are kept for ten minutes. When GitHub gives no answer, as under
its rate limit (60 requests an hour without a token), the build fails saying
so, and can be retried.

A login on the instance is not the GitHub account of the same name. Whoever
triggered a build from an instance gets a cache.garnix.io token of their own,
under the name `login@slug`, and `garnix.server.authorizeDeployerGithubKeys`
authorizes no GitHub keys on its servers.

## Registering an instance from the UI

With `services.garnixServer.allowForgeRegistration = true` (the environment
variable `GARNIX_FORGE_REGISTRATION=true`), people can add an instance
themselves, to log in through it. It is off by default.

1. `POST /api/auth/start` with `{ "url": "https://git.example.com" }` answers
   `{ "login": <authorize URL> }` for an instance garnix knows, or
   `{ "register": { "slug", "callback" } }` for one it does not.
2. On the instance, create an OAuth2 application whose redirect URI is the
   `callback` (`https://<garnix>/auth/<host>/login/cb`).
3. `POST /api/forges` with `{ "url", "clientId", "clientSecret" }`. garnix
   checks that `<url>/api/v1/version` answers like Gitea or Forgejo, stores
   the instance as pending, and answers its authorize URL. It also sets an
   HttpOnly cookie that ties the registration to this browser.
4. The first successful login through it from that browser, or connect of it
   to the account that browser is logged in to, makes it active, and that
   garnix account its registrant. Until then, logins from other browsers are
   refused: nobody gets an account through an instance nobody vouched for
   yet. A registration that sees no such login within 24 hours is dropped.

For 15 minutes after it is submitted, a pending registration can only be
replaced from the browser that submitted it; anyone else is told a
registration is already in progress. After that, a new registration from
anywhere replaces it.

One registration serves everyone on the instance; its slug is the host.
Instances served under a path (`https://example.com/gitea`) cannot be
registered: configure them in the forges file. The forges file wins over a
registration of the same slug or host.

The client secret is encrypted with the instance's age key, like users' OAuth
tokens, and no endpoint ever returns it. The registrant account, or an
account whose identity on the instance the instance calls an administrator
(Gitea's `is_admin`, as of that identity's last login), can replace it with
`PUT /api/forges/<slug>/secret` (`{ "clientSecret" }`), and disable the
instance with `DELETE /api/forges/<slug>`. Only from a browser session: an
API token manages no instance. Turning registration off ends the sessions of
every account that only has identities on registered instances, and deletes
nothing.

Disabling an instance ends logins, sessions and webhooks through it: an
account left with no identity on an active forge can no longer log in, and
its API tokens no longer give a session. It deletes nothing. The instance
comes back with everyone who logged in through it in two ways:

- At once, in place: the registrant or an administrator of it submits a new
  secret with `PUT /api/forges/<slug>/secret`. That takes a browser session,
  so an account whose only identity is on the disabled instance has to log
  in through another forge first, or register the instance again.
- By a new registration of its host, from anyone, completed less than 30
  days after it was disabled. Whoever completes it becomes its registrant.

A host can change hands (its domain expires and someone else buys it), and
the new owner's Gitea can have users with the same names. So a new
registration of a host that was disabled 30 days or more before, or that has
no row left, forgets, when it is activated and in the same transaction, every
identity on it, with its OAuth credentials and module settings, and every
account left with no identity at all, with its API tokens. Builds, their
requesters and other history stay. Logins through the instance then create
new accounts; nobody lands in the accounts of the people who logged in
through it before, and those people connect it to their account again from
Settings. A registration that is never completed keeps the time the instance
was disabled, so it does not restart the 30 days.

The administrators of a registered instance administer its repositories on
garnix, as the `admins` of a configured forge do. On github.com and on the
forges in the forges file, what the forge says about an identity grants
nothing: only the configured `admins` count.

Anyone can register an instance, so garnix does not trust its URL. It must be
https, and so must anything it redirects to. Every connection garnix makes
to it (the version probe, the OAuth token exchange, API calls) is refused when its host resolves to a non-public
address: loopback, private and link-local ranges (including cloud metadata
at 169.254.169.254), CGNAT, IPv6 unique-local and the like. The check runs on
each connection, so a name that later resolves elsewhere is refused too.
Each answer may be at most 1 MiB, and must arrive within 60 seconds.
Registered instances are only used to log in: garnix builds no repository of
theirs and hands none of their URLs to git or nix, which would resolve the
name themselves.

`/api/auth/start` and `POST /api/forges` are rate-limited per client address:
30 a minute, and 10 an hour. Behind a reverse proxy or load balancer on a
non-public address, the client is the last entry of `X-Forwarded-For`,
without its port. An IPv4 client counts as itself, mapped into IPv6 too, and
an IPv6 client counts as its /64.
