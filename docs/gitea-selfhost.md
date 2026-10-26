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
    "oauthClientId": "…",
    "oauthClientSecretFile": "/run/secrets/oauth_client_secret",
    "admins": ["alice"]
  }
}
```

Secrets live in their own files, so the forges file itself can sit in the
Nix store. The slug `github` is reserved for github.com.

Nothing is built until someone installs garnix on the instance (see
"Installing garnix" below). `apiTokenFile` is optional: see "A bot token".

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

## Installing garnix

Like the GitHub App, garnix builds what people install it on. A logged-in
user with an identity on the instance installs it on:

- their own account: garnix creates a user webhook (Gitea 1.20 or later,
  Forgejo 7 or later);
- an organisation they own (members of its Owners team; Gitea's own rule
  for organisation webhooks): an organisation webhook;
- the whole instance, if they are a site administrator: a system webhook
  (Gitea 1.22 or later, Forgejo 7 or later; older versions only create
  "default" webhooks, which never reach existing repositories).

garnix creates the webhook through the instance's API with the installer's
own OAuth token, which the login already grants (Gitea and Forgejo give a
token asked for no particular scope access to everything its user may do).
Each installation gets a secret of its own, and its own delivery URL,
`https://<garnix>/api/forges/<slug>/installations/<id>/webhook`, that checks
the signature against that secret only. There is no instance-wide webhook
secret, and webhooks created by hand are not read.

Installing enables no repository. The installer, or anyone else who may
install on the same target, enables each repository to build. A repository
builds through one installation only: when the owner's account, its
organisation and the instance all have garnix installed, it is enabled in
one of them, and deliveries from the others are ignored. Deliveries for
repositories that are not enabled are ignored too.

Builds call the instance (commit statuses, cloning, private inputs) with the
installer's token, renewed with its refresh token as it expires, and only for
repositories it can push to.

### Needs reinstall

An installation stops building, and says why in the logs and in the API,
when:

- the instance no longer accepts the installer's token, and refuses to renew
  it (they revoked garnix's access);
- the installer may no longer install on the target (they left the
  organisation, or are no longer a site administrator);
- the installer's identity is gone (disconnected from their garnix account,
  or the account deleted).

An instance that is down or overloaded when garnix renews the token stops
nothing: the build fails, and the next one tries again.

It keeps its repositories. Anyone who may install on its target reinstalls
it: garnix creates a new webhook with a new secret, deletes the old one, and
the person who reinstalled becomes its installer. Logging in again does not
bring it back by itself. Until then its repositories stay enabled in it:
enabling one in another installation answers 409, saying that the
installation holding it needs a reinstall.

### Uninstalling

Uninstalling deletes the webhook on the instance (as far as the caller's
token can; one left behind fails to deliver) and the installation with its
list of repositories. Builds and their history stay.

### The API

Under `/api/forges/<slug>/`, from a browser session (an API token installs
nothing). Who may install on a target, and so manage the installation there,
is asked of the instance with the caller's own token on every request.

| Request | Answer |
|---|---|
| `GET installable` | `[{ "kind": "user" \| "org" \| "instance", "target", "installation": id \| null }]` |
| `GET installations` | the installations the caller may manage: `[{ "id", "forge", "kind", "target", "installer", "status": "active" \| "needs_reinstall", "reason", "createdAt" }]` |
| `POST installations` with `{ "kind", "target" }` | the new installation; 409 if the target has one |
| `POST installations/<id>/reinstall` | the installation; 409 if another reinstall won meanwhile |
| `DELETE installations/<id>` | |
| `GET installations/<id>/repos?page=<n>&limit=<m>` (page from 1, at most 50) | `{ "repos": [{ "owner", "name", "private", "enabledIn": id \| null }], "total" }`: one page, and how many repositories the target has (the account's own, the organisation's, or the instance's) |
| `PUT installations/<id>/repos/<owner>/<name>` | 409 naming the installation it is already enabled in, and whether that one needs a reinstall |
| `DELETE installations/<id>/repos/<owner>/<name>` | |

`target` is the account or organisation login, and empty for the instance.

### A bot token

With `apiTokenFile`, garnix calls the instance and clones with that bot
account's token instead of the installer's, for every installation on the
instance. The bot then needs write access to every repository garnix builds
there. Installing, and enabling repositories, still work as above.

## Private flake inputs

A repository can use private repositories of the same owner on the same
instance as flake inputs, as `git+https://…`, `tarball+https://…` or
`file+https://…` URLs pinned in `flake.lock`. The rules of the
collaborator check are the same as on GitHub.

Every input URL on the instance's host must point into a single repository,
as `https://git.example.com/owner/repo` does. garnix refuses inputs on that
host that do not (paths outside the instance, `..` segments), and `github:`
or `gitlab:` inputs whose `host` is the instance.

The repository's token (the installer's, or the bot's) reaches every
repository its user sees, so it is only used to fetch the private inputs
garnix checked, into the Nix store, before evaluation. Evaluation and builds run without it. A consequence: fetching a
private repository of the instance from within `flake.nix`, with
`builtins.fetchGit`, `builtins.fetchTree` or `builtins.fetchurl`, fails.
Declare it as a flake input instead. Private inputs with `submodules = true`
or `lfs = true` are refused: `.gitmodules` and `.lfsconfig` could point at
any repository that token sees.

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
   HttpOnly cookie that ties the registration to this browser. An instance
   that answers its API to signed-in users only (`REQUIRE_SIGNIN_VIEW`)
   answers that request 401 or 403; garnix then accepts it if
   `<url>/.well-known/openid-configuration` names
   `<url>/login/oauth/authorize` and `<url>/login/oauth/access_token` as its
   OAuth endpoints.
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

garnix builds the repositories of a registered instance once someone
installs garnix there, like a configured one's. git resolves names itself, so
whenever git fetches from a registered instance (the clone, and the private
flake inputs on it), garnix resolves the host through the same check just
before and pins git to the addresses it let through
(`http.curloptResolve`), with redirects and proxies off: a name that
resolves elsewhere by the time git connects still reaches the checked
address, and TLS is still verified against the name. nix downloads tarball
and file inputs itself, and cannot be pinned, so private inputs of a
registered instance must be git inputs (`git+https://…`); others are
refused. Public inputs are fetched as any URL in any flake is.

`/api/auth/start` and `POST /api/forges` are rate-limited per client address:
30 a minute, and 10 an hour. Behind a reverse proxy or load balancer on a
non-public address, the client is the last entry of `X-Forwarded-For`,
without its port. An IPv4 client counts as itself, mapped into IPv6 too, and
an IPv6 client counts as its /64.
