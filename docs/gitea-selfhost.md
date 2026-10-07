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
