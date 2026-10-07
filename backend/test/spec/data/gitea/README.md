Real Gitea webhook deliveries, used as fixtures by `Garnix.Forge.Gitea.WebhookSpec`.

- `push.json`: the example push payload from the Gitea 1.20 docs,
  https://github.com/go-gitea/gitea/blob/v1.20.0/docs/content/doc/usage/webhooks.en-us.md
- `pull_request_*.json`: deliveries captured by drone/go-scm for its Gitea driver,
  https://github.com/drone/go-scm/tree/master/scm/driver/gitea/testdata/webhooks
