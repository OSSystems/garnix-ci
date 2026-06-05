{ pkgs }:
pkgs.writeShellScriptBin "withSecrets" ''
  set -eu
  set -o pipefail

  REPO_ROOT=$(git rev-parse --show-toplevel)

  if !([[ -v WITH_SECRETS ]]); then
    export WITH_SECRETS=true
    >&2 echo decrypting secrets...

    SECRETS_YAML="$(sops -d "$REPO_ROOT/secrets/dev.yaml")"

    extract() {
      key="$1"
      ${pkgs.yq}/bin/yq -r ".\"$key\"" <<< "$SECRETS_YAML"
    }

    export GITHUB_APP_ID=$(extract github_app_id)
    export JWT_KEY="$REPO_ROOT/backend/dev-key.jwt"
    export GITHUB_CLIENT_ID=$(extract github_client_id)
    export GITHUB_APP_NAME='test-app-jkarni'
    export METRICS_PASSWORD="supersecret"
    export GITHUB_APP_PK=$(extract github_app_pk)
    export GITHUB_WEBHOOK_SECRET=$(extract github_webhook_secret)
    export GITHUB_CLIENT_SECRET=$(extract github_client_secret)
    export OPENSEARCH_API=$(extract opensearch-garnix)
    export REPO_SECRETS_KEY_PATH=$REPO_ROOT/backend/test/spec/data/repo-secrets.key
    export REPO_SECRETS_PUB_KEY=age107r0e6nxchkrqdxg42tzdxeauez2ce7cpsajcggjwmpjgrlrnqfqy6tnlf
    export S3_CACHE_ACCESS_KEY_ID=$(extract s3-cache-access-key-id)
    export S3_CACHE_SECRET_ACCESS_KEY=$(extract s3-cache-secret-access-key)
    export S3_CACHE_PUBLIC_BUCKET="test-public"
    export S3_CACHE_PUBLIC_BASE_URL="https://pub-aed3ff3b65d444b3aeee39d6ea1767b0.r2.dev"
    export S3_CACHE_PRIVATE_BUCKET="test-private"
    export S3_CACHE_HOST="79e0f6a031ca6d9650034b607922ba45.r2.cloudflarestorage.com"
    export S3_CACHE_REGION="auto"

    export GARNIX_SERVER_SSH_FILE=$(mktemp)
    export CACHE_PRIV_KEY_FILE=$(mktemp)
    trap 'rm -v "$GARNIX_SERVER_SSH_FILE" "$CACHE_PRIV_KEY_FILE" 1>&2' EXIT
    extract garnix_server_ssh > $GARNIX_SERVER_SSH_FILE
    export GARNIX_SERVER_SSH_KEYS="$GARNIX_SERVER_SSH_FILE"
    extract cache-priv-key > $CACHE_PRIV_KEY_FILE
  fi

  if test -e "$REPO_ROOT/personal-dev.sh" ; then
    source "$REPO_ROOT/personal-dev.sh"
  fi

  "$@"
''
