# list all just recipes
list:
  just --list

watch-fast *args="":
  just _watch --skip @slow {{ args }}

watch *args="":
  just _watch {{ args }}

_watch *args="":
  #!/usr/bin/env bash

  cd backend
  ghcid \
    --command "cabal repl test:spec" \
    --restart="./garnix.cabal" \
    --allow-eval \
    --test ':main {{ args }}' \
    --warnings

server:
  #!/usr/bin/env bash

  cd backend
  export GARNIX_URL="https://testing.garnix.io/"
  export S3_CACHE_PUBLIC_BUCKET="test-public"
  export S3_CACHE_PUBLIC_BASE_URL="https://pub-aed3ff3b65d444b3aeee39d6ea1767b0.r2.dev"
  export S3_CACHE_PRIVATE_BUCKET="test-private"
  export S3_CACHE_HOST="79e0f6a031ca6d9650034b607922ba45.r2.cloudflarestorage.com"
  export S3_CACHE_REGION="auto"
  withSecrets cabal run server -- \
    --enable DevApi \
    --enable OpenSearchMocks \
    --enable CacheUploadMocks \
    --port 8017 \
    --monitoring-port 8018 \
    --metrics-port 8019 \
    --build-logs-dir logs \

# Generate a server infrastructure diagram
docs-infrastructure-generate *args="":
  d2 --layout elk docs/infrastructure.d2 docs/infrastructure.svg {{ args }}

# Generate a server infrastructure diagram (watch mode)
docs-infrastructure-generate-watch:
  just docs-infrastructure-generate --watch
