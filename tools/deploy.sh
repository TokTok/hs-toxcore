#!/usr/bin/env bash
#
# Manual deploy tool. Shouldn't normally be used, but useful when debugging in
# production (which is currently needed for webhooks).

set -eux

APP=toxcore
PKG=toxcore

DIR=$(realpath "$(dirname "$0")"/..)
cd "$DIR"

readarray -d '' FILES < <(find . "-(" -name "*.*hs" -or -name "Dockerfile*" "-)" -print0)

docker build \
  -t "toxchat/haskell:hs-$PKG" \
  -f ".github/docker/Dockerfile" \
  - < <(tar c LICENSE stack.yaml "$PKG.cabal" "${FILES[@]}")
docker build \
  -t "registry.heroku.com/$APP/web" \
  -f ".github/docker/Dockerfile.heroku" \
  ".github/docker"
docker push "registry.heroku.com/$APP/web"

echo "HEROKU_API_KEY=\$(cat .heroku-api-key) heroku container:release -a $APP web" | ssh toktok -t
