#!/usr/bin/env bash

set -euCo pipefail

readonly ROKI_WEB_GH_ACTIONS_NODE_VERSION="${ROKI_WEB_GH_ACTIONS_NODE_VERSION:-v0.0.0}"
BASEDIR="$(dirname "$0")"
readonly BASEDIR
readonly MAX_OLD_SPACE_SIZE=8192
readonly NODE_ENV=production
export NODE_ENV

fail() {
    echo -n "Error: " >&2
    echo -n "expected node version was $ROKI_WEB_GH_ACTIONS_NODE_VERSION " >&2
    echo "but actual node version was $(node --version)"  >&2
    exit 1
}

cmp_expect() {
    [[ "$ROKI_WEB_GH_ACTIONS_NODE_VERSION" = "$1" ]]
}

is_set() {
  ! cmp_expect "v0.0.0"
}

if command -v nvm &> /dev/null; then
    if is_set && ! cmp_expect "$(nvm current)"; then
        fail
    fi
    node() {
      nvm exec "$(nvm current)" node "$@"
    }
elif is_set && ! cmp_expect "$(node --version)"; then
  fail
fi

node --max-old-space-size=$MAX_OLD_SPACE_SIZE "$BASEDIR/katex.js" "$@"
