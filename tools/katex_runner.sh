#!/usr/bin/env bash

set -euCo pipefail

readonly ROKI_WEB_GH_ACTIONS_NODE_VERSION="${ROKI_WEB_GH_ACTIONS_NODE_VERSION:-v0.0.0}"
BASEDIR="$(dirname "$0")"
readonly BASEDIR
readonly MAX_OLD_SPACE_SIZE=8192
readonly NODE_ENV=production
export NODE_ENV

# リトライ設定
readonly MAX_RETRIES=3
readonly RETRY_DELAY=1
if ! TMP_ROOT="$(mktemp -d)"; then
    echo "KaTeX runner failed to create a temporary directory." >&2
    exit 1
fi
readonly TMP_ROOT

cleanup() {
    rm -rf "$TMP_ROOT"
}
trap cleanup EXIT

readonly STDIN_FILE="$TMP_ROOT/stdin"
readonly STDOUT_FILE="$TMP_ROOT/stdout"
readonly STDERR_FILE="$TMP_ROOT/stderr"

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

# リトライ機能付きでKaTeXを実行
run_katex_with_retry() {
    local retry_count=0
    local exit_code=1

    cat >| "$STDIN_FILE"
    
    while [ $retry_count -lt $MAX_RETRIES ] && [ $exit_code -ne 0 ]; do
        if [ $retry_count -gt 0 ]; then
            echo "KaTeX処理でエラーが発生しました。${RETRY_DELAY}秒後に再試行します... (${retry_count}/${MAX_RETRIES})" >&2
            sleep $RETRY_DELAY
        fi
        
        if npx tsx --max-old-space-size=$MAX_OLD_SPACE_SIZE "$BASEDIR/katex.ts" "$@" < "$STDIN_FILE" >| "$STDOUT_FILE" 2>| "$STDERR_FILE"; then
            exit_code=0
        else
            exit_code=$?
            cat "$STDERR_FILE" >&2
            retry_count=$((retry_count + 1))
        fi
    done
    
    if [ $exit_code -ne 0 ]; then
        echo "KaTeX処理が${MAX_RETRIES}回試行しても失敗しました。" >&2
        exit $exit_code
    fi

    cat "$STDERR_FILE" >&2
    cat "$STDOUT_FILE"
}

run_katex_with_retry "$@"
