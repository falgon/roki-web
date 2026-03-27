#!/usr/bin/env bash

set -euo pipefail

normalize_pr_numbers() {
    jq -cn --arg raw "${1:-}" '
        $raw
        | split(",")
        | map(gsub("^\\s+|\\s+$"; ""))
        | map(select(length > 0))
        | map(tonumber)
        | unique
        | sort
    '
}

if [[ -n "${INPUT_PR_NUMBERS:-}" ]]; then
    pr_numbers_json="$(normalize_pr_numbers "${INPUT_PR_NUMBERS}")"
else
    pr_numbers_json="$(
        gh pr list \
            --state open \
            --base develop \
            --label "dependabot/npm" \
            --label "automerge" \
            --json number,author \
            --limit 100 \
        | jq -c '
            map(select(.author.login == "app/dependabot" or .author.login == "dependabot[bot]"))
            | map(.number)
            | unique
            | sort
        '
    )"
fi

{
    echo "pr-numbers=${pr_numbers_json}"
    echo "dry-run=${INPUT_DRY_RUN:-false}"
} >> "${GITHUB_OUTPUT}"
