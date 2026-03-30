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

workflow_run_pr_numbers() {
    jq -c '
        if (
            .workflow_run.event != "pull_request" or
            .workflow_run.conclusion != "success"
        ) then
            []
        else
            (.workflow_run.pull_requests // [])
            | map(.number)
            | map(select(. != null))
            | map(tonumber)
            | unique
            | sort
        end
    ' "${GITHUB_EVENT_PATH}"
}

collect_workflow_run_candidate_pr_numbers() {
    local workflow_run_pr_numbers_json="$1"
    local candidate_pr_numbers=""
    local pull_number
    local pr_json

    while IFS= read -r pull_number; do
        [[ -z "${pull_number}" ]] && continue

        if ! pr_json="$(
            gh pr view "${pull_number}" \
                --json number,state,author,baseRefName,labels
        )"; then
            echo "failed to load PR #${pull_number} from workflow_run payload" >&2
            exit 1
        fi

        if jq -e '
            .state == "OPEN" and
            .baseRefName == "develop" and
            (.author.login == "app/dependabot" or .author.login == "dependabot[bot]") and
            any(.labels[]?; .name == "dependabot/npm") and
            any(.labels[]?; .name == "automerge")
        ' <<<"${pr_json}" >/dev/null; then
            candidate_pr_numbers+="${pull_number}"$'\n'
        fi
    done < <(jq -r '.[]' <<<"${workflow_run_pr_numbers_json}")

    printf '%s' "${candidate_pr_numbers}" | jq -Rsc '
        split("\n")
        | map(select(length > 0) | tonumber)
        | unique
        | sort
    '
}

if [[ -n "${INPUT_PR_NUMBERS:-}" ]]; then
    pr_numbers_json="$(normalize_pr_numbers "${INPUT_PR_NUMBERS}")"
elif [[ "${GITHUB_EVENT_NAME:-}" == "workflow_run" ]]; then
    pr_numbers_json="$(
        collect_workflow_run_candidate_pr_numbers "$(workflow_run_pr_numbers)"
    )"
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
