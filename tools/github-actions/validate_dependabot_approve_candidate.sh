#!/usr/bin/env bash

set -euo pipefail

readonly pr_number="${PR_NUMBER:?PR_NUMBER is required}"
script_dir="$(
    cd "$(dirname "${BASH_SOURCE[0]}")" && pwd
)"
readonly script_dir

# shellcheck source=tools/github-actions/dependabot_auto_approve_lib.sh
source "${script_dir}/dependabot_auto_approve_lib.sh"

pr_json="$(load_candidate_pr_json "${pr_number}")"

refresh_pr_state "${pr_json}"
require_current_head_claude_review_verdict="$(
    load_require_current_head_claude_review_verdict
)"
evaluate_candidate \
    "${pr_number}" \
    "false" \
    "false" \
    "false" \
    "${require_current_head_claude_review_verdict}"

{
    if [[ -n "${skip_reason}" ]]; then
        echo "candidate=false"
        echo "skip-reason=${skip_reason}"
    else
        echo "candidate=true"
        echo "skip-reason="
    fi
    echo "pr-url=$(jq -r '.url' <<<"${pr_json}")"
    echo "head-sha=$(jq -r '.headRefOid' <<<"${pr_json}")"
    echo "base-sha=$(jq -r '.baseRefOid' <<<"${pr_json}")"
} >> "${GITHUB_OUTPUT}"
