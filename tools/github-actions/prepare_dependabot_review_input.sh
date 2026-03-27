#!/usr/bin/env bash

set -euo pipefail

readonly pr_number="${PR_NUMBER:?PR_NUMBER is required}"
readonly review_input_root="${REVIEW_INPUT_ROOT:?REVIEW_INPUT_ROOT is required}"
readonly input_dir="${review_input_root}/${pr_number}"

mkdir -p "${input_dir}"

pr_json="$(
    gh pr view "${pr_number}" \
        --json number,title,url,author,baseRefName,headRefName,mergeable,reviewDecision,labels,statusCheckRollup,files
)"

skip() {
    local reason="$1"

    printf '%s\n' "${pr_json}" > "${input_dir}/metadata.json"

    {
        echo "candidate=false"
        echo "skip-reason=${reason}"
        echo "input-dir=${input_dir}"
        echo "pr-url=$(jq -r '.url' <<<"${pr_json}")"
    } >> "${GITHUB_OUTPUT}"
}

author_login="$(jq -r '.author.login' <<<"${pr_json}")"
base_ref="$(jq -r '.baseRefName' <<<"${pr_json}")"
mergeable_state="$(jq -r '.mergeable' <<<"${pr_json}")"
review_decision="$(jq -r '.reviewDecision // "REVIEW_REQUIRED"' <<<"${pr_json}")"
has_label="$(
    jq -r 'any(.labels[]?; .name == "dependabot/npm")' <<<"${pr_json}"
)"
disallowed_files="$(
    jq -r '
        [
            .files[]?.path
            | select(
                . != "package.json" and
                . != "package-lock.json" and
                . != "biome.json"
            )
        ]
        | unique
        | join(", ")
    ' <<<"${pr_json}"
)"
non_passing_checks="$(
    jq -r '
        def status_nodes:
            if .statusCheckRollup == null then
                empty
            elif (.statusCheckRollup | type) == "array" then
                .statusCheckRollup[]
            elif (.statusCheckRollup.contexts? | type) == "array" then
                .statusCheckRollup.contexts[]
            elif (.statusCheckRollup.contexts?.nodes? | type) == "array" then
                .statusCheckRollup.contexts.nodes[]
            else
                empty
            end;

        [
            status_nodes
            | select(
                (.__typename == "CheckRun" and (
                    .status != "COMPLETED" or
                    (
                        (.conclusion // "") != "SUCCESS" and
                        (.conclusion // "") != "SKIPPED" and
                        (.conclusion // "") != "NEUTRAL"
                    )
                )) or
                (.__typename == "StatusContext" and .state != "SUCCESS")
            )
            | if .__typename == "CheckRun" then
                "\(.name):\(.conclusion // .status)"
              else
                "\(.context):\(.state)"
              end
        ]
        | join(", ")
    ' <<<"${pr_json}"
)"

if [[ "${author_login}" != "app/dependabot" && "${author_login}" != "dependabot[bot]" ]]; then
    skip "PR #${pr_number} is not authored by Dependabot."
    exit 0
fi

if [[ "${base_ref}" != "develop" ]]; then
    skip "PR #${pr_number} does not target develop."
    exit 0
fi

if [[ "${has_label}" != "true" ]]; then
    skip "PR #${pr_number} does not have the dependabot/npm label."
    exit 0
fi

if [[ -n "${disallowed_files}" ]]; then
    skip "PR #${pr_number} changes files outside the dependency manifest allowlist: ${disallowed_files}"
    exit 0
fi

if [[ "${review_decision}" == "APPROVED" ]]; then
    skip "PR #${pr_number} is already approved."
    exit 0
fi

if [[ "${mergeable_state}" != "MERGEABLE" ]]; then
    skip "PR #${pr_number} is not mergeable (state: ${mergeable_state})."
    exit 0
fi

if [[ -n "${non_passing_checks}" ]]; then
    skip "PR #${pr_number} has non-passing checks: ${non_passing_checks}"
    exit 0
fi

gh pr diff "${pr_number}" --patch > "${input_dir}/diff.patch"
printf '%s\n' "${pr_json}" > "${input_dir}/metadata.json"

{
    echo "candidate=true"
    echo "skip-reason="
    echo "input-dir=${input_dir}"
    echo "pr-url=$(jq -r '.url' <<<"${pr_json}")"
} >> "${GITHUB_OUTPUT}"
