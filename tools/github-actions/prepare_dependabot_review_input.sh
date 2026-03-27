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
status_states_json="$(
    jq -c '
        def status_nodes:
            if .statusCheckRollup == null then
                []
            elif (.statusCheckRollup | type) == "array" then
                .statusCheckRollup
            elif (.statusCheckRollup.contexts? | type) == "array" then
                .statusCheckRollup.contexts
            elif (.statusCheckRollup.contexts?.nodes? | type) == "array" then
                .statusCheckRollup.contexts.nodes
            else
                []
            end;

        reduce status_nodes[] as $node
            ({};
             if $node.__typename == "CheckRun" then
                 . + {
                     ($node.name): {
                         "state": ($node.conclusion // $node.status // "MISSING"),
                         "passing": (
                             ($node.status == "COMPLETED") and (
                                 ($node.conclusion // "") == "SUCCESS" or
                                 ($node.conclusion // "") == "SKIPPED" or
                                 ($node.conclusion // "") == "NEUTRAL"
                             )
                         )
                     }
                 }
             elif $node.__typename == "StatusContext" then
                 . + {
                     ($node.context): {
                         "state": ($node.state // "MISSING"),
                         "passing": (($node.state // "") == "SUCCESS")
                     }
                 }
             else
                 .
             end)
    ' <<<"${pr_json}"
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
    skip "PR #${pr_number} は Dependabot 作成ではありません。"
    exit 0
fi

if [[ "${base_ref}" != "develop" ]]; then
    skip "PR #${pr_number} の対象ブランチが develop ではありません。"
    exit 0
fi

if [[ "${has_label}" != "true" ]]; then
    skip "PR #${pr_number} に dependabot/npm ラベルがありません。"
    exit 0
fi

if [[ -n "${disallowed_files}" ]]; then
    skip "PR #${pr_number} は許可対象外のファイルを変更しています: ${disallowed_files}"
    exit 0
fi

if [[ "${review_decision}" == "APPROVED" ]]; then
    skip "PR #${pr_number} は既に承認済みです。"
    exit 0
fi

if [[ "${mergeable_state}" != "MERGEABLE" ]]; then
    skip "PR #${pr_number} はマージ可能ではありません (state: ${mergeable_state})。"
    exit 0
fi

if ! protection_json="$(gh api "repos/{owner}/{repo}/branches/${base_ref}/protection" 2>/dev/null)"; then
    skip "PR #${pr_number} の branch protection を取得できないため判定できません。"
    exit 0
fi

required_contexts_json="$(
    jq -c '
        (
            ((.required_status_checks.checks // []) | map(.context)) +
            (.required_status_checks.contexts // []) +
            ["lint-and-test"]
        )
        | unique
    ' <<<"${protection_json}"
)"

missing_required_checks="$(
    jq -nr \
        --argjson required_contexts "${required_contexts_json}" \
        --argjson status_states "${status_states_json}" '
            $required_contexts
            | map(select($status_states[.] == null))
            | join(", ")
        '
)"

if [[ -n "${missing_required_checks}" ]]; then
    skip "PR #${pr_number} は必須チェックがまだ出揃っていません: ${missing_required_checks}"
    exit 0
fi

non_success_required_checks="$(
    jq -nr \
        --argjson required_contexts "${required_contexts_json}" \
        --argjson status_states "${status_states_json}" '
            $required_contexts
            | map(
                select(($status_states[.].passing // false) | not)
                | . + ":" + ($status_states[.].state // "MISSING")
            )
            | join(", ")
        '
)"

if [[ -n "${non_success_required_checks}" ]]; then
    skip "PR #${pr_number} は必須チェックが未成功です: ${non_success_required_checks}"
    exit 0
fi

if [[ -n "${non_passing_checks}" ]]; then
    skip "PR #${pr_number} に未成功のチェックがあります: ${non_passing_checks}"
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
