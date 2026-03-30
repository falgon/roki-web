#!/usr/bin/env bash

set -euo pipefail

readonly pr_number="${PR_NUMBER:?PR_NUMBER is required}"
readonly review_input_root="${REVIEW_INPUT_ROOT:?REVIEW_INPUT_ROOT is required}"
readonly input_dir="${review_input_root}/${pr_number}"
readonly skip_candidate_validation="${SKIP_CANDIDATE_VALIDATION:-false}"
readonly expected_head_sha="${EXPECTED_HEAD_SHA:-}"
readonly expected_base_sha="${EXPECTED_BASE_SHA:-}"
script_dir="$(
    cd "$(dirname "${BASH_SOURCE[0]}")" && pwd
)"
readonly script_dir

# shellcheck source=tools/github-actions/dependabot_auto_approve_lib.sh
source "${script_dir}/dependabot_auto_approve_lib.sh"

mkdir -p "${input_dir}"

expected_snapshot_skip_reason=""

normalize_review_metadata_pr_json() {
    local source_pr_json="$1"

    jq '
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
        def node_name($node):
            if $node.__typename == "CheckRun" then
                $node.name
            elif $node.__typename == "StatusContext" then
                $node.context
            else
                null
            end;
        def node_timestamp($node):
            if $node.__typename == "CheckRun" then
                ($node.completedAt // $node.startedAt // "")
            elif $node.__typename == "StatusContext" then
                ($node.createdAt // "")
            else
                ""
            end;
        def node_priority($node):
            if $node.__typename == "CheckRun" and (($node.status // "COMPLETED") != "COMPLETED") then
                2
            elif (node_timestamp($node) != "") then
                1
            else
                0
            end;
        def latest_status_nodes:
            reduce (status_nodes | to_entries[]) as $entry
                ({};
                 ($entry.value) as $node
                 | ($entry.key) as $index
                 | (node_name($node)) as $name
                 | if $name == null then
                     .
                   elif .[$name] == null or
                        node_priority(.[$name].node) < node_priority($node) or
                        (
                            node_priority(.[$name].node) == node_priority($node) and
                            (
                                node_timestamp(.[$name].node) < node_timestamp($node) or
                                (
                                    node_timestamp(.[$name].node) == node_timestamp($node) and
                                    ((.[$name].index // -1) < $index)
                                )
                            )
                        ) then
                     . + {
                         ($name): {
                             "node": $node,
                             "index": $index
                         }
                     }
                   else
                     .
                   end)
            | [to_entries[] | .value]
            | sort_by(.index)
            | map(.node);
        def normalized_status_check_rollup:
            if .statusCheckRollup == null then
                null
            elif (.statusCheckRollup | type) == "array" then
                latest_status_nodes
            elif (.statusCheckRollup.contexts? | type) == "array" then
                .statusCheckRollup + {contexts: latest_status_nodes}
            elif (.statusCheckRollup.contexts?.nodes? | type) == "array" then
                .statusCheckRollup + {
                    contexts: (
                        .statusCheckRollup.contexts + {
                            nodes: latest_status_nodes
                        }
                    )
                }
            else
                .statusCheckRollup
            end;
        . + {statusCheckRollup: normalized_status_check_rollup}
    ' <<<"${source_pr_json}"
}

write_metadata_json() {
    local metadata_path="$1"
    local source_pr_json="$2"
    local metadata_json

    metadata_json="$(normalize_review_metadata_pr_json "${source_pr_json}")"
    printf '%s\n' "${metadata_json}" > "${metadata_path}"
}

load_pr_json() {
    load_candidate_pr_json "${pr_number}"
}

ensure_snapshot_matches_expected() {
    local current_head_sha="$1"
    local current_base_sha="$2"
    local mismatch_parts=()

    expected_snapshot_skip_reason=""

    if [[ -n "${expected_head_sha}" && "${current_head_sha}" != "${expected_head_sha}" ]]; then
        mismatch_parts+=("HEAD (${expected_head_sha} -> ${current_head_sha})")
    fi

    if [[ -n "${expected_base_sha}" && "${current_base_sha}" != "${expected_base_sha}" ]]; then
        mismatch_parts+=("BASE (${expected_base_sha} -> ${current_base_sha})")
    fi

    if [[ "${#mismatch_parts[@]}" -gt 0 ]]; then
        expected_snapshot_skip_reason="PR #${pr_number} の HEAD または BASE が評価後に更新されたため、レビュー用スナップショットを保存しません: ${mismatch_parts[*]}。"
        return 1
    fi
}

evaluate_or_passthrough_candidate() {
    local require_current_head_claude_review_verdict

    refresh_pr_state "${pr_json}"

    if [[ "${skip_candidate_validation}" == "true" ]]; then
        # 通常版 workflow では候補判定を evaluate step 側で済ませているため、
        # ここでは保存対象スナップショットの整合性だけを確認する。
        skip_reason=""
        pr_json="${current_pr_state_json}"
        return
    fi

    require_current_head_claude_review_verdict="$(
        load_require_current_head_claude_review_verdict
    )"
    evaluate_candidate \
        "${pr_number}" \
        "false" \
        "false" \
        "false" \
        "${require_current_head_claude_review_verdict}"
    pr_json="${current_pr_state_json}"
}

capture_review_snapshot() {
    local before_pr_json
    local before_head_sha
    local before_base_sha
    local after_pr_json
    local after_head_sha
    local after_base_sha
    local current_pr_json
    local current_head_sha
    local current_base_sha
    local first_diff_path="${input_dir}/diff.patch.first.tmp"
    local second_diff_path="${input_dir}/diff.patch.tmp"
    local remaining_attempts=3

    rm -f "${input_dir}/diff.patch" "${first_diff_path}" "${second_diff_path}"

    # Keep the reviewed patch on a stable HEAD and only retry when the visible patch changes.
    while (( remaining_attempts > 0 )); do
        if ! before_pr_json="$(load_pr_json)"; then
            echo "failed to load PR #${pr_number} before capturing review snapshot" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        if ! before_head_sha="$(jq -er '.headRefOid' <<<"${before_pr_json}")"; then
            echo "failed to parse headRefOid for PR #${pr_number} before capturing review snapshot" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        if ! before_base_sha="$(jq -er '.baseRefOid' <<<"${before_pr_json}")"; then
            echo "failed to parse baseRefOid for PR #${pr_number} before capturing review snapshot" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        if ! ensure_snapshot_matches_expected "${before_head_sha}" "${before_base_sha}"; then
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 3
        fi

        if ! gh pr diff "${pr_number}" --patch > "${first_diff_path}"; then
            echo "failed to capture the first review patch for PR #${pr_number}" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi

        if ! after_pr_json="$(load_pr_json)"; then
            echo "failed to reload PR #${pr_number} after capturing the first review patch" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        if ! after_head_sha="$(jq -er '.headRefOid' <<<"${after_pr_json}")"; then
            echo "failed to parse headRefOid for PR #${pr_number} after capturing the first review patch" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        if ! after_base_sha="$(jq -er '.baseRefOid' <<<"${after_pr_json}")"; then
            echo "failed to parse baseRefOid for PR #${pr_number} after capturing the first review patch" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        pr_json="${after_pr_json}"
        if ! ensure_snapshot_matches_expected "${after_head_sha}" "${after_base_sha}"; then
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 3
        fi

        if [[ "${before_head_sha}" != "${after_head_sha}" || "${before_base_sha}" != "${after_base_sha}" ]]; then
            rm -f "${first_diff_path}"
            remaining_attempts=$((remaining_attempts - 1))
            continue
        fi

        if ! gh pr diff "${pr_number}" --patch > "${second_diff_path}"; then
            echo "failed to capture the second review patch for PR #${pr_number}" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi

        if ! current_pr_json="$(load_pr_json)"; then
            echo "failed to reload PR #${pr_number} after capturing the second review patch" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        if ! current_head_sha="$(jq -er '.headRefOid' <<<"${current_pr_json}")"; then
            echo "failed to parse headRefOid for PR #${pr_number} after capturing the second review patch" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        if ! current_base_sha="$(jq -er '.baseRefOid' <<<"${current_pr_json}")"; then
            echo "failed to parse baseRefOid for PR #${pr_number} after capturing the second review patch" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        pr_json="${current_pr_json}"
        if ! ensure_snapshot_matches_expected "${current_head_sha}" "${current_base_sha}"; then
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 3
        fi

        if [[ "${after_head_sha}" != "${current_head_sha}" || "${after_base_sha}" != "${current_base_sha}" ]]; then
            rm -f "${first_diff_path}" "${second_diff_path}"
            remaining_attempts=$((remaining_attempts - 1))
            continue
        fi

        if patches_match_ignoring_transient_metadata "${first_diff_path}" "${second_diff_path}"; then
            mv "${second_diff_path}" "${input_dir}/diff.patch"
            rm -f "${first_diff_path}"
            return 0
        fi

        rm -f "${first_diff_path}" "${second_diff_path}"
        remaining_attempts=$((remaining_attempts - 1))
    done

    return 1
}

pr_json="$(load_pr_json)"

skip() {
    local reason="$1"

    write_metadata_json "${input_dir}/metadata.json" "${pr_json}"

    {
        echo "candidate=false"
        echo "skip-reason=${reason}"
        echo "input-dir=${input_dir}"
        echo "pr-url=$(jq -r '.url' <<<"${pr_json}")"
        echo "head-sha=$(jq -r '.headRefOid' <<<"${pr_json}")"
        echo "base-sha=$(jq -r '.baseRefOid' <<<"${pr_json}")"
    } >> "${GITHUB_OUTPUT}"
}

refresh_pr_state "${pr_json}"
evaluate_or_passthrough_candidate
if [[ -n "${skip_reason}" ]]; then
    skip "${skip_reason}"
    exit 0
fi

set +e
capture_review_snapshot
capture_review_snapshot_status=$?
set -e

if [[ "${capture_review_snapshot_status}" -eq 1 ]]; then
    skip "PR #${pr_number} のレビュー入力を作成中に HEAD、BASE、または patch が連続して更新されたため、自動承認しません。"
    exit 0
fi

if [[ "${capture_review_snapshot_status}" -eq 3 ]]; then
    skip "${expected_snapshot_skip_reason}"
    exit 0
fi

if [[ "${capture_review_snapshot_status}" -ne 0 ]]; then
    exit "${capture_review_snapshot_status}"
fi

refresh_pr_state "${pr_json}"
evaluate_or_passthrough_candidate
if [[ -n "${skip_reason}" ]]; then
    skip "${skip_reason}"
    exit 0
fi

write_metadata_json "${input_dir}/metadata.json" "${pr_json}"

{
    echo "candidate=true"
    echo "skip-reason="
    echo "input-dir=${input_dir}"
    echo "pr-url=$(jq -r '.url' <<<"${pr_json}")"
    echo "head-sha=$(jq -r '.headRefOid' <<<"${pr_json}")"
    echo "base-sha=$(jq -r '.baseRefOid' <<<"${pr_json}")"
} >> "${GITHUB_OUTPUT}"
