#!/usr/bin/env bash

set -euo pipefail

readonly pr_number="${PR_NUMBER:?PR_NUMBER is required}"
readonly reviewed_head_sha="${REVIEWED_HEAD_SHA:?REVIEWED_HEAD_SHA is required}"
readonly reviewed_base_sha="${REVIEWED_BASE_SHA:-}"
readonly reviewed_diff_path="${REVIEWED_DIFF_PATH:?REVIEWED_DIFF_PATH is required}"
readonly review_body="${REVIEW_BODY:?REVIEW_BODY is required}"
readonly review_dismissal_fallback_token="${REVIEW_DISMISSAL_FALLBACK_TOKEN:-}"
readonly skip_candidate_validation="${SKIP_CANDIDATE_VALIDATION:-false}"
readonly -a approval_confirmation_backoff_seconds=(1 2 4)
script_dir="$(
    cd "$(dirname "${BASH_SOURCE[0]}")" && pwd
)"
readonly script_dir
created_review_ids=()
dismiss_failed_review_ids=()

# shellcheck source=tools/github-actions/dependabot_auto_approve_lib.sh
source "${script_dir}/dependabot_auto_approve_lib.sh"
configured_require_current_head_claude_review_verdict="$(
    load_require_current_head_claude_review_verdict
)"
readonly configured_require_current_head_claude_review_verdict

if [[ ! -f "${reviewed_diff_path}" ]]; then
    echo "reviewed diff file does not exist: ${reviewed_diff_path}" >&2
    exit 1
fi

run_without_errexit() {
    set +e
    "$@"
    local exit_code=$?
    set -e
    return "${exit_code}"
}

run_gh_with_review_fallback() {
    local primary_status
    local fallback_status

    if run_without_errexit gh "$@"; then
        return 0
    else
        primary_status=$?
    fi

    if [[ -n "${review_dismissal_fallback_token}" && "${GH_TOKEN:-}" != "${review_dismissal_fallback_token}" ]]; then
        if run_without_errexit env GH_TOKEN="${review_dismissal_fallback_token}" gh "$@"; then
            return 0
        else
            fallback_status=$?
        fi
        return "${fallback_status}"
    fi

    return "${primary_status}"
}

load_pr_json() {
    load_candidate_pr_json "${pr_number}"
}

load_current_pr_head_snapshot_json() {
    run_gh_read_command api \
        -H "Accept: application/vnd.github+json" \
        "repos/${GITHUB_REPOSITORY}/pulls/${pr_number}"
}

load_status_check_rollup_snapshot_json() {
    local pull_number="$1"
    local repository="${GITHUB_REPOSITORY:-}"
    local owner
    local repo

    if [[ -z "${repository}" || "${repository}" != */* ]]; then
        echo "GITHUB_REPOSITORY must be set to OWNER/REPO" >&2
        return 1
    fi

    owner="${repository%%/*}"
    repo="${repository#*/}"

    # shellcheck disable=SC2016
    run_gh_read_command api graphql \
        -f query='
            query($owner: String!, $repo: String!, $number: Int!) {
              repository(owner: $owner, name: $repo) {
                pullRequest(number: $number) {
                  headRefOid
                  statusCheckRollup {
                    commit {
                      oid
                    }
                    contexts(first: 100) {
                      nodes {
                        __typename
                        ... on CheckRun {
                          name
                          conclusion
                          status
                          completedAt
                          startedAt
                        }
                        ... on StatusContext {
                          context
                          state
                          createdAt
                        }
                      }
                    }
                  }
                }
              }
            }
        ' \
        -f owner="${owner}" \
        -f repo="${repo}" \
        -F number="${pull_number}"
}

refresh_current_pr_state() {
    local allow_approved_review="${1:-false}"
    local allow_non_open_state="${2:-false}"
    local allow_unknown_mergeable="${3:-false}"
    local next_pr_json

    if ! next_pr_json="$(load_pr_json)"; then
        echo "failed to load current PR state for PR #${pr_number}" >&2
        return 1
    fi
    current_pr_json="${next_pr_json}"
    reevaluate_current_pr_state "${allow_approved_review}" "${allow_non_open_state}" "${allow_unknown_mergeable}"
}

reevaluate_current_pr_state() {
    local allow_approved_review="${1:-false}"
    local allow_non_open_state="${2:-false}"
    local allow_unknown_mergeable="${3:-false}"
    local require_current_head_claude_review_verdict

    refresh_pr_state "${current_pr_json}"
    # shellcheck disable=SC2154
    current_head_sha="${pr_head_sha}"
    # shellcheck disable=SC2154
    current_base_sha="${pr_base_sha}"

    if [[ "${skip_candidate_validation}" == "true" ]]; then
        local current_pr_state="${pr_state:-OPEN}"
        local current_review_decision="${review_decision:-REVIEW_REQUIRED}"
        local current_mergeable_state="${mergeable_state:-UNKNOWN}"

        # shellcheck disable=SC2154
        current_pr_json="${current_pr_state_json}"
        current_skip_reason=""

        if [[ "${current_pr_state}" != "OPEN" && "${allow_non_open_state}" != "true" ]]; then
            current_skip_reason="PR #${pr_number} はオープン状態ではありません。"
            return
        fi

        if [[ "${current_review_decision}" == "APPROVED" && "${allow_approved_review}" != "true" ]]; then
            current_skip_reason="PR #${pr_number} は既に承認済みです。"
            return
        fi

        if [[ "${current_review_decision}" == "CHANGES_REQUESTED" ]]; then
            current_skip_reason="PR #${pr_number} は変更要求レビューが付いているため自動承認しません。"
            return
        fi

        if [[ "${current_mergeable_state}" != "MERGEABLE" ]]; then
            if [[ "${current_mergeable_state}" == "UNKNOWN" && "${allow_unknown_mergeable}" == "true" ]]; then
                :
            else
                current_skip_reason="PR #${pr_number} はマージ可能ではありません (state: ${current_mergeable_state})。"
            fi
        fi

        return
    fi

    require_current_head_claude_review_verdict="$(
        load_require_current_head_claude_review_verdict
    )"
    evaluate_candidate \
        "${pr_number}" \
        "${allow_approved_review}" \
        "${allow_non_open_state}" \
        "${allow_unknown_mergeable}" \
        "${require_current_head_claude_review_verdict}"
    current_pr_json="${current_pr_state_json}"
    current_head_sha="${pr_head_sha}"
    current_base_sha="${pr_base_sha}"
    current_skip_reason="${skip_reason:-}"
}

build_mismatch_message() {
    local expected_head_sha="$1"
    local actual_head_sha="$2"
    local expected_base_sha="$3"
    local actual_base_sha="$4"
    local patch_changed="$5"
    local head_changed_during_capture="$6"
    local base_changed_during_capture="$7"
    local patch_changed_during_capture="$8"
    local current_head_claude_review_verdict_required="$9"
    local mismatch_parts=()
    local has_snapshot_mismatch="false"
    local base_mismatch="false"

    if [[ -n "${expected_base_sha}" && "${actual_base_sha}" != "${expected_base_sha}" ]]; then
        base_mismatch="true"
        has_snapshot_mismatch="true"
    fi

    if [[ "${head_changed_during_capture}" == "true" || "${base_changed_during_capture}" == "true" || "${patch_changed_during_capture}" == "true" || "${patch_changed}" == "true" ]]; then
        has_snapshot_mismatch="true"
    fi

    if [[ "${actual_head_sha}" != "${expected_head_sha}" && "${current_head_claude_review_verdict_required}" != "true" ]]; then
        has_snapshot_mismatch="true"
    fi

    if [[ "${actual_head_sha}" != "${expected_head_sha}" && "${has_snapshot_mismatch}" == "true" ]]; then
        mismatch_parts+=("HEAD (reviewed: ${expected_head_sha}, current: ${actual_head_sha})")
    fi

    if [[ "${base_mismatch}" == "true" ]]; then
        mismatch_parts+=("BASE (reviewed: ${expected_base_sha}, current: ${actual_base_sha})")
    fi

    if [[ "${head_changed_during_capture}" == "true" ]]; then
        mismatch_parts+=("HEAD changed while capturing the current PR diff")
    fi

    if [[ "${base_changed_during_capture}" == "true" ]]; then
        mismatch_parts+=("BASE changed while capturing the current PR diff")
    fi

    if [[ "${patch_changed_during_capture}" == "true" ]]; then
        mismatch_parts+=("patch changed while capturing the current PR diff")
    fi

    if [[ "${patch_changed}" == "true" ]]; then
        mismatch_parts+=("patch (reviewed diff no longer matches current PR diff)")
    fi

    if [[ "${#mismatch_parts[@]}" -gt 0 ]]; then
        printf '%s' "${mismatch_parts[*]}"
    fi
}

capture_current_diff() {
    local before_pr_json
    local before_head_sha
    local before_base_sha
    local after_first_pr_json
    local after_first_head_sha
    local after_first_base_sha
    local after_second_pr_json
    local after_second_head_sha
    local after_second_base_sha
    local first_diff_path
    local second_diff_path
    local remaining_attempts=3

    current_diff_path=""

    while (( remaining_attempts > 0 )); do
        head_changed_during_capture="false"
        base_changed_during_capture="false"
        patch_changed_during_capture="false"
        first_diff_path="$(mktemp)"
        second_diff_path="$(mktemp)"

        if ! before_pr_json="$(load_pr_json)"; then
            echo "failed to load PR #${pr_number} before capturing the current diff" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        if ! before_head_sha="$(jq -er '.headRefOid' <<<"${before_pr_json}")"; then
            echo "failed to parse headRefOid for PR #${pr_number} before capturing the current diff" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        if ! before_base_sha="$(jq -er '.baseRefOid' <<<"${before_pr_json}")"; then
            echo "failed to parse baseRefOid for PR #${pr_number} before capturing the current diff" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi

        if ! run_gh_with_review_fallback pr diff "${pr_number}" --patch > "${first_diff_path}"; then
            echo "failed to capture the first current diff for PR #${pr_number}" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi

        if ! after_first_pr_json="$(load_pr_json)"; then
            echo "failed to reload PR #${pr_number} after capturing the first current diff" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        if ! after_first_head_sha="$(jq -er '.headRefOid' <<<"${after_first_pr_json}")"; then
            echo "failed to parse headRefOid for PR #${pr_number} after capturing the first current diff" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        if ! after_first_base_sha="$(jq -er '.baseRefOid' <<<"${after_first_pr_json}")"; then
            echo "failed to parse baseRefOid for PR #${pr_number} after capturing the first current diff" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        current_pr_json="${after_first_pr_json}"
        current_head_sha="${after_first_head_sha}"
        current_base_sha="${after_first_base_sha}"

        if [[ "${before_head_sha}" != "${after_first_head_sha}" ]]; then
            head_changed_during_capture="true"
        fi

        if [[ "${before_base_sha}" != "${after_first_base_sha}" ]]; then
            base_changed_during_capture="true"
        fi

        if [[ "${head_changed_during_capture}" == "true" || "${base_changed_during_capture}" == "true" ]]; then
            rm -f "${first_diff_path}" "${second_diff_path}"
            remaining_attempts=$((remaining_attempts - 1))
            continue
        fi

        if ! run_gh_with_review_fallback pr diff "${pr_number}" --patch > "${second_diff_path}"; then
            echo "failed to capture the second current diff for PR #${pr_number}" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi

        if ! after_second_pr_json="$(load_pr_json)"; then
            echo "failed to reload PR #${pr_number} after capturing the second current diff" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        if ! after_second_head_sha="$(jq -er '.headRefOid' <<<"${after_second_pr_json}")"; then
            echo "failed to parse headRefOid for PR #${pr_number} after capturing the second current diff" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        if ! after_second_base_sha="$(jq -er '.baseRefOid' <<<"${after_second_pr_json}")"; then
            echo "failed to parse baseRefOid for PR #${pr_number} after capturing the second current diff" >&2
            rm -f "${first_diff_path}" "${second_diff_path}"
            return 2
        fi
        current_pr_json="${after_second_pr_json}"
        current_head_sha="${after_second_head_sha}"
        current_base_sha="${after_second_base_sha}"

        if [[ "${after_first_head_sha}" != "${after_second_head_sha}" ]]; then
            head_changed_during_capture="true"
        fi

        if [[ "${after_first_base_sha}" != "${after_second_base_sha}" ]]; then
            base_changed_during_capture="true"
        fi

        if [[ "${head_changed_during_capture}" == "true" || "${base_changed_during_capture}" == "true" ]]; then
            rm -f "${first_diff_path}" "${second_diff_path}"
            remaining_attempts=$((remaining_attempts - 1))
            continue
        fi

        if patches_match_ignoring_transient_metadata "${first_diff_path}" "${second_diff_path}"; then
            current_diff_path="${second_diff_path}"
            rm -f "${first_diff_path}"
            return 0
        fi

        patch_changed_during_capture="true"
        rm -f "${first_diff_path}" "${second_diff_path}"
        remaining_attempts=$((remaining_attempts - 1))
    done

    return 1
}

compare_review_snapshot() {
    local patch_changed="false"
    local capture_current_diff_status
    local comparison_function="patches_match_for_review"

    mismatch_message=""

    set +e
    capture_current_diff
    capture_current_diff_status=$?
    set -e

    if [[ "${capture_current_diff_status}" -eq 0 ]]; then
        if [[ "${current_head_sha}" == "${reviewed_head_sha}" ]] && {
            [[ -z "${reviewed_base_sha}" ]] || [[ "${current_base_sha}" == "${reviewed_base_sha}" ]]
        }; then
            comparison_function="patches_match_ignoring_transient_metadata"
        fi

        if ! "${comparison_function}" "${reviewed_diff_path}" "${current_diff_path}"; then
            patch_changed="true"
        fi
    elif [[ "${capture_current_diff_status}" -ne 1 ]]; then
        return "${capture_current_diff_status}"
    fi

    mismatch_message="$(
        build_mismatch_message \
            "${reviewed_head_sha}" \
            "${current_head_sha}" \
            "${reviewed_base_sha}" \
            "${current_base_sha}" \
            "${patch_changed}" \
            "${head_changed_during_capture}" \
            "${base_changed_during_capture}" \
            "${patch_changed_during_capture:-false}" \
            "${configured_require_current_head_claude_review_verdict}"
    )"

    if [[ -n "${current_diff_path:-}" ]]; then
        rm -f "${current_diff_path}"
        current_diff_path=""
    fi
}

write_skip_output() {
    local reason="$1"

    {
        echo "approved=false"
        echo "skip-reason=${reason}"
    } >> "${GITHUB_OUTPUT}"
}

record_created_review_id() {
    local review_id="$1"
    local existing_review_id

    if [[ -z "${review_id}" || "${review_id}" == "null" ]]; then
        return
    fi

    if [[ "${#created_review_ids[@]}" -gt 0 ]]; then
        for existing_review_id in "${created_review_ids[@]}"; do
            if [[ "${existing_review_id}" == "${review_id}" ]]; then
                return
            fi
        done
    fi

    created_review_ids+=("${review_id}")
}

format_review_ids_for_message() {
    local review_id
    local formatted_review_ids=""

    for review_id in "$@"; do
        if [[ -z "${review_id}" || "${review_id}" == "null" ]]; then
            continue
        fi

        if [[ -n "${formatted_review_ids}" ]]; then
            formatted_review_ids="${formatted_review_ids}, "
        fi
        formatted_review_ids="${formatted_review_ids}#${review_id}"
    done

    printf '%s' "${formatted_review_ids}"
}

submit_approval_review() {
    gh api \
        --method POST \
        -H "Accept: application/vnd.github+json" \
        "repos/${GITHUB_REPOSITORY}/pulls/${pr_number}/reviews" \
        -f event=APPROVE \
        -f body="${review_body}" \
        -f commit_id="${approval_commit_sha}"
}

dismiss_review() {
    local review_id="$1"
    local dismissal_message="$2"
    local auth_token="${3:-}"

    if [[ -n "${auth_token}" ]]; then
        GH_TOKEN="${auth_token}" gh api \
            --method PUT \
            -H "Accept: application/vnd.github+json" \
            "repos/${GITHUB_REPOSITORY}/pulls/${pr_number}/reviews/${review_id}/dismissals" \
            -f message="${dismissal_message}" \
            >/dev/null
        return
    fi

    gh api \
        --method PUT \
        -H "Accept: application/vnd.github+json" \
        "repos/${GITHUB_REPOSITORY}/pulls/${pr_number}/reviews/${review_id}/dismissals" \
        -f message="${dismissal_message}" \
        >/dev/null
}

current_pr_state_for_dismissal() {
    local current_pr_state_from_json=""

    if [[ -n "${current_pr_json:-}" ]]; then
        current_pr_state_from_json="$(jq -r '.state // "OPEN"' <<<"${current_pr_json}")"
    fi

    if [[ "${pr_state:-}" == "MERGED" || "${current_pr_state_from_json}" == "MERGED" ]]; then
        printf '%s' "MERGED"
        return
    fi

    if [[ -n "${current_pr_state_from_json}" ]]; then
        printf '%s' "${current_pr_state_from_json}"
        return
    fi

    if [[ -n "${pr_state:-}" ]]; then
        printf '%s' "${pr_state}"
        return
    fi

    printf '%s' "OPEN"
}

dismiss_review_if_reopenable() {
    local review_id="$1"
    local dismissal_message="$2"
    dismiss_review_ids_if_reopenable "${dismissal_message}" "${review_id}"
}

dismiss_review_ids_if_reopenable() {
    local dismissal_message="$1"
    shift
    local current_pr_state
    local review_id
    local dismissal_status=0

    dismiss_failed_review_ids=()
    current_pr_state="$(current_pr_state_for_dismissal)"
    if [[ "${current_pr_state}" == "MERGED" ]]; then
        return
    fi

    for review_id in "$@"; do
        if [[ -z "${review_id}" || "${review_id}" == "null" ]]; then
            continue
        fi

        if run_without_errexit dismiss_review_with_retries "${review_id}" "${dismissal_message}"; then
            :
        else
            dismissal_status=$?
            dismiss_failed_review_ids+=("${review_id}")
        fi
    done

    if [[ "${#dismiss_failed_review_ids[@]}" -gt 0 ]]; then
        return "${dismissal_status}"
    fi
}

dismiss_recorded_reviews_if_reopenable() {
    local dismissal_message="$1"

    if [[ "${#created_review_ids[@]}" -eq 0 ]]; then
        return 0
    fi

    dismiss_review_ids_if_reopenable "${dismissal_message}" "${created_review_ids[@]}"
}

dismiss_review_with_retries() {
    local review_id="$1"
    local dismissal_message="$2"
    local -a dismissal_auth_tokens=("")
    local last_exit_code=1
    local auth_token
    local dismiss_attempt_count

    dismiss_review_failure_message=""

    if [[ -n "${review_dismissal_fallback_token}" ]]; then
        dismissal_auth_tokens+=("${review_dismissal_fallback_token}")
    fi

    for auth_token in "${dismissal_auth_tokens[@]}"; do
        dismiss_attempt_count=0
        while (( dismiss_attempt_count < 2 )); do
            dismiss_attempt_count=$((dismiss_attempt_count + 1))
            if run_without_errexit dismiss_review "${review_id}" "${dismissal_message}" "${auth_token}"; then
                return 0
            else
                last_exit_code=$?
            fi
        done
    done

    if [[ -n "${review_dismissal_fallback_token}" ]]; then
        dismiss_review_failure_message="configured credentials と REVIEW_DISMISSAL_FALLBACK_TOKEN の両方で dismiss を再試行しましたが失敗しました。"
    else
        dismiss_review_failure_message="dismiss を再試行しましたが失敗し、REVIEW_DISMISSAL_FALLBACK_TOKEN も未設定でした。"
    fi

    return "${last_exit_code}"
}

handle_review_dismissal_failure() {
    local review_ids_message="$1"
    local dismissal_reason="$2"
    local exit_code="$3"
    local failure_message

    failure_message="PR #${pr_number} の自動承認 review ${review_ids_message} を取り消せませんでした。手動で dismiss してください。取り消しが必要な理由: ${dismissal_reason}"
    if [[ -n "${dismiss_review_failure_message:-}" ]]; then
        failure_message="${failure_message} ${dismiss_review_failure_message}"
    fi

    write_skip_output "${failure_message}"
    echo "${failure_message}" >&2
    exit "${exit_code}"
}

handle_post_approval_recheck_failure() {
    local review_id="$1"
    local failure_reason="$2"
    local exit_code="$3"
    local dismissal_message
    local dismissal_status
    local failed_review_ids_message
    local current_pr_state

    dismissal_message="PR #${pr_number} の承認後再検証に失敗したため、この自動承認を取り消します: ${failure_reason}"
    record_created_review_id "${review_id}"
    if run_without_errexit dismiss_recorded_reviews_if_reopenable "${dismissal_message}"; then
        current_pr_state="$(current_pr_state_for_dismissal)"
        if [[ "${current_pr_state}" == "MERGED" ]]; then
            {
                echo "approved=true"
                echo "skip-reason="
            } >> "${GITHUB_OUTPUT}"
            exit 0
        fi
        write_skip_output "${dismissal_message}"
        exit "${exit_code}"
    else
        dismissal_status=$?
    fi
    failed_review_ids_message=""
    if [[ "${#dismiss_failed_review_ids[@]}" -gt 0 ]]; then
        failed_review_ids_message="$(format_review_ids_for_message "${dismiss_failed_review_ids[@]}")"
    fi
    if [[ -z "${failed_review_ids_message}" ]]; then
        failed_review_ids_message="$(format_review_ids_for_message "${review_id}")"
    fi
    handle_review_dismissal_failure "${failed_review_ids_message}" "${dismissal_message}" "${dismissal_status}"
}

if_pr_closed_without_merge() {
    local current_pr_state

    current_pr_state="$(current_pr_state_for_dismissal)"
    [[ "${current_pr_state}" != "OPEN" && "${current_pr_state}" != "MERGED" ]]
}

latest_head_is_approved() {
    [[ \
        "${review_decision:-REVIEW_REQUIRED}" == "APPROVED" && \
        "${current_head_sha:-}" == "${approval_commit_sha:-}" \
    ]]
}

load_approval_review_json() {
    local review_id="$1"

    run_gh_with_review_fallback api \
        -H "Accept: application/vnd.github+json" \
        "repos/${GITHUB_REPOSITORY}/pulls/${pr_number}/reviews/${review_id}"
}

approval_review_matches_latest_head() {
    local review_id="$1"
    local review_json
    local review_state
    local review_commit_sha

    if [[ -z "${review_id}" || "${review_id}" == "null" ]]; then
        return 1
    fi

    if ! review_json="$(load_approval_review_json "${review_id}" 2>/dev/null)"; then
        return 1
    fi

    if ! review_state="$(jq -r '.state // ""' <<<"${review_json}")"; then
        return 1
    fi

    if ! review_commit_sha="$(jq -r '.commit_id // .commitId // ""' <<<"${review_json}")"; then
        return 1
    fi

    [[ \
        "${review_state}" == "APPROVED" && \
        -n "${review_commit_sha}" && \
        "${review_commit_sha}" == "${approval_commit_sha:-}" \
    ]]
}

latest_head_has_confirmed_approval() {
    local review_id="$1"

    if latest_head_is_approved; then
        return 0
    fi

    if [[ "${current_head_sha:-}" != "${approval_commit_sha:-}" ]]; then
        return 1
    fi

    approval_review_matches_latest_head "${review_id}"
}

latest_head_snapshot_indicates_closed_without_merge() {
    local latest_head_snapshot_json="$1"
    local snapshot_state
    local snapshot_merged

    if ! snapshot_state="$(jq -r '.state // ""' <<<"${latest_head_snapshot_json}")"; then
        return 1
    fi
    if ! snapshot_merged="$(
        jq -r '
            if .merged == true or (.merged_at? != null) or ((.state // "") | ascii_upcase) == "MERGED" then
                "true"
            else
                "false"
            end
        ' <<<"${latest_head_snapshot_json}"
    )"; then
        return 1
    fi

    snapshot_state="$(tr '[:lower:]' '[:upper:]' <<<"${snapshot_state}")"
    [[ "${snapshot_state}" == "CLOSED" && "${snapshot_merged}" != "true" ]]
}

sleep_before_approval_confirmation_retry() {
    local attempt_number="$1"
    local backoff_index=$((attempt_number - 1))
    local sleep_seconds="${approval_confirmation_backoff_seconds[backoff_index]}"

    sleep "${sleep_seconds}"
}

dismiss_review_and_skip() {
    local review_id="$1"
    local dismissal_message="$2"
    local dismissal_status
    local failed_review_ids_message

    record_created_review_id "${review_id}"
    if run_without_errexit dismiss_recorded_reviews_if_reopenable "${dismissal_message}"; then
        write_skip_output "${dismissal_message}"
        exit 0
    else
        dismissal_status=$?
    fi
    failed_review_ids_message=""
    if [[ "${#dismiss_failed_review_ids[@]}" -gt 0 ]]; then
        failed_review_ids_message="$(format_review_ids_for_message "${dismiss_failed_review_ids[@]}")"
    fi
    if [[ -z "${failed_review_ids_message}" ]]; then
        failed_review_ids_message="$(format_review_ids_for_message "${review_id}")"
    fi
    handle_review_dismissal_failure "${failed_review_ids_message}" "${dismissal_message}" "${dismissal_status}"
}

submit_approval_review_with_retry() {
    local review_error_file
    local review_submission_head_refresh_count=0
    local max_review_submission_head_refreshes=2

    review_response=""
    review_error_file="$(mktemp)"
    while ! review_response="$(submit_approval_review 2>"${review_error_file}")"; do
        # commit_id race の直後は GitHub が mergeability を再計算中で UNKNOWN を返すことがある。
        refresh_current_pr_state false false true

        if [[ -n "${current_skip_reason}" ]]; then
            rm -f "${review_error_file}"
            write_skip_output "${current_skip_reason}"
            exit 0
        fi

        compare_review_snapshot
        if [[ -n "${mismatch_message}" ]]; then
            rm -f "${review_error_file}"
            write_skip_output "PR #${pr_number} の差分前提が承認送信前に更新されたため、自動承認をスキップします: ${mismatch_message}。"
            exit 0
        fi
        reevaluate_current_pr_state false false true
        if [[ -n "${current_skip_reason}" ]]; then
            rm -f "${review_error_file}"
            write_skip_output "${current_skip_reason}"
            exit 0
        fi

        if [[ "${current_head_sha}" != "${approval_commit_sha}" ]]; then
            review_submission_head_refresh_count=$((review_submission_head_refresh_count + 1))
            if (( review_submission_head_refresh_count >= max_review_submission_head_refreshes )); then
                rm -f "${review_error_file}"
                write_skip_output "PR #${pr_number} の HEAD が承認送信前に連続して更新されたため、自動承認をスキップします。"
                exit 0
            fi
            approval_commit_sha="${current_head_sha}"
            approval_base_sha="${current_base_sha}"
            continue
        fi

        cat "${review_error_file}" >&2
        rm -f "${review_error_file}"
        exit 1
    done
    rm -f "${review_error_file}"
}

revalidate_post_approval_state_or_exit() {
    local review_id="$1"
    local post_approval_compare_status
    local post_approval_refresh_status
    local post_approval_compare_after_refresh_status
    local post_approval_verified_head_sha
    local post_approval_verified_base_sha
    local dismissal_message

    if run_without_errexit compare_review_snapshot; then
        post_approval_compare_status=0
    else
        post_approval_compare_status=$?
        handle_post_approval_recheck_failure \
            "${review_id}" \
            "差分再検証で current PR の取得または diff 取得に失敗しました。" \
            "${post_approval_compare_status}"
    fi

    if [[ -n "${mismatch_message}" ]]; then
        dismissal_message="PR #${pr_number} の差分前提が承認送信中に更新されたため、この自動承認を取り消します: ${mismatch_message}。"
        dismiss_review_and_skip "${review_id}" "${dismissal_message}"
    fi
    post_approval_verified_head_sha="${current_head_sha}"
    post_approval_verified_base_sha="${current_base_sha}"

    if run_without_errexit refresh_current_pr_state true true true; then
        post_approval_refresh_status=0
    else
        post_approval_refresh_status=$?
        handle_post_approval_recheck_failure \
            "${review_id}" \
            "候補条件の再評価で current PR の取得に失敗しました。" \
            "${post_approval_refresh_status}"
    fi

    if [[ -n "${current_skip_reason}" ]]; then
        dismissal_message="PR #${pr_number} の候補条件が承認送信中に変わったため、この自動承認を取り消します: ${current_skip_reason}"
        dismiss_review_and_skip "${review_id}" "${dismissal_message}"
    fi

    if [[ "${current_head_sha}" != "${post_approval_verified_head_sha}" || "${current_base_sha}" != "${post_approval_verified_base_sha}" ]]; then
        if run_without_errexit compare_review_snapshot; then
            post_approval_compare_after_refresh_status=0
        else
            post_approval_compare_after_refresh_status=$?
            handle_post_approval_recheck_failure \
                "${review_id}" \
                "差分再検証で current PR の取得または diff 取得に失敗しました。" \
                "${post_approval_compare_after_refresh_status}"
        fi

        if [[ -n "${mismatch_message}" ]]; then
            dismissal_message="PR #${pr_number} の差分前提が承認送信中に更新されたため、この自動承認を取り消します: ${mismatch_message}。"
            dismiss_review_and_skip "${review_id}" "${dismissal_message}"
        fi

        reevaluate_current_pr_state true true true
        if [[ -n "${current_skip_reason}" ]]; then
            dismissal_message="PR #${pr_number} の候補条件が承認送信中に変わったため、この自動承認を取り消します: ${current_skip_reason}"
            dismiss_review_and_skip "${review_id}" "${dismissal_message}"
        fi
    fi

    if if_pr_closed_without_merge; then
        dismissal_message="PR #${pr_number} は承認送信中に未マージのまま close されたため、この自動承認を取り消します。"
        dismiss_review_and_skip "${review_id}" "${dismissal_message}"
    fi
}

wait_for_latest_head_approval_or_reapprove() {
    local review_id="$1"
    local confirmation_attempt=1
    local max_confirmation_attempts=4
    local latest_head_snapshot_json
    local latest_confirmed_head_sha
    local approval_confirmation_refresh_status
    local approval_confirmation_compare_status
    local dismissal_message

    while (( confirmation_attempt <= max_confirmation_attempts )); do
        if latest_head_snapshot_json="$(load_current_pr_head_snapshot_json)"; then
            if ! latest_confirmed_head_sha="$(jq -er '.head.sha // ""' <<<"${latest_head_snapshot_json}")"; then
                handle_post_approval_recheck_failure \
                    "${review_id}" \
                    "承認状態の再確認で current PR の HEAD を解析できませんでした。" \
                    1
            fi
        else
            handle_post_approval_recheck_failure \
                "${review_id}" \
                "承認状態の再確認で current PR の HEAD 取得に失敗しました。" \
                1
        fi

        if latest_head_snapshot_indicates_closed_without_merge "${latest_head_snapshot_json}"; then
            dismissal_message="PR #${pr_number} は承認送信中に未マージのまま close されたため、この自動承認を取り消します。"
            dismiss_review_and_skip "${review_id}" "${dismissal_message}"
        fi

        if [[ "${latest_confirmed_head_sha}" != "${approval_commit_sha}" ]]; then
            if run_without_errexit refresh_current_pr_state true true true; then
                approval_confirmation_refresh_status=0
            else
                approval_confirmation_refresh_status=$?
                handle_post_approval_recheck_failure \
                    "${review_id}" \
                    "承認状態の再確認で current PR の取得に失敗しました。" \
                    "${approval_confirmation_refresh_status}"
            fi

            if [[ -n "${current_skip_reason}" ]]; then
                dismissal_message="PR #${pr_number} の候補条件が承認送信中に変わったため、この自動承認を取り消します: ${current_skip_reason}"
                dismiss_review_and_skip "${review_id}" "${dismissal_message}"
            fi

            if [[ "${current_head_sha}" != "${approval_commit_sha}" || "${current_base_sha}" != "${approval_base_sha}" ]]; then
                if run_without_errexit compare_review_snapshot; then
                    approval_confirmation_compare_status=0
                else
                    approval_confirmation_compare_status=$?
                    handle_post_approval_recheck_failure \
                        "${review_id}" \
                        "承認状態の再確認で current PR の取得または diff 取得に失敗しました。" \
                        "${approval_confirmation_compare_status}"
                fi

                if [[ -n "${mismatch_message}" ]]; then
                    dismissal_message="PR #${pr_number} の差分前提が承認送信中に更新されたため、この自動承認を取り消します: ${mismatch_message}。"
                    dismiss_review_and_skip "${review_id}" "${dismissal_message}"
                fi

                reevaluate_current_pr_state true true true
                if [[ -n "${current_skip_reason}" ]]; then
                    dismissal_message="PR #${pr_number} の候補条件が承認送信中に変わったため、この自動承認を取り消します: ${current_skip_reason}"
                    dismiss_review_and_skip "${review_id}" "${dismissal_message}"
                fi

                if if_pr_closed_without_merge; then
                    dismissal_message="PR #${pr_number} は承認送信中に未マージのまま close されたため、この自動承認を取り消します。"
                    dismiss_review_and_skip "${review_id}" "${dismissal_message}"
                fi

                if [[ "${current_head_sha}" != "${approval_commit_sha}" ]]; then
                    return 10
                fi

                if latest_head_has_confirmed_approval "${review_id}"; then
                    return 0
                fi

                approval_base_sha="${current_base_sha}"
            fi
        fi

        if [[ "${current_head_sha}" != "${approval_commit_sha}" ]]; then
            return 10
        fi

        if latest_head_has_confirmed_approval "${review_id}"; then
            return 0
        fi

        if (( confirmation_attempt == max_confirmation_attempts )); then
            break
        fi

        sleep_before_approval_confirmation_retry "${confirmation_attempt}"

        if run_without_errexit refresh_current_pr_state true true true; then
            approval_confirmation_refresh_status=0
        else
            approval_confirmation_refresh_status=$?
            handle_post_approval_recheck_failure \
                "${review_id}" \
                "承認状態の再確認で current PR の取得に失敗しました。" \
                "${approval_confirmation_refresh_status}"
        fi

        if [[ -n "${current_skip_reason}" ]]; then
            dismissal_message="PR #${pr_number} の候補条件が承認送信中に変わったため、この自動承認を取り消します: ${current_skip_reason}"
            dismiss_review_and_skip "${review_id}" "${dismissal_message}"
        fi

        if [[ "${current_head_sha}" != "${approval_commit_sha}" || "${current_base_sha}" != "${approval_base_sha}" ]]; then
            if run_without_errexit compare_review_snapshot; then
                approval_confirmation_compare_status=0
            else
                approval_confirmation_compare_status=$?
                handle_post_approval_recheck_failure \
                    "${review_id}" \
                    "承認状態の再確認で current PR の取得または diff 取得に失敗しました。" \
                    "${approval_confirmation_compare_status}"
            fi

            if [[ -n "${mismatch_message}" ]]; then
                dismissal_message="PR #${pr_number} の差分前提が承認送信中に更新されたため、この自動承認を取り消します: ${mismatch_message}。"
                dismiss_review_and_skip "${review_id}" "${dismissal_message}"
            fi

            reevaluate_current_pr_state true true true
            if [[ -n "${current_skip_reason}" ]]; then
                dismissal_message="PR #${pr_number} の候補条件が承認送信中に変わったため、この自動承認を取り消します: ${current_skip_reason}"
                dismiss_review_and_skip "${review_id}" "${dismissal_message}"
            fi

            if if_pr_closed_without_merge; then
                dismissal_message="PR #${pr_number} は承認送信中に未マージのまま close されたため、この自動承認を取り消します。"
                dismiss_review_and_skip "${review_id}" "${dismissal_message}"
            fi

            if [[ "${current_head_sha}" != "${approval_commit_sha}" ]]; then
                return 10
            fi

            if latest_head_has_confirmed_approval "${review_id}"; then
                return 0
            fi

            approval_base_sha="${current_base_sha}"
        fi

        if if_pr_closed_without_merge; then
            dismissal_message="PR #${pr_number} は承認送信中に未マージのまま close されたため、この自動承認を取り消します。"
            dismiss_review_and_skip "${review_id}" "${dismissal_message}"
        fi

        confirmation_attempt=$((confirmation_attempt + 1))
    done

    return 1
}

refresh_current_pr_state false false
if [[ -n "${current_skip_reason}" ]]; then
    write_skip_output "${current_skip_reason}"
    exit 0
fi

compare_review_snapshot
if [[ -n "${mismatch_message}" ]]; then
    write_skip_output "PR #${pr_number} の差分前提がレビュー後に更新されました: ${mismatch_message}。"
    exit 0
fi
approval_commit_sha="${current_head_sha}"
approval_base_sha="${current_base_sha}"

refresh_current_pr_state false false
if [[ -n "${current_skip_reason}" ]]; then
    write_skip_output "${current_skip_reason}"
    exit 0
fi

if [[ "${current_head_sha}" != "${approval_commit_sha}" || "${current_base_sha}" != "${approval_base_sha}" ]]; then
    compare_review_snapshot
    if [[ -n "${mismatch_message}" ]]; then
        write_skip_output "PR #${pr_number} の差分前提が承認送信前に更新されたため、自動承認をスキップします: ${mismatch_message}。"
        exit 0
    fi
    reevaluate_current_pr_state false false
    if [[ -n "${current_skip_reason}" ]]; then
        write_skip_output "${current_skip_reason}"
        exit 0
    fi
    approval_commit_sha="${current_head_sha}"
    approval_base_sha="${current_base_sha}"
fi

review_id=""
for _approval_attempt in 1 2 3 4 5; do
    submit_approval_review_with_retry
    review_id="$(jq -r '.id' <<<"${review_response}")"

    if [[ -z "${review_id}" || "${review_id}" == "null" ]]; then
        echo "failed to capture review id for PR #${pr_number}" >&2
        exit 1
    fi
    record_created_review_id "${review_id}"

    revalidate_post_approval_state_or_exit "${review_id}"

    if wait_for_latest_head_approval_or_reapprove "${review_id}"; then
        break
    else
        approval_confirmation_status=$?
    fi

    if [[ "${approval_confirmation_status}" -ne 10 ]]; then
        dismissal_message="PR #${pr_number} の最新 HEAD (${current_head_sha}) に有効な承認が付かなかったため、この自動承認を取り消します。"
        dismiss_review_and_skip "${review_id}" "${dismissal_message}"
    fi

    approval_commit_sha="${current_head_sha}"
    approval_base_sha="${current_base_sha}"
done

if ! latest_head_has_confirmed_approval "${review_id}"; then
    dismissal_message="PR #${pr_number} の最新 HEAD (${current_head_sha}) への承認送信が安定しなかったため、この自動承認を取り消します。"
    dismiss_review_and_skip "${review_id}" "${dismissal_message}"
fi

{
    echo "approved=true"
    echo "skip-reason="
} >> "${GITHUB_OUTPUT}"
