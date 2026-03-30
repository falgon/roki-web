#!/usr/bin/env bash

set -euo pipefail

script_dir="$(
    cd "$(dirname "${BASH_SOURCE[0]}")" && pwd
)"
readonly script_dir
readonly target_script="${script_dir}/../prepare_dependabot_review_input.sh"

assert_line_equals() {
    local file_path="$1"
    local expected_line="$2"

    if ! grep -Fqx -- "${expected_line}" "${file_path}"; then
        echo "Expected line not found: ${expected_line}" >&2
        echo "--- ${file_path} ---" >&2
        cat "${file_path}" >&2
        exit 1
    fi
}

assert_contains() {
    local file_path="$1"
    local expected_text="$2"

    if ! grep -Fq -- "${expected_text}" "${file_path}"; then
        echo "Expected text not found: ${expected_text}" >&2
        echo "--- ${file_path} ---" >&2
        cat "${file_path}" >&2
        exit 1
    fi
}

assert_not_contains() {
    local file_path="$1"
    local unexpected_text="$2"

    if grep -Fq -- "${unexpected_text}" "${file_path}"; then
        echo "Unexpected text found: ${unexpected_text}" >&2
        echo "--- ${file_path} ---" >&2
        cat "${file_path}" >&2
        exit 1
    fi
}

assert_file_absent() {
    local file_path="$1"

    if [[ -e "${file_path}" ]]; then
        echo "Expected file to be absent: ${file_path}" >&2
        exit 1
    fi
}

assert_exit_code() {
    local actual_exit_code="$1"
    local expected_exit_code="$2"

    if [[ "${actual_exit_code}" != "${expected_exit_code}" ]]; then
        echo "Expected exit code ${expected_exit_code}, got ${actual_exit_code}" >&2
        exit 1
    fi
}

write_pr_json() {
    local file_path="$1"
    local head_sha="$2"
    local base_sha="$3"

    cat <<EOF > "${file_path}"
{
  "number": 123,
  "title": "Bump example dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "${base_sha}",
  "headRefName": "dependabot/npm_and_yarn/example-1.2.3",
  "headRefOid": "${head_sha}",
  "mergeable": "MERGEABLE",
  "reviewDecision": null,
  "labels": [
    { "name": "dependabot/npm" },
    { "name": "automerge" }
  ],
  "statusCheckRollup": [
    {
      "__typename": "CheckRun",
      "name": "lint-and-test",
      "status": "COMPLETED",
      "conclusion": "SUCCESS"
    }
  ],
  "files": [
    { "path": "package-lock.json" }
  ]
}
EOF
}

write_non_npm_pr_json() {
    local file_path="$1"
    local head_sha="$2"
    local base_sha="$3"
    local target_path="$4"

    cat <<EOF > "${file_path}"
{
  "number": 123,
  "title": "Bump Docker base image",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "${base_sha}",
  "headRefName": "dependabot/non-npm/example",
  "headRefOid": "${head_sha}",
  "mergeable": "MERGEABLE",
  "reviewDecision": null,
  "labels": [
    { "name": "automerge" }
  ],
  "statusCheckRollup": [
    {
      "__typename": "CheckRun",
      "name": "lint-and-test",
      "status": "COMPLETED",
      "conclusion": "SUCCESS"
    },
    {
      "__typename": "CheckRun",
      "name": "build",
      "status": "COMPLETED",
      "conclusion": "SUCCESS"
    },
    {
      "__typename": "CheckRun",
      "name": "Analyze (javascript)",
      "status": "COMPLETED",
      "conclusion": "SUCCESS"
    },
    {
      "__typename": "CheckRun",
      "name": "restyled",
      "status": "COMPLETED",
      "conclusion": "SUCCESS"
    }
  ],
  "files": [
    { "path": "${target_path}" }
  ]
}
EOF
}

write_protection_json() {
    local file_path="$1"

    cat <<'EOF' > "${file_path}"
{
  "required_status_checks": {
    "checks": [],
    "contexts": []
  }
}
EOF
}

setup_fake_gh() {
    local bin_dir="$1"

    mkdir -p "${bin_dir}"
    cat <<'EOF' > "${bin_dir}/gh"
#!/usr/bin/env bash

set -euo pipefail

readonly state_dir="${FAKE_GH_STATE_DIR:?}"

select_current_pr_path() {
    if [[ -f "${state_dir}/last_pr_view_path" ]]; then
        local selected_pr_path
        selected_pr_path="$(<"${state_dir}/last_pr_view_path")"
        if [[ -f "${selected_pr_path}" ]]; then
            printf '%s\n' "${selected_pr_path}"
            return
        fi
    fi

    ls "${state_dir}"/pr_view_*.json 2>/dev/null | sort | tail -n 1 || true
}

extract_page_number() {
    local endpoint="$1"

    if [[ "${endpoint}" =~ [\?\&]page=([0-9]+) ]]; then
        printf '%s\n' "${BASH_REMATCH[1]}"
        return
    fi

    printf '1\n'
}

case "${1:-}" in
    pr)
        case "${2:-}" in
            view)
                counter_file="${state_dir}/pr_view_count"
                count=0
                if [[ -f "${counter_file}" ]]; then
                    count="$(<"${counter_file}")"
                fi
                count=$((count + 1))
                printf '%s' "${count}" > "${counter_file}"
                exit_code_file="${state_dir}/pr_view_${count}.exit_code"
                stderr_file="${state_dir}/pr_view_${count}.stderr"
                if [[ -f "${exit_code_file}" ]]; then
                    if [[ -f "${stderr_file}" ]]; then
                        cat "${stderr_file}" >&2
                    fi
                    exit "$(<"${exit_code_file}")"
                fi
                response_file="${state_dir}/pr_view_${count}.json"
                latest_response="$(
                    ls "${state_dir}"/pr_view_*.json 2>/dev/null | sort | tail -n 1 || true
                )"
                if [[ -f "${response_file}" ]]; then
                    printf '%s\n' "${response_file}" > "${state_dir}/last_pr_view_path"
                    cat "${response_file}"
                elif [[ -n "${latest_response}" ]]; then
                    printf '%s\n' "${latest_response}" > "${state_dir}/last_pr_view_path"
                    cat "${latest_response}"
                else
                    echo "missing fake gh pr view response: ${response_file}" >&2
                    exit 1
                fi
                ;;
            diff)
                counter_file="${state_dir}/pr_diff_count"
                count=0
                if [[ -f "${counter_file}" ]]; then
                    count="$(<"${counter_file}")"
                fi
                count=$((count + 1))
                printf '%s' "${count}" > "${counter_file}"
                exit_code_file="${state_dir}/diff_${count}.exit_code"
                stderr_file="${state_dir}/diff_${count}.stderr"
                if [[ -f "${exit_code_file}" ]]; then
                    if [[ -f "${stderr_file}" ]]; then
                        cat "${stderr_file}" >&2
                    fi
                    exit "$(<"${exit_code_file}")"
                fi

                response_file="${state_dir}/diff_${count}.patch"
                if [[ -f "${response_file}" ]]; then
                    cat "${response_file}"
                elif [[ -f "${state_dir}/diff.patch" ]]; then
                    cat "${state_dir}/diff.patch"
                else
                    latest_response="$(
                        ls "${state_dir}"/diff_*.patch 2>/dev/null | sort | tail -n 1 || true
                    )"
                    if [[ -n "${latest_response}" ]]; then
                        cat "${latest_response}"
                    else
                        echo "missing fake gh pr diff response: ${response_file}" >&2
                        exit 1
                    fi
                fi
                ;;
            *)
                echo "unexpected gh pr invocation: $*" >&2
                exit 1
                ;;
        esac
        ;;
    api)
        shift
        endpoint=""

        while (( $# > 0 )); do
            case "${1:-}" in
                graphql)
                    endpoint="graphql"
                    break
                    ;;
                -H | --method | -f | -F)
                    shift 2
                    ;;
                *)
                    endpoint="${1}"
                    break
                    ;;
            esac
        done

        if [[ "${endpoint}" == "graphql" ]]; then
            counter_file="${state_dir}/graphql_count"
            count=0
            if [[ -f "${counter_file}" ]]; then
                count="$(<"${counter_file}")"
            fi
            count=$((count + 1))
            printf '%s' "${count}" > "${counter_file}"

            response_file="${state_dir}/status_rollup_${count}.json"
            if [[ -f "${response_file}" ]]; then
                cat "${response_file}"
                exit 0
            fi

            if [[ -f "${state_dir}/status_rollup.json" ]]; then
                cat "${state_dir}/status_rollup.json"
                exit 0
            fi

            if [[ -f "${state_dir}/last_pr_view_path" ]]; then
                selected_pr_path="$(<"${state_dir}/last_pr_view_path")"
                jq '{
                  data: {
                    repository: {
                      pullRequest: {
                        headRefOid: .headRefOid,
                        statusCheckRollup: (
                          if .statusCheckRollup == null then
                            null
                          else
                            {
                              commit: {
                                oid: (
                                  if (.statusCheckRollup | type) == "object" then
                                    (.statusCheckRollup.commit.oid // .headRefOid)
                                  else
                                    .headRefOid
                                  end
                                )
                              },
                              contexts: {
                                nodes: (
                                  if (.statusCheckRollup | type) == "array" then
                                    .statusCheckRollup
                                  elif (.statusCheckRollup.contexts? | type) == "array" then
                                    .statusCheckRollup.contexts
                                  elif (.statusCheckRollup.contexts?.nodes? | type) == "array" then
                                    .statusCheckRollup.contexts.nodes
                                  else
                                    []
                                  end
                                )
                              }
                            }
                          end
                        )
                      }
                    }
                  }
                }' "${selected_pr_path}"
                exit 0
            fi

            echo "missing fake gh graphql response" >&2
            exit 1
        fi

        if [[ "${endpoint}" == "repos/{owner}/{repo}/branches/develop/protection" ]]; then
            cat "${state_dir}/protection.json"
            exit 0
        fi

        if [[ "${endpoint}" == "repos/falgon/roki-web/issues/123/comments?per_page=100"* ]]; then
            page_number="$(extract_page_number "${endpoint}")"
            page_file="${state_dir}/issue_comments_page_${page_number}.json"
            if [[ -f "${page_file}" ]]; then
                cat "${page_file}"
                exit 0
            fi

            if [[ "${page_number}" == "1" && -f "${state_dir}/issue_comments.json" ]]; then
                cat "${state_dir}/issue_comments.json"
                exit 0
            fi

            if [[ "${page_number}" != "1" ]]; then
                printf '[]\n'
                exit 0
            fi

            selected_pr_path="$(select_current_pr_path)"
            if [[ -z "${selected_pr_path}" ]]; then
                echo "missing fake gh PR for issue comments" >&2
                exit 1
            fi

            current_head_sha="$(jq -r '.headRefOid' "${selected_pr_path}")"
            current_base_sha="$(jq -r '.baseRefOid' "${selected_pr_path}")"
            jq -cn \
                --arg base_sha "${current_base_sha}" \
                --arg head_sha "${current_head_sha}" '
                    [
                        {
                            id: 1,
                            updated_at: "2026-03-30T00:10:00Z",
                            user: { login: "github-actions[bot]" },
                            body: (
                                "<!-- dependabot-claude-review-verdict -->\n\n```json\n" +
                                ({
                                    pr_number: 123,
                                    head_sha: $head_sha,
                                    base_sha: $base_sha,
                                    review_status: "pass",
                                    finding_count: 0,
                                    summary: "No findings.",
                                    workflow_run_id: 1001,
                                    workflow_run_attempt: 2
                                } | tojson) +
                                "\n```"
                            )
                        }
                    ]
                '
            exit 0
        fi

        if [[ "${endpoint}" == "repos/falgon/roki-web/pulls/123/comments?per_page=100"* ]]; then
            page_number="$(extract_page_number "${endpoint}")"
            page_file="${state_dir}/review_comments_page_${page_number}.json"
            if [[ -f "${page_file}" ]]; then
                cat "${page_file}"
                exit 0
            fi

            if [[ "${page_number}" == "1" && -f "${state_dir}/review_comments.json" ]]; then
                cat "${state_dir}/review_comments.json"
                exit 0
            fi

            printf '[]\n'
            exit 0
        fi

        if [[ "${endpoint}" == "repos/falgon/roki-web/actions/runs/"* ]]; then
            workflow_run_id="${endpoint##*/}"
            workflow_run_path="${state_dir}/workflow_run_${workflow_run_id}.json"

            if [[ -f "${workflow_run_path}" ]]; then
                cat "${workflow_run_path}"
                exit 0
            fi

            if [[ -f "${state_dir}/workflow_run.json" ]]; then
                cat "${state_dir}/workflow_run.json"
                exit 0
            fi

            selected_pr_path="$(select_current_pr_path)"
            if [[ -z "${selected_pr_path}" ]]; then
                echo "missing fake gh PR for workflow run" >&2
                exit 1
            fi

            current_head_sha="$(jq -r '.headRefOid' "${selected_pr_path}")"
            jq -cn --arg head_sha "${current_head_sha}" '
                {
                    name: "Claude Code Review for Bot PRs",
                    path: ".github/workflows/claude-code-review.yml",
                    event: "pull_request",
                    head_sha: $head_sha,
                    run_attempt: 2,
                    run_started_at: "2026-03-30T00:00:00Z",
                    completed_at: "2026-03-30T00:20:00Z",
                    updated_at: "2026-03-30T00:20:00Z",
                    created_at: "2026-03-30T00:00:00Z",
                    pull_requests: [
                        {
                            number: 123
                        }
                    ]
                }
            '
            exit 0
        fi

        echo "unexpected gh api invocation: $*" >&2
        exit 1
        ;;
    *)
        echo "unexpected gh invocation: $*" >&2
        exit 1
        ;;
esac
EOF
    chmod +x "${bin_dir}/gh"
}

run_prepare_script() {
    local case_dir="$1"

    (
        export PATH="${case_dir}/bin:${PATH}"
        export FAKE_GH_STATE_DIR="${case_dir}/state"
        export DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="${DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS:-,}"
        export GITHUB_OUTPUT="${case_dir}/github_output.txt"
        export GITHUB_REPOSITORY="falgon/roki-web"
        export PR_NUMBER=123
        export REVIEW_INPUT_ROOT="${case_dir}/review-input"
        export SKIP_CANDIDATE_VALIDATION="${SKIP_CANDIDATE_VALIDATION:-}"
        export EXPECTED_HEAD_SHA="${EXPECTED_HEAD_SHA:-}"
        export EXPECTED_BASE_SHA="${EXPECTED_BASE_SHA:-}"
        bash "${target_script}"
    )
}

run_prepare_script_expect_failure() {
    local case_dir="$1"

    set +e
    PATH="${case_dir}/bin:${PATH}" \
    FAKE_GH_STATE_DIR="${case_dir}/state" \
    DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="${DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS:-,}" \
    GITHUB_OUTPUT="${case_dir}/github_output.txt" \
    GITHUB_REPOSITORY="falgon/roki-web" \
    PR_NUMBER=123 \
    REVIEW_INPUT_ROOT="${case_dir}/review-input" \
    SKIP_CANDIDATE_VALIDATION="${SKIP_CANDIDATE_VALIDATION:-}" \
    EXPECTED_HEAD_SHA="${EXPECTED_HEAD_SHA:-}" \
    EXPECTED_BASE_SHA="${EXPECTED_BASE_SHA:-}" \
    bash "${target_script}" \
        >"${case_dir}/stdout.txt" \
        2>"${case_dir}/stderr.txt"
    local exit_code=$?
    set -e

    printf '%s\n' "${exit_code}"
}

run_prepare_script_with_default_monitored_contexts() {
    local case_dir="$1"

    (
        export PATH="${case_dir}/bin:${PATH}"
        export FAKE_GH_STATE_DIR="${case_dir}/state"
        unset DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS
        unset DEPENDABOT_AUTO_APPROVE_MONITORED_CONTEXTS
        export GITHUB_OUTPUT="${case_dir}/github_output.txt"
        export GITHUB_REPOSITORY="falgon/roki-web"
        export PR_NUMBER=123
        export REVIEW_INPUT_ROOT="${case_dir}/review-input"
        export SKIP_CANDIDATE_VALIDATION="${SKIP_CANDIDATE_VALIDATION:-}"
        export EXPECTED_HEAD_SHA="${EXPECTED_HEAD_SHA:-}"
        export EXPECTED_BASE_SHA="${EXPECTED_BASE_SHA:-}"
        bash "${target_script}"
    )
}

test_captures_review_snapshot_only_when_head_and_base_are_stable() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/pr_view_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_4.json" "head-sha-1" "base-sha-1"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/diff.patch"

    run_prepare_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "head-sha=head-sha-1"
    assert_line_equals "${case_dir}/github_output.txt" "base-sha=base-sha-1"
    assert_contains "${case_dir}/review-input/123/diff.patch" "diff --git a/package-lock.json b/package-lock.json"

    if [[ "$(jq -r '.baseRefOid' "${case_dir}/review-input/123/metadata.json")" != "base-sha-1" ]]; then
        echo "metadata.json does not contain the reviewed base SHA" >&2
        exit 1
    fi
}

test_captures_npm_snapshot_when_branch_protection_requires_claude_review_but_verdict_requirement_is_disabled() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    cat <<'EOF' > "${case_dir}/state/protection.json"
{
  "required_status_checks": {
    "checks": [],
    "contexts": ["claude-review"]
  }
}
EOF
    write_pr_json "${case_dir}/state/pr_view_1.json" "head-sha-1" "base-sha-1"
    cp "${case_dir}/state/pr_view_1.json" "${case_dir}/state/pr_view_2.json"
    cp "${case_dir}/state/pr_view_1.json" "${case_dir}/state/pr_view_3.json"
    cp "${case_dir}/state/pr_view_1.json" "${case_dir}/state/pr_view_4.json"
    cat <<'EOF' > "${case_dir}/state/status_rollup.json"
{
  "data": {
    "repository": {
      "pullRequest": {
        "headRefOid": "head-sha-1",
        "statusCheckRollup": {
          "commit": {
            "oid": "head-sha-1"
          },
          "contexts": {
            "nodes": [
              {
                "__typename": "CheckRun",
                "name": "lint-and-test",
                "status": "COMPLETED",
                "conclusion": "SUCCESS"
              },
              {
                "__typename": "CheckRun",
                "name": "claude-review",
                "status": "COMPLETED",
                "conclusion": "FAILURE"
              }
            ]
          }
        }
      }
    }
  }
}
EOF
    cat <<'EOF' > "${case_dir}/state/diff.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+new
EOF

    (
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="claude-review" \
        DEPENDABOT_AUTO_APPROVE_REQUIRE_CURRENT_HEAD_CLAUDE_REVIEW_VERDICT=false \
        run_prepare_script "${case_dir}"
    )

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_contains "${case_dir}/review-input/123/diff.patch" "diff --git a/package-lock.json b/package-lock.json"
}

test_captures_non_npm_snapshot_when_default_monitored_contexts_do_not_wait_for_claude_review_status() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/pr_view_1.json" \
        "head-sha-1" \
        "base-sha-1" \
        "docker/Dockerfile"
    cp "${case_dir}/state/pr_view_1.json" "${case_dir}/state/pr_view_2.json"
    cp "${case_dir}/state/pr_view_1.json" "${case_dir}/state/pr_view_3.json"
    cp "${case_dir}/state/pr_view_1.json" "${case_dir}/state/pr_view_4.json"
    cat <<'EOF' > "${case_dir}/state/diff.patch"
diff --git a/docker/Dockerfile b/docker/Dockerfile
--- a/docker/Dockerfile
+++ b/docker/Dockerfile
@@ -1,4 +1,4 @@
-FROM node:20.18.1
+FROM node:20.18.2
EOF

    run_prepare_script_with_default_monitored_contexts "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "head-sha=head-sha-1"
    assert_line_equals "${case_dir}/github_output.txt" "base-sha=base-sha-1"
    assert_contains "${case_dir}/review-input/123/diff.patch" "diff --git a/docker/Dockerfile b/docker/Dockerfile"
}

test_skips_candidate_when_claude_verdict_comment_missing() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/pr_view_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_4.json" "head-sha-1" "base-sha-1"
    printf '[]\n' > "${case_dir}/state/issue_comments.json"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/diff.patch"

    run_prepare_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "machine-readable な Claude review verdict comment がまだありません"
    assert_file_absent "${case_dir}/review-input/123/diff.patch"
}

test_accepts_when_base_moves_but_patch_stays_same() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/pr_view_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_3.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/pr_view_4.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/pr_view_5.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/pr_view_6.json" "head-sha-1" "base-sha-2"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/diff.patch"

    run_prepare_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "head-sha=head-sha-1"
    assert_line_equals "${case_dir}/github_output.txt" "base-sha=base-sha-2"
    assert_contains "${case_dir}/review-input/123/diff.patch" "diff --git a/package-lock.json b/package-lock.json"
}

test_accepts_when_same_snapshot_only_changes_patch_metadata() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/pr_view_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_4.json" "head-sha-1" "base-sha-1"
    cat <<'EOF' > "${case_dir}/state/diff_1.patch"
diff --git a/package-lock.json b/package-lock.json
index 1111111..2222222 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -10,7 +10,7 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/diff_2.patch"
diff --git a/package-lock.json b/package-lock.json
index aaaabbb..cccdddd 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -27,7 +27,7 @@
-old
+new
EOF

    run_prepare_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "head-sha=head-sha-1"
    assert_line_equals "${case_dir}/github_output.txt" "base-sha=base-sha-1"
    assert_contains "${case_dir}/review-input/123/diff.patch" "@@ -27,7 +27,7 @@"
}

test_retries_when_base_changes_after_second_diff_before_final_view() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/pr_view_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_4.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/pr_view_5.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/pr_view_6.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/pr_view_7.json" "head-sha-1" "base-sha-2"
    cat <<'EOF' > "${case_dir}/state/diff_1.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+older
EOF
    cat <<'EOF' > "${case_dir}/state/diff_2.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+older
EOF
    cat <<'EOF' > "${case_dir}/state/diff_3.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-new
+newer
EOF
    cat <<'EOF' > "${case_dir}/state/diff_4.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-new
+newer
EOF

    run_prepare_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "base-sha=base-sha-2"
    assert_contains "${case_dir}/review-input/123/diff.patch" "+newer"
}

test_skips_when_patch_keeps_changing_while_building_review_snapshot() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/pr_view_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_3.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/pr_view_4.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/pr_view_5.json" "head-sha-1" "base-sha-3"
    write_pr_json "${case_dir}/state/pr_view_6.json" "head-sha-1" "base-sha-3"
    write_pr_json "${case_dir}/state/pr_view_7.json" "head-sha-1" "base-sha-4"
    write_pr_json "${case_dir}/state/pr_view_8.json" "head-sha-1" "base-sha-4"
    cat <<'EOF' > "${case_dir}/state/diff_1.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-first
+second
EOF
    cat <<'EOF' > "${case_dir}/state/diff_2.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-second
+third
EOF
    cat <<'EOF' > "${case_dir}/state/diff_3.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-third
+fourth
EOF
    cat <<'EOF' > "${case_dir}/state/diff_4.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-fourth
+fifth
EOF
    cat <<'EOF' > "${case_dir}/state/diff_5.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-fifth
+sixth
EOF
    cat <<'EOF' > "${case_dir}/state/diff_6.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-sixth
+seventh
EOF

    run_prepare_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "HEAD、BASE、または patch が連続して更新"
    assert_line_equals "${case_dir}/github_output.txt" "base-sha=base-sha-4"
    assert_file_absent "${case_dir}/review-input/123/diff.patch"
}

test_fails_closed_when_diff_capture_errors_during_review_snapshot() {
    local case_dir
    local exit_code
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/pr_view_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/pr_view_2.json" "head-sha-1" "base-sha-1"
    printf '%s\n' '1' > "${case_dir}/state/diff_1.exit_code"
    printf '%s\n' 'gh: API rate limit exceeded' > "${case_dir}/state/diff_1.stderr"

    exit_code="$(run_prepare_script_expect_failure "${case_dir}")"

    assert_exit_code "${exit_code}" "2"
    assert_contains "${case_dir}/stderr.txt" "failed to capture the first review patch for PR #123"
    assert_file_absent "${case_dir}/review-input/123/diff.patch"
    assert_file_absent "${case_dir}/github_output.txt"
}

test_accepts_latest_green_check_after_rerun() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/pr_view_1.json"
{
  "number": 123,
  "title": "Bump example dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/npm_and_yarn/example-1.2.3",
  "headRefOid": "head-sha-1",
  "mergeable": "MERGEABLE",
  "reviewDecision": null,
  "labels": [
    { "name": "dependabot/npm" },
    { "name": "automerge" }
  ],
  "statusCheckRollup": [
    {
      "__typename": "CheckRun",
      "name": "lint-and-test",
      "status": "COMPLETED",
      "conclusion": "FAILURE",
      "completedAt": "2026-03-29T09:00:00Z"
    },
    {
      "__typename": "CheckRun",
      "name": "lint-and-test",
      "status": "COMPLETED",
      "conclusion": "SUCCESS",
      "completedAt": "2026-03-29T09:05:00Z"
    }
  ],
  "files": [
    { "path": "package-lock.json" }
  ]
}
EOF
    cp "${case_dir}/state/pr_view_1.json" "${case_dir}/state/pr_view_2.json"
    cp "${case_dir}/state/pr_view_1.json" "${case_dir}/state/pr_view_3.json"
    cp "${case_dir}/state/pr_view_1.json" "${case_dir}/state/pr_view_4.json"
    cat <<'EOF' > "${case_dir}/state/diff.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+new
EOF

    run_prepare_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    if [[ "$(jq '.statusCheckRollup.contexts.nodes | length' "${case_dir}/review-input/123/metadata.json")" != "1" ]]; then
        echo "metadata.json should only retain the latest status node per check name" >&2
        cat "${case_dir}/review-input/123/metadata.json" >&2
        exit 1
    fi
    if [[ "$(jq -r '.statusCheckRollup.contexts.nodes[0].conclusion' "${case_dir}/review-input/123/metadata.json")" != "SUCCESS" ]]; then
        echo "metadata.json should retain the rerun SUCCESS node" >&2
        cat "${case_dir}/review-input/123/metadata.json" >&2
        exit 1
    fi
}

test_skips_when_monitored_workflow_checks_have_not_reported_yet() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/pr_view_1.json" "head-sha-1" "base-sha-1"

    (
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="build,lint-and-test,Analyze (javascript),restyled,claude-review" \
        run_prepare_script "${case_dir}"
    )

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "必須または監視対象チェックがまだ出揃っていません"
    assert_contains "${case_dir}/github_output.txt" "build"
    assert_contains "${case_dir}/github_output.txt" "Analyze (javascript)"
    assert_contains "${case_dir}/github_output.txt" "restyled"
    assert_contains "${case_dir}/github_output.txt" "claude-review"
    assert_not_contains "${case_dir}/github_output.txt" "upload"
    assert_not_contains "${case_dir}/github_output.txt" "boot-circle-ci"
    assert_file_absent "${case_dir}/review-input/123/diff.patch"
}

test_skips_when_claude_review_has_not_reported_on_current_head() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/pr_view_1.json" "head-sha-1" "base-sha-1"
    cat <<'EOF' > "${case_dir}/state/diff.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+new
EOF

    (
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="claude-review" \
        run_prepare_script "${case_dir}"
    )

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "必須または監視対象チェックがまだ出揃っていません"
    assert_contains "${case_dir}/github_output.txt" "claude-review"
    assert_file_absent "${case_dir}/review-input/123/diff.patch"
}

test_treats_matrix_claude_review_check_name_as_monitored_context() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/pr_view_1.json"
{
  "number": 123,
  "title": "Bump example dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/npm_and_yarn/example-1.2.3",
  "headRefOid": "head-sha-1",
  "mergeable": "MERGEABLE",
  "reviewDecision": null,
  "labels": [
    { "name": "dependabot/npm" },
    { "name": "automerge" }
  ],
  "statusCheckRollup": [
    {
      "__typename": "CheckRun",
      "name": "lint-and-test",
      "status": "COMPLETED",
      "conclusion": "SUCCESS",
      "completedAt": "2026-03-29T09:00:00Z"
    },
    {
      "__typename": "CheckRun",
      "name": "claude-review (123)",
      "status": "IN_PROGRESS",
      "conclusion": null,
      "startedAt": "2026-03-29T09:05:00Z"
    }
  ],
  "files": [
    { "path": "package-lock.json" }
  ]
}
EOF

    (
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="claude-review" \
        run_prepare_script "${case_dir}"
    )

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "監視対象チェックが未成功です"
    assert_contains "${case_dir}/github_output.txt" "claude-review:IN_PROGRESS"
    assert_not_contains "${case_dir}/github_output.txt" "まだ出揃っていません"
    assert_file_absent "${case_dir}/review-input/123/diff.patch"
}

test_skips_when_latest_check_run_is_a_queued_rerun() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/pr_view_1.json"
{
  "number": 123,
  "title": "Bump example dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/npm_and_yarn/example-1.2.3",
  "headRefOid": "head-sha-1",
  "mergeable": "MERGEABLE",
  "reviewDecision": null,
  "labels": [
    { "name": "dependabot/npm" },
    { "name": "automerge" }
  ],
  "statusCheckRollup": [
    {
      "__typename": "CheckRun",
      "name": "lint-and-test",
      "status": "COMPLETED",
      "conclusion": "SUCCESS",
      "completedAt": "2026-03-29T09:00:00Z"
    },
    {
      "__typename": "CheckRun",
      "name": "lint-and-test",
      "status": "QUEUED",
      "conclusion": null
    }
  ],
  "files": [
    { "path": "package-lock.json" }
  ]
}
EOF

    run_prepare_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason=PR #123 は必須チェックが未成功です: lint-and-test:QUEUED"
    assert_file_absent "${case_dir}/review-input/123/diff.patch"
}

test_skips_when_status_rollup_still_points_to_previous_head() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/pr_view_1.json" "head-sha-2" "base-sha-1"
    cat <<'EOF' > "${case_dir}/state/status_rollup.json"
{
  "data": {
    "repository": {
      "pullRequest": {
        "headRefOid": "head-sha-2",
        "statusCheckRollup": {
          "commit": {
            "oid": "head-sha-1"
          }
        }
      }
    }
  }
}
EOF

    run_prepare_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason=PR #123 の必須チェック結果が現在の HEAD (head-sha-2) ではなく head-sha-1 に紐付いているため、自動承認しません。"
    assert_file_absent "${case_dir}/review-input/123/diff.patch"
}

test_writes_refreshed_status_rollup_to_metadata_after_graphql_refresh() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/pr_view_1.json"
{
  "number": 123,
  "title": "Bump example dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/npm_and_yarn/example-1.2.3",
  "headRefOid": "head-sha-1",
  "mergeable": "MERGEABLE",
  "reviewDecision": null,
  "labels": [
    { "name": "dependabot/npm" },
    { "name": "automerge" }
  ],
  "statusCheckRollup": {
    "commit": {
      "oid": "head-sha-1"
    },
    "contexts": [
      {
        "__typename": "CheckRun",
        "name": "lint-and-test",
        "status": "COMPLETED",
        "conclusion": "FAILURE",
        "completedAt": "2026-03-29T09:00:00Z"
      }
    ]
  },
  "files": [
    { "path": "package-lock.json" }
  ]
}
EOF
    cp "${case_dir}/state/pr_view_1.json" "${case_dir}/state/pr_view_2.json"
    cp "${case_dir}/state/pr_view_1.json" "${case_dir}/state/pr_view_3.json"
    cp "${case_dir}/state/pr_view_1.json" "${case_dir}/state/pr_view_4.json"
    cat <<'EOF' > "${case_dir}/state/status_rollup.json"
{
  "data": {
    "repository": {
      "pullRequest": {
        "headRefOid": "head-sha-1",
        "statusCheckRollup": {
          "commit": {
            "oid": "head-sha-1"
          },
          "contexts": {
            "nodes": [
              {
                "__typename": "CheckRun",
                "name": "lint-and-test",
                "status": "COMPLETED",
                "conclusion": "SUCCESS",
                "completedAt": "2026-03-29T09:05:00Z"
              }
            ]
          }
        }
      }
    }
  }
}
EOF
    cat <<'EOF' > "${case_dir}/state/diff.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+new
EOF

    run_prepare_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    if [[ "$(jq -r '.statusCheckRollup.contexts.nodes[0].conclusion' "${case_dir}/review-input/123/metadata.json")" != "SUCCESS" ]]; then
        echo "metadata.json should contain the refreshed SUCCESS status rollup" >&2
        cat "${case_dir}/review-input/123/metadata.json" >&2
        exit 1
    fi
}

test_skips_when_expected_snapshot_changes_after_evaluation() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_pr_json "${case_dir}/state/pr_view_1.json" "head-sha-1" "base-sha-2"

    (
        SKIP_CANDIDATE_VALIDATION=true \
        EXPECTED_HEAD_SHA=head-sha-1 \
        EXPECTED_BASE_SHA=base-sha-1 \
        run_prepare_script "${case_dir}"
    )

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "HEAD または BASE が評価後に更新されたため"
    assert_contains "${case_dir}/github_output.txt" "BASE (base-sha-1 -> base-sha-2)"
    assert_file_absent "${case_dir}/review-input/123/diff.patch"
}

test_captures_review_snapshot_only_when_head_and_base_are_stable
test_captures_npm_snapshot_when_branch_protection_requires_claude_review_but_verdict_requirement_is_disabled
test_captures_non_npm_snapshot_when_default_monitored_contexts_do_not_wait_for_claude_review_status
test_skips_candidate_when_claude_verdict_comment_missing
test_accepts_when_base_moves_but_patch_stays_same
test_accepts_when_same_snapshot_only_changes_patch_metadata
test_retries_when_base_changes_after_second_diff_before_final_view
test_skips_when_patch_keeps_changing_while_building_review_snapshot
test_fails_closed_when_diff_capture_errors_during_review_snapshot
test_accepts_latest_green_check_after_rerun
test_skips_when_monitored_workflow_checks_have_not_reported_yet
test_skips_when_claude_review_has_not_reported_on_current_head
test_treats_matrix_claude_review_check_name_as_monitored_context
test_skips_when_latest_check_run_is_a_queued_rerun
test_skips_when_status_rollup_still_points_to_previous_head
test_writes_refreshed_status_rollup_to_metadata_after_graphql_refresh
test_skips_when_expected_snapshot_changes_after_evaluation

echo "prepare_dependabot_review_input tests passed"
