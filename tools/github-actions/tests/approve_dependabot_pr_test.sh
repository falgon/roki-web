#!/usr/bin/env bash

set -euo pipefail

script_dir="$(
    cd "$(dirname "${BASH_SOURCE[0]}")" && pwd
)"
readonly script_dir
readonly target_script="${script_dir}/../approve_dependabot_pr.sh"
readonly bash_under_test="${BASH_UNDER_TEST:-/bin/bash}"

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

assert_line_count_equals() {
    local file_path="$1"
    local expected_count="$2"
    local actual_count

    actual_count="$(wc -l < "${file_path}")"
    actual_count="${actual_count//[[:space:]]/}"

    if [[ "${actual_count}" != "${expected_count}" ]]; then
        echo "Expected ${expected_count} lines in ${file_path}, got ${actual_count}" >&2
        echo "--- ${file_path} ---" >&2
        cat "${file_path}" >&2
        exit 1
    fi
}

write_pr_json() {
    local file_path="$1"
    local head_sha="$2"
    local base_sha="$3"
    local review_decision="${4:-null}"
    local include_automerge="${5:-true}"
    local pr_state="${6:-OPEN}"
    local mergeable_state="${7:-MERGEABLE}"
    local review_decision_json="null"
    local automerge_label=''

    if [[ "${review_decision}" != "null" ]]; then
        review_decision_json="\"${review_decision}\""
    fi

    if [[ "${include_automerge}" == "true" ]]; then
        automerge_label=$',\n    { "name": "automerge" }'
    fi

    cat <<EOF > "${file_path}"
{
  "state": "${pr_state}",
  "number": 123,
  "title": "Bump example dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "${base_sha}",
  "headRefName": "dependabot/npm_and_yarn/example-1.2.3",
  "headRefOid": "${head_sha}",
  "mergeable": "${mergeable_state}",
  "reviewDecision": ${review_decision_json},
  "labels": [
    { "name": "dependabot/npm" }${automerge_label}
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

write_npm_pr_json() {
    local file_path="$1"
    local files_json="$2"

    cat <<EOF > "${file_path}"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump npm dependency",
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
        "conclusion": "SUCCESS"
      }
    ]
  },
  "files": ${files_json}
}
EOF
}

write_compare_json() {
    local file_path="$1"
    local merge_base_sha="$2"

    cat <<EOF > "${file_path}"
{
  "merge_base_commit": {
    "sha": "${merge_base_sha}"
  }
}
EOF
}

write_npm_package_patch() {
    local file_path="$1"
    local before_version="$2"
    local after_version="$3"

    cat <<EOF > "${file_path}"
diff --git a/package.json b/package.json
--- a/package.json
+++ b/package.json
@@ -5,7 +5,7 @@
-    "vitest": "${before_version}"
+    "vitest": "${after_version}"
diff --git a/package-lock.json b/package-lock.json
--- a/package-lock.json
+++ b/package-lock.json
@@ -1,4 +1,4 @@
-      "vitest": "${before_version}"
+      "vitest": "${after_version}"
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

write_collaborator_permission_json() {
    local file_path="$1"
    local permission="${2:-write}"

    jq -cn \
        --arg permission "${permission}" '
            {
                permission: $permission
            }
        ' > "${file_path}"
}

write_github_actions_patch() {
    local file_path="$1"
    local action_name="$2"
    local before_version="$3"
    local after_version="$4"

    cat <<EOF > "${file_path}"
diff --git a/.github/workflows/manually-cleanup-artifacts.yml b/.github/workflows/manually-cleanup-artifacts.yml
--- a/.github/workflows/manually-cleanup-artifacts.yml
+++ b/.github/workflows/manually-cleanup-artifacts.yml
@@ -10,7 +10,7 @@ jobs:
-      uses: ${action_name}@${before_version}
+      uses: ${action_name}@${after_version}
EOF
}

write_dockerfile_patch() {
    local file_path="$1"
    local before_image="$2"
    local after_image="$3"

    cat <<EOF > "${file_path}"
diff --git a/docker/Dockerfile b/docker/Dockerfile
--- a/docker/Dockerfile
+++ b/docker/Dockerfile
@@ -1,4 +1,4 @@
-FROM ${before_image}
+FROM ${after_image}
EOF
}

write_dockerfile_from_line_patch() {
    local file_path="$1"
    local before_line="$2"
    local after_line="$3"

    cat <<EOF > "${file_path}"
diff --git a/docker/Dockerfile b/docker/Dockerfile
--- a/docker/Dockerfile
+++ b/docker/Dockerfile
@@ -1,4 +1,4 @@
-${before_line}
+${after_line}
EOF
}

write_github_actions_patch_with_unrelated_edit() {
    local file_path="$1"
    local action_name="$2"
    local before_version="$3"
    local after_version="$4"

    cat <<EOF > "${file_path}"
diff --git a/.github/workflows/manually-cleanup-artifacts.yml b/.github/workflows/manually-cleanup-artifacts.yml
--- a/.github/workflows/manually-cleanup-artifacts.yml
+++ b/.github/workflows/manually-cleanup-artifacts.yml
@@ -1,7 +1,7 @@
  permissions:
-  contents: read
+  contents: write
  jobs:
-      uses: ${action_name}@${before_version}
+      uses: ${action_name}@${after_version}
EOF
}

write_review_response() {
    local file_path="$1"
    local review_id="$2"

    cat <<EOF > "${file_path}"
{
  "id": ${review_id}
}
EOF
}

write_review_details_json() {
    local file_path="$1"
    local review_id="$2"
    local review_state="${3:-APPROVED}"
    local commit_sha="${4:-head-sha-1}"

    cat <<EOF > "${file_path}"
{
  "id": ${review_id},
  "state": "${review_state}",
  "commit_id": "${commit_sha}"
}
EOF
}

write_review_error() {
    local file_path="$1"

    cat <<'EOF' > "${file_path}"
gh: Validation Failed (HTTP 422)
{"message":"Validation Failed","errors":[{"resource":"PullRequestReview","field":"commit_id","code":"invalid"}]}
EOF
}

write_claude_review_verdict_comments_json() {
    local file_path="$1"
    local head_sha="$2"
    local review_status="${3:-pass}"
    local finding_count="${4:-0}"
    local summary="${5:-No findings.}"
    local workflow_run_id="${6:-1001}"
    local workflow_run_attempt="${7:-2}"
    local updated_at="${8:-2026-03-30T00:10:00Z}"
    local comment_author_login="${9:-github-actions[bot]}"
    local base_sha="${10:-base-sha-1}"

    jq -cn \
        --arg head_sha "${head_sha}" \
        --arg base_sha "${base_sha}" \
        --arg review_status "${review_status}" \
        --argjson finding_count "${finding_count}" \
        --arg summary "${summary}" \
        --argjson workflow_run_id "${workflow_run_id}" \
        --argjson workflow_run_attempt "${workflow_run_attempt}" \
        --arg updated_at "${updated_at}" \
        --arg comment_author_login "${comment_author_login}" '
            [
                {
                    id: 1,
                    updated_at: $updated_at,
                    user: { login: $comment_author_login },
                    body: (
                        "<!-- dependabot-claude-review-verdict -->\n\n```json\n" +
                        ({
                            pr_number: 123,
                            head_sha: $head_sha,
                            base_sha: $base_sha,
                            review_status: $review_status,
                            finding_count: $finding_count,
                            summary: $summary,
                            workflow_run_id: $workflow_run_id,
                            workflow_run_attempt: $workflow_run_attempt
                        } | tojson) +
                        "\n```"
                    )
                }
            ]
        ' > "${file_path}"
}

write_claude_review_inline_comments_json() {
    local file_path="$1"
    local head_sha="$2"
    local body="${3:-【Claude review】 Blocking finding}"
    local created_at="${4:-2026-03-30T00:05:00Z}"
    local comment_author_login="${5:-github-actions[bot]}"

    jq -cn \
        --arg head_sha "${head_sha}" \
        --arg body "${body}" \
        --arg created_at "${created_at}" \
        --arg comment_author_login "${comment_author_login}" '
            [
                {
                    id: 1,
                    user: { login: $comment_author_login },
                    commit_id: $head_sha,
                    created_at: $created_at,
                    body: $body
                }
            ]
        ' > "${file_path}"
}

write_workflow_run_json() {
    local file_path="$1"
    local run_attempt="${2:-2}"
    local run_started_at="${3:-2026-03-30T00:00:00Z}"
    local completed_at="${4:-2026-03-30T00:20:00Z}"
    local head_sha="${5:-head-sha-1}"
    local event="${6:-pull_request}"
    local include_pull_requests="${7:-true}"

    jq -cn \
        --argjson run_attempt "${run_attempt}" \
        --arg run_started_at "${run_started_at}" \
        --arg completed_at "${completed_at}" \
        --arg head_sha "${head_sha}" \
        --arg event "${event}" \
        --arg include_pull_requests "${include_pull_requests}" '
            {
                name: "Claude Code Review for Bot PRs",
                path: ".github/workflows/claude-code-review.yml",
                event: $event,
                head_sha: $head_sha,
                run_attempt: $run_attempt,
                run_started_at: $run_started_at,
                completed_at: $completed_at,
                updated_at: $completed_at,
                created_at: $run_started_at,
                pull_requests: (
                    if $include_pull_requests == "true" then
                        [
                            {
                                number: 123
                            }
                        ]
                    else
                        []
                    end
                )
            }
        ' > "${file_path}"
}

mark_review_approved_after_creation() {
    local case_dir="$1"

    : > "${case_dir}/state/mark_review_approved_after_creation"
}

setup_fake_gh() {
    local bin_dir="$1"

    mkdir -p "${bin_dir}"
    cat <<'EOF' > "${bin_dir}/gh"
#!/usr/bin/env bash

set -euo pipefail

readonly state_dir="${FAKE_GH_STATE_DIR:?}"

ensure_expected_general_token() {
    if [[ ! -f "${state_dir}/expected_general_token.txt" ]]; then
        return
    fi

    expected_general_token="$(<"${state_dir}/expected_general_token.txt")"
    if [[ "${GH_TOKEN:-}" != "${expected_general_token}" ]]; then
        echo "missing expected GH_TOKEN for generic request" >&2
        exit 1
    fi

    printf 'generic-gh-token=%s\n' "${GH_TOKEN:-}" >> "${state_dir}/api_args.txt"
}

select_current_pr_path() {
    if [[ -f "${state_dir}/last_pr_view_path" ]]; then
        cat "${state_dir}/last_pr_view_path"
        return
    fi

    if [[ -f "${state_dir}/current_pr.json" ]]; then
        printf '%s\n' "${state_dir}/current_pr.json"
        return
    fi

    ls "${state_dir}"/current_pr_*.json 2>/dev/null | sort | tail -n 1 || true
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
                ensure_expected_general_token
                view_count_file="${state_dir}/pr_view_count"
                view_count=0

                if [[ -f "${view_count_file}" ]]; then
                    view_count="$(<"${view_count_file}")"
                fi

                view_count=$((view_count + 1))
                printf '%s\n' "${view_count}" > "${view_count_file}"
                exit_code_file="${state_dir}/current_pr_${view_count}.exit_code"
                stderr_file="${state_dir}/current_pr_${view_count}.stderr"
                if [[ -f "${exit_code_file}" ]]; then
                    if [[ -f "${stderr_file}" ]]; then
                        cat "${stderr_file}" >&2
                    fi
                    exit "$(<"${exit_code_file}")"
                fi

                response_file="${state_dir}/current_pr_${view_count}.json"
                selected_response=""
                if [[ -f "${response_file}" ]]; then
                    selected_response="${response_file}"
                elif [[ -f "${state_dir}/current_pr.json" ]]; then
                    selected_response="${state_dir}/current_pr.json"
                else
                    latest_response="$(
                        ls "${state_dir}"/current_pr_*.json 2>/dev/null | sort | tail -n 1 || true
                    )"
                    if [[ -n "${latest_response}" ]]; then
                        selected_response="${latest_response}"
                    else
                        echo "missing fake gh pr view response: ${response_file}" >&2
                        exit 1
                    fi
                fi

                if [[ -f "${state_dir}/mark_review_approved_after_creation" && -f "${state_dir}/successful_review_count" ]]; then
                    printf '%s\n' "${selected_response}" > "${state_dir}/last_pr_view_path"
                    jq '.reviewDecision = "APPROVED"' "${selected_response}"
                else
                    printf '%s\n' "${selected_response}" > "${state_dir}/last_pr_view_path"
                    cat "${selected_response}"
                fi
                exit 0
                ;;
            diff)
                ensure_expected_general_token
                diff_count_file="${state_dir}/pr_diff_count"
                diff_count=0

                if [[ -f "${diff_count_file}" ]]; then
                    diff_count="$(<"${diff_count_file}")"
                fi

                diff_count=$((diff_count + 1))
                printf '%s\n' "${diff_count}" > "${diff_count_file}"
                exit_code_file="${state_dir}/current_diff_${diff_count}.exit_code"
                stderr_file="${state_dir}/current_diff_${diff_count}.stderr"
                if [[ -f "${exit_code_file}" ]]; then
                    if [[ -f "${stderr_file}" ]]; then
                        cat "${stderr_file}" >&2
                    fi
                    exit "$(<"${exit_code_file}")"
                fi

                response_file="${state_dir}/current_diff_${diff_count}.patch"
                if [[ -f "${response_file}" ]]; then
                    cat "${response_file}"
                elif [[ -f "${state_dir}/current_diff.patch" ]]; then
                    cat "${state_dir}/current_diff.patch"
                else
                    latest_response="$(
                        ls "${state_dir}"/current_diff_*.patch 2>/dev/null | sort | tail -n 1 || true
                    )"
                    if [[ -n "${latest_response}" ]]; then
                        cat "${latest_response}"
                    else
                        echo "missing fake gh pr diff response: ${response_file}" >&2
                        exit 1
                    fi
                fi
                exit 0
                ;;
        esac
        ;;
    api)
        printf '%s\n' "$*" >> "${state_dir}/api_args.txt"
        endpoint=""
        skip_next_arg="false"
        for api_arg in "$@"; do
            if [[ "${skip_next_arg}" == "true" ]]; then
                skip_next_arg="false"
                continue
            fi

            case "${api_arg}" in
                api)
                    ;;
                -H | --method | -f | -F)
                    skip_next_arg="true"
                    ;;
                *)
                    endpoint="${api_arg}"
                    break
                    ;;
            esac
        done
        if [[ "${2:-}" == "repos/falgon/roki-web/compare/develop...head-sha-1" ]]; then
            ensure_expected_general_token
            cat "${state_dir}/compare.json"
            exit 0
        fi
        if [[ "${2:-}" == "repos/falgon/roki-web/contents/package.json?ref="* ]]; then
            ensure_expected_general_token
            ref="${2##*ref=}"
            cat "${state_dir}/package_json_${ref}.json"
            exit 0
        fi
        if [[ "${2:-}" == "graphql" ]]; then
            ensure_expected_general_token
            graphql_count_file="${state_dir}/graphql_count"
            graphql_count=0

            if [[ -f "${graphql_count_file}" ]]; then
                graphql_count="$(<"${graphql_count_file}")"
            fi

            graphql_count=$((graphql_count + 1))
            printf '%s\n' "${graphql_count}" > "${graphql_count_file}"
            response_file="${state_dir}/status_rollup_${graphql_count}.json"

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
        if [[ "${2:-}" == "repos/{owner}/{repo}/branches/develop/protection" ]]; then
            if [[ -f "${state_dir}/expected_protection_token.txt" ]]; then
                expected_protection_token="$(<"${state_dir}/expected_protection_token.txt")"
                if [[ "${GH_TOKEN:-}" != "${expected_protection_token}" ]]; then
                    echo "missing expected GH_TOKEN for protection request" >&2
                    exit 1
                fi
            fi
            printf 'protection-gh-token=%s\n' "${GH_TOKEN:-}" >> "${state_dir}/api_args.txt"
            cat "${state_dir}/protection.json"
            exit 0
        fi
        if [[ "$*" == *"repos/falgon/roki-web/issues/123/comments?per_page=100"* ]]; then
            ensure_expected_general_token
            page_number="$(extract_page_number "$*")"
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
            current_base_sha="$(jq -r '.baseRefOid // "base-sha-1"' "${selected_pr_path}")"
            jq -cn \
                --arg head_sha "${current_head_sha}" \
                --arg base_sha "${current_base_sha}" '
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
        if [[ "${endpoint}" == "repos/falgon/roki-web/collaborators/"*"/permission" ]]; then
            ensure_expected_general_token
            collaborator_login="${endpoint#repos/falgon/roki-web/collaborators/}"
            collaborator_login="${collaborator_login%/permission}"
            collaborator_login="${collaborator_login//%5B/[}"
            collaborator_login="${collaborator_login//%5D/]}"
            collaborator_login="${collaborator_login//%2F/\/}"
            permission_path="${state_dir}/collaborator_permission_${collaborator_login}.json"

            if [[ -f "${permission_path}" ]]; then
                cat "${permission_path}"
                exit 0
            fi

            if [[ "${collaborator_login}" == "github-actions[bot]" ]]; then
                jq -cn '{ permission: "write" }'
                exit 0
            fi

            echo "missing fake collaborator permission for ${collaborator_login}" >&2
            exit 1
        fi
        if [[ "$*" =~ repos/falgon/roki-web/actions/runs/([0-9]+) ]]; then
            ensure_expected_general_token
            workflow_run_id="${BASH_REMATCH[1]}"
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
            current_head_sha="head-sha-1"
            if [[ -n "${selected_pr_path}" && -f "${selected_pr_path}" ]]; then
                current_head_sha="$(jq -r '.headRefOid // "head-sha-1"' "${selected_pr_path}")"
            fi

            jq -cn \
                --arg head_sha "${current_head_sha}" '
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
        if [[ "$*" == *"repos/falgon/roki-web/pulls/123/comments?per_page=100"* ]]; then
            ensure_expected_general_token
            page_number="$(extract_page_number "$*")"
            page_file="${state_dir}/review_comments_page_${page_number}.json"
            if [[ -f "${page_file}" ]]; then
                cat "${page_file}"
                exit 0
            fi

            if [[ "${page_number}" == "1" && -f "${state_dir}/review_comments.json" ]]; then
                cat "${state_dir}/review_comments.json"
                exit 0
            fi

            if [[ "${page_number}" != "1" ]]; then
                printf '[]\n'
                exit 0
            fi

            printf '[]\n'
            exit 0
        fi
        if [[ "$*" == *"repos/falgon/roki-web/pulls/123"* && "$*" != *"/reviews"* ]]; then
            ensure_expected_general_token
            pull_head_count_file="${state_dir}/pull_head_count"
            pull_head_count=0

            if [[ -f "${pull_head_count_file}" ]]; then
                pull_head_count="$(<"${pull_head_count_file}")"
            fi

            pull_head_count=$((pull_head_count + 1))
            printf '%s\n' "${pull_head_count}" > "${pull_head_count_file}"
            response_file="${state_dir}/pull_head_${pull_head_count}.json"

            if [[ -f "${response_file}" ]]; then
                cat "${response_file}"
                exit 0
            fi

            if [[ -f "${state_dir}/pull_head.json" ]]; then
                cat "${state_dir}/pull_head.json"
                exit 0
            fi

            if [[ -f "${state_dir}/last_pr_view_path" ]]; then
                selected_pr_path="$(<"${state_dir}/last_pr_view_path")"
            elif [[ -f "${state_dir}/current_pr.json" ]]; then
                selected_pr_path="${state_dir}/current_pr.json"
            else
                selected_pr_path="$(
                    ls "${state_dir}"/current_pr_*.json 2>/dev/null | sort | tail -n 1 || true
                )"
            fi

            if [[ -z "${selected_pr_path}" ]]; then
                echo "missing fake gh pull head response" >&2
                exit 1
            fi

            jq '
                {
                    head: {
                        sha: .headRefOid
                    },
                    state: (
                        if (.state // "OPEN") == "MERGED" then
                            "closed"
                        elif (.state // "OPEN") == "CLOSED" then
                            "closed"
                        else
                            "open"
                        end
                    ),
                    merged: ((.state // "OPEN") == "MERGED")
                }
            ' "${selected_pr_path}"
            exit 0
        fi
        if [[ "$*" == *"/dismissals"* ]]; then
            dismiss_review_count_file="${state_dir}/dismiss_review_count"
            dismiss_review_count=0

            if [[ -f "${dismiss_review_count_file}" ]]; then
                dismiss_review_count="$(<"${dismiss_review_count_file}")"
            fi

            dismiss_review_count=$((dismiss_review_count + 1))
            printf '%s\n' "${dismiss_review_count}" > "${dismiss_review_count_file}"
            printf 'dismissal-gh-token=%s\n' "${GH_TOKEN:-}" >> "${state_dir}/api_args.txt"
            exit_code_file="${state_dir}/dismiss_review_${dismiss_review_count}.exit_code"
            stderr_file="${state_dir}/dismiss_review_${dismiss_review_count}.stderr"

            if [[ -f "${exit_code_file}" ]]; then
                if [[ -f "${stderr_file}" ]]; then
                    cat "${stderr_file}" >&2
                fi
                exit "$(<"${exit_code_file}")"
            fi

            if [[ -f "${stderr_file}" ]]; then
                cat "${stderr_file}" >&2
                exit 1
            fi

            printf '{}\n'
            exit 0
        fi
        if [[ "$*" == *"/reviews/"* && "$*" != *"/dismissals"* ]]; then
            ensure_expected_general_token
            review_status_count_file="${state_dir}/review_status_count"
            review_status_count=0

            if [[ -f "${review_status_count_file}" ]]; then
                review_status_count="$(<"${review_status_count_file}")"
            fi

            review_status_count=$((review_status_count + 1))
            printf '%s\n' "${review_status_count}" > "${review_status_count_file}"
            response_file="${state_dir}/review_status_${review_status_count}.json"

            if [[ -f "${response_file}" ]]; then
                cat "${response_file}"
                exit 0
            fi

            if [[ -f "${state_dir}/review_status.json" ]]; then
                cat "${state_dir}/review_status.json"
                exit 0
            fi

            echo "missing fake gh review response: ${response_file}" >&2
            exit 1
        fi
        if [[ "$*" == *"--method POST"* && "$*" == *"/reviews"* ]]; then
            printf 'create-review-gh-token=%s\n' "${GH_TOKEN:-}" >> "${state_dir}/api_args.txt"
            create_review_count_file="${state_dir}/create_review_count"
            create_review_count=0

            if [[ -f "${create_review_count_file}" ]]; then
                create_review_count="$(<"${create_review_count_file}")"
            fi

            create_review_count=$((create_review_count + 1))
            printf '%s\n' "${create_review_count}" > "${create_review_count_file}"
            exit_code_file="${state_dir}/create_review_${create_review_count}.exit_code"
            stderr_file="${state_dir}/create_review_${create_review_count}.stderr"
            response_file="${state_dir}/create_review_${create_review_count}.json"

            if [[ -f "${exit_code_file}" ]]; then
                if [[ -f "${stderr_file}" ]]; then
                    cat "${stderr_file}" >&2
                fi
                exit "$(<"${exit_code_file}")"
            fi

            if [[ -f "${stderr_file}" ]]; then
                cat "${stderr_file}" >&2
                exit 1
            fi

            if [[ -f "${response_file}" ]]; then
                successful_review_count_file="${state_dir}/successful_review_count"
                successful_review_count=0
                if [[ -f "${successful_review_count_file}" ]]; then
                    successful_review_count="$(<"${successful_review_count_file}")"
                fi
                successful_review_count=$((successful_review_count + 1))
                printf '%s\n' "${successful_review_count}" > "${successful_review_count_file}"
                cat "${response_file}"
                exit 0
            fi

            if [[ -f "${state_dir}/create_review_error.txt" ]]; then
                cat "${state_dir}/create_review_error.txt" >&2
                exit 1
            fi
            if [[ -f "${state_dir}/create_review_response.json" ]]; then
                successful_review_count_file="${state_dir}/successful_review_count"
                successful_review_count=0
                if [[ -f "${successful_review_count_file}" ]]; then
                    successful_review_count="$(<"${successful_review_count_file}")"
                fi
                successful_review_count=$((successful_review_count + 1))
                printf '%s\n' "${successful_review_count}" > "${successful_review_count_file}"
                cat "${state_dir}/create_review_response.json"
            else
                successful_review_count_file="${state_dir}/successful_review_count"
                successful_review_count=0
                if [[ -f "${successful_review_count_file}" ]]; then
                    successful_review_count="$(<"${successful_review_count_file}")"
                fi
                successful_review_count=$((successful_review_count + 1))
                printf '%s\n' "${successful_review_count}" > "${successful_review_count_file}"
                printf '{"id":1}\n'
            fi
        else
            printf '{}\n'
        fi
        exit 0
        ;;
esac

echo "unexpected gh invocation: $*" >&2
exit 1
EOF
    chmod +x "${bin_dir}/gh"

    cat <<'EOF' > "${bin_dir}/sleep"
#!/usr/bin/env bash

set -euo pipefail

readonly state_dir="${FAKE_GH_STATE_DIR:?}"

printf '%s\n' "${1:-}" >> "${state_dir}/sleep_args.txt"
EOF
    chmod +x "${bin_dir}/sleep"
}

prepare_case_workflow_allowlist() {
    local case_dir="$1"
    local allowlist_path="${case_dir}/workflow_allowlist.txt"
    local pr_metadata_paths=()
    local workflow_paths=""

    shopt -s nullglob
    pr_metadata_paths=(
        "${case_dir}/state"/current_pr*.json
        "${case_dir}/state"/pr_view_*.json
    )
    shopt -u nullglob

    if [[ "${#pr_metadata_paths[@]}" -eq 0 ]]; then
        rm -f "${allowlist_path}"
        return
    fi

    workflow_paths="$(
        jq -r '.files[]?.path // empty' "${pr_metadata_paths[@]}" 2>/dev/null \
            | grep -Fx '.github/workflows/manually-cleanup-artifacts.yml' \
            | sort -u || true
    )"

    if [[ -z "${workflow_paths}" ]]; then
        rm -f "${allowlist_path}"
        return
    fi

    printf '%s\n' "${workflow_paths}" > "${allowlist_path}"
}

run_approve_script() {
    local case_dir="$1"
    local reviewed_head_sha="$2"
    local reviewed_base_sha="${3:-}"
    local require_current_head_claude_review_verdict="${4:-${DEPENDABOT_AUTO_APPROVE_REQUIRE_CURRENT_HEAD_CLAUDE_REVIEW_VERDICT:-}}"

    prepare_case_workflow_allowlist "${case_dir}"

    (
        export PATH="${case_dir}/bin:${PATH}"
        export FAKE_GH_STATE_DIR="${case_dir}/state"
        export DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="${DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS:-,}"
        export DEPENDABOT_AUTO_APPROVE_REQUIRE_CURRENT_HEAD_CLAUDE_REVIEW_VERDICT="${require_current_head_claude_review_verdict}"
        export GITHUB_OUTPUT="${case_dir}/github_output.txt"
        export GITHUB_REPOSITORY="falgon/roki-web"
        export PR_NUMBER=123
        export REVIEWED_HEAD_SHA="${reviewed_head_sha}"
        export REVIEWED_BASE_SHA="${reviewed_base_sha}"
        export REVIEWED_DIFF_PATH="${case_dir}/reviewed.diff.patch"
        export REVIEW_BODY="Approved by test"
        export REVIEW_DISMISSAL_FALLBACK_TOKEN="${REVIEW_DISMISSAL_FALLBACK_TOKEN:-}"
        export SKIP_CANDIDATE_VALIDATION="${SKIP_CANDIDATE_VALIDATION:-}"
        export DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND="${DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND:-}"
        export DEPENDABOT_AUTO_APPROVE_READ_BRANCH_PROTECTION_TOKEN="${DEPENDABOT_AUTO_APPROVE_READ_BRANCH_PROTECTION_TOKEN:-}"
        if [[ -f "${case_dir}/workflow_allowlist.txt" ]]; then
            export DEPENDABOT_AUTO_APPROVE_NON_NPM_WORKFLOW_ALLOWLIST_PATH="${case_dir}/workflow_allowlist.txt"
        fi
        "${bash_under_test}" "${target_script}"
    )
}

run_approve_script_expect_failure() {
    local case_dir="$1"
    local reviewed_head_sha="$2"
    local reviewed_base_sha="${3:-}"
    local require_current_head_claude_review_verdict="${4:-${DEPENDABOT_AUTO_APPROVE_REQUIRE_CURRENT_HEAD_CLAUDE_REVIEW_VERDICT:-}}"

    set +e
    prepare_case_workflow_allowlist "${case_dir}"
    (
        export PATH="${case_dir}/bin:${PATH}"
        export FAKE_GH_STATE_DIR="${case_dir}/state"
        export DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="${DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS:-,}"
        export DEPENDABOT_AUTO_APPROVE_REQUIRE_CURRENT_HEAD_CLAUDE_REVIEW_VERDICT="${require_current_head_claude_review_verdict}"
        export GITHUB_OUTPUT="${case_dir}/github_output.txt"
        export GITHUB_REPOSITORY="falgon/roki-web"
        export PR_NUMBER=123
        export REVIEWED_HEAD_SHA="${reviewed_head_sha}"
        export REVIEWED_BASE_SHA="${reviewed_base_sha}"
        export REVIEWED_DIFF_PATH="${case_dir}/reviewed.diff.patch"
        export REVIEW_BODY="Approved by test"
        export REVIEW_DISMISSAL_FALLBACK_TOKEN="${REVIEW_DISMISSAL_FALLBACK_TOKEN:-}"
        export SKIP_CANDIDATE_VALIDATION="${SKIP_CANDIDATE_VALIDATION:-}"
        export DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND="${DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND:-}"
        export DEPENDABOT_AUTO_APPROVE_READ_BRANCH_PROTECTION_TOKEN="${DEPENDABOT_AUTO_APPROVE_READ_BRANCH_PROTECTION_TOKEN:-}"
        if [[ -f "${case_dir}/workflow_allowlist.txt" ]]; then
            export DEPENDABOT_AUTO_APPROVE_NON_NPM_WORKFLOW_ALLOWLIST_PATH="${case_dir}/workflow_allowlist.txt"
        fi
        "${bash_under_test}" "${target_script}" \
            >"${case_dir}/stdout.txt" \
            2>"${case_dir}/stderr.txt"
    )
    local exit_code=$?
    set -e

    printf '%s\n' "${exit_code}"
}

test_approves_when_reviewed_head_matches_current_pr() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 1
    mark_review_approved_after_creation "${case_dir}"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-1"
}

test_approves_when_base_has_changed_but_patch_is_identical() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-2"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 2
    mark_review_approved_after_creation "${case_dir}"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_when_base_has_changed_but_patch_is_identical_with_reviewed_base_sha() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-2"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"

    run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "差分前提がレビュー後に更新されました"
    assert_contains "${case_dir}/github_output.txt" "BASE (reviewed: base-sha-1, current: base-sha-2)"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_approves_when_base_only_change_moves_hunk_headers_without_reviewed_base_sha() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-2"
    cat <<'EOF' > "${case_dir}/reviewed.diff.patch"
diff --git a/package-lock.json b/package-lock.json
index 1111111..2222222 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -10,7 +10,7 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff.patch"
diff --git a/package-lock.json b/package-lock.json
index aaaabbb..cccdddd 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -27,7 +27,7 @@
-old
+new
EOF
    write_review_response "${case_dir}/state/create_review_response.json" 14
    mark_review_approved_after_creation "${case_dir}"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-1"
}

test_skips_when_base_only_change_moves_hunk_headers_with_reviewed_base_sha() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-2"
    cat <<'EOF' > "${case_dir}/reviewed.diff.patch"
diff --git a/package-lock.json b/package-lock.json
index 1111111..2222222 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -10,7 +10,7 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff.patch"
diff --git a/package-lock.json b/package-lock.json
index aaaabbb..cccdddd 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -27,7 +27,7 @@
-old
+new
EOF

    run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "差分前提がレビュー後に更新されました"
    assert_contains "${case_dir}/github_output.txt" "patch (reviewed diff no longer matches current PR diff)"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_approves_when_reviewed_head_has_rebased_but_patch_is_identical() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-2" "base-sha-1"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 11
    mark_review_approved_after_creation "${case_dir}"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-2"
}

test_skips_when_identical_force_push_happens_without_current_head_claude_verdict_requirement() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-2" "base-sha-1"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"

    run_approve_script "${case_dir}" "head-sha-1" "" "false"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "差分前提がレビュー後に更新されました"
    assert_contains "${case_dir}/github_output.txt" "HEAD (reviewed: head-sha-1, current: head-sha-2)"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_approves_when_same_snapshot_only_changes_patch_metadata() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    cat <<'EOF' > "${case_dir}/reviewed.diff.patch"
diff --git a/package-lock.json b/package-lock.json
index 1111111..2222222 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -10,7 +10,7 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff.patch"
diff --git a/package-lock.json b/package-lock.json
index aaaabbb..cccdddd 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -27,7 +27,7 @@
-old
+new
EOF
    write_review_response "${case_dir}/state/create_review_response.json" 12
    mark_review_approved_after_creation "${case_dir}"

    run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-1"
}

test_approves_when_current_diff_capture_only_changes_patch_metadata() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    cat <<'EOF' > "${case_dir}/reviewed.diff.patch"
diff --git a/package-lock.json b/package-lock.json
index 1111111..2222222 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -10,7 +10,7 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_1.patch"
diff --git a/package-lock.json b/package-lock.json
index aaaabbb..cccdddd 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -27,7 +27,7 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_2.patch"
diff --git a/package-lock.json b/package-lock.json
index eeeefff..ggghhhh 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -31,7 +31,7 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_3.patch"
diff --git a/package-lock.json b/package-lock.json
index iiiijjj..kkkllll 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -44,7 +44,7 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_4.patch"
diff --git a/package-lock.json b/package-lock.json
index mmmmnnn..ooopppp 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -52,7 +52,7 @@
-old
+new
EOF
    write_review_response "${case_dir}/state/create_review_response.json" 13
    mark_review_approved_after_creation "${case_dir}"

    run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-1"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
}

test_skips_when_rebased_patch_moves_to_a_different_hunk_location() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-2" "base-sha-2"
    cat <<'EOF' > "${case_dir}/reviewed.diff.patch"
diff --git a/package-lock.json b/package-lock.json
index 1111111..2222222 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -10,7 +10,7 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff.patch"
diff --git a/package-lock.json b/package-lock.json
index aaaabbb..cccdddd 100644
--- a/package-lock.json
+++ b/package-lock.json
@@ -27,7 +27,7 @@
-old
+new
EOF

    run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "差分前提がレビュー後に更新されました"
    assert_contains "${case_dir}/github_output.txt" "patch (reviewed diff no longer matches current PR diff)"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_when_patch_has_changed_since_review() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-2"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    cat <<'EOF' > "${case_dir}/state/current_diff.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+new
EOF
    write_review_response "${case_dir}/state/create_review_response.json" 456

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "差分前提がレビュー後に更新されました"
    assert_contains "${case_dir}/github_output.txt" "patch (reviewed diff no longer matches current PR diff)"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_when_base_only_patch_changes_right_before_review_submission() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-2"
    cat <<'EOF' > "${case_dir}/reviewed.diff.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+older
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+older
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_3.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-new
+newer
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_4.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-new
+newer
EOF

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "差分前提が承認送信前に更新されたため"
    assert_contains "${case_dir}/github_output.txt" "patch (reviewed diff no longer matches current PR diff)"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_approves_when_base_changes_while_capturing_current_diff_and_patch_is_identical() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-2"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 3
    mark_review_approved_after_creation "${case_dir}"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_when_base_changes_during_diff_capture_and_retry_reveals_new_patch() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-2"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-2"
    cat <<'EOF' > "${case_dir}/reviewed.diff.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+older
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_1.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+older
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_2.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-new
+newer
EOF

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "差分前提がレビュー後に更新されました"
    assert_contains "${case_dir}/github_output.txt" "patch (reviewed diff no longer matches current PR diff)"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_when_automerge_label_is_removed_before_review_submission() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1" null false
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "automerge ラベルがありません"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_approves_when_current_diff_stabilizes_after_a_stale_first_read() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    cat <<'EOF' > "${case_dir}/reviewed.diff.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_1.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+older
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_2.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_3.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_4.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_5.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_6.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+new
EOF
    write_review_response "${case_dir}/state/create_review_response.json" 333
    mark_review_approved_after_creation "${case_dir}"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
}

test_fails_closed_when_current_pr_reload_errors_during_diff_capture() {
    local case_dir
    local exit_code
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff_1.patch"
    printf '%s\n' '1' > "${case_dir}/state/current_pr_3.exit_code"
    printf '%s\n' 'gh: secondary rate limit' > "${case_dir}/state/current_pr_3.stderr"

    exit_code="$(run_approve_script_expect_failure "${case_dir}" "head-sha-1")"

    assert_exit_code "${exit_code}" "2"
    assert_contains "${case_dir}/stderr.txt" "failed to reload PR #123 after capturing the first current diff"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_file_absent "${case_dir}/github_output.txt"
}

test_keeps_review_when_post_approval_revalidation_sees_approved_state() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1" APPROVED
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1" APPROVED
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1" APPROVED
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 135

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
}

test_waits_for_review_decision_to_catch_up_after_review_creation() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_10.json" "head-sha-1" "base-sha-1" APPROVED
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 138

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
    if [[ -f "${case_dir}/state/sleep_args.txt" ]]; then
        assert_line_count_equals "${case_dir}/state/sleep_args.txt" "1"
        assert_line_equals "${case_dir}/state/sleep_args.txt" "1"
    fi
}

test_keeps_review_when_created_review_confirms_latest_head_approval_before_review_decision_updates() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 137
    write_review_details_json "${case_dir}/state/review_status.json" 137 APPROVED "head-sha-1"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/137"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
    assert_file_absent "${case_dir}/state/sleep_args.txt"
}

test_dismisses_review_when_created_review_never_confirms_latest_head_approval() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 137
    write_review_details_json "${case_dir}/state/review_status.json" 137 COMMENTED "head-sha-1"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "最新 HEAD (head-sha-1) に有効な承認が付かなかったため"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/137"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/137/dismissals"
    assert_line_count_equals "${case_dir}/state/sleep_args.txt" "3"
    assert_contains "${case_dir}/state/sleep_args.txt" "1"
    assert_contains "${case_dir}/state/sleep_args.txt" "2"
    assert_contains "${case_dir}/state/sleep_args.txt" "4"
}

test_uses_fallback_token_when_primary_review_dismissal_fails() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 139
    printf '%s\n' '1' > "${case_dir}/state/dismiss_review_1.exit_code"
    printf '%s\n' 'gh: primary dismissal failed' > "${case_dir}/state/dismiss_review_1.stderr"
    printf '%s\n' '1' > "${case_dir}/state/dismiss_review_2.exit_code"
    printf '%s\n' 'gh: primary dismissal failed again' > "${case_dir}/state/dismiss_review_2.stderr"

    REVIEW_DISMISSAL_FALLBACK_TOKEN="fallback-dismiss-token" \
        run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "最新 HEAD (head-sha-1) に有効な承認が付かなかったため"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/139/dismissals"
    assert_contains "${case_dir}/state/api_args.txt" "dismissal-gh-token="
    assert_contains "${case_dir}/state/api_args.txt" "dismissal-gh-token=fallback-dismiss-token"
}

test_uses_fallback_token_for_pr_reads_and_diff_but_not_review_submission() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    printf '%s\n' 'fallback-gh-token' > "${case_dir}/state/expected_general_token.txt"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 141
    mark_review_approved_after_creation "${case_dir}"

    REVIEW_DISMISSAL_FALLBACK_TOKEN="fallback-gh-token" \
        run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_contains "${case_dir}/state/api_args.txt" "generic-gh-token=fallback-gh-token"
    assert_contains "${case_dir}/state/api_args.txt" "create-review-gh-token="
    assert_not_contains "${case_dir}/state/api_args.txt" "create-review-gh-token=fallback-gh-token"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_fails_when_primary_review_submission_fails_without_reusing_fallback_token() {
    local case_dir
    local exit_code
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    printf '%s\n' 'fallback-gh-token' > "${case_dir}/state/expected_general_token.txt"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    printf '%s\n' '1' > "${case_dir}/state/create_review_1.exit_code"
    printf '%s\n' 'gh: primary review submission failed' > "${case_dir}/state/create_review_1.stderr"

    exit_code="$(
        REVIEW_DISMISSAL_FALLBACK_TOKEN="fallback-gh-token" \
            run_approve_script_expect_failure "${case_dir}" "head-sha-1"
    )"

    assert_exit_code "${exit_code}" "1"
    assert_contains "${case_dir}/stderr.txt" "gh: primary review submission failed"
    assert_contains "${case_dir}/state/api_args.txt" "generic-gh-token=fallback-gh-token"
    assert_contains "${case_dir}/state/api_args.txt" "create-review-gh-token="
    assert_not_contains "${case_dir}/state/api_args.txt" "create-review-gh-token=fallback-gh-token"
}

test_fails_closed_when_review_dismissal_fails_even_with_fallback_token() {
    local case_dir
    local exit_code
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 140
    printf '%s\n' '1' > "${case_dir}/state/dismiss_review_1.exit_code"
    printf '%s\n' 'gh: primary dismissal failed' > "${case_dir}/state/dismiss_review_1.stderr"
    printf '%s\n' '1' > "${case_dir}/state/dismiss_review_2.exit_code"
    printf '%s\n' 'gh: primary dismissal failed again' > "${case_dir}/state/dismiss_review_2.stderr"
    printf '%s\n' '1' > "${case_dir}/state/dismiss_review_3.exit_code"
    printf '%s\n' 'gh: fallback dismissal failed' > "${case_dir}/state/dismiss_review_3.stderr"
    printf '%s\n' '1' > "${case_dir}/state/dismiss_review_4.exit_code"
    printf '%s\n' 'gh: fallback dismissal failed again' > "${case_dir}/state/dismiss_review_4.stderr"

    exit_code="$(
        REVIEW_DISMISSAL_FALLBACK_TOKEN="fallback-dismiss-token" \
            run_approve_script_expect_failure "${case_dir}" "head-sha-1"
    )"

    assert_exit_code "${exit_code}" "1"
    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "review #140 を取り消せませんでした"
    assert_contains "${case_dir}/github_output.txt" "REVIEW_DISMISSAL_FALLBACK_TOKEN"
    assert_contains "${case_dir}/stderr.txt" "review #140 を取り消せませんでした"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/140/dismissals"
    assert_contains "${case_dir}/state/api_args.txt" "dismissal-gh-token=fallback-dismiss-token"
}

test_keeps_review_when_post_approval_revalidation_sees_unknown_mergeable_state() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1" APPROVED true OPEN UNKNOWN
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1" APPROVED true OPEN UNKNOWN
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1" APPROVED true OPEN UNKNOWN
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 136

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
}

test_reapproves_when_head_changes_after_post_approval_revalidation_before_first_confirmation() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1" APPROVED
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1" APPROVED
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1" APPROVED
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-2" "base-sha-1" APPROVED
    write_pr_json "${case_dir}/state/current_pr_10.json" "head-sha-2" "base-sha-1" APPROVED
    write_pr_json "${case_dir}/state/current_pr_11.json" "head-sha-2" "base-sha-1" APPROVED
    write_pr_json "${case_dir}/state/current_pr_12.json" "head-sha-2" "base-sha-1" APPROVED
    write_pr_json "${case_dir}/state/current_pr_13.json" "head-sha-2" "base-sha-1" APPROVED
    for idx in 14 15 16 17 18 19 20; do
        write_pr_json "${case_dir}/state/current_pr_${idx}.json" "head-sha-2" "base-sha-1" APPROVED
    done
    cat <<'EOF' > "${case_dir}/state/pull_head_1.json"
{
  "head": {
    "sha": "head-sha-2"
  }
}
EOF
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_1.json" 901
    write_review_response "${case_dir}/state/create_review_2.json" 902

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-1"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-2"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
}

test_keeps_review_when_post_approval_refresh_sees_merged_pr() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1" APPROVED true MERGED
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1" APPROVED true MERGED
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1" APPROVED true MERGED
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 579

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
}

test_dismisses_review_when_post_approval_compare_errors_after_review_creation() {
    local case_dir
    local exit_code
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 581
    printf '%s\n' '1' > "${case_dir}/state/current_pr_7.exit_code"
    printf '%s\n' 'gh: post-approval compare reload failed' > "${case_dir}/state/current_pr_7.stderr"

    exit_code="$(run_approve_script_expect_failure "${case_dir}" "head-sha-1")"

    assert_exit_code "${exit_code}" "2"
    assert_contains "${case_dir}/stderr.txt" "failed to reload PR #123 after capturing the first current diff"
    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "承認後再検証に失敗したため、この自動承認を取り消します"
    assert_contains "${case_dir}/github_output.txt" "差分再検証で current PR の取得または diff 取得に失敗しました。"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/581/dismissals"
}

test_dismisses_review_when_post_approval_refresh_errors_after_review_creation() {
    local case_dir
    local exit_code
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 582
    printf '%s\n' '1' > "${case_dir}/state/current_pr_9.exit_code"
    printf '%s\n' 'gh: post-approval refresh failed' > "${case_dir}/state/current_pr_9.stderr"

    exit_code="$(run_approve_script_expect_failure "${case_dir}" "head-sha-1")"

    assert_exit_code "${exit_code}" "1"
    assert_contains "${case_dir}/stderr.txt" "gh: post-approval refresh failed"
    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "承認後再検証に失敗したため、この自動承認を取り消します"
    assert_contains "${case_dir}/github_output.txt" "候補条件の再評価で current PR の取得に失敗しました。"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/582/dismissals"
}

test_keeps_merged_pr_successful_when_post_approval_refresh_errors_after_merge() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-1" "base-sha-1" null true MERGED
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 585
    printf '%s\n' '1' > "${case_dir}/state/current_pr_10.exit_code"
    printf '%s\n' 'gh: post-approval refresh failed after merge' > "${case_dir}/state/current_pr_10.stderr"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
    if [[ -f "${case_dir}/state/sleep_args.txt" ]]; then
        assert_line_count_equals "${case_dir}/state/sleep_args.txt" "1"
        assert_line_equals "${case_dir}/state/sleep_args.txt" "1"
    fi
}

test_keeps_merged_pr_successful_when_refresh_fails_after_compare_observes_merge() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1" APPROVED true MERGED
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1" APPROVED true MERGED
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1" APPROVED true MERGED
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 586
    printf '%s\n' '1' > "${case_dir}/state/current_pr_9.exit_code"
    printf '%s\n' 'gh: post-approval refresh failed after compare observed merge' > "${case_dir}/state/current_pr_9.stderr"
    mark_review_approved_after_creation "${case_dir}"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
}

test_dismisses_review_when_pull_head_snapshot_sees_closed_pr_before_confirmation() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1" APPROVED
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1" APPROVED
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1" APPROVED
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-1" "base-sha-1" APPROVED
    cat <<'EOF' > "${case_dir}/state/pull_head_1.json"
{
  "head": {
    "sha": "head-sha-1"
  },
  "state": "closed",
  "merged": false
}
EOF
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 587

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "未マージのまま close されたため"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/587/dismissals"
}

test_dismisses_review_when_post_approval_refresh_sees_closed_pr() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1" APPROVED true CLOSED
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1" APPROVED true CLOSED
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1" APPROVED true CLOSED
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-1" "base-sha-1" APPROVED true CLOSED
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 583

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "未マージのまま close されたため"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/583/dismissals"
}

test_skips_when_post_approval_revalidation_sees_merged_pr_with_removed_automerge_label() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1" APPROVED false MERGED
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1" APPROVED false MERGED
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1" APPROVED false MERGED
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 580

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "候補条件が承認送信中に変わったため、この自動承認を取り消します"
    assert_contains "${case_dir}/github_output.txt" "automerge ラベルがありません"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
}

test_dismisses_review_when_candidate_conditions_change_after_post_approval_refresh_on_closed_pr() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1" APPROVED false CLOSED
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1" APPROVED false CLOSED
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1" APPROVED false CLOSED
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-1" "base-sha-1" APPROVED false CLOSED
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 584

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "候補条件が承認送信中に変わったため、この自動承認を取り消します"
    assert_contains "${case_dir}/github_output.txt" "automerge ラベルがありません"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/584/dismissals"
}

test_dismisses_review_when_patch_changes_during_approval_submission() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff_1.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff_2.patch"
    cat <<'EOF' > "${case_dir}/state/current_diff_3.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+new
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_4.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+new
EOF
    write_review_response "${case_dir}/state/create_review_response.json" 456

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "承認送信中に更新されたため、この自動承認を取り消します"
    assert_contains "${case_dir}/github_output.txt" "patch (reviewed diff no longer matches current PR diff)"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-1"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/456/dismissals"
}

test_reapproves_when_review_decision_is_stale_after_head_changes_during_approval_submission() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-2" "base-sha-2" APPROVED
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-2" "base-sha-2" APPROVED
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-2" "base-sha-2" APPROVED
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-2" "base-sha-2" APPROVED
    write_pr_json "${case_dir}/state/current_pr_10.json" "head-sha-2" "base-sha-2" APPROVED
    write_pr_json "${case_dir}/state/current_pr_11.json" "head-sha-2" "base-sha-2" APPROVED
    write_pr_json "${case_dir}/state/current_pr_12.json" "head-sha-2" "base-sha-2" APPROVED
    write_pr_json "${case_dir}/state/current_pr_13.json" "head-sha-2" "base-sha-2" APPROVED
    for idx in 14 15 16 17 18 19 20; do
        write_pr_json "${case_dir}/state/current_pr_${idx}.json" "head-sha-2" "base-sha-2" APPROVED
    done
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_1.json" 789
    write_review_response "${case_dir}/state/create_review_2.json" 790

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-1"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-2"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
}

test_reapproves_latest_head_when_identical_rebase_happens_after_review_creation() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_10.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_11.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_12.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_13.json" "head-sha-2" "base-sha-1" APPROVED
    for idx in 14 15 16 17 18 19 20; do
        write_pr_json "${case_dir}/state/current_pr_${idx}.json" "head-sha-2" "base-sha-1" APPROVED
    done
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_1.json" 790
    write_review_response "${case_dir}/state/create_review_2.json" 791

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-1"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-2"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
}

test_dismisses_all_recorded_reviews_when_second_approval_becomes_invalid() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_10.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_11.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_12.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_13.json" "head-sha-2" "base-sha-1" APPROVED false
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_1.json" 790
    write_review_response "${case_dir}/state/create_review_2.json" 791

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "候補条件が承認送信中に変わったため、この自動承認を取り消します"
    assert_contains "${case_dir}/github_output.txt" "automerge ラベルがありません"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/790/dismissals"
    if grep -Fq "repos/falgon/roki-web/pulls/123/reviews/791" "${case_dir}/state/api_args.txt"; then
        assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/791/dismissals"
    fi
}

test_dismisses_review_when_changes_are_requested_during_approval_submission() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1" CHANGES_REQUESTED
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1" CHANGES_REQUESTED
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1" CHANGES_REQUESTED
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 246

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "候補条件が承認送信中に変わったため、この自動承認を取り消します"
    assert_contains "${case_dir}/github_output.txt" "変更要求レビューが付いているため自動承認しません"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/246/dismissals"
}

test_dismisses_review_when_candidate_conditions_change_after_post_approval_refresh() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1" APPROVED false
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1" APPROVED false
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1" APPROVED false
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 864

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "候補条件が承認送信中に変わったため、この自動承認を取り消します"
    assert_contains "${case_dir}/github_output.txt" "automerge ラベルがありません"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/864/dismissals"
}

test_dismisses_review_when_candidate_conditions_change_after_post_approval_compare() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-1" "base-sha-1" APPROVED false
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 865

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "候補条件が承認送信中に変わったため、この自動承認を取り消します"
    assert_contains "${case_dir}/github_output.txt" "automerge ラベルがありません"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/865/dismissals"
}

test_dismisses_review_when_patch_changes_after_post_approval_refresh() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-1" "base-sha-2" APPROVED
    write_pr_json "${case_dir}/state/current_pr_10.json" "head-sha-1" "base-sha-2" APPROVED
    write_pr_json "${case_dir}/state/current_pr_11.json" "head-sha-1" "base-sha-2" APPROVED
    write_pr_json "${case_dir}/state/current_pr_12.json" "head-sha-1" "base-sha-2" APPROVED
    cat <<'EOF' > "${case_dir}/reviewed.diff.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+older
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-old
+older
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_5.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-new
+newer
EOF
    cat <<'EOF' > "${case_dir}/state/current_diff_6.patch"
diff --git a/package-lock.json b/package-lock.json
@@ -1 +1 @@
-new
+newer
EOF
    write_review_response "${case_dir}/state/create_review_response.json" 866

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "承認送信中に更新されたため、この自動承認を取り消します"
    assert_contains "${case_dir}/github_output.txt" "patch (reviewed diff no longer matches current PR diff)"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews/866/dismissals"
}

test_retries_review_creation_when_head_rebases_but_patch_is_identical() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-2" "base-sha-1"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    printf '%s\n' '1' > "${case_dir}/state/create_review_1.exit_code"
    write_review_error "${case_dir}/state/create_review_1.stderr"
    write_review_response "${case_dir}/state/create_review_2.json" 987
    mark_review_approved_after_creation "${case_dir}"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-1"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-2"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
}

test_retries_review_creation_when_mergeable_is_temporarily_unknown_after_commit_id_race() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-2" "base-sha-1" null true OPEN UNKNOWN
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-2" "base-sha-1" null true OPEN UNKNOWN
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-2" "base-sha-1" null true OPEN UNKNOWN
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-2" "base-sha-1" null true OPEN UNKNOWN
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    printf '%s\n' '1' > "${case_dir}/state/create_review_1.exit_code"
    write_review_error "${case_dir}/state/create_review_1.stderr"
    write_review_response "${case_dir}/state/create_review_2.json" 988
    mark_review_approved_after_creation "${case_dir}"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-1"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-2"
    assert_not_contains "${case_dir}/github_output.txt" "マージ可能ではありません (state: UNKNOWN)"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
}

test_skips_when_review_creation_keeps_racing_with_identical_rebases() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-2" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_10.json" "head-sha-3" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_11.json" "head-sha-3" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_12.json" "head-sha-3" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_13.json" "head-sha-3" "base-sha-1"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    printf '%s\n' '1' > "${case_dir}/state/create_review_1.exit_code"
    write_review_error "${case_dir}/state/create_review_1.stderr"
    printf '%s\n' '1' > "${case_dir}/state/create_review_2.exit_code"
    write_review_error "${case_dir}/state/create_review_2.stderr"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "HEAD が承認送信前に連続して更新されたため、自動承認をスキップします"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-1"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-2"
    assert_not_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-3"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
}

test_skips_when_review_creation_fails_and_candidate_conditions_change_after_refresh() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr_1.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_2.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_3.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_4.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_5.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_6.json" "head-sha-1" "base-sha-1"
    write_pr_json "${case_dir}/state/current_pr_7.json" "head-sha-1" "base-sha-1" null false
    write_pr_json "${case_dir}/state/current_pr_8.json" "head-sha-1" "base-sha-1" null false
    write_pr_json "${case_dir}/state/current_pr_9.json" "head-sha-1" "base-sha-1" null false
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_error "${case_dir}/state/create_review_error.txt"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "automerge ラベルがありません"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_not_contains "${case_dir}/state/api_args.txt" "/dismissals"
}

test_uses_branch_protection_read_token_for_candidate_rechecks() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-1" "base-sha-1"
    printf '%s\n' 'read-branch-token' > "${case_dir}/state/expected_protection_token.txt"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 10
    mark_review_approved_after_creation "${case_dir}"

    DEPENDABOT_AUTO_APPROVE_READ_BRANCH_PROTECTION_TOKEN="read-branch-token" \
        run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_contains "${case_dir}/state/api_args.txt" "protection-gh-token=read-branch-token"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_when_latest_check_run_is_a_queued_rerun() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
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
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason=PR #123 は必須チェックが未成功です: lint-and-test:QUEUED"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_when_status_rollup_still_points_to_previous_head() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_pr_json "${case_dir}/state/current_pr.json" "head-sha-2" "base-sha-1"
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
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"

    run_approve_script "${case_dir}" "head-sha-2"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason=PR #123 の必須チェック結果が現在の HEAD (head-sha-2) ではなく head-sha-1 に紐付いているため、自動承認しません。"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_when_fallback_status_rollup_reports_active_rerun_on_same_head() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
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
      "conclusion": "SUCCESS",
      "completedAt": "2026-03-29T09:00:00Z"
    }
  ],
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
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
                "completedAt": "2026-03-29T09:00:00Z"
              },
              {
                "__typename": "CheckRun",
                "name": "lint-and-test",
                "status": "IN_PROGRESS",
                "conclusion": null,
                "startedAt": "2026-03-29T09:05:00Z"
              }
            ]
          }
        }
      }
    }
  }
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason=PR #123 は必須チェックが未成功です: lint-and-test:IN_PROGRESS"
    assert_contains "${case_dir}/state/api_args.txt" "contexts(first: 100)"
    assert_contains "${case_dir}/state/api_args.txt" "... on CheckRun {"
    assert_contains "${case_dir}/state/api_args.txt" "... on StatusContext {"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_when_graphql_rollup_reports_active_rerun_even_if_current_pr_has_matching_commit_oid() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
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
        "conclusion": "SUCCESS",
        "completedAt": "2026-03-29T09:00:00Z"
      }
    ]
  },
  "files": [
    { "path": "package-lock.json" }
  ]
}
EOF
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
                "completedAt": "2026-03-29T09:00:00Z"
              },
              {
                "__typename": "CheckRun",
                "name": "lint-and-test",
                "status": "IN_PROGRESS",
                "conclusion": null,
                "startedAt": "2026-03-29T09:05:00Z"
              }
            ]
          }
        }
      }
    }
  }
}
EOF
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"

    run_approve_script "${case_dir}" "head-sha-1"

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason=PR #123 は必須チェックが未成功です: lint-and-test:IN_PROGRESS"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_approves_npm_candidate_when_allowlisted_devdependency_is_patch_updated() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        '[{ "path": "package.json" }, { "path": "package-lock.json" }]'
    write_compare_json "${case_dir}/state/compare.json" "merge-base-sha-1"
    cat <<'EOF' > "${case_dir}/state/package_json_merge-base-sha-1.json"
{
  "private": true,
  "scripts": {
    "test": "vitest"
  },
  "dependencies": {
    "react": "^18.3.1"
  },
  "devDependencies": {
    "@types/node": "^22.12.0",
    "vitest": "^1.6.0"
  }
}
EOF
    cat <<'EOF' > "${case_dir}/state/package_json_head-sha-1.json"
{
  "private": true,
  "scripts": {
    "test": "vitest"
  },
  "dependencies": {
    "react": "^18.3.1"
  },
  "devDependencies": {
    "@types/node": "^22.12.0",
    "vitest": "^1.6.1"
  }
}
EOF
    write_npm_package_patch "${case_dir}/reviewed.diff.patch" "^1.6.0" "^1.6.1"
    write_npm_package_patch "${case_dir}/state/current_diff.patch" "^1.6.0" "^1.6.1"
    write_review_response "${case_dir}/state/create_review_response.json" 1004
    mark_review_approved_after_creation "${case_dir}"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/compare/develop...head-sha-1"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/contents/package.json?ref=merge-base-sha-1"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_npm_candidate_when_external_claude_review_verdict_is_missing_before_approval() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        '[{ "path": "package.json" }, { "path": "package-lock.json" }]'
    write_compare_json "${case_dir}/state/compare.json" "merge-base-sha-1"
    cat <<'EOF' > "${case_dir}/state/package_json_merge-base-sha-1.json"
{
  "private": true,
  "scripts": {
    "test": "vitest"
  },
  "dependencies": {
    "react": "^18.3.1"
  },
  "devDependencies": {
    "@types/node": "^22.12.0",
    "vitest": "^1.6.0"
  }
}
EOF
    cat <<'EOF' > "${case_dir}/state/package_json_head-sha-1.json"
{
  "private": true,
  "scripts": {
    "test": "vitest"
  },
  "dependencies": {
    "react": "^18.3.1"
  },
  "devDependencies": {
    "@types/node": "^22.12.0",
    "vitest": "^1.6.1"
  }
}
EOF
    write_npm_package_patch "${case_dir}/reviewed.diff.patch" "^1.6.0" "^1.6.1"
    write_npm_package_patch "${case_dir}/state/current_diff.patch" "^1.6.0" "^1.6.1"
    printf '[]\n' > "${case_dir}/state/issue_comments.json"
    write_review_response "${case_dir}/state/create_review_response.json" 1004
    mark_review_approved_after_creation "${case_dir}"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "machine-readable な Claude review verdict comment がまだありません"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_approves_npm_candidate_when_shared_claude_review_check_has_failed_but_current_head_verdict_is_not_required() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        '[{ "path": "package.json" }, { "path": "package-lock.json" }]'
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
    write_npm_package_patch "${case_dir}/reviewed.diff.patch" "^1.6.0" "^1.6.1"
    write_npm_package_patch "${case_dir}/state/current_diff.patch" "^1.6.0" "^1.6.1"
    write_compare_json "${case_dir}/state/compare.json" "merge-base-sha-1"
    cat <<'EOF' > "${case_dir}/state/package_json_merge-base-sha-1.json"
{
  "private": true,
  "scripts": {
    "test": "vitest"
  },
  "dependencies": {
    "react": "^18.3.1"
  },
  "devDependencies": {
    "@types/node": "^22.12.0",
    "vitest": "^1.6.0"
  }
}
EOF
    cat <<'EOF' > "${case_dir}/state/package_json_head-sha-1.json"
{
  "private": true,
  "scripts": {
    "test": "vitest"
  },
  "dependencies": {
    "react": "^18.3.1"
  },
  "devDependencies": {
    "@types/node": "^22.12.0",
    "vitest": "^1.6.1"
  }
}
EOF
    write_review_response "${case_dir}/state/create_review_response.json" 1001
    mark_review_approved_after_creation "${case_dir}"

    (
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="claude-review" \
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=npm \
        DEPENDABOT_AUTO_APPROVE_REQUIRE_CURRENT_HEAD_CLAUDE_REVIEW_VERDICT=false \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_approves_npm_candidate_when_branch_protection_requires_claude_review_but_verdict_requirement_is_disabled() {
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
    write_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        '[{ "path": "package.json" }, { "path": "package-lock.json" }]'
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
    write_npm_package_patch "${case_dir}/reviewed.diff.patch" "^1.6.0" "^1.6.1"
    write_npm_package_patch "${case_dir}/state/current_diff.patch" "^1.6.0" "^1.6.1"
    write_compare_json "${case_dir}/state/compare.json" "merge-base-sha-1"
    cat <<'EOF' > "${case_dir}/state/package_json_merge-base-sha-1.json"
{
  "private": true,
  "scripts": {
    "test": "vitest"
  },
  "dependencies": {
    "react": "^18.3.1"
  },
  "devDependencies": {
    "@types/node": "^22.12.0",
    "vitest": "^1.6.0"
  }
}
EOF
    cat <<'EOF' > "${case_dir}/state/package_json_head-sha-1.json"
{
  "private": true,
  "scripts": {
    "test": "vitest"
  },
  "dependencies": {
    "react": "^18.3.1"
  },
  "devDependencies": {
    "@types/node": "^22.12.0",
    "vitest": "^1.6.1"
  }
}
EOF
    write_review_response "${case_dir}/state/create_review_response.json" 1001
    mark_review_approved_after_creation "${case_dir}"

    (
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="claude-review" \
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=npm \
        DEPENDABOT_AUTO_APPROVE_REQUIRE_CURRENT_HEAD_CLAUDE_REVIEW_VERDICT=false \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_npm_candidate_when_package_json_changes_outside_dependency_fields() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        '[{ "path": "package.json" }, { "path": "package-lock.json" }]'
    write_compare_json "${case_dir}/state/compare.json" "merge-base-sha-1"
    cat <<'EOF' > "${case_dir}/state/package_json_merge-base-sha-1.json"
{
  "private": true,
  "scripts": {
    "test": "vitest"
  },
  "devDependencies": {
    "vitest": "^1.6.0"
  }
}
EOF
    cat <<'EOF' > "${case_dir}/state/package_json_head-sha-1.json"
{
  "private": true,
  "scripts": {
    "test": "vitest --run"
  },
  "devDependencies": {
    "vitest": "^1.6.1"
  }
}
EOF
    write_npm_package_patch "${case_dir}/reviewed.diff.patch" "^1.6.0" "^1.6.1"
    write_npm_package_patch "${case_dir}/state/current_diff.patch" "^1.6.0" "^1.6.1"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "package.json の依存バージョン以外も変更している"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_approves_non_npm_candidate_when_supported_manifests_only_change() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_review_response "${case_dir}/state/create_review_response.json" 1002
    mark_review_approved_after_creation "${case_dir}"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-1"
}

test_approves_non_npm_candidate_when_diff_recheck_uses_fallback_token() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    printf '%s\n' 'fallback-gh-token' > "${case_dir}/state/expected_general_token.txt"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_review_response "${case_dir}/state/create_review_response.json" 1003
    mark_review_approved_after_creation "${case_dir}"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        REVIEW_DISMISSAL_FALLBACK_TOKEN="fallback-gh-token" \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_contains "${case_dir}/state/api_args.txt" "generic-gh-token=fallback-gh-token"
    assert_contains "${case_dir}/state/api_args.txt" "create-review-gh-token="
    assert_not_contains "${case_dir}/state/api_args.txt" "create-review-gh-token=fallback-gh-token"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_claude_review_verdict_turns_fail_before_approval() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_claude_review_verdict_comments_json \
        "${case_dir}/state/issue_comments.json" \
        "head-sha-1" \
        "fail" \
        "1" \
        "Blocking finding"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "Claude review"
    assert_contains "${case_dir}/github_output.txt" "status=fail"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_approves_non_npm_candidate_when_claude_review_verdict_comment_author_is_not_github_actions_bot() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_claude_review_verdict_comments_json \
        "${case_dir}/state/issue_comments.json" \
        "head-sha-1" \
        "pass" \
        "0" \
        "No findings." \
        "1001" \
        "2" \
        "2026-03-30T00:10:00Z" \
        "repo-maintainer"
    write_collaborator_permission_json \
        "${case_dir}/state/collaborator_permission_repo-maintainer.json" \
        "write"
    write_review_response "${case_dir}/state/create_review_response.json" 1006
    mark_review_approved_after_creation "${case_dir}"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_claude_review_posts_inline_finding_before_approval() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_claude_review_inline_comments_json \
        "${case_dir}/state/review_comments.json" \
        "head-sha-1"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "inline finding が 1 件"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_claude_review_posts_inline_finding_on_later_page_before_approval() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    jq -cn '
        [
            range(0; 100)
            | {
                id: (. + 1),
                user: { login: "github-actions[bot]" },
                commit_id: "old-head-sha",
                body: "non-blocking historical comment"
            }
        ]
    ' > "${case_dir}/state/review_comments_page_1.json"
    write_claude_review_inline_comments_json \
        "${case_dir}/state/review_comments_page_2.json" \
        "head-sha-1"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "inline finding が 1 件"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_current_head_has_inline_findings_from_older_claude_run() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_claude_review_verdict_comments_json \
        "${case_dir}/state/issue_comments.json" \
        "head-sha-1" \
        "pass" \
        "0" \
        "No findings." \
        "2002" \
        "5" \
        "2026-03-30T00:20:00Z"
    write_workflow_run_json \
        "${case_dir}/state/workflow_run_2002.json" \
        "5" \
        "2026-03-30T00:10:00Z"
    write_claude_review_inline_comments_json \
        "${case_dir}/state/review_comments.json" \
        "head-sha-1" \
        "【Claude review】 Historical finding" \
        "2026-03-30T00:05:00Z"
    write_review_response "${case_dir}/state/create_review_response.json" 1001
    mark_review_approved_after_creation "${case_dir}"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "inline finding が 1 件"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_current_head_has_inline_findings_from_trusted_non_bot_verdict_publisher_before_approval() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_claude_review_verdict_comments_json \
        "${case_dir}/state/issue_comments.json" \
        "head-sha-1" \
        "pass" \
        "0" \
        "No findings." \
        "2002" \
        "5" \
        "2026-03-30T00:20:00Z" \
        "repo-maintainer"
    write_collaborator_permission_json \
        "${case_dir}/state/collaborator_permission_repo-maintainer.json" \
        "write"
    write_workflow_run_json \
        "${case_dir}/state/workflow_run_2002.json" \
        "5" \
        "2026-03-30T00:10:00Z"
    write_claude_review_inline_comments_json \
        "${case_dir}/state/review_comments.json" \
        "head-sha-1" \
        "【Claude review】 Maintainer finding" \
        "2026-03-30T00:05:00Z" \
        "repo-maintainer"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "inline finding が 1 件"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_current_head_has_inline_findings_from_newer_claude_rerun_before_approval() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_claude_review_verdict_comments_json \
        "${case_dir}/state/issue_comments.json" \
        "head-sha-1" \
        "pass" \
        "0" \
        "No findings." \
        "2002" \
        "5" \
        "2026-03-30T00:20:00Z"
    write_workflow_run_json \
        "${case_dir}/state/workflow_run_2002.json" \
        "5" \
        "2026-03-30T00:10:00Z"
    write_claude_review_inline_comments_json \
        "${case_dir}/state/review_comments.json" \
        "head-sha-1" \
        "【Claude review】 Finding from rerun" \
        "2026-03-30T00:25:00Z"
    write_review_response "${case_dir}/state/create_review_response.json" 1001
    mark_review_approved_after_creation "${case_dir}"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "inline finding が 1 件"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_approves_non_npm_candidate_when_trusted_verdict_comes_from_scheduled_catch_up_run() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
  "mergeable": "MERGEABLE",
  "reviewDecision": null,
  "labels": [
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
        "conclusion": "SUCCESS"
      }
    ]
  },
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_claude_review_verdict_comments_json \
        "${case_dir}/state/issue_comments.json" \
        "head-sha-1" \
        "pass" \
        "0" \
        "No findings." \
        "4004" \
        "3" \
        "2026-03-30T00:15:00Z"
    write_workflow_run_json \
        "${case_dir}/state/workflow_run_4004.json" \
        "3" \
        "2026-03-30T00:10:00Z" \
        "2026-03-30T00:20:00Z" \
        "head-sha-1" \
        "schedule" \
        "false"
    write_review_response "${case_dir}/state/create_review_response.json" 1007
    mark_review_approved_after_creation "${case_dir}"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_claude_review_check_is_pending() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
  "mergeable": "MERGEABLE",
  "reviewDecision": null,
  "labels": [
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
        "conclusion": "SUCCESS"
      }
    ]
  },
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
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
                "status": "IN_PROGRESS",
                "conclusion": null
              }
            ]
          }
        }
      }
    }
  }
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_review_response "${case_dir}/state/create_review_response.json" 1004
    mark_review_approved_after_creation "${case_dir}"

    (
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="claude-review" \
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "監視対象チェックが未成功です"
    assert_contains "${case_dir}/github_output.txt" "claude-review:IN_PROGRESS"
}

test_skips_non_npm_candidate_when_claude_review_has_not_reported_on_current_head() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
  "mergeable": "MERGEABLE",
  "reviewDecision": null,
  "labels": [
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
        "conclusion": "SUCCESS"
      }
    ]
  },
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
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
              }
            ]
          }
        }
      }
    }
  }
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_review_response "${case_dir}/state/create_review_response.json" 1005
    mark_review_approved_after_creation "${case_dir}"

    (
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="claude-review" \
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "必須または監視対象チェックがまだ出揃っていません"
    assert_contains "${case_dir}/github_output.txt" "claude-review"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_approves_non_npm_candidate_when_only_unrelated_optional_check_is_red() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
  "mergeable": "MERGEABLE",
  "reviewDecision": null,
  "labels": [
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
        "conclusion": "SUCCESS"
      },
      {
        "__typename": "CheckRun",
        "name": "label",
        "status": "COMPLETED",
        "conclusion": "FAILURE"
      }
    ]
  },
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
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
                "name": "label",
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
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_review_response "${case_dir}/state/create_review_response.json" 1006
    mark_review_approved_after_creation "${case_dir}"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_npm_candidate_when_unrelated_optional_check_is_red() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        '[{ "path": "package-lock.json" }]'
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
                "name": "label",
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
    write_npm_package_patch "${case_dir}/reviewed.diff.patch" "^1.6.0" "^1.6.1"
    write_npm_package_patch "${case_dir}/state/current_diff.patch" "^1.6.0" "^1.6.1"
    write_compare_json "${case_dir}/state/compare.json" "merge-base-sha-1"
    cat <<'EOF' > "${case_dir}/state/package_json_merge-base-sha-1.json"
{
  "private": true,
  "scripts": {
    "test": "vitest"
  },
  "devDependencies": {
    "vitest": "^1.6.0"
  }
}
EOF
    cat <<'EOF' > "${case_dir}/state/package_json_head-sha-1.json"
{
  "private": true,
  "scripts": {
    "test": "vitest"
  },
  "devDependencies": {
    "vitest": "^1.6.1"
  }
}
EOF

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "未成功のチェックがあります"
    assert_contains "${case_dir}/github_output.txt" "label:FAILURE"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_monitored_workflow_check_has_failed() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
  "mergeable": "MERGEABLE",
  "reviewDecision": null,
  "labels": [
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
        "conclusion": "SUCCESS"
      }
    ]
  },
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
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
                "name": "build",
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
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="build" \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "監視対象チェックが未成功です"
    assert_contains "${case_dir}/github_output.txt" "build:FAILURE"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_unsupported_files_change() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump unsupported dependency manifest",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/pip/example-1.2.3",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": "requirements.txt" }
  ]
}
EOF
    printf '%s\n' 'diff --git a/requirements.txt b/requirements.txt' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/requirements.txt b/requirements.txt' > "${case_dir}/state/current_diff.patch"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "GitHub Actions / Docker の依存 manifest 以外を変更しています: requirements.txt"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_version_update_is_not_patch() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump actions/setup-node from 4.0.2 to 4.0.3",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/setup-node-5.0.0",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/setup-node" \
        "4.0.2" \
        "5.0.0"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/setup-node" \
        "4.0.2" \
        "5.0.0"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "non-npm 依存の major update"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_title_claims_patch_but_diff_is_minor() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump ubuntu from 24.04 to 24.04.1",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/docker/ubuntu-24.10",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": "docker/Dockerfile" }
  ]
}
EOF
    write_dockerfile_patch \
        "${case_dir}/reviewed.diff.patch" \
        "ubuntu:24.04" \
        "ubuntu:24.10"
    write_dockerfile_patch \
        "${case_dir}/state/current_diff.patch" \
        "ubuntu:24.04" \
        "ubuntu:24.10"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "non-npm 依存の minor update"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_patch_swaps_to_different_image_identity() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported container image",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/docker/alpine-3.20.1",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": "docker/Dockerfile" }
  ]
}
EOF
    write_dockerfile_patch \
        "${case_dir}/reviewed.diff.patch" \
        "ubuntu:24.04" \
        "alpine:3.20.1"
    write_dockerfile_patch \
        "${case_dir}/state/current_diff.patch" \
        "ubuntu:24.04" \
        "alpine:3.20.1"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "patch 相当と判定できない"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_approves_non_npm_candidate_when_composite_docker_tag_only_changes_trailing_patch_version() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported container image",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/docker/node-20-alpine3.20.2",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": "docker/Dockerfile" }
  ]
}
EOF
    write_dockerfile_patch \
        "${case_dir}/reviewed.diff.patch" \
        "node:20-alpine3.20.1" \
        "node:20-alpine3.20.2"
    write_dockerfile_patch \
        "${case_dir}/state/current_diff.patch" \
        "node:20-alpine3.20.1" \
        "node:20-alpine3.20.2"
    write_review_response "${case_dir}/state/create_review_response.json" 1007
    mark_review_approved_after_creation "${case_dir}"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_composite_docker_tag_changes_variant_alongside_patch_version() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported container image",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/docker/example-app-1.2.4",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": "docker/Dockerfile" }
  ]
}
EOF
    write_dockerfile_patch \
        "${case_dir}/reviewed.diff.patch" \
        "ghcr.io/example/app:1.2.3-alpine3.18" \
        "ghcr.io/example/app:1.2.4-alpine3.20"
    write_dockerfile_patch \
        "${case_dir}/state/current_diff.patch" \
        "ghcr.io/example/app:1.2.3-alpine3.18" \
        "ghcr.io/example/app:1.2.4-alpine3.20"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "patch 相当と判定できない"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_docker_from_changes_platform_or_alias() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported container image",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/docker/ubuntu-24.04.1",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": "docker/Dockerfile" }
  ]
}
EOF
    write_dockerfile_from_line_patch \
        "${case_dir}/reviewed.diff.patch" \
        "FROM ubuntu:24.04 AS base" \
        "FROM --platform=linux/amd64 ubuntu:24.04.1 AS runtime"
    write_dockerfile_from_line_patch \
        "${case_dir}/state/current_diff.patch" \
        "FROM ubuntu:24.04 AS base" \
        "FROM --platform=linux/amd64 ubuntu:24.04.1 AS runtime"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "patch 相当と判定できない"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_allowlisted_workflow_contains_unrelated_edits() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": ".github/workflows/manually-cleanup-artifacts.yml" }
  ]
}
EOF
    write_github_actions_patch_with_unrelated_edit \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch_with_unrelated_edit \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "patch 相当と判定できない"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_auto_approve_workflow_itself_changes() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": ".github/workflows/dependabot-claude-auto-approve.yml" }
  ]
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "GitHub Actions / Docker の依存 manifest 以外を変更しています"
    assert_contains "${case_dir}/github_output.txt" ".github/workflows/dependabot-claude-auto-approve.yml"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skips_non_npm_candidate_when_approval_gate_workflows_change() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump supported workflow dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/github_actions/actions/checkout-4.2.2",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": ".github/workflows/build_pr.yml" },
    { "path": ".github/workflows/build.yml" },
    { "path": ".github/workflows/ci-lint-test.yml" },
    { "path": ".github/workflows/claude.yml" },
    { "path": ".github/workflows/restyled.yml" },
    { "path": ".github/workflows/claude-code-review.yml" },
    { "path": ".github/workflows/codeql.yml" },
    { "path": ".github/workflows/dependabot-auto-approve.yml" },
    { "path": ".github/workflows/dependabot-claude-auto-approve.yml" },
    { "path": ".github/workflows/kiirotori-auto-approve.yml" },
    { "path": ".github/workflows/pr-labeler.yml" },
    { "path": ".github/workflows/update-biome-schema.yml" }
  ]
}
EOF
    write_github_actions_patch \
        "${case_dir}/reviewed.diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    (
        DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND=non-npm \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=false"
    assert_contains "${case_dir}/github_output.txt" "GitHub Actions / Docker の依存 manifest 以外を変更しています"
    assert_contains "${case_dir}/github_output.txt" ".github/workflows/build_pr.yml"
    assert_contains "${case_dir}/github_output.txt" ".github/workflows/build.yml"
    assert_contains "${case_dir}/github_output.txt" ".github/workflows/ci-lint-test.yml"
    assert_contains "${case_dir}/github_output.txt" ".github/workflows/claude.yml"
    assert_contains "${case_dir}/github_output.txt" ".github/workflows/restyled.yml"
    assert_contains "${case_dir}/github_output.txt" ".github/workflows/claude-code-review.yml"
    assert_contains "${case_dir}/github_output.txt" ".github/workflows/codeql.yml"
    assert_contains "${case_dir}/github_output.txt" ".github/workflows/dependabot-auto-approve.yml"
    assert_contains "${case_dir}/github_output.txt" ".github/workflows/dependabot-claude-auto-approve.yml"
    assert_contains "${case_dir}/github_output.txt" ".github/workflows/kiirotori-auto-approve.yml"
    assert_contains "${case_dir}/github_output.txt" ".github/workflows/pr-labeler.yml"
    assert_contains "${case_dir}/github_output.txt" ".github/workflows/update-biome-schema.yml"
    assert_not_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
}

test_skip_candidate_validation_mode_allows_non_npm_candidate() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    cat <<'EOF' > "${case_dir}/state/current_pr.json"
{
  "state": "OPEN",
  "number": 123,
  "title": "Bump example dependency",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/bundler/example-1.2.3",
  "headRefOid": "head-sha-1",
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
    }
  ],
  "files": [
    { "path": "package-lock.json" }
  ]
}
EOF
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/reviewed.diff.patch"
    printf '%s\n' 'diff --git a/package-lock.json b/package-lock.json' > "${case_dir}/state/current_diff.patch"
    write_review_response "${case_dir}/state/create_review_response.json" 1001
    mark_review_approved_after_creation "${case_dir}"

    (
        SKIP_CANDIDATE_VALIDATION=true \
        run_approve_script "${case_dir}" "head-sha-1" "base-sha-1"
    )

    assert_line_equals "${case_dir}/github_output.txt" "approved=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_contains "${case_dir}/state/api_args.txt" "repos/falgon/roki-web/pulls/123/reviews"
    assert_contains "${case_dir}/state/api_args.txt" "commit_id=head-sha-1"
}

test_approves_when_reviewed_head_matches_current_pr
test_approves_when_base_has_changed_but_patch_is_identical
test_skips_when_base_has_changed_but_patch_is_identical_with_reviewed_base_sha
test_approves_when_base_only_change_moves_hunk_headers_without_reviewed_base_sha
test_approves_when_reviewed_head_has_rebased_but_patch_is_identical
test_skips_when_identical_force_push_happens_without_current_head_claude_verdict_requirement
test_approves_when_same_snapshot_only_changes_patch_metadata
test_approves_when_current_diff_capture_only_changes_patch_metadata
test_skips_when_rebased_patch_moves_to_a_different_hunk_location
test_skips_when_base_only_change_moves_hunk_headers_with_reviewed_base_sha
test_skips_when_patch_has_changed_since_review
test_skips_when_base_only_patch_changes_right_before_review_submission
test_approves_when_base_changes_while_capturing_current_diff_and_patch_is_identical
test_skips_when_base_changes_during_diff_capture_and_retry_reveals_new_patch
test_skips_when_automerge_label_is_removed_before_review_submission
test_approves_when_current_diff_stabilizes_after_a_stale_first_read
test_fails_closed_when_current_pr_reload_errors_during_diff_capture
test_keeps_review_when_post_approval_revalidation_sees_approved_state
test_waits_for_review_decision_to_catch_up_after_review_creation
test_keeps_review_when_created_review_confirms_latest_head_approval_before_review_decision_updates
test_dismisses_review_when_created_review_never_confirms_latest_head_approval
test_uses_fallback_token_when_primary_review_dismissal_fails
test_uses_fallback_token_for_pr_reads_and_diff_but_not_review_submission
test_fails_when_primary_review_submission_fails_without_reusing_fallback_token
test_fails_closed_when_review_dismissal_fails_even_with_fallback_token
test_keeps_review_when_post_approval_revalidation_sees_unknown_mergeable_state
test_reapproves_when_head_changes_after_post_approval_revalidation_before_first_confirmation
test_keeps_review_when_post_approval_refresh_sees_merged_pr
test_dismisses_review_when_post_approval_compare_errors_after_review_creation
test_dismisses_review_when_post_approval_refresh_errors_after_review_creation
test_keeps_merged_pr_successful_when_post_approval_refresh_errors_after_merge
test_keeps_merged_pr_successful_when_refresh_fails_after_compare_observes_merge
test_dismisses_review_when_pull_head_snapshot_sees_closed_pr_before_confirmation
test_dismisses_review_when_post_approval_refresh_sees_closed_pr
test_skips_when_post_approval_revalidation_sees_merged_pr_with_removed_automerge_label
test_dismisses_review_when_candidate_conditions_change_after_post_approval_refresh_on_closed_pr
test_dismisses_review_when_patch_changes_during_approval_submission
test_reapproves_when_review_decision_is_stale_after_head_changes_during_approval_submission
test_reapproves_latest_head_when_identical_rebase_happens_after_review_creation
test_dismisses_all_recorded_reviews_when_second_approval_becomes_invalid
test_dismisses_review_when_changes_are_requested_during_approval_submission
test_dismisses_review_when_candidate_conditions_change_after_post_approval_refresh
test_dismisses_review_when_candidate_conditions_change_after_post_approval_compare
test_dismisses_review_when_patch_changes_after_post_approval_refresh
test_retries_review_creation_when_head_rebases_but_patch_is_identical
test_retries_review_creation_when_mergeable_is_temporarily_unknown_after_commit_id_race
test_skips_when_review_creation_keeps_racing_with_identical_rebases
test_skips_when_review_creation_fails_and_candidate_conditions_change_after_refresh
test_uses_branch_protection_read_token_for_candidate_rechecks
test_skips_when_latest_check_run_is_a_queued_rerun
test_skips_when_status_rollup_still_points_to_previous_head
test_skips_when_fallback_status_rollup_reports_active_rerun_on_same_head
test_skips_when_graphql_rollup_reports_active_rerun_even_if_current_pr_has_matching_commit_oid
test_approves_npm_candidate_when_allowlisted_devdependency_is_patch_updated
test_skips_npm_candidate_when_external_claude_review_verdict_is_missing_before_approval
test_approves_npm_candidate_when_shared_claude_review_check_has_failed_but_current_head_verdict_is_not_required
test_approves_npm_candidate_when_branch_protection_requires_claude_review_but_verdict_requirement_is_disabled
test_skips_npm_candidate_when_package_json_changes_outside_dependency_fields
test_approves_non_npm_candidate_when_supported_manifests_only_change
test_approves_non_npm_candidate_when_diff_recheck_uses_fallback_token
test_skips_non_npm_candidate_when_claude_review_verdict_turns_fail_before_approval
test_approves_non_npm_candidate_when_claude_review_verdict_comment_author_is_not_github_actions_bot
test_skips_non_npm_candidate_when_claude_review_posts_inline_finding_before_approval
test_skips_non_npm_candidate_when_claude_review_posts_inline_finding_on_later_page_before_approval
test_skips_non_npm_candidate_when_current_head_has_inline_findings_from_older_claude_run
test_skips_non_npm_candidate_when_current_head_has_inline_findings_from_trusted_non_bot_verdict_publisher_before_approval
test_skips_non_npm_candidate_when_current_head_has_inline_findings_from_newer_claude_rerun_before_approval
test_approves_non_npm_candidate_when_trusted_verdict_comes_from_scheduled_catch_up_run
test_skips_non_npm_candidate_when_claude_review_check_is_pending
test_skips_non_npm_candidate_when_claude_review_has_not_reported_on_current_head
test_approves_non_npm_candidate_when_only_unrelated_optional_check_is_red
test_skips_npm_candidate_when_unrelated_optional_check_is_red
test_skips_non_npm_candidate_when_monitored_workflow_check_has_failed
test_skips_non_npm_candidate_when_unsupported_files_change
test_skips_non_npm_candidate_when_version_update_is_not_patch
test_skips_non_npm_candidate_when_title_claims_patch_but_diff_is_minor
test_skips_non_npm_candidate_when_patch_swaps_to_different_image_identity
test_approves_non_npm_candidate_when_composite_docker_tag_only_changes_trailing_patch_version
test_skips_non_npm_candidate_when_docker_from_changes_platform_or_alias
test_skips_non_npm_candidate_when_composite_docker_tag_changes_variant_alongside_patch_version
test_skips_non_npm_candidate_when_allowlisted_workflow_contains_unrelated_edits
test_skips_non_npm_candidate_when_auto_approve_workflow_itself_changes
test_skip_candidate_validation_mode_allows_non_npm_candidate

echo "approve_dependabot_pr tests passed"
