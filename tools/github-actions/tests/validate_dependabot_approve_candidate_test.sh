#!/usr/bin/env bash

set -euo pipefail

script_dir="$(
    cd "$(dirname "${BASH_SOURCE[0]}")" && pwd
)"
readonly script_dir
readonly target_script="${script_dir}/../validate_dependabot_approve_candidate.sh"

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

write_non_npm_pr_json() {
    local file_path="$1"
    local title="$2"
    local target_path="$3"

    cat <<EOF > "${file_path}"
{
  "state": "OPEN",
  "number": 123,
  "title": "${title}",
  "url": "https://example.com/pull/123",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "baseRefOid": "base-sha-1",
  "headRefName": "dependabot/non-npm/example",
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
    { "path": "${target_path}" }
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

write_github_actions_anonymous_step_patch() {
    local file_path="$1"
    local action_name="$2"
    local before_version="$3"
    local after_version="$4"

    cat <<EOF > "${file_path}"
diff --git a/.github/workflows/manually-cleanup-artifacts.yml b/.github/workflows/manually-cleanup-artifacts.yml
--- a/.github/workflows/manually-cleanup-artifacts.yml
+++ b/.github/workflows/manually-cleanup-artifacts.yml
@@ -10,7 +10,7 @@ jobs:
-    - uses: ${action_name}@${before_version}
+    - uses: ${action_name}@${after_version}
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

write_github_actions_identity_swap_patch() {
    local file_path="$1"
    local before_action_name="$2"
    local before_version="$3"
    local after_action_name="$4"
    local after_version="$5"

    cat <<EOF > "${file_path}"
diff --git a/.github/workflows/manually-cleanup-artifacts.yml b/.github/workflows/manually-cleanup-artifacts.yml
--- a/.github/workflows/manually-cleanup-artifacts.yml
+++ b/.github/workflows/manually-cleanup-artifacts.yml
@@ -10,7 +10,7 @@ jobs:
-      uses: ${before_action_name}@${before_version}
+      uses: ${after_action_name}@${after_version}
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

setup_fake_gh() {
    local bin_dir="$1"

    mkdir -p "${bin_dir}"
    cat <<'EOF' > "${bin_dir}/gh"
#!/usr/bin/env bash

set -euo pipefail

readonly state_dir="${FAKE_GH_STATE_DIR:?}"

select_current_pr_path() {
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
                cat "${state_dir}/current_pr.json"
                ;;
            diff)
                cat "${state_dir}/current_diff.patch"
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
            if [[ -f "${state_dir}/status_rollup.json" ]]; then
                cat "${state_dir}/status_rollup.json"
                exit 0
            fi

            if [[ -f "${state_dir}/current_pr.json" ]]; then
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
                }' "${state_dir}/current_pr.json"
                exit 0
            fi

            echo "unexpected gh api graphql invocation" >&2
            exit 1
        fi

        if [[ "${endpoint}" == "repos/falgon/roki-web/compare/develop...head-sha-1" ]]; then
            cat "${state_dir}/compare.json"
            exit 0
        fi

        if [[ "${endpoint}" == "repos/falgon/roki-web/contents/package.json?ref="* ]]; then
            ref="${endpoint##*ref=}"
            cat "${state_dir}/package_json_${ref}.json"
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

            if [[ "${page_number}" != "1" ]]; then
                printf '[]\n'
                exit 0
            fi

            printf '[]\n'
            exit 0
        fi

        if [[ "${endpoint}" == "repos/falgon/roki-web/collaborators/"*"/permission" ]]; then
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
            current_head_sha="head-sha-1"
            current_base_sha="base-sha-1"
            if [[ -n "${selected_pr_path}" && -f "${selected_pr_path}" ]]; then
                current_head_sha="$(jq -r '.headRefOid // "head-sha-1"' "${selected_pr_path}")"
                current_base_sha="$(jq -r '.baseRefOid // "base-sha-1"' "${selected_pr_path}")"
            fi

            jq -cn \
                --arg head_sha "${current_head_sha}" \
                --arg base_sha "${current_base_sha}" '
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

        cat "${state_dir}/protection.json"
        ;;
    *)
        echo "unexpected gh invocation: $*" >&2
        exit 1
        ;;
esac
EOF
    chmod +x "${bin_dir}/gh"
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

run_validate_script() {
    local case_dir="$1"
    local candidate_kind="${2:-non-npm}"

    prepare_case_workflow_allowlist "${case_dir}"

    (
        export PATH="${case_dir}/bin:${PATH}"
        export FAKE_GH_STATE_DIR="${case_dir}/state"
        export DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="${DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS:-,}"
        export GH_TOKEN="test-gh-token"
        export GITHUB_OUTPUT="${case_dir}/github_output.txt"
        export GITHUB_REPOSITORY="falgon/roki-web"
        export PR_NUMBER=123
        export DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND="${candidate_kind}"
        if [[ -f "${case_dir}/workflow_allowlist.txt" ]]; then
            export DEPENDABOT_AUTO_APPROVE_NON_NPM_WORKFLOW_ALLOWLIST_PATH="${case_dir}/workflow_allowlist.txt"
        fi
        bash "${target_script}" \
            2>"${case_dir}/stderr.txt"
    )
}

run_validate_script_with_default_monitored_contexts() {
    local case_dir="$1"
    local candidate_kind="${2:-non-npm}"

    prepare_case_workflow_allowlist "${case_dir}"

    (
        export PATH="${case_dir}/bin:${PATH}"
        export FAKE_GH_STATE_DIR="${case_dir}/state"
        unset DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS
        unset DEPENDABOT_AUTO_APPROVE_MONITORED_CONTEXTS
        export GH_TOKEN="test-gh-token"
        export GITHUB_OUTPUT="${case_dir}/github_output.txt"
        export GITHUB_REPOSITORY="falgon/roki-web"
        export PR_NUMBER=123
        export DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND="${candidate_kind}"
        if [[ -f "${case_dir}/workflow_allowlist.txt" ]]; then
            export DEPENDABOT_AUTO_APPROVE_NON_NPM_WORKFLOW_ALLOWLIST_PATH="${case_dir}/workflow_allowlist.txt"
        fi
        bash "${target_script}" \
            2>"${case_dir}/stderr.txt"
    )
}

test_accepts_non_npm_patch_update_from_diff_even_when_title_is_generic() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
}

test_accepts_non_npm_candidate_when_default_monitored_contexts_do_not_wait_for_claude_review_status() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump Docker base image" \
        "docker/Dockerfile"
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
            ]
          }
        }
      }
    }
  }
}
EOF
    write_dockerfile_patch \
        "${case_dir}/state/current_diff.patch" \
        "node:20.18.1" \
        "node:20.18.2"

    run_validate_script_with_default_monitored_contexts "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
}

test_skips_non_npm_candidate_when_claude_review_verdict_is_missing_for_current_head() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    printf '[]\n' > "${case_dir}/state/issue_comments.json"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "machine-readable な Claude review verdict comment がまだありません"
}

test_accepts_non_npm_candidate_when_claude_review_verdict_comment_author_is_not_github_actions_bot() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
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

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
}

test_accepts_non_npm_candidate_when_latest_verdict_comment_is_untrusted_but_older_trusted_verdict_matches_current_head() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    jq -cn '
        [
            {
                id: 2,
                updated_at: "2026-03-30T00:15:00Z",
                user: { login: "external-user" },
                body: (
                    "<!-- dependabot-claude-review-verdict -->\n\n```json\n" +
                    ({
                        pr_number: 123,
                        head_sha: "head-sha-1",
                        base_sha: "base-sha-1",
                        review_status: "pass",
                        finding_count: 0,
                        summary: "Forged latest verdict.",
                        workflow_run_id: 9999,
                        workflow_run_attempt: 1
                    } | tojson) +
                    "\n```"
                )
            },
            {
                id: 1,
                updated_at: "2026-03-30T00:10:00Z",
                user: { login: "github-actions[bot]" },
                body: (
                    "<!-- dependabot-claude-review-verdict -->\n\n```json\n" +
                    ({
                        pr_number: 123,
                        head_sha: "head-sha-1",
                        base_sha: "base-sha-1",
                        review_status: "pass",
                        finding_count: 0,
                        summary: "Trusted verdict.",
                        workflow_run_id: 1001,
                        workflow_run_attempt: 2
                    } | tojson) +
                    "\n```"
                )
            }
        ]
    ' > "${case_dir}/state/issue_comments.json"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
}

test_skips_non_npm_candidate_when_latest_trusted_verdict_for_current_head_fails() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    jq -cn '
        [
            {
                id: 2,
                updated_at: "2026-03-30T00:15:00Z",
                user: { login: "github-actions[bot]" },
                body: (
                    "<!-- dependabot-claude-review-verdict -->\n\n```json\n" +
                    ({
                        pr_number: 123,
                        head_sha: "head-sha-1",
                        base_sha: "base-sha-1",
                        review_status: "fail",
                        finding_count: 1,
                        summary: "Latest trusted verdict found a blocker.",
                        workflow_run_id: 1002,
                        workflow_run_attempt: 3
                    } | tojson) +
                    "\n```"
                )
            },
            {
                id: 1,
                updated_at: "2026-03-30T00:10:00Z",
                user: { login: "github-actions[bot]" },
                body: (
                    "<!-- dependabot-claude-review-verdict -->\n\n```json\n" +
                    ({
                        pr_number: 123,
                        head_sha: "head-sha-1",
                        base_sha: "base-sha-1",
                        review_status: "pass",
                        finding_count: 0,
                        summary: "Older trusted verdict.",
                        workflow_run_id: 1001,
                        workflow_run_attempt: 2
                    } | tojson) +
                    "\n```"
                )
            }
        ]
    ' > "${case_dir}/state/issue_comments.json"
    write_workflow_run_json \
        "${case_dir}/state/workflow_run_1002.json" \
        "3" \
        "2026-03-30T00:10:00Z" \
        "2026-03-30T00:20:00Z" \
        "head-sha-1"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "zero findings を明示していない"
    assert_contains "${case_dir}/github_output.txt" "status=fail"
}

test_accepts_allowlisted_npm_devdependency_patch_update() {
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

    run_validate_script "${case_dir}" "npm"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
}

test_skips_npm_candidate_when_external_claude_review_verdict_is_missing() {
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
    printf '[]\n' > "${case_dir}/state/issue_comments.json"

    run_validate_script "${case_dir}" "npm"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "machine-readable な Claude review verdict comment がまだありません"
}

test_accepts_npm_candidate_when_shared_claude_review_check_has_failed_but_current_head_verdict_is_not_required() {
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

    (
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="claude-review" \
        DEPENDABOT_AUTO_APPROVE_REQUIRE_CURRENT_HEAD_CLAUDE_REVIEW_VERDICT=false \
        run_validate_script "${case_dir}" "npm"
    )

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
}

test_skips_npm_candidate_when_external_claude_review_verdict_fails() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        '[{ "path": "package-lock.json" }]'
    write_claude_review_verdict_comments_json \
        "${case_dir}/state/issue_comments.json" \
        "head-sha-1" \
        "fail" \
        "1" \
        "Blocking finding"

    run_validate_script "${case_dir}" "npm"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "Claude review"
    assert_contains "${case_dir}/github_output.txt" "status=fail"
}

test_skips_npm_candidate_when_external_claude_review_posts_inline_finding() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        '[{ "path": "package-lock.json" }]'
    write_claude_review_inline_comments_json \
        "${case_dir}/state/review_comments.json" \
        "head-sha-1"

    run_validate_script "${case_dir}" "npm"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "inline finding が 1 件ある"
}

test_accepts_npm_candidate_when_branch_protection_requires_claude_review_but_verdict_requirement_is_disabled() {
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

    (
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="claude-review" \
        DEPENDABOT_AUTO_APPROVE_REQUIRE_CURRENT_HEAD_CLAUDE_REVIEW_VERDICT=false \
        run_validate_script "${case_dir}" "npm"
    )

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
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

    run_validate_script "${case_dir}" "npm"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "package.json の依存バージョン以外も変更している"
}

test_skips_npm_candidate_when_dependency_is_added() {
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
  "devDependencies": {
    "vitest": "^1.6.0"
  }
}
EOF
    cat <<'EOF' > "${case_dir}/state/package_json_head-sha-1.json"
{
  "private": true,
  "devDependencies": {
    "@types/node": "^22.12.0",
    "vitest": "^1.6.0"
  }
}
EOF

    run_validate_script "${case_dir}" "npm"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "依存関係を追加または削除している"
}

test_skips_npm_candidate_when_non_dev_dependency_changes() {
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
  "dependencies": {
    "react": "^18.3.1"
  },
  "devDependencies": {
    "vitest": "^1.6.0"
  }
}
EOF
    cat <<'EOF' > "${case_dir}/state/package_json_head-sha-1.json"
{
  "private": true,
  "dependencies": {
    "react": "^18.3.2"
  },
  "devDependencies": {
    "vitest": "^1.6.0"
  }
}
EOF

    run_validate_script "${case_dir}" "npm"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "devDependencies 以外を更新している"
}

test_skips_npm_candidate_when_devdependency_is_not_allowlisted() {
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
  "devDependencies": {
    "eslint": "^9.25.0"
  }
}
EOF
    cat <<'EOF' > "${case_dir}/state/package_json_head-sha-1.json"
{
  "private": true,
  "devDependencies": {
    "eslint": "^9.25.1"
  }
}
EOF

    run_validate_script "${case_dir}" "npm"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "許可リスト外の devDependencies"
    assert_contains "${case_dir}/github_output.txt" "eslint"
}

test_skips_npm_candidate_when_update_is_not_patch_level() {
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
  "devDependencies": {
    "vitest": "^1.6.0"
  }
}
EOF
    cat <<'EOF' > "${case_dir}/state/package_json_head-sha-1.json"
{
  "private": true,
  "devDependencies": {
    "vitest": "^1.7.0"
  }
}
EOF

    run_validate_script "${case_dir}" "npm"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "patch update のみではない"
}

test_skips_npm_candidate_when_biome_json_changes_without_biome_update() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        '[{ "path": "package.json" }, { "path": "package-lock.json" }, { "path": "biome.json" }]'
    write_compare_json "${case_dir}/state/compare.json" "merge-base-sha-1"
    cat <<'EOF' > "${case_dir}/state/package_json_merge-base-sha-1.json"
{
  "private": true,
  "devDependencies": {
    "vitest": "^1.6.0"
  }
}
EOF
    cat <<'EOF' > "${case_dir}/state/package_json_head-sha-1.json"
{
  "private": true,
  "devDependencies": {
    "vitest": "^1.6.1"
  }
}
EOF

    run_validate_script "${case_dir}" "npm"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "@biomejs/biome 以外の更新で biome.json を変更している"
}

test_skips_when_monitored_workflow_checks_have_not_reported_yet() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    (
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="build,lint-and-test,Analyze (javascript),restyled,claude-review" \
        run_validate_script "${case_dir}"
    )

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "必須または監視対象チェックがまだ出揃っていません"
    assert_contains "${case_dir}/github_output.txt" "build"
    assert_contains "${case_dir}/github_output.txt" "Analyze (javascript)"
    assert_contains "${case_dir}/github_output.txt" "restyled"
    assert_contains "${case_dir}/github_output.txt" "claude-review"
    assert_not_contains "${case_dir}/github_output.txt" "upload"
    assert_not_contains "${case_dir}/github_output.txt" "boot-circle-ci"
}

test_skips_when_monitored_workflow_check_has_failed() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
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
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    (
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="build" \
        run_validate_script "${case_dir}"
    )

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "監視対象チェックが未成功です"
    assert_contains "${case_dir}/github_output.txt" "build:FAILURE"
}

test_skips_non_npm_candidate_when_claude_review_check_is_pending() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
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
                "name": "claude-review (123)",
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
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    (
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="claude-review" \
        run_validate_script "${case_dir}"
    )

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "監視対象チェックが未成功です"
    assert_contains "${case_dir}/github_output.txt" "claude-review:IN_PROGRESS"
}

test_skips_non_npm_candidate_when_claude_review_has_not_reported_on_current_head() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
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
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    (
        DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS="claude-review" \
        run_validate_script "${case_dir}"
    )

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "必須または監視対象チェックがまだ出揃っていません"
    assert_contains "${case_dir}/github_output.txt" "claude-review"
}

test_skips_non_npm_candidate_when_claude_review_verdict_fails_for_current_head() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
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

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "Claude review"
    assert_contains "${case_dir}/github_output.txt" "status=fail"
}

test_skips_non_npm_candidate_when_claude_review_verdict_base_sha_is_stale() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
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
        "github-actions[bot]" \
        "base-sha-stale"
    write_workflow_run_json "${case_dir}/state/workflow_run_1001.json"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "現在の BASE (base-sha-1)"
}

test_skips_non_npm_candidate_when_claude_review_posts_inline_finding_for_current_head() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"
    write_claude_review_inline_comments_json \
        "${case_dir}/state/review_comments.json" \
        "head-sha-1"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "inline finding が 1 件"
}

test_skips_non_npm_candidate_when_claude_review_posts_inline_finding_on_later_page_for_current_head() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
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

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "inline finding が 1 件"
}

test_skips_non_npm_candidate_when_current_head_has_inline_findings_from_older_claude_run() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
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

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "inline finding が 1 件"
}

test_skips_non_npm_candidate_when_current_head_has_inline_findings_from_trusted_non_bot_verdict_publisher() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
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

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "inline finding が 1 件"
}

test_skips_non_npm_candidate_when_current_head_has_inline_findings_from_newer_claude_rerun_after_pass_verdict() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
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

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "inline finding が 1 件"
}

test_skips_non_npm_candidate_when_trusted_verdict_comment_is_outside_claude_review_run_window() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
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
        "3003" \
        "7" \
        "2026-03-30T00:25:00Z"
    write_workflow_run_json \
        "${case_dir}/state/workflow_run_3003.json" \
        "7" \
        "2026-03-30T00:10:00Z" \
        "2026-03-30T00:20:00Z" \
        "head-sha-1"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "workflow run metadata が verdict と一致しない"
}

test_accepts_non_npm_candidate_when_trusted_verdict_comes_from_scheduled_catch_up_run() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
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

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
}

test_accepts_non_npm_candidate_when_only_unrelated_optional_check_is_red() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
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
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_not_contains "${case_dir}/github_output.txt" "label:FAILURE"
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

    run_validate_script "${case_dir}" npm

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "未成功のチェックがあります"
    assert_contains "${case_dir}/github_output.txt" "label:FAILURE"
}

test_skips_when_title_claims_patch_but_diff_is_minor() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump ubuntu from 24.04 to 24.04.1" \
        "docker/Dockerfile"
    write_dockerfile_patch \
        "${case_dir}/state/current_diff.patch" \
        "ubuntu:24.04" \
        "ubuntu:24.10"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "non-npm 依存の minor update"
}

test_accepts_when_docker_from_patch_only_changes_version() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump ubuntu from 24.04 to 24.04.1" \
        "docker/Dockerfile"
    write_dockerfile_from_line_patch \
        "${case_dir}/state/current_diff.patch" \
        "FROM --platform=linux/amd64 ubuntu:24.04 AS base" \
        "FROM --platform=linux/amd64 ubuntu:24.04.1 AS base"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
}

test_accepts_when_composite_docker_tag_only_changes_trailing_patch_version() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump node image patch version" \
        "docker/Dockerfile"
    write_dockerfile_patch \
        "${case_dir}/state/current_diff.patch" \
        "node:20-alpine3.20.1" \
        "node:20-alpine3.20.2"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
}

test_accepts_when_composite_docker_tag_only_changes_embedded_patch_version() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported container image" \
        "docker/Dockerfile"
    write_dockerfile_patch \
        "${case_dir}/state/current_diff.patch" \
        "ghcr.io/example/app:1.2.3-alpine3.20" \
        "ghcr.io/example/app:1.2.4-alpine3.20"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
}

test_accepts_patch_versions_with_leading_zero_segments_without_bash_octal_warnings() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "v2024.08.1" \
        "v2024.08.2"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
    assert_not_contains "${case_dir}/stderr.txt" "value too great for base"
}

test_skips_when_docker_from_patch_changes_platform_or_alias() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump ubuntu from 24.04 to 24.04.1" \
        "docker/Dockerfile"
    write_dockerfile_from_line_patch \
        "${case_dir}/state/current_diff.patch" \
        "FROM ubuntu:24.04 AS base" \
        "FROM --platform=linux/amd64 ubuntu:24.04.1 AS runtime"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "patch 相当と判定できない"
}

test_skips_when_non_npm_patch_swaps_to_a_different_action_identity() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
    write_github_actions_identity_swap_patch \
        "${case_dir}/state/current_diff.patch" \
        "docker/login-action" \
        "v3.1.0" \
        "docker/build-push-action" \
        "v3.1.1"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "patch 相当と判定できない"
}

test_skips_when_composite_action_tag_changes_variant_alongside_patch_version() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "owner/example-action" \
        "v1.2.3-node20" \
        "v1.2.4-node21"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "patch 相当と判定できない"
}

test_skips_when_allowlisted_workflow_contains_unrelated_edits() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
    write_github_actions_patch_with_unrelated_edit \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "patch 相当と判定できない"
}

test_skips_when_non_npm_candidate_updates_auto_approve_workflow() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/dependabot-auto-approve.yml"
    write_github_actions_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "GitHub Actions / Docker の依存 manifest 以外を変更しています"
    assert_contains "${case_dir}/github_output.txt" ".github/workflows/dependabot-auto-approve.yml"
}

test_skips_when_non_npm_candidate_updates_approval_gate_workflows() {
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
  "headRefName": "dependabot/non-npm/example",
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
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
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
}

test_accepts_non_npm_patch_update_from_anonymous_uses_step() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
    write_github_actions_anonymous_step_patch \
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=true"
    assert_line_equals "${case_dir}/github_output.txt" "skip-reason="
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
  "headRefName": "dependabot/non-npm/example",
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
                "status": "QUEUED",
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
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "lint-and-test:QUEUED"
}

test_skips_when_graphql_rollup_reports_active_rerun_even_if_pr_view_has_matching_commit_oid() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"
    write_protection_json "${case_dir}/state/protection.json"
    write_non_npm_pr_json \
        "${case_dir}/state/current_pr.json" \
        "Bump supported workflow dependency" \
        ".github/workflows/manually-cleanup-artifacts.yml"
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
                "status": "QUEUED",
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
        "${case_dir}/state/current_diff.patch" \
        "actions/checkout" \
        "4.2.1" \
        "4.2.2"

    run_validate_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "candidate=false"
    assert_contains "${case_dir}/github_output.txt" "lint-and-test:QUEUED"
}

test_accepts_non_npm_patch_update_from_diff_even_when_title_is_generic
test_skips_non_npm_candidate_when_claude_review_verdict_is_missing_for_current_head
test_accepts_non_npm_candidate_when_claude_review_verdict_comment_author_is_not_github_actions_bot
test_accepts_non_npm_candidate_when_latest_verdict_comment_is_untrusted_but_older_trusted_verdict_matches_current_head
test_skips_non_npm_candidate_when_latest_trusted_verdict_for_current_head_fails
test_accepts_allowlisted_npm_devdependency_patch_update
test_skips_npm_candidate_when_external_claude_review_verdict_is_missing
test_accepts_npm_candidate_when_shared_claude_review_check_has_failed_but_current_head_verdict_is_not_required
test_skips_npm_candidate_when_external_claude_review_verdict_fails
test_skips_npm_candidate_when_external_claude_review_posts_inline_finding
test_accepts_npm_candidate_when_branch_protection_requires_claude_review_but_verdict_requirement_is_disabled
test_skips_npm_candidate_when_package_json_changes_outside_dependency_fields
test_skips_npm_candidate_when_dependency_is_added
test_skips_npm_candidate_when_non_dev_dependency_changes
test_skips_npm_candidate_when_devdependency_is_not_allowlisted
test_skips_npm_candidate_when_update_is_not_patch_level
test_skips_npm_candidate_when_biome_json_changes_without_biome_update
test_accepts_non_npm_candidate_when_default_monitored_contexts_do_not_wait_for_claude_review_status
test_skips_when_monitored_workflow_checks_have_not_reported_yet
test_skips_when_monitored_workflow_check_has_failed
test_skips_non_npm_candidate_when_claude_review_check_is_pending
test_skips_non_npm_candidate_when_claude_review_has_not_reported_on_current_head
test_skips_non_npm_candidate_when_claude_review_verdict_fails_for_current_head
test_skips_non_npm_candidate_when_claude_review_verdict_base_sha_is_stale
test_skips_non_npm_candidate_when_claude_review_posts_inline_finding_for_current_head
test_skips_non_npm_candidate_when_claude_review_posts_inline_finding_on_later_page_for_current_head
test_skips_non_npm_candidate_when_current_head_has_inline_findings_from_older_claude_run
test_skips_non_npm_candidate_when_current_head_has_inline_findings_from_trusted_non_bot_verdict_publisher
test_skips_non_npm_candidate_when_current_head_has_inline_findings_from_newer_claude_rerun_after_pass_verdict
test_skips_non_npm_candidate_when_trusted_verdict_comment_is_outside_claude_review_run_window
test_accepts_non_npm_candidate_when_trusted_verdict_comes_from_scheduled_catch_up_run
test_accepts_non_npm_candidate_when_only_unrelated_optional_check_is_red
test_skips_npm_candidate_when_unrelated_optional_check_is_red
test_skips_when_title_claims_patch_but_diff_is_minor
test_accepts_patch_versions_with_leading_zero_segments_without_bash_octal_warnings
test_accepts_when_docker_from_patch_only_changes_version
test_accepts_when_composite_docker_tag_only_changes_trailing_patch_version
test_accepts_when_composite_docker_tag_only_changes_embedded_patch_version
test_skips_when_docker_from_patch_changes_platform_or_alias
test_skips_when_non_npm_patch_swaps_to_a_different_action_identity
test_skips_when_composite_action_tag_changes_variant_alongside_patch_version
test_skips_when_allowlisted_workflow_contains_unrelated_edits
test_skips_when_non_npm_candidate_updates_auto_approve_workflow
test_accepts_non_npm_patch_update_from_anonymous_uses_step
test_skips_when_fallback_status_rollup_reports_active_rerun_on_same_head
test_skips_when_graphql_rollup_reports_active_rerun_even_if_pr_view_has_matching_commit_oid

echo "validate_dependabot_approve_candidate tests passed"
