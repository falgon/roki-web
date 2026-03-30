#!/usr/bin/env bash

set -euo pipefail

script_dir="$(
    cd "$(dirname "${BASH_SOURCE[0]}")" && pwd
)"
readonly script_dir
readonly target_script="${script_dir}/../collect_dependabot_prs.sh"

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

setup_fake_gh() {
    local bin_dir="$1"

    mkdir -p "${bin_dir}"
    cat <<'EOF' > "${bin_dir}/gh"
#!/usr/bin/env bash

set -euo pipefail

readonly state_dir="${FAKE_GH_STATE_DIR:?}"

case "${1:-}" in
    pr)
        case "${2:-}" in
            view)
                pull_number="${3:?}"
                cat "${state_dir}/pr_${pull_number}.json"
                ;;
            *)
                echo "unexpected gh pr invocation: $*" >&2
                exit 1
                ;;
        esac
        ;;
    *)
        echo "unexpected gh invocation: $*" >&2
        exit 1
        ;;
esac
EOF
    chmod +x "${bin_dir}/gh"
}

run_collect_script() {
    local case_dir="$1"

    PATH="${case_dir}/bin:${PATH}" \
    FAKE_GH_STATE_DIR="${case_dir}/state" \
    GITHUB_OUTPUT="${case_dir}/github_output.txt" \
    GITHUB_EVENT_NAME=workflow_run \
    GITHUB_EVENT_PATH="${case_dir}/event.json" \
    INPUT_DRY_RUN=false \
    bash "${target_script}"
}

test_collects_only_dependabot_npm_prs_from_workflow_run() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"

    cat <<'EOF' > "${case_dir}/event.json"
{
  "workflow_run": {
    "conclusion": "success",
    "event": "pull_request",
    "pull_requests": [
      { "number": 101 },
      { "number": 102 },
      { "number": 103 }
    ]
  }
}
EOF

    cat <<'EOF' > "${case_dir}/state/pr_101.json"
{
  "number": 101,
  "state": "OPEN",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "labels": [
    { "name": "dependabot/npm" },
    { "name": "automerge" }
  ]
}
EOF

    cat <<'EOF' > "${case_dir}/state/pr_102.json"
{
  "number": 102,
  "state": "OPEN",
  "author": { "login": "dependabot[bot]" },
  "baseRefName": "develop",
  "labels": [
    { "name": "automerge" }
  ]
}
EOF

    cat <<'EOF' > "${case_dir}/state/pr_103.json"
{
  "number": 103,
  "state": "OPEN",
  "author": { "login": "someone-else" },
  "baseRefName": "develop",
  "labels": [
    { "name": "dependabot/npm" },
    { "name": "automerge" }
  ]
}
EOF

    run_collect_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "pr-numbers=[101]"
    assert_line_equals "${case_dir}/github_output.txt" "dry-run=false"
}

test_returns_empty_for_non_pull_request_workflow_run() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"

    cat <<'EOF' > "${case_dir}/event.json"
{
  "workflow_run": {
    "conclusion": "success",
    "event": "push",
    "pull_requests": [
      { "number": 101 }
    ]
  }
}
EOF

    run_collect_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "pr-numbers=[]"
    assert_line_equals "${case_dir}/github_output.txt" "dry-run=false"
}

test_returns_empty_for_unsuccessful_pull_request_workflow_run() {
    local case_dir
    case_dir="$(mktemp -d)"

    mkdir -p "${case_dir}/state"
    setup_fake_gh "${case_dir}/bin"

    cat <<'EOF' > "${case_dir}/event.json"
{
  "workflow_run": {
    "conclusion": "failure",
    "event": "pull_request",
    "pull_requests": [
      { "number": 101 }
    ]
  }
}
EOF

    run_collect_script "${case_dir}"

    assert_line_equals "${case_dir}/github_output.txt" "pr-numbers=[]"
    assert_line_equals "${case_dir}/github_output.txt" "dry-run=false"
}

test_collects_only_dependabot_npm_prs_from_workflow_run
test_returns_empty_for_non_pull_request_workflow_run
test_returns_empty_for_unsuccessful_pull_request_workflow_run

echo "collect_dependabot_prs tests passed"
