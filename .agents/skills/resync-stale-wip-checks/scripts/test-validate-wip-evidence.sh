#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
filter_file="${script_dir}/validate-wip-evidence.jq"
head_sha="aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

run_success() {
    local fixture="$1"
    local expected="$2"
    local actual
    actual="$(jq -c -f "${filter_file}" <<<"${fixture}")"
    jq -e --argjson expected "${expected}" '. == $expected' <<<"${actual}" >/dev/null
}

run_failure() {
    local fixture="$1"
    local rc
    set +e
    jq -e -f "${filter_file}" <<<"${fixture}" >/dev/null 2>&1
    rc=$?
    set -e
    [[ "${rc}" -eq 5 ]]
}

run_success \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"check_suite_total_count\":0,\"status_pages\":[[]],\"check_pages\":[{\"total_count\":0,\"check_runs\":[]}]}" \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"status_ids\":[],\"latest_statuses\":[],\"wip_statuses\":[],\"wip_check_runs\":[],\"exact_wip_statuses\":[],\"exact_wip_check_runs\":[]}"

run_success \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"check_suite_total_count\":0,\"status_pages\":[[{\"context\":\"WIP\",\"state\":\"success\",\"created_at\":\"2026-08-05T00:00:00Z\",\"id\":3}]],\"check_pages\":[{\"total_count\":0,\"check_runs\":[]}]}" \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"status_ids\":[3],\"latest_statuses\":[{\"context\":\"WIP\",\"state\":\"success\",\"created_at\":\"2026-08-05T00:00:00Z\",\"id\":3}],\"wip_statuses\":[{\"context\":\"WIP\",\"state\":\"success\",\"created_at\":\"2026-08-05T00:00:00Z\",\"id\":3}],\"wip_check_runs\":[],\"exact_wip_statuses\":[{\"context\":\"WIP\",\"state\":\"success\",\"created_at\":\"2026-08-05T00:00:00Z\",\"id\":3}],\"exact_wip_check_runs\":[]}"

run_success \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"check_suite_total_count\":1,\"status_pages\":[[]],\"check_pages\":[{\"total_count\":1,\"check_runs\":[{\"id\":7,\"name\":\"WIP\",\"head_sha\":\"${head_sha}\",\"status\":\"completed\",\"conclusion\":\"success\",\"app\":{\"id\":3414,\"slug\":\"wip\"}}]}]}" \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"status_ids\":[],\"latest_statuses\":[],\"wip_statuses\":[],\"wip_check_runs\":[{\"id\":7,\"name\":\"WIP\",\"head_sha\":\"${head_sha}\",\"status\":\"completed\",\"conclusion\":\"success\",\"app\":{\"id\":3414,\"slug\":\"wip\"}}],\"exact_wip_statuses\":[],\"exact_wip_check_runs\":[{\"id\":7,\"name\":\"WIP\",\"head_sha\":\"${head_sha}\",\"status\":\"completed\",\"conclusion\":\"success\",\"app\":{\"id\":3414,\"slug\":\"wip\"}}]}"

run_failure \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"check_suite_total_count\":1,\"status_pages\":[[]],\"check_pages\":[{\"total_count\":1,\"check_runs\":[{\"id\":8,\"name\":\"wip\",\"head_sha\":\"${head_sha}\",\"status\":\"completed\",\"conclusion\":\"failure\",\"app\":{\"id\":3414,\"slug\":\"wip\"}}]}]}"

run_failure \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"check_suite_total_count\":0,\"status_pages\":[[{\"context\":\"wip\",\"state\":\"failure\",\"created_at\":\"2026-08-05T00:00:00Z\",\"id\":2}]],\"check_pages\":[{\"total_count\":0,\"check_runs\":[]}]}"

run_failure \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"check_suite_total_count\":0,\"status_pages\":[[{\"context\":\"WIP\",\"state\":\"success\",\"created_at\":\"2026-08-05T00:00:00Z\",\"id\":2},{\"context\":\"wip\",\"state\":\"failure\",\"created_at\":\"2026-08-05T00:00:01Z\",\"id\":3}]],\"check_pages\":[{\"total_count\":0,\"check_runs\":[]}]}"

run_failure \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"check_suite_total_count\":0,\"status_pages\":{},\"check_pages\":[{\"total_count\":0,\"check_runs\":[]}]}"

run_failure \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"check_suite_total_count\":0,\"status_pages\":[],\"check_pages\":[{\"total_count\":0,\"check_runs\":[]}]}"

run_failure \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"check_suite_total_count\":0,\"status_pages\":[[{\"context\":\"WIP\",\"state\":\"success\",\"created_at\":\"bad\",\"id\":1}]],\"check_pages\":[{\"total_count\":0,\"check_runs\":[]}]}"

run_failure \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"check_suite_total_count\":0,\"status_pages\":[[{\"context\":\"build\",\"state\":\"success\",\"created_at\":\"2026-08-05T00:00:02Z\",\"id\":1},{\"context\":\"build\",\"state\":\"success\",\"created_at\":\"2026-08-05T00:00:01Z\",\"id\":2}]],\"check_pages\":[{\"total_count\":0,\"check_runs\":[]}]}"

run_failure \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"check_suite_total_count\":0,\"status_pages\":[[]],\"check_pages\":[{\"total_count\":1,\"check_runs\":[]}]}"

run_failure \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"check_suite_total_count\":0,\"status_pages\":[[]],\"check_pages\":[{\"total_count\":1,\"check_runs\":[{\"id\":1,\"name\":\"WIP\",\"head_sha\":\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",\"status\":\"completed\",\"conclusion\":\"success\",\"app\":{\"id\":3414,\"slug\":\"wip\"}}]}]}"

run_failure \
    "{\"head_sha\":\"${head_sha}\",\"required_context\":\"WIP\",\"check_suite_total_count\":1000,\"status_pages\":[[]],\"check_pages\":[{\"total_count\":0,\"check_runs\":[]}]}"

echo "validate-wip-evidence fixtures passed"
