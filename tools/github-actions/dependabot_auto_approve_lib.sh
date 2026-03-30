#!/usr/bin/env bash

dependabot_auto_approve_lib_dir="$(
    cd "$(dirname "${BASH_SOURCE[0]}")" && pwd
)"
readonly dependabot_auto_approve_lib_dir

normalize_patch_body_ignoring_transient_metadata() {
    local patch_path="$1"
    local normalized_path="$2"

    awk '
        /^index / { next }
        /^@@/ { next }
        { print }
    ' "${patch_path}" > "${normalized_path}"
}

extract_patch_hunk_headers() {
    local patch_path="$1"
    local headers_path="$2"

    awk '
        /^@@/ { print }
    ' "${patch_path}" > "${headers_path}"
}

patches_match_ignoring_transient_metadata() {
    local first_patch_path="$1"
    local second_patch_path="$2"
    local normalized_first_patch_path
    local normalized_second_patch_path
    local comparison_status

    normalized_first_patch_path="$(mktemp)"
    normalized_second_patch_path="$(mktemp)"

    normalize_patch_body_ignoring_transient_metadata "${first_patch_path}" "${normalized_first_patch_path}"
    normalize_patch_body_ignoring_transient_metadata "${second_patch_path}" "${normalized_second_patch_path}"

    if cmp -s "${normalized_first_patch_path}" "${normalized_second_patch_path}"; then
        comparison_status=0
    else
        comparison_status=1
    fi

    rm -f "${normalized_first_patch_path}" "${normalized_second_patch_path}"
    return "${comparison_status}"
}

patches_match_for_review() {
    local first_patch_path="$1"
    local second_patch_path="$2"
    local first_hunk_headers_path
    local second_hunk_headers_path
    local comparison_status

    if ! patches_match_ignoring_transient_metadata "${first_patch_path}" "${second_patch_path}"; then
        return 1
    fi

    first_hunk_headers_path="$(mktemp)"
    second_hunk_headers_path="$(mktemp)"

    extract_patch_hunk_headers "${first_patch_path}" "${first_hunk_headers_path}"
    extract_patch_hunk_headers "${second_patch_path}" "${second_hunk_headers_path}"

    if cmp -s "${first_hunk_headers_path}" "${second_hunk_headers_path}"; then
        comparison_status=0
    else
        comparison_status=1
    fi

    rm -f "${first_hunk_headers_path}" "${second_hunk_headers_path}"
    return "${comparison_status}"
}

run_gh_read_command() {
    if declare -F run_gh_with_review_fallback >/dev/null 2>&1; then
        run_gh_with_review_fallback "$@"
        return
    fi

    gh "$@"
}

load_candidate_pr_json() {
    local pull_number="$1"

    run_gh_read_command pr view "${pull_number}" \
        --json state,number,title,url,author,baseRefName,baseRefOid,headRefName,headRefOid,mergeable,reviewDecision,labels,statusCheckRollup,files
}

load_branch_protection_json() {
    local base_ref="$1"

    if [[ -n "${DEPENDABOT_AUTO_APPROVE_READ_BRANCH_PROTECTION_TOKEN:-}" ]]; then
        GH_TOKEN="${DEPENDABOT_AUTO_APPROVE_READ_BRANCH_PROTECTION_TOKEN}" \
            gh api "repos/{owner}/{repo}/branches/${base_ref}/protection"
        return
    fi

    run_gh_read_command api "repos/{owner}/{repo}/branches/${base_ref}/protection"
}

load_compare_json() {
    local base_ref="$1"
    local head_sha="$2"
    local repository="${GITHUB_REPOSITORY:-}"

    if [[ -z "${repository}" || "${repository}" != */* ]]; then
        echo "GITHUB_REPOSITORY must be set to OWNER/REPO" >&2
        return 1
    fi

    run_gh_read_command api "repos/${repository}/compare/${base_ref}...${head_sha}"
}

load_repository_file_text() {
    local path="$1"
    local ref="$2"
    local repository="${GITHUB_REPOSITORY:-}"

    if [[ -z "${repository}" || "${repository}" != */* ]]; then
        echo "GITHUB_REPOSITORY must be set to OWNER/REPO" >&2
        return 1
    fi

    run_gh_read_command api \
        "repos/${repository}/contents/${path}?ref=${ref}" \
        -H "Accept: application/vnd.github.raw"
}

load_issue_comments_json() {
    local pull_number="$1"
    local repository="${GITHUB_REPOSITORY:-}"

    if [[ -z "${repository}" || "${repository}" != */* ]]; then
        echo "GITHUB_REPOSITORY must be set to OWNER/REPO" >&2
        return 1
    fi

    load_paginated_rest_array_json \
        "repos/${repository}/issues/${pull_number}/comments?per_page=100"
}

load_review_comments_json() {
    local pull_number="$1"
    local repository="${GITHUB_REPOSITORY:-}"

    if [[ -z "${repository}" || "${repository}" != */* ]]; then
        echo "GITHUB_REPOSITORY must be set to OWNER/REPO" >&2
        return 1
    fi

    load_paginated_rest_array_json \
        "repos/${repository}/pulls/${pull_number}/comments?per_page=100"
}

load_workflow_run_json() {
    local workflow_run_id="$1"
    local repository="${GITHUB_REPOSITORY:-}"

    if [[ -z "${repository}" || "${repository}" != */* ]]; then
        echo "GITHUB_REPOSITORY must be set to OWNER/REPO" >&2
        return 1
    fi

    run_gh_read_command api \
        -H "Accept: application/vnd.github+json" \
        "repos/${repository}/actions/runs/${workflow_run_id}"
}

url_encode_component() {
    local value="$1"

    jq -rn --arg value "${value}" '$value | @uri'
}

load_collaborator_permission_json() {
    local collaborator_login="$1"
    local repository="${GITHUB_REPOSITORY:-}"
    local encoded_login

    if [[ -z "${repository}" || "${repository}" != */* ]]; then
        echo "GITHUB_REPOSITORY must be set to OWNER/REPO" >&2
        return 1
    fi

    encoded_login="$(url_encode_component "${collaborator_login}")"
    run_gh_read_command api \
        -H "Accept: application/vnd.github+json" \
        "repos/${repository}/collaborators/${encoded_login}/permission"
}

load_paginated_rest_array_json() {
    local endpoint="$1"
    local page=1
    local paged_endpoint
    local page_json
    local page_length
    local aggregated_json='[]'

    while :; do
        paged_endpoint="${endpoint}"
        if [[ "${paged_endpoint}" == *\?* ]]; then
            paged_endpoint="${paged_endpoint}&page=${page}"
        else
            paged_endpoint="${paged_endpoint}?page=${page}"
        fi

        if ! page_json="$(
            run_gh_read_command api \
                -H "Accept: application/vnd.github+json" \
                "${paged_endpoint}"
        )"; then
            return 1
        fi

        if ! jq -e 'type == "array"' <<<"${page_json}" >/dev/null 2>&1; then
            echo "Expected array response from ${paged_endpoint}" >&2
            return 1
        fi

        page_length="$(jq -r 'length' <<<"${page_json}")"
        aggregated_json="$(
            jq -cn \
                --argjson aggregated "${aggregated_json}" \
                --argjson page "${page_json}" \
                '$aggregated + $page'
        )"

        if [[ "${page_length}" -lt 100 ]]; then
            printf '%s\n' "${aggregated_json}"
            return 0
        fi

        page=$((page + 1))
    done
}

load_supported_non_npm_workflow_filenames_json() {
    local allowlist_path="${DEPENDABOT_AUTO_APPROVE_NON_NPM_WORKFLOW_ALLOWLIST_PATH:-${dependabot_auto_approve_lib_dir}/non_npm_workflow_manifest_allowlist.txt}"

    if [[ ! -f "${allowlist_path}" ]]; then
        printf '[]\n'
        return 0
    fi

    jq -Rn '
        [
            inputs
            | gsub("\r"; "")
            | gsub("^\\s+|\\s+$"; "")
            | select(length > 0 and (startswith("#") | not))
        ]
    ' < "${allowlist_path}"
}

resolve_candidate_kind() {
    local configured_kind="${1:-auto}"
    local has_npm_label="${2:-false}"
    local detected_kind="non-npm"

    if [[ "${has_npm_label}" == "true" ]]; then
        detected_kind="npm"
    fi

    case "${configured_kind}" in
        "" | auto)
            printf '%s\n' "${detected_kind}"
            ;;
        npm | non-npm)
            printf '%s\n' "${configured_kind}"
            ;;
        *)
            printf '%s\n' "${configured_kind}"
            ;;
    esac
}

extract_numeric_version_triplet_contexts() {
    local raw_version="$1"
    local remaining="${raw_version}"
    local search_offset=0
    local matched_triplet=""
    local match_prefix=""
    local match_offset=0
    local prefix=""
    local suffix=""
    local emitted_context="false"

    while [[ "${remaining}" =~ [0-9]+(\.[0-9]+){0,2} ]]; do
        matched_triplet="${BASH_REMATCH[0]}"
        match_prefix="${remaining%%"${matched_triplet}"*}"
        match_offset=$((search_offset + ${#match_prefix}))
        prefix="${raw_version:0:match_offset}"
        suffix="${raw_version:match_offset + ${#matched_triplet}}"
        IFS='.' read -r -a version_parts <<<"${matched_triplet}"
        printf '%s\x1f%s\x1f%s\x1f%s\x1f%s\n' \
            "${prefix}" \
            "${version_parts[0]}" \
            "${version_parts[1]:-0}" \
            "${version_parts[2]:-0}" \
            "${suffix}"
        emitted_context="true"
        search_offset=$((match_offset + ${#matched_triplet}))
        remaining="${raw_version:search_offset}"
    done

    [[ "${emitted_context}" == "true" ]]
}

trim_leading_whitespace() {
    local value="$1"

    value="${value#"${value%%[![:space:]]*}"}"
    printf '%s\n' "${value}"
}

strip_wrapping_delimiters() {
    local value="$1"
    local first_char=""
    local last_char=""

    value="${value%,}"
    if [[ -n "${value}" ]]; then
        first_char="${value:0:1}"
        last_char="${value: -1}"
    fi

    if [[ "${#value}" -ge 2 ]]; then
        if [[ "${first_char}" == '"' && "${last_char}" == '"' ]]; then
            value="${value:1:${#value}-2}"
        elif [[ "${first_char}" == "'" && "${last_char}" == "'" ]]; then
            value="${value:1:${#value}-2}"
        fi
    fi

    printf '%s\n' "${value}"
}

extract_action_reference_components_from_line() {
    local content="$1"
    local ref
    local identity
    local version

    if [[ "${content}" == -* ]]; then
        content="${content#-}"
        content="$(trim_leading_whitespace "${content}")"
    fi

    ref="${content#uses:}"
    ref="$(trim_leading_whitespace "${ref}")"
    ref="${ref%%[[:space:]#]*}"
    ref="$(strip_wrapping_delimiters "${ref}")"

    if [[ "${ref}" != *@* ]]; then
        return 1
    fi

    identity="${ref%@*}"
    version="${ref##*@}"
    if [[ -z "${identity}" || -z "${version}" ]]; then
        return 1
    fi

    printf 'action\t%s\t%s\t-\n' "${identity}" "${version}"
}

extract_docker_reference_components_from_line() {
    local content="$1"
    local ref
    local identity
    local version
    local context_key="-"
    local remainder
    local -a tokens=()
    local token_index=0
    local ref_index=0
    local suffix=""

    case "${content}" in
        FROM* | from*)
            read -r -a tokens <<< "${content}"
            if (( ${#tokens[@]} < 2 )); then
                return 1
            fi
            token_index=1
            while (( token_index < ${#tokens[@]} )) && [[ "${tokens[token_index]}" == --* ]]; do
                ((token_index += 1))
            done
            if (( token_index >= ${#tokens[@]} )); then
                return 1
            fi
            ref_index="${token_index}"
            ref="${tokens[ref_index]}"
            ((token_index += 1))
            if (( token_index < ${#tokens[@]} )); then
                suffix="${tokens[*]:token_index}"
            else
                suffix=""
            fi
            context_key="${tokens[0]}"
            if (( ref_index > 1 )); then
                context_key+=" ${tokens[*]:1:ref_index-1}"
            fi
            context_key+=$'\x1f'
            context_key+="${suffix}"
            ;;
        image:*)
            remainder="${content#image:}"
            remainder="$(trim_leading_whitespace "${remainder}")"
            read -r -a tokens <<< "${remainder}"
            if (( ${#tokens[@]} == 0 )); then
                return 1
            fi
            ref="${tokens[0]}"
            if (( ${#tokens[@]} > 1 )); then
                suffix="${tokens[*]:1}"
            else
                suffix=""
            fi
            context_key=$'image:\x1f'
            context_key+="${suffix}"
            ;;
        *)
            return 1
            ;;
    esac

    ref="${ref%%[[:space:]#]*}"
    ref="$(strip_wrapping_delimiters "${ref}")"

    if [[ -z "${ref}" || "${ref}" == *'$'* ]]; then
        return 1
    fi

    ref="${ref%@sha256:*}"
    identity="${ref%:*}"
    version="${ref##*:}"

    if [[ "${version}" == "${ref}" || -z "${identity}" || -z "${version}" || "${version}" == */* ]]; then
        return 1
    fi

    printf 'docker\t%s\t%s\t%s\n' "${identity}" "${version}" "${context_key}"
}

extract_non_npm_reference_components_from_patch_line() {
    local patch_line="$1"
    local content

    case "${patch_line}" in
        ---* | +++* | @@*)
            return 1
            ;;
        [-+]*)
            content="${patch_line:1}"
            ;;
        *)
            return 1
            ;;
    esac

    content="$(trim_leading_whitespace "${content}")"

    case "${content}" in
        uses:* | -\ uses:*)
            extract_action_reference_components_from_line "${content}"
            ;;
        FROM* | from* | image:*)
            extract_docker_reference_components_from_line "${content}"
            ;;
        *)
            return 1
            ;;
    esac
}

extract_non_npm_patch_reference_pairs() {
    local patch_path="$1"
    local removed_references_path
    local added_references_path
    local pair_count
    local removed_count
    local added_count
    local patch_line
    local reference_components

    removed_references_path="$(mktemp)"
    added_references_path="$(mktemp)"

    while IFS= read -r patch_line || [[ -n "${patch_line}" ]]; do
        case "${patch_line}" in
            "" | " "* | diff\ --git\ * | index\ * | ---* | +++* | @@* | \\*)
                continue
                ;;
        esac

        if ! reference_components="$(extract_non_npm_reference_components_from_patch_line "${patch_line}")"; then
            rm -f "${removed_references_path}" "${added_references_path}"
            return 1
        fi

        case "${patch_line}" in
            -*)
                printf '%s\n' "${reference_components}" >> "${removed_references_path}"
                ;;
            +*)
                printf '%s\n' "${reference_components}" >> "${added_references_path}"
                ;;
            *)
                rm -f "${removed_references_path}" "${added_references_path}"
                return 1
                ;;
        esac
    done < "${patch_path}"

    removed_count="$(wc -l < "${removed_references_path}")"
    removed_count="${removed_count//[[:space:]]/}"
    added_count="$(wc -l < "${added_references_path}")"
    added_count="${added_count//[[:space:]]/}"

    if [[ "${removed_count}" == "0" || "${removed_count}" != "${added_count}" ]]; then
        rm -f "${removed_references_path}" "${added_references_path}"
        return 1
    fi

    paste "${removed_references_path}" "${added_references_path}"
    pair_count=$?
    rm -f "${removed_references_path}" "${added_references_path}"
    return "${pair_count}"
}

classify_version_transition() {
    local before_version
    local after_version
    local before_contexts_text=""
    local after_contexts_text=""
    local before_contexts=()
    local after_contexts=()
    local before_triplet_context
    local after_triplet_context
    local before_prefix
    local before_major
    local before_minor
    local before_patch
    local before_suffix
    local after_prefix
    local after_major
    local after_minor
    local after_patch
    local after_suffix

    before_version="$1"
    after_version="$2"

    if ! before_contexts_text="$(
        extract_numeric_version_triplet_contexts "${before_version}"
    )"; then
        printf '%s\n' "unknown"
        return 0
    fi
    while IFS= read -r before_triplet_context || [[ -n "${before_triplet_context}" ]]; do
        before_contexts+=("${before_triplet_context}")
    done <<<"${before_contexts_text}"

    if ! after_contexts_text="$(
        extract_numeric_version_triplet_contexts "${after_version}"
    )"; then
        printf '%s\n' "unknown"
        return 0
    fi
    while IFS= read -r after_triplet_context || [[ -n "${after_triplet_context}" ]]; do
        after_contexts+=("${after_triplet_context}")
    done <<<"${after_contexts_text}"

    for before_triplet_context in "${before_contexts[@]}"; do
        IFS=$'\x1f' read -r before_prefix before_major before_minor before_patch before_suffix <<<"${before_triplet_context}"

        for after_triplet_context in "${after_contexts[@]}"; do
            IFS=$'\x1f' read -r after_prefix after_major after_minor after_patch after_suffix <<<"${after_triplet_context}"

            if [[ "${before_prefix}" != "${after_prefix}" || "${before_suffix}" != "${after_suffix}" ]]; then
                continue
            fi

            before_major=$((10#${before_major}))
            before_minor=$((10#${before_minor}))
            before_patch=$((10#${before_patch}))
            after_major=$((10#${after_major}))
            after_minor=$((10#${after_minor}))
            after_patch=$((10#${after_patch}))

            if (( after_major != before_major )); then
                printf '%s\n' "major"
                return 0
            fi

            if (( after_minor != before_minor )); then
                printf '%s\n' "minor"
                return 0
            fi

            if (( after_patch != before_patch )); then
                printf '%s\n' "patch"
                return 0
            fi

            if [[ "${before_version}" == "${after_version}" ]]; then
                printf '%s\n' "none"
                return 0
            fi
        done
    done

    if [[ "${before_version}" != "${after_version}" ]]; then
        printf '%s\n' "unknown"
        return 0
    fi

    printf '%s\n' "none"
}

classify_non_npm_version_update_from_patch() {
    local patch_path="$1"
    local reference_pairs
    local overall_update_type="none"
    local current_update_type
    local before_kind
    local before_identity
    local before_version
    local before_context_key
    local after_kind
    local after_identity
    local after_version
    local after_context_key

    if ! reference_pairs="$(extract_non_npm_patch_reference_pairs "${patch_path}")"; then
        printf '%s\n' "unknown"
        return 0
    fi

    while IFS=$'\t' read -r before_kind before_identity before_version before_context_key after_kind after_identity after_version after_context_key; do
        if [[ -z "${before_kind}" || -z "${after_kind}" || "${before_kind}" != "${after_kind}" || "${before_identity}" != "${after_identity}" ]]; then
            printf '%s\n' "unknown"
            return 0
        fi

        if [[ "${before_context_key}" != "${after_context_key}" ]]; then
            printf '%s\n' "unknown"
            return 0
        fi

        current_update_type="$(classify_version_transition "${before_version}" "${after_version}")"

        case "${current_update_type}" in
            unknown)
                printf '%s\n' "unknown"
                return 0
                ;;
            major)
                printf '%s\n' "major"
                return 0
                ;;
            minor)
                overall_update_type="minor"
                ;;
            patch)
                if [[ "${overall_update_type}" == "none" ]]; then
                    overall_update_type="patch"
                fi
                ;;
            none)
                :
                ;;
            *)
                printf '%s\n' "unknown"
                return 0
                ;;
        esac
    done <<<"${reference_pairs}"

    printf '%s\n' "${overall_update_type}"
}

classify_non_npm_version_update() {
    local pull_number="$1"
    local patch_path_override="${2:-}"
    local temporary_patch_path=""
    local patch_path
    local update_type

    if [[ -n "${patch_path_override}" && -f "${patch_path_override}" ]]; then
        patch_path="${patch_path_override}"
    else
        temporary_patch_path="$(mktemp)"
        if ! run_gh_read_command pr diff "${pull_number}" --patch > "${temporary_patch_path}"; then
            rm -f "${temporary_patch_path}"
            printf '%s\n' "unknown"
            return 0
        fi
        patch_path="${temporary_patch_path}"
    fi

    update_type="$(classify_non_npm_version_update_from_patch "${patch_path}")"

    if [[ -n "${temporary_patch_path}" ]]; then
        rm -f "${temporary_patch_path}"
    fi

    printf '%s\n' "${update_type}"
}

strip_dependency_sections_from_manifest() {
    local manifest_json="$1"

    jq -cS 'del(.dependencies, .devDependencies, .optionalDependencies, .peerDependencies)' <<<"${manifest_json}"
}

build_package_manifest_dependency_changes_json() {
    local base_manifest_json="$1"
    local head_manifest_json="$2"

    jq -cnS \
        --argjson base "${base_manifest_json}" \
        --argjson head "${head_manifest_json}" '
            def sections: ["dependencies", "devDependencies", "optionalDependencies", "peerDependencies"];

            [
                sections[] as $section
                | ((($base[$section] // {}) + ($head[$section] // {})) | keys_unsorted[]) as $name
                | select(($base[$section][$name] // null) != ($head[$section][$name] // null))
                | {
                    name: $name,
                    section: $section,
                    before: ($base[$section][$name] // null),
                    after: ($head[$section][$name] // null),
                    beforeType: (
                        if (($base[$section] // {}) | has($name)) then
                            ($base[$section][$name] | type)
                        else
                            "null"
                        end
                    ),
                    afterType: (
                        if (($head[$section] // {}) | has($name)) then
                            ($head[$section][$name] | type)
                        else
                            "null"
                        end
                    )
                }
            ]
            | sort_by(.section, .name)
        '
}

format_dependency_changes_from_json() {
    local dependency_changes_json="$1"

    jq -r '
        map("\(.name): \(.before) -> \(.after)")
        | join(", ")
    ' <<<"${dependency_changes_json}"
}

extract_non_allowlisted_npm_dependency_names() {
    local dependency_changes_json="$1"

    jq -r '
        [
            .[]
            | select(
                (.name | test("^@biomejs/biome$|^vitest$|^@vitest/.+$|^@types/.+$")) | not
            )
            | .name
        ]
        | unique
        | join(", ")
    ' <<<"${dependency_changes_json}"
}

evaluate_npm_candidate() {
    local pull_number="$1"
    local lockfile_only
    local compare_json
    local merge_base_sha
    local base_manifest_json
    local head_manifest_json
    local stripped_base_manifest_json
    local stripped_head_manifest_json
    local dependency_changes_json
    local dependency_changes_formatted
    local non_allowlisted_dependencies
    local changed_biome_json
    local dependency_name
    local before_version
    local after_version
    local current_update_type
    local joined_non_patch_changes
    local -a non_patch_changes=()

    lockfile_only="$(
        jq -r '
            ([.files[]?.path] | unique | sort) == ["package-lock.json"]
        ' <<<"${current_pr_state_json}"
    )"

    if [[ "${lockfile_only}" == "true" ]]; then
        return 0
    fi

    if ! jq -e 'any(.files[]?; .path == "package.json")' <<<"${current_pr_state_json}" >/dev/null; then
        skip_reason="PR #${pull_number} は lockfile-only ではないのに package.json を更新していないため、自動承認しません。"
        return 1
    fi

    if ! compare_json="$(load_compare_json "${base_ref}" "${pr_head_sha}" 2>/dev/null)"; then
        skip_reason="PR #${pull_number} の merge base を取得できないため、package.json を再検証できません。"
        return 1
    fi

    if ! merge_base_sha="$(jq -er '.merge_base_commit.sha' <<<"${compare_json}")"; then
        skip_reason="PR #${pull_number} の merge base SHA を特定できないため、package.json を再検証できません。"
        return 1
    fi

    if ! base_manifest_json="$(
        load_repository_file_text "package.json" "${merge_base_sha}" 2>/dev/null | jq -cS '.'
    )"; then
        skip_reason="PR #${pull_number} の merge base (${merge_base_sha}) の package.json を取得できないため、自動承認しません。"
        return 1
    fi

    if ! head_manifest_json="$(
        load_repository_file_text "package.json" "${pr_head_sha}" 2>/dev/null | jq -cS '.'
    )"; then
        skip_reason="PR #${pull_number} の HEAD (${pr_head_sha}) の package.json を取得できないため、自動承認しません。"
        return 1
    fi

    stripped_base_manifest_json="$(strip_dependency_sections_from_manifest "${base_manifest_json}")"
    stripped_head_manifest_json="$(strip_dependency_sections_from_manifest "${head_manifest_json}")"

    if [[ "${stripped_base_manifest_json}" != "${stripped_head_manifest_json}" ]]; then
        skip_reason="PR #${pull_number} は package.json の依存バージョン以外も変更しているため、自動承認しません。"
        return 1
    fi

    dependency_changes_json="$(
        build_package_manifest_dependency_changes_json "${base_manifest_json}" "${head_manifest_json}"
    )"

    if [[ "$(jq 'length' <<<"${dependency_changes_json}")" == "0" ]]; then
        skip_reason="PR #${pull_number} は package.json の依存バージョンを変更していないため、自動承認しません。"
        return 1
    fi

    dependency_changes_formatted="$(format_dependency_changes_from_json "${dependency_changes_json}")"

    if jq -e 'any(.[]; .before == null or .after == null)' <<<"${dependency_changes_json}" >/dev/null; then
        skip_reason="PR #${pull_number} は依存関係を追加または削除しているため、自動承認しません: ${dependency_changes_formatted}"
        return 1
    fi

    if jq -e 'any(.[]; .beforeType != "string" or .afterType != "string")' <<<"${dependency_changes_json}" >/dev/null; then
        skip_reason="PR #${pull_number} の package.json に文字列以外の依存バージョン変更が含まれるため、自動承認しません。"
        return 1
    fi

    if jq -e 'any(.[]; .section != "devDependencies")' <<<"${dependency_changes_json}" >/dev/null; then
        skip_reason="PR #${pull_number} は devDependencies 以外を更新しているため、自動承認しません: ${dependency_changes_formatted}"
        return 1
    fi

    non_allowlisted_dependencies="$(
        extract_non_allowlisted_npm_dependency_names "${dependency_changes_json}"
    )"

    if [[ -n "${non_allowlisted_dependencies}" ]]; then
        skip_reason="PR #${pull_number} は許可リスト外の devDependencies を更新しているため、自動承認しません: ${non_allowlisted_dependencies}"
        return 1
    fi

    while IFS=$'\t' read -r dependency_name before_version after_version; do
        current_update_type="$(classify_version_transition "${before_version}" "${after_version}")"
        if [[ "${current_update_type}" != "patch" ]]; then
            non_patch_changes+=("${dependency_name}: ${before_version} -> ${after_version}")
        fi
    done < <(
        jq -r '.[] | [.name, .before, .after] | @tsv' <<<"${dependency_changes_json}"
    )

    if (( ${#non_patch_changes[@]} > 0 )); then
        joined_non_patch_changes="$(IFS=', '; printf '%s' "${non_patch_changes[*]}")"
        skip_reason="PR #${pull_number} は patch update のみではないため、自動承認しません: ${joined_non_patch_changes}"
        return 1
    fi

    changed_biome_json="$(
        jq -r 'any(.files[]?; .path == "biome.json")' <<<"${current_pr_state_json}"
    )"

    if [[ "${changed_biome_json}" == "true" ]] && ! jq -e 'all(.[]; .name == "@biomejs/biome")' <<<"${dependency_changes_json}" >/dev/null; then
        skip_reason="PR #${pull_number} は @biomejs/biome 以外の更新で biome.json を変更しているため、自動承認しません。"
        return 1
    fi

    return 0
}

compute_disallowed_files() {
    local pr_json="$1"
    local candidate_kind="$2"
    local supported_non_npm_workflow_filenames_json

    case "${candidate_kind}" in
        npm)
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
            ;;
        non-npm)
            supported_non_npm_workflow_filenames_json="$(
                load_supported_non_npm_workflow_filenames_json
            )"
            jq -r \
                --argjson supported_workflow_filenames "${supported_non_npm_workflow_filenames_json}" '
                [
                    .files[]?.path
                    | select(
                        (
                            (
                                . as $path
                                | test("^\\.github/workflows/[^/]+\\.ya?ml$") and
                                (($supported_workflow_filenames | index($path)) != null)
                            ) or
                            test("^docker/Dockerfile$") or
                            test("^docker/docker-compose(-[^/]+)?\\.ya?ml$")
                        ) | not
                    )
                ]
                | unique
                | join(", ")
            ' <<<"${pr_json}"
            ;;
        *)
            jq -r '
                [
                    .files[]?.path
                ]
                | unique
                | join(", ")
            ' <<<"${pr_json}"
            ;;
    esac
}

ensure_current_head_claude_review_is_clean() {
    local pull_number="$1"
    local issue_comments_json
    local verdict_candidate_json
    local verdict_comment_json=""
    local verdict_payload=""
    local claude_review_verdict_json=""
    local verdict_pr_number=""
    local verdict_head_sha=""
    local verdict_base_sha=""
    local verdict_review_status=""
    local verdict_finding_count=""
    local verdict_workflow_run_id=""
    local verdict_workflow_run_attempt=""
    local verdict_comment_updated_at=""
    local verdict_author_login=""
    local verdict_author_permission=""
    local claude_review_summary=""
    local trusted_verdict_failure_reason=""
    local workflow_run_json=""
    local workflow_run_attempt=""
    local workflow_run_started_at=""
    local workflow_run_completed_at=""
    local workflow_run_name=""
    local workflow_run_path=""
    local workflow_run_event=""
    local workflow_run_head_sha=""
    local workflow_run_targets_pr=""
    local collaborator_permission_json
    local review_comments_json
    local current_head_finding_count

    if ! issue_comments_json="$(load_issue_comments_json "${pull_number}" 2>/dev/null)"; then
        skip_reason="PR #${pull_number} の Claude review verdict comment を取得できないため判定できません。"
        return 1
    fi

    while IFS= read -r verdict_candidate_json || [[ -n "${verdict_candidate_json}" ]]; do
        [[ -z "${verdict_candidate_json}" ]] && continue

        verdict_author_login="$(
            jq -r '.user.login // ""' <<<"${verdict_candidate_json}"
        )"
        if [[ -z "${verdict_author_login}" ]]; then
            continue
        fi

        verdict_author_permission=""
        if [[ "${verdict_author_login}" == "github-actions[bot]" ]]; then
            verdict_author_permission="github-actions[bot]"
        else
            if ! collaborator_permission_json="$(
                load_collaborator_permission_json "${verdict_author_login}" 2>/dev/null
            )"; then
                continue
            fi
            verdict_author_permission="$(
                jq -r '.permission // ""' <<<"${collaborator_permission_json}"
            )"
            case "${verdict_author_permission}" in
                admin | maintain | write)
                    ;;
                *)
                    continue
                    ;;
            esac
        fi

        verdict_payload="$(
            jq -r '
                try (
                    (.body // "")
                    | capture("```json\\s*(?<payload>[\\s\\S]*?)\\s*```")
                    | .payload
                ) catch ""
            ' <<<"${verdict_candidate_json}"
        )"
        if [[ -z "${verdict_payload}" ]]; then
            if [[ -z "${trusted_verdict_failure_reason}" ]]; then
                trusted_verdict_failure_reason="PR #${pull_number} の信頼できる Claude review verdict comment が壊れているため判定できません。"
            fi
            continue
        fi

        if ! claude_review_verdict_json="$(jq -cer '.' <<<"${verdict_payload}" 2>/dev/null)"; then
            if [[ -z "${trusted_verdict_failure_reason}" ]]; then
                trusted_verdict_failure_reason="PR #${pull_number} の信頼できる Claude review verdict JSON を解釈できないため判定できません。"
            fi
            continue
        fi

        verdict_pr_number="$(jq -r '.pr_number // ""' <<<"${claude_review_verdict_json}")"
        if [[ "${verdict_pr_number}" != "${pull_number}" ]]; then
            if [[ -z "${trusted_verdict_failure_reason}" ]]; then
                trusted_verdict_failure_reason="PR #${pull_number} の信頼できる Claude review verdict が別 PR (#${verdict_pr_number}) を指しているため、自動承認しません。"
            fi
            continue
        fi

        verdict_head_sha="$(jq -r '.head_sha // ""' <<<"${claude_review_verdict_json}")"
        if [[ "${verdict_head_sha}" != "${pr_head_sha}" ]]; then
            if [[ -z "${trusted_verdict_failure_reason}" ]]; then
                trusted_verdict_failure_reason="PR #${pull_number} の信頼できる Claude review verdict が現在の HEAD (${pr_head_sha}) ではなく ${verdict_head_sha:-MISSING} を見ているため、自動承認しません。"
            fi
            continue
        fi

        verdict_base_sha="$(jq -r '.base_sha // ""' <<<"${claude_review_verdict_json}")"
        if [[ "${verdict_base_sha}" != "${pr_base_sha}" ]]; then
            if [[ -z "${trusted_verdict_failure_reason}" ]]; then
                trusted_verdict_failure_reason="PR #${pull_number} の信頼できる Claude review verdict が現在の BASE (${pr_base_sha}) ではなく ${verdict_base_sha:-MISSING} を見ているため、自動承認しません。"
            fi
            continue
        fi

        verdict_review_status="$(jq -r '.review_status // ""' <<<"${claude_review_verdict_json}")"
        verdict_finding_count="$(jq -r '.finding_count // ""' <<<"${claude_review_verdict_json}")"
        claude_review_summary="$(
            jq -r '
                (.summary // "")
                | gsub("\\s+"; " ")
                | gsub("^\\s+|\\s+$"; "")
            ' <<<"${claude_review_verdict_json}"
        )"
        if [[ "${verdict_review_status}" != "pass" || "${verdict_finding_count}" != "0" || -z "${claude_review_summary}" ]]; then
            if [[ -z "${trusted_verdict_failure_reason}" ]]; then
                trusted_verdict_failure_reason="PR #${pull_number} の Claude review が現在の HEAD/BASE に対して zero findings を明示していないため、自動承認しません: status=${verdict_review_status:-MISSING}, finding_count=${verdict_finding_count:-MISSING}, summary=${claude_review_summary:-MISSING}"
            fi
            break
        fi

        verdict_workflow_run_id="$(jq -r '.workflow_run_id // ""' <<<"${claude_review_verdict_json}")"
        verdict_workflow_run_attempt="$(jq -r '.workflow_run_attempt // ""' <<<"${claude_review_verdict_json}")"
        verdict_comment_updated_at="$(
            jq -r '.updated_at // .created_at // ""' <<<"${verdict_candidate_json}"
        )"
        if [[ ! "${verdict_workflow_run_id}" =~ ^[0-9]+$ || ! "${verdict_workflow_run_attempt}" =~ ^[0-9]+$ || -z "${verdict_comment_updated_at}" ]]; then
            if [[ -z "${trusted_verdict_failure_reason}" ]]; then
                trusted_verdict_failure_reason="PR #${pull_number} の Claude review verdict に workflow run metadata が不足しているため、自動承認しません。"
            fi
            break
        fi

        if ! workflow_run_json="$(load_workflow_run_json "${verdict_workflow_run_id}" 2>/dev/null)"; then
            if [[ -z "${trusted_verdict_failure_reason}" ]]; then
                trusted_verdict_failure_reason="PR #${pull_number} の Claude review workflow run (${verdict_workflow_run_id}) を取得できないため判定できません。"
            fi
            break
        fi

        workflow_run_attempt="$(jq -r '.run_attempt // ""' <<<"${workflow_run_json}")"
        workflow_run_started_at="$(
            jq -r '.run_started_at // .created_at // ""' <<<"${workflow_run_json}"
        )"
        workflow_run_completed_at="$(
            jq -r '.completed_at // .updated_at // ""' <<<"${workflow_run_json}"
        )"
        workflow_run_name="$(jq -r '.name // ""' <<<"${workflow_run_json}")"
        workflow_run_path="$(jq -r '.path // ""' <<<"${workflow_run_json}")"
        workflow_run_event="$(jq -r '.event // ""' <<<"${workflow_run_json}")"
        workflow_run_head_sha="$(jq -r '.head_sha // ""' <<<"${workflow_run_json}")"
        workflow_run_targets_pr="$(
            jq -r \
                --argjson pull_number "${pull_number}" '
                    if (.pull_requests? | type) == "array" and (.pull_requests | length) > 0 then
                        any(.pull_requests[]?; (.number // 0) == $pull_number)
                    else
                        "unknown"
                    end
                ' <<<"${workflow_run_json}"
        )"
        case "${workflow_run_event}" in
            pull_request | schedule)
                ;;
            *)
                workflow_run_event="invalid"
                ;;
        esac

        if [[ \
            "${workflow_run_attempt}" != "${verdict_workflow_run_attempt}" || \
            -z "${workflow_run_started_at}" || \
            -z "${workflow_run_completed_at}" || \
            "${workflow_run_name}" != "Claude Code Review for Bot PRs" || \
            "${workflow_run_path}" != *"claude-code-review.yml"* || \
            "${workflow_run_event}" == "invalid" || \
            "${workflow_run_head_sha}" != "${pr_head_sha}" || \
            ( "${workflow_run_targets_pr}" != "true" && "${workflow_run_targets_pr}" != "unknown" ) || \
            "${verdict_comment_updated_at}" < "${workflow_run_started_at}" || \
            "${verdict_comment_updated_at}" > "${workflow_run_completed_at}" \
        ]]; then
            if [[ -z "${trusted_verdict_failure_reason}" ]]; then
                trusted_verdict_failure_reason="PR #${pull_number} の Claude review workflow run metadata が verdict と一致しないため、自動承認しません。"
            fi
            break
        fi

        verdict_comment_json="${verdict_candidate_json}"
        break
    done < <(
        jq -cer '
            [
                .[]?
                | select(
                    ((.body // "") | contains("<!-- dependabot-claude-review-verdict -->"))
                )
            ]
            | sort_by(.updated_at // .created_at // "")
            | reverse
            | .[]
        ' <<<"${issue_comments_json}" 2>/dev/null || true
    )

    if [[ -z "${verdict_comment_json}" ]]; then
        if [[ -n "${trusted_verdict_failure_reason}" ]]; then
            skip_reason="${trusted_verdict_failure_reason}"
        else
            skip_reason="PR #${pull_number} の現在の HEAD に対応する信頼できる machine-readable な Claude review verdict comment がまだありません。"
        fi
        return 1
    fi

    if ! review_comments_json="$(load_review_comments_json "${pull_number}" 2>/dev/null)"; then
        skip_reason="PR #${pull_number} の Claude review inline comment を取得できないため判定できません。"
        return 1
    fi

    current_head_finding_count="$(
        jq -r \
            --arg head_sha "${pr_head_sha}" \
            --arg prefix "【Claude review】" \
            --arg author_login "${verdict_author_login}" '
                [
                    .[]?
                    | select(
                        (.user.login // "") == $author_login and
                        ((.commit_id // .original_commit_id // "") == $head_sha) and
                        ((.body // "") | startswith($prefix))
                    )
                ]
                | length
            ' \
            <<<"${review_comments_json}"
    )"
    if [[ "${current_head_finding_count}" != "0" ]]; then
        skip_reason="PR #${pull_number} の現在の HEAD に Claude review の inline finding が ${current_head_finding_count} 件あるため、自動承認しません。"
        return 1
    fi
}

extract_status_check_rollup_commit_oid() {
    local pr_json="$1"

    jq -r '
        if .statusCheckRollup == null then
            ""
        elif (.statusCheckRollup | type) == "object" then
            (.statusCheckRollup.commit?.oid // "")
        else
            ""
        end
    ' <<<"${pr_json}"
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

merge_status_check_rollup_snapshot_into_pr_json() {
    local pr_json="$1"
    local status_rollup_snapshot_json="$2"

    jq -c \
        --argjson snapshot "${status_rollup_snapshot_json}" '
            ($snapshot.data.repository.pullRequest // {}) as $snapshot_pull_request
            | . + {
                headRefOid: ($snapshot_pull_request.headRefOid // .headRefOid),
                statusCheckRollup: (
                    $snapshot_pull_request.statusCheckRollup // .statusCheckRollup
                )
            }
        ' <<<"${pr_json}"
}

build_status_states_json() {
    local pr_json="$1"

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
        def node_status($node):
            if $node.__typename == "CheckRun" then
                {
                    "state": ($node.conclusion // $node.status // "MISSING"),
                    "passing": (
                        ($node.status == "COMPLETED") and (
                            ($node.conclusion // "") == "SUCCESS" or
                            ($node.conclusion // "") == "SKIPPED" or
                            ($node.conclusion // "") == "NEUTRAL"
                        )
                    ),
                    "active": (($node.status // "COMPLETED") != "COMPLETED"),
                    "timestamp": node_timestamp($node)
                }
            elif $node.__typename == "StatusContext" then
                {
                    "state": ($node.state // "MISSING"),
                    "passing": (($node.state // "") == "SUCCESS"),
                    "active": false,
                    "timestamp": node_timestamp($node)
                }
            else
                null
            end;
        def status_priority($status):
            if ($status.active // false) then
                2
            elif (($status.timestamp // "") != "") then
                1
            else
                0
            end;

        reduce (status_nodes | to_entries[]) as $entry
            ({};
             ($entry.value) as $node
             | ($entry.key) as $index
             | (node_name($node)) as $name
             | (node_status($node)) as $status
             | if $name == null or $status == null then
                 .
               elif .[$name] == null or
                    status_priority(.[$name]) < status_priority($status) or
                    (
                        status_priority(.[$name]) == status_priority($status) and
                        (
                            .[$name].timestamp < $status.timestamp or
                            (
                                .[$name].timestamp == $status.timestamp and
                                ((.[$name].index // -1) < $index)
                            )
                        )
                    ) then
                 . + {
                     ($name): ($status + { "index": $index })
                 }
               else
                 .
               end)
        | . as $statuses
        | reduce ($statuses | to_entries[]) as $entry
            ($statuses;
             ($entry.key) as $name
             | ($entry.value) as $status
             | if ($name | test("^claude-review \\(.+\\)$")) and (
                    .["claude-review"] == null or
                    status_priority(.["claude-review"]) < status_priority($status) or
                    (
                        status_priority(.["claude-review"]) == status_priority($status) and
                        (
                            .["claude-review"].timestamp < $status.timestamp or
                            (
                                .["claude-review"].timestamp == $status.timestamp and
                                ((.["claude-review"].index // -1) < ($status.index // -1))
                            )
                        )
                    )
                ) then
                 . + {
                     "claude-review": $status
                 }
               else
                 .
               end)
    ' <<<"${pr_json}"
}

load_monitored_check_contexts_json() {
    local require_current_head_claude_review_verdict="${1:-true}"
    # statusCheckRollup exposes job/check-run names rather than workflow names.
    local configured_contexts="${DEPENDABOT_AUTO_APPROVE_MONITORED_CONTEXTS:-${DEPENDABOT_AUTO_APPROVE_MONITORED_WORKFLOW_CONTEXTS:-build,lint-and-test,Analyze (javascript),restyled}}"

    jq -cn \
        --arg raw "${configured_contexts}" \
        --arg require_current_head_claude_review_verdict "${require_current_head_claude_review_verdict}" '
        $raw
        | split(",")
        | map(gsub("^\\s+|\\s+$"; ""))
        | map(select(length > 0))
        | if $require_current_head_claude_review_verdict == "true" then
              .
          else
              map(select(. != "claude-review"))
          end
        | unique
    '
}

load_monitored_check_contexts_allowed_to_be_missing_json() {
    jq -cn '[]'
}

load_require_current_head_claude_review_verdict() {
    case "${DEPENDABOT_AUTO_APPROVE_REQUIRE_CURRENT_HEAD_CLAUDE_REVIEW_VERDICT:-auto}" in
        true | false)
            printf '%s\n' "${DEPENDABOT_AUTO_APPROVE_REQUIRE_CURRENT_HEAD_CLAUDE_REVIEW_VERDICT}"
            ;;
        auto | "")
            printf 'true\n'
            ;;
        *)
            printf 'true\n'
            ;;
    esac
}

ensure_status_checks_target_current_head() {
    local pull_number="$1"
    local status_rollup_commit_oid
    local status_rollup_snapshot_json
    local rollup_head_sha
    local candidate_pr_json="${current_pr_state_json:-}"
    local refreshed_candidate_pr_json

    if ! status_rollup_snapshot_json="$(load_status_check_rollup_snapshot_json "${pull_number}" 2>/dev/null)"; then
        # shellcheck disable=SC2034
        skip_reason="PR #${pull_number} の最新 statusCheckRollup を確認できないため判定できません。"
        return
    fi

    rollup_head_sha="$(jq -r '.data.repository.pullRequest.headRefOid // ""' <<<"${status_rollup_snapshot_json}")"
    status_rollup_commit_oid="$(
        jq -r '.data.repository.pullRequest.statusCheckRollup.commit.oid // ""' <<<"${status_rollup_snapshot_json}"
    )"

    if [[ -z "${rollup_head_sha}" ]]; then
        # shellcheck disable=SC2034
        skip_reason="PR #${pull_number} の現在の HEAD を statusCheckRollup から確認できないため判定できません。"
        return
    fi

    if [[ "${rollup_head_sha}" != "${pr_head_sha}" ]]; then
        # shellcheck disable=SC2034
        skip_reason="PR #${pull_number} の HEAD が判定中に ${pr_head_sha} から ${rollup_head_sha} へ更新されたため、自動承認しません。"
        return
    fi

    if ! refreshed_candidate_pr_json="$(
        merge_status_check_rollup_snapshot_into_pr_json \
            "${candidate_pr_json}" \
            "${status_rollup_snapshot_json}"
    )"; then
        # shellcheck disable=SC2034
        skip_reason="PR #${pull_number} の最新 statusCheckRollup を再構築できないため判定できません。"
        return
    fi
    refresh_pr_state "${refreshed_candidate_pr_json}"

    if [[ -z "${status_rollup_commit_oid}" ]]; then
        # shellcheck disable=SC2034
        skip_reason="PR #${pull_number} の必須チェック結果が現在の HEAD (${pr_head_sha}) にまだ紐付いていないため、自動承認しません。"
        return
    fi

    if [[ "${status_rollup_commit_oid}" != "${pr_head_sha}" ]]; then
        # shellcheck disable=SC2034
        skip_reason="PR #${pull_number} の必須チェック結果が現在の HEAD (${pr_head_sha}) ではなく ${status_rollup_commit_oid} に紐付いているため、自動承認しません。"
    fi
}

refresh_pr_state() {
    local pr_json="$1"

    current_pr_state_json="${pr_json}"
    pr_state="$(jq -r '.state // "OPEN"' <<<"${pr_json}")"
    # shellcheck disable=SC2034
    pr_head_sha="$(jq -r '.headRefOid' <<<"${pr_json}")"
    # shellcheck disable=SC2034
    pr_base_sha="$(jq -r '.baseRefOid' <<<"${pr_json}")"
    author_login="$(jq -r '.author.login' <<<"${pr_json}")"
    base_ref="$(jq -r '.baseRefName' <<<"${pr_json}")"
    mergeable_state="$(jq -r '.mergeable' <<<"${pr_json}")"
    review_decision="$(jq -r '.reviewDecision // "REVIEW_REQUIRED"' <<<"${pr_json}")"
    has_npm_label="$(
        jq -r 'any(.labels[]?; .name == "dependabot/npm")' <<<"${pr_json}"
    )"
    has_automerge_label="$(
        jq -r 'any(.labels[]?; .name == "automerge")' <<<"${pr_json}"
    )"
    configured_candidate_kind="${DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND:-auto}"
    resolved_candidate_kind="$(
        resolve_candidate_kind "${configured_candidate_kind}" "${has_npm_label}"
    )"
    status_states_json="$(build_status_states_json "${pr_json}")"
    disallowed_files="$(compute_disallowed_files "${pr_json}" "${resolved_candidate_kind}")"
    non_passing_checks="$(
        jq -r '
            [
                to_entries[]
                | select((.value.passing // false) | not)
                | "\(.key):\(.value.state // "MISSING")"
            ]
            | join(", ")
        ' <<<"${status_states_json}"
    )"
}

evaluate_candidate() {
    local pull_number="$1"
    local allow_approved_review="${2:-false}"
    local allow_non_open_state="${3:-false}"
    local allow_unknown_mergeable="${4:-false}"
    local require_current_head_claude_review_verdict="${5:-false}"
    local protection_json
    local required_contexts_json
    local monitored_workflow_contexts_json
    local monitored_contexts_allowed_to_be_missing_json
    local awaited_contexts_json
    local missing_awaited_checks
    local missing_required_checks
    local non_success_required_checks
    local non_success_monitored_checks
    local effective_non_passing_checks
    local non_npm_version_update_type

    # shellcheck disable=SC2034
    skip_reason=""

    if [[ "${pr_state}" != "OPEN" && "${allow_non_open_state}" != "true" ]]; then
        skip_reason="PR #${pull_number} はオープン状態ではありません。"
        return
    fi

    if [[ "${author_login}" != "app/dependabot" && "${author_login}" != "dependabot[bot]" ]]; then
        skip_reason="PR #${pull_number} は Dependabot 作成ではありません。"
        return
    fi

    if [[ "${base_ref}" != "develop" ]]; then
        skip_reason="PR #${pull_number} の対象ブランチが develop ではありません。"
        return
    fi

    case "${configured_candidate_kind}" in
        "" | auto)
            :
            ;;
        npm)
            if [[ "${has_npm_label}" != "true" ]]; then
                skip_reason="PR #${pull_number} に dependabot/npm ラベルがありません。"
                return
            fi
            ;;
        non-npm)
            if [[ "${has_npm_label}" == "true" ]]; then
                skip_reason="PR #${pull_number} は dependabot/npm ラベル付きのため、この workflow では扱いません。"
                return
            fi
            ;;
        *)
            skip_reason="PR #${pull_number} の candidate kind 設定が不正です: ${configured_candidate_kind}"
            return
            ;;
    esac

    if [[ "${has_automerge_label}" != "true" ]]; then
        skip_reason="PR #${pull_number} に automerge ラベルがありません。"
        return
    fi

    if [[ -n "${disallowed_files}" ]]; then
        case "${resolved_candidate_kind}" in
            npm)
                skip_reason="PR #${pull_number} は許可対象外のファイルを変更しています: ${disallowed_files}"
                ;;
            non-npm)
                skip_reason="PR #${pull_number} は GitHub Actions / Docker の依存 manifest 以外を変更しています: ${disallowed_files}"
                ;;
            *)
                skip_reason="PR #${pull_number} は candidate kind ${resolved_candidate_kind} で許可対象外のファイルを変更しています: ${disallowed_files}"
                ;;
        esac
        return
    fi

    if [[ "${resolved_candidate_kind}" == "npm" ]]; then
        if ! evaluate_npm_candidate "${pull_number}"; then
            return
        fi
    fi

    if [[ "${resolved_candidate_kind}" == "non-npm" ]]; then
        non_npm_version_update_type="$(classify_non_npm_version_update "${pull_number}")"

        if [[ "${non_npm_version_update_type}" != "patch" ]]; then
            if [[ "${non_npm_version_update_type}" == "unknown" ]]; then
                skip_reason="PR #${pull_number} の diff から non-npm 依存更新を patch 相当と判定できないため、自動承認しません。"
            else
                skip_reason="PR #${pull_number} は non-npm 依存の ${non_npm_version_update_type} update のため、自動承認しません。"
            fi
            return
        fi
    fi

    if [[ "${review_decision}" == "APPROVED" && "${allow_approved_review}" != "true" ]]; then
        skip_reason="PR #${pull_number} は既に承認済みです。"
        return
    fi

    if [[ "${review_decision}" == "CHANGES_REQUESTED" ]]; then
        skip_reason="PR #${pull_number} は変更要求レビューが付いているため自動承認しません。"
        return
    fi

    if [[ "${mergeable_state}" != "MERGEABLE" ]]; then
        # GitHub can briefly return UNKNOWN while recomputing mergeability after the approval is posted.
        if [[ "${mergeable_state}" == "UNKNOWN" && "${allow_unknown_mergeable}" == "true" ]]; then
            :
        else
            skip_reason="PR #${pull_number} はマージ可能ではありません (state: ${mergeable_state})。"
            return
        fi
    fi

    ensure_status_checks_target_current_head "${pull_number}"
    if [[ -n "${skip_reason}" ]]; then
        return
    fi

    if ! protection_json="$(load_branch_protection_json "${base_ref}" 2>/dev/null)"; then
        skip_reason="PR #${pull_number} の branch protection を取得できないため判定できません。"
        return
    fi

    required_contexts_json="$(
        jq -c \
            --arg require_current_head_claude_review_verdict "${require_current_head_claude_review_verdict}" '
            (
                ((.required_status_checks.checks // []) | map(.context)) +
                (.required_status_checks.contexts // []) +
                ["lint-and-test"]
            )
            | if $require_current_head_claude_review_verdict == "true" then
                  .
              else
                  map(select(. != "claude-review"))
              end
            | unique
        ' <<<"${protection_json}"
    )"

    monitored_workflow_contexts_json="$(
        load_monitored_check_contexts_json "${require_current_head_claude_review_verdict}"
    )"
    monitored_contexts_allowed_to_be_missing_json="$(
        load_monitored_check_contexts_allowed_to_be_missing_json
    )"
    awaited_contexts_json="$(
        jq -cn \
            --argjson required_contexts "${required_contexts_json}" \
            --argjson monitored_contexts "${monitored_workflow_contexts_json}" \
            --argjson monitored_contexts_allowed_to_be_missing "${monitored_contexts_allowed_to_be_missing_json}" \
            --argjson status_states "${status_states_json}" '
                (
                    $required_contexts +
                    (
                        $monitored_contexts
                        | map(
                            . as $context
                            | select(
                                ($monitored_contexts_allowed_to_be_missing | index($context)) == null or
                                $status_states[$context] != null
                            )
                        )
                    )
                )
                | unique
            '
    )"

    missing_awaited_checks="$(
        jq -nr \
            --argjson awaited_contexts "${awaited_contexts_json}" \
            --argjson status_states "${status_states_json}" '
                $awaited_contexts
                | map(select($status_states[.] == null))
                | join(", ")
            '
    )"

    if [[ -n "${missing_awaited_checks}" ]]; then
        skip_reason="PR #${pull_number} は必須または監視対象チェックがまだ出揃っていません: ${missing_awaited_checks}"
        return
    fi

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
        skip_reason="PR #${pull_number} は必須チェックがまだ出揃っていません: ${missing_required_checks}"
        return
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
        skip_reason="PR #${pull_number} は必須チェックが未成功です: ${non_success_required_checks}"
        return
    fi

    non_success_monitored_checks="$(
        jq -nr \
            --argjson monitored_contexts "${monitored_workflow_contexts_json}" \
            --argjson status_states "${status_states_json}" '
                $monitored_contexts
                | map(
                    select($status_states[.] != null and (($status_states[.].passing // false) | not))
                    | . + ":" + ($status_states[.].state // "MISSING")
                )
                | join(", ")
            '
    )"

    if [[ -n "${non_success_monitored_checks}" ]]; then
        skip_reason="PR #${pull_number} は監視対象チェックが未成功です: ${non_success_monitored_checks}"
        return
    fi

    if [[ "${require_current_head_claude_review_verdict}" == "true" ]]; then
        if ! ensure_current_head_claude_review_is_clean "${pull_number}"; then
            if [[ -n "${skip_reason}" ]]; then
                return
            fi
        fi
    fi

    effective_non_passing_checks="${non_passing_checks}"
    if [[ "${resolved_candidate_kind}" == "npm" && "${require_current_head_claude_review_verdict}" != "true" ]]; then
        effective_non_passing_checks="$(
            jq -nr \
                --argjson status_states "${status_states_json}" '
                    $status_states
                    | to_entries
                    | map(
                        select(
                            ((.value.passing // false) | not) and
                            ((.key | test("^claude-review( \\(.+\\))?$")) | not)
                        )
                        | "\(.key):\(.value.state // "MISSING")"
                    )
                    | join(", ")
                '
        )"
    fi

    if [[ "${resolved_candidate_kind}" == "npm" && -n "${effective_non_passing_checks}" ]]; then
        # shellcheck disable=SC2034
        skip_reason="PR #${pull_number} に未成功のチェックがあります: ${effective_non_passing_checks}"
    fi
}
