def fail($message): error($message);
def is_hex40: type == "string" and test("^[0-9a-f]{40}$");
def is_integer: type == "number" and . == floor;
def is_timestamp:
  type == "string"
  and test("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$");
def monotonic_status_group:
  sort_by(.id) as $items
  | all(range(1; $items | length); $items[. - 1].created_at <= $items[.].created_at);
def strictly_descending_ids:
  . as $items
  | all(range(1; $items | length); $items[. - 1].id > $items[.].id);

if type != "object" then fail("root must be an object") else . end
| if (.head_sha | is_hex40) | not then fail("invalid head_sha") else . end
| if .required_context != "WIP" then fail("required_context must be exact WIP") else . end
| if (.check_suite_total_count | is_integer | not) or .check_suite_total_count < 0
  then fail("invalid check_suite_total_count")
  elif .check_suite_total_count >= 1000
  then fail("check suite completeness limit reached")
  else .
  end
| if (.status_pages | type) != "array" or (.status_pages | length) == 0 or any(.status_pages[]; type != "array")
  then fail("malformed status pages")
  else .
  end
| if (.check_pages | type) != "array" or (.check_pages | length) == 0 or any(.check_pages[]; type != "object" or (.check_runs | type) != "array" or (.total_count | is_integer | not))
  then fail("malformed check pages")
  else .
  end
| .head_sha as $head_sha
| .required_context as $required_context
| (.status_pages | add // []) as $statuses
| [.check_pages[].check_runs[]] as $check_runs
| (.check_pages | map(.total_count) | unique) as $check_totals
| if any($statuses[];
      type != "object"
      or (.context | type) != "string"
      or (.state | type) != "string"
      or (.created_at | is_timestamp | not)
      or (.id | is_integer | not))
  then fail("malformed status")
  else .
  end
| if any($statuses[]; .id <= 0)
    or (($statuses | map(.id) | unique | length) != ($statuses | length))
    or (($statuses | strictly_descending_ids) | not)
    or (($statuses | group_by(.context | ascii_downcase) | all(.[]; monotonic_status_group)) | not)
  then fail("non-monotonic status freshness")
  else .
  end
| if ($check_totals | length) != 1 or $check_totals[0] != ($check_runs | length)
  then fail("check total_count mismatch")
  else .
  end
| if any($check_runs[];
      type != "object"
      or (.name | type) != "string"
      or (.head_sha | is_hex40 | not)
      or .head_sha != $head_sha
      or (.status | type) != "string"
      or ((.conclusion | type) != "string" and .conclusion != null)
      or (.id | is_integer | not)
      or (.app | type) != "object"
      or (.app.id | is_integer | not)
      or (.app.slug | type) != "string")
  then fail("malformed check run")
  else .
  end
| if any($check_runs[]; .id <= 0)
    or (($check_runs | map(.id) | unique | length) != ($check_runs | length))
  then fail("invalid check run ids")
  else .
  end
| if any($statuses[]; (.context | ascii_downcase) == ($required_context | ascii_downcase) and .context != $required_context)
    or any($check_runs[]; (.name | ascii_downcase) == ($required_context | ascii_downcase) and .name != $required_context)
  then fail("case-variant WIP collision")
  else .
  end
| ($statuses | sort_by(.id)) as $statuses_by_id
| {
    head_sha: $head_sha,
    required_context: $required_context,
    status_ids: [$statuses_by_id[].id],
    latest_statuses: (
      $statuses_by_id
      | group_by(.context | ascii_downcase)
      | map(max_by(.id))
    ),
    wip_statuses: [
      $statuses_by_id[]
      | select((.context | ascii_downcase) == ($required_context | ascii_downcase))
    ],
    wip_check_runs: [
      $check_runs[]
      | select((.name | ascii_downcase) == ($required_context | ascii_downcase))
    ],
    exact_wip_statuses: [
      $statuses_by_id[]
      | select(.context == $required_context)
    ],
    exact_wip_check_runs: [
      $check_runs[]
      | select(.name == $required_context)
    ]
  }
