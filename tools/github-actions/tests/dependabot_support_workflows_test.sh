#!/usr/bin/env bash

set -euo pipefail

script_dir="$(
    cd "$(dirname "${BASH_SOURCE[0]}")" && pwd
)"
readonly script_dir
readonly claude_workflow_path="${script_dir}/../../../.github/workflows/claude-code-review.yml"
readonly biome_workflow_path="${script_dir}/../../../.github/workflows/update-biome-schema.yml"

ruby - "${claude_workflow_path}" "${biome_workflow_path}" <<'RUBY'
# -*- coding: utf-8 -*-
require "yaml"

claude_workflow_path = ARGV.fetch(0)
biome_workflow_path = ARGV.fetch(1)

claude_workflow = YAML.load_file(claude_workflow_path)
claude_workflow_source = File.read(claude_workflow_path, encoding: "UTF-8")
claude_on_config = claude_workflow["on"] || claude_workflow[true] || {}
claude_schedule = claude_on_config["schedule"] || []
raise "claude-review workflow must keep a scheduled catch-up trigger for stale head/base-bound verdicts" unless claude_schedule.any? { |entry| entry["cron"] == "*/15 * * * *" }
raise "claude-review workflow must not grow a workflow_dispatch-only maintenance path" if claude_on_config.key?("workflow_dispatch")
raise "claude-review workflow must rerun for reopened PRs" unless claude_workflow_source.match?(/types:\s*\[\s*opened,\s*synchronize,\s*reopened\s*\]/)

collect_job = claude_workflow.dig("jobs", "collect-target-prs")
raise "claude-review workflow must collect target PRs before reviewing" unless collect_job
raise "claude-review collector must stay repository-scoped" unless collect_job["if"] == "github.repository == 'falgon/roki-web'"
collect_step = collect_job.fetch("steps").find { |step| step["id"] == "collect" }
raise "claude-review collector step must exist" unless collect_step
collect_script = collect_step.dig("with", "script").to_s
raise "claude-review collector must allow app/dependabot PRs" unless collect_script.include?('login === "app/dependabot"')
raise "claude-review collector must still allow dependabot[bot] PRs" unless collect_script.include?('login === "dependabot[bot]"')
raise "claude-review collector must still allow github-actions[bot] PRs" unless collect_script.include?('login === "github-actions[bot]"')
raise "claude-review collector must read the pull_request payload directly for PR-triggered runs" unless collect_script.include?('context.eventName === "pull_request"') && collect_script.include?("context.payload.pull_request")
raise "claude-review collector must list open develop bot PRs during scheduled catch-up runs" unless collect_script.include?("github.paginate(github.rest.pulls.list") && collect_script.include?('state: "open"') && collect_script.include?('base: "develop"')
raise "claude-review collector must skip scheduled reruns only after the current head/base already has a trusted clean machine-readable verdict" unless collect_script.include?("dependabot-claude-review-verdict") && collect_script.include?("github.rest.users.getAuthenticated") && collect_script.include?("github.rest.issues.listComments") && collect_script.include?("parsedVerdict.head_sha") && collect_script.include?("parsedVerdict.base_sha") && collect_script.include?('String(parsedVerdict.review_status ?? "") === "pass"') && collect_script.include?("parsedVerdict.finding_count") && collect_script.include?("parsedVerdict.summary") && collect_script.include?("passing machine-readable verdict")
raise "claude-review collector must fail closed when it cannot resolve the authenticated verdict publisher" unless collect_script.include?("ignore existing verdict comments because the authenticated verdict publisher could not be resolved") && collect_script.include?("if (!verdictPublisherLogin)")
raise "claude-review collector must keep rerunning while the current head still has Claude inline findings" unless collect_script.include?("github.rest.pulls.listReviewComments") && collect_script.include?("【Claude review】") && collect_script.include?("currentHeadFindingCount") && collect_script.include?("Re-running scheduled Claude review")
raise "claude-review collector must not retain dead workflow_dispatch fallback paths" if collect_script.include?("process.env.INPUT_PR_NUMBERS")

claude_review_job = claude_workflow.dig("jobs", "claude-review")
raise "claude-review job must depend on collected PR numbers" unless claude_review_job&.dig("needs") == "collect-target-prs"
claude_review_concurrency = claude_review_job.fetch("concurrency")
raise "claude-review.concurrency.group must be PR-scoped" unless claude_review_concurrency["group"] == "claude-review-pr-${{ matrix.pr-number }}"
raise "claude-review.concurrency.cancel-in-progress must be false" unless claude_review_concurrency["cancel-in-progress"] == false
load_pr_step = claude_review_job.fetch("steps").find { |step| step["id"] == "load-pr" }
raise "claude-review workflow must load live PR context before checkout" unless load_pr_step
load_pr_script = load_pr_step.dig("with", "script").to_s
raise "claude-review workflow must resolve the current PR head via pulls.get" unless load_pr_script.include?("github.rest.pulls.get")
raise "claude-review workflow must still restrict reviews to supported bot PRs" unless load_pr_script.include?('login === "app/dependabot"') && load_pr_script.include?('login === "dependabot[bot]"') && load_pr_script.include?('login === "github-actions[bot]"')
raise "claude-review workflow must expose the live PR base sha before review" unless load_pr_script.include?('core.setOutput("base-sha", baseSha);')
raise "claude-review workflow must checkout the loaded PR head before review" unless claude_workflow_source.include?("ref: ${{ steps.load-pr.outputs.head-sha }}")

allowed_bots = claude_review_job.fetch("steps")
  .find { |step| step["id"] == "claude-review" }
  .dig("with", "allowed_bots").to_s
raise "Claude action must allow app/dependabot" unless allowed_bots.include?("app/dependabot")
raise "Claude action must still allow dependabot" unless allowed_bots.include?("dependabot")
raise "Claude action must still allow github-actions" unless allowed_bots.include?("github-actions")

claude_review_step = claude_review_job.fetch("steps")
  .find { |step| step["id"] == "claude-review" }
raise "claude-review step must exist" unless claude_review_step
raise "claude-review step must use a writable Dependabot-available token for inline and sticky comments" unless claude_review_step.dig("with", "github_token") == "${{ secrets.UPDATE_BIOME_SCHEMA_TOKEN }}"
claude_prompt = claude_review_step.dig("with", "prompt").to_s
claude_args = claude_review_step.dig("with", "claude_args").to_s
raise "claude-review prompt must prefix actionable inline findings for later filtering" unless claude_prompt.include?("【Claude review】")
raise "claude-review prompt must require structured output only" unless claude_prompt.include?("structured output のみ")
raise "claude-review prompt must bind the structured verdict to the loaded current head/base sha" unless claude_prompt.include?("Head SHA: ${{ steps.load-pr.outputs.head-sha }}") && claude_prompt.include?("Base SHA: ${{ steps.load-pr.outputs.base-sha }}") && claude_prompt.include?("head_sha は ${{ steps.load-pr.outputs.head-sha }} と一致") && claude_prompt.include?("base_sha は ${{ steps.load-pr.outputs.base-sha }} と一致") && claude_prompt.include?("pr_number は ${{ steps.load-pr.outputs.pr-number }} と一致")
raise "claude-review step must request a machine-readable verdict schema with base_sha" unless claude_args.include?("--json-schema") && claude_args.include?('"review_status"') && claude_args.include?('"finding_count"') && claude_args.include?('"base_sha"')

publish_verdict_step = claude_review_job.fetch("steps")
  .find { |step| step["id"] == "publish-verdict" }
raise "claude-review workflow must publish a machine-readable verdict comment" unless publish_verdict_step
raise "publish-verdict step must use a writable Dependabot-available token instead of github.token" unless publish_verdict_step.dig("with", "github-token") == "${{ secrets.UPDATE_BIOME_SCHEMA_TOKEN }}"
publish_verdict_script = publish_verdict_step.dig("with", "script").to_s
raise "publish-verdict step must write the dependabot-claude-review marker comment" unless publish_verdict_script.include?("dependabot-claude-review-verdict")
raise "publish-verdict step must fail closed when the structured verdict is missing or invalid" unless publish_verdict_script.include?("structured_output") && publish_verdict_script.include?("core.setFailed")
raise "publish-verdict step must re-read the live PR head/base before updating the sticky verdict comment" unless publish_verdict_script.include?("github.rest.pulls.get") && publish_verdict_script.include?("currentHeadSha !== headSha") && publish_verdict_script.include?("currentBaseSha !== baseSha")
raise "publish-verdict step must skip sticky comment updates for stale Claude runs" unless publish_verdict_script.include?("Skipping verdict comment update for stale Claude review run")
raise "publish-verdict step must validate base_sha before publishing the verdict" unless publish_verdict_script.include?("parsedResult.base_sha") && publish_verdict_script.include?("did not match ${baseSha}")
raise "publish-verdict step must resolve the actual verdict publisher before updating the sticky comment" unless publish_verdict_script.include?("github.rest.users.getAuthenticated") && publish_verdict_script.include?("verdictPublisherLogin")
raise "publish-verdict step must fail closed when it cannot resolve the authenticated verdict publisher" unless publish_verdict_script.include?("Failed to resolve the authenticated verdict publisher login.")
raise "publish-verdict step must validate finding_count against the actual Claude inline comments" unless publish_verdict_script.include?("github.rest.pulls.listReviewComments") && publish_verdict_script.include?("【Claude review】") && publish_verdict_script.include?("currentHeadFindingCount") && publish_verdict_script.include?("did not match the actual inline finding count")
raise "publish-verdict step must reject pass verdicts while current-head Claude inline findings still exist" unless publish_verdict_script.include?("cannot report pass while current-head Claude inline findings still exist")
raise "publish-verdict step must not update arbitrary verdict comments when the publisher login is unresolved" if publish_verdict_script.include?("!verdictPublisherLogin ||")

biome_workflow = YAML.load_file(biome_workflow_path)
update_schema_if = biome_workflow.dig("jobs", "update-schema", "if").to_s
raise "update-biome-schema workflow must allow app/dependabot PRs" unless update_schema_if.include?("github.actor == 'app/dependabot'")
raise "update-biome-schema workflow must still allow dependabot[bot] PRs" unless update_schema_if.include?("github.actor == 'dependabot[bot]'")
raise "update-biome-schema workflow must stay scoped to Dependabot branches" unless update_schema_if.include?("contains(github.head_ref, 'dependabot')")
raise "update-biome-schema workflow must stay scoped to Biome branches" unless update_schema_if.include?("contains(github.head_ref, 'biome')")

puts "dependabot_support_workflows tests passed"
RUBY
