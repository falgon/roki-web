#!/usr/bin/env bash

set -euo pipefail

script_dir="$(
    cd "$(dirname "${BASH_SOURCE[0]}")" && pwd
)"
readonly script_dir
readonly workflow_path="${script_dir}/../../../.github/workflows/dependabot-auto-approve.yml"
readonly allowlist_path="${script_dir}/../non_npm_workflow_manifest_allowlist.txt"

ruby - "${workflow_path}" "${allowlist_path}" <<'RUBY'
# -*- coding: utf-8 -*-
require "yaml"

workflow_path = ARGV.fetch(0)
allowlist_path = ARGV.fetch(1)
workflow = YAML.load_file(workflow_path)
workflow_allowlist = File.readlines(allowlist_path, chomp: true)
  .map(&:strip)
  .reject { |line| line.empty? || line.start_with?("#") }
on_config = workflow["on"] || workflow[true] || {}
workflow_run_workflows = on_config.dig("workflow_run", "workflows") || []

raise "dependabot-auto-approve workflow must rerun when Claude Code Review for Bot PRs completes" unless workflow_run_workflows.include?("Claude Code Review for Bot PRs")

collect_if = workflow.dig("jobs", "collect-target-prs", "if")
raise "collect-target-prs.if must stay repository-scoped" unless collect_if == "github.repository == 'falgon/roki-web'"

auto_approve_job = workflow.dig("jobs", "auto-approve")
auto_approve_if = auto_approve_job.fetch("if").to_s
raise "auto-approve.if must require successful collector job" unless auto_approve_if.include?("needs.collect-target-prs.result == 'success'")
raise "auto-approve.concurrency.cancel-in-progress must be false" unless auto_approve_job.dig("concurrency", "cancel-in-progress") == false
raise "auto-approve job must grant actions:read for fallback workflow-run lookups" unless auto_approve_job.dig("permissions", "actions") == "read"
raise "auto-approve job must grant pull-requests:write for dismissal fallback" unless auto_approve_job.dig("permissions", "pull-requests") == "write"

collect_step = workflow.dig("jobs", "collect-target-prs", "steps").find { |step| step["id"] == "collect" }
raise "collect step missing" unless collect_step
collect_script = collect_step.dig("with", "script").to_s
raise "collect step must ignore non-pull_request workflow_run events" unless collect_script.include?('context.payload.workflow_run.event !== "pull_request"')
raise "collect step must ignore unsuccessful workflow_run events" unless collect_script.include?('context.payload.workflow_run.conclusion !== "success"')
raise "collect step must treat app/dependabot as Dependabot" unless collect_script.include?('login === "app/dependabot"') && collect_script.include?('login === "dependabot[bot]"')

evaluate_step = auto_approve_job.fetch("steps").find { |step| step["id"] == "evaluate" }
raise "evaluate step missing" unless evaluate_step
evaluate_script = evaluate_step.dig("with", "script").to_s
raise "evaluate step must treat app/dependabot as Dependabot" unless evaluate_script.include?('login === "app/dependabot"') && evaluate_script.include?('login === "dependabot[bot]"')
raise "evaluate step must keep npm candidates on the Claude workflow" unless evaluate_script.include?("Dependabot Claude auto-approve workflow")
raise "evaluate step must load the shared non-npm workflow allowlist" unless evaluate_script.include?('non_npm_workflow_manifest_allowlist.txt') && evaluate_script.include?('readFileSync') && evaluate_script.include?('supportedNonNpmWorkflowFilenames')
raise "evaluate step must limit non-npm candidates to allowlisted workflows plus Docker manifests" unless evaluate_script.include?("^\\.github\\/workflows\\/[^/]+\\.ya?ml$") && evaluate_script.include?("^docker\\/Dockerfile$") && evaluate_script.include?("^docker\\/docker-compose(?:-[^/]+)?\\.ya?ml$")
[
  ".github/workflows/cleanup-cache.yml",
  ".github/workflows/manually-cleanup-caches.yml",
].each do |workflow_filename|
  raise "shared workflow allowlist must contain #{workflow_filename}" unless workflow_allowlist.include?(workflow_filename)
end
raise "shared workflow allowlist must exclude workflow_dispatch-only maintenance workflows" if workflow_allowlist.include?(".github/workflows/manually-cleanup-artifacts.yml")
[
  ".github/workflows/build.yml",
  ".github/workflows/build_pr.yml",
  ".github/workflows/ci-lint-test.yml",
  ".github/workflows/claude.yml",
  ".github/workflows/claude-code-review.yml",
  ".github/workflows/codeql.yml",
  ".github/workflows/dependabot-auto-approve.yml",
  ".github/workflows/dependabot-claude-auto-approve.yml",
  ".github/workflows/kiirotori-auto-approve.yml",
  ".github/workflows/pr-labeler.yml",
  ".github/workflows/restyled.yml",
  ".github/workflows/update-biome-schema.yml",
].each do |workflow_filename|
  raise "shared workflow allowlist must exclude privileged workflow #{workflow_filename}" if workflow_allowlist.include?(workflow_filename)
end
raise "evaluate step must classify non-npm version updates from file patches when listFiles provides them" unless evaluate_script.include?("classifyNonNpmVersionUpdateFromFiles(files)") && evaluate_script.include?("extractReferencePairsFromPatch") && evaluate_script.include?("file.patch ?? \"\"")
raise "evaluate step must defer non-npm classification when listFiles omits patch details" unless evaluate_script.include?("file.patch == null") && evaluate_script.include?("Deferring non-npm patch classification to the snapshot and approval scripts")
raise "evaluate step must accept anonymous '- uses:' workflow steps" unless evaluate_script.include?('(?:-\\s*)?uses:')
raise "evaluate step must preserve action or image identity while pairing updates" unless evaluate_script.include?("beforeReference.identity !== afterReference.identity") && evaluate_script.include?("beforeReference.kind !== afterReference.kind")
raise "evaluate step must preserve Docker line context while pairing updates" unless evaluate_script.include?("contextKey") && evaluate_script.include?("beforeReference.contextKey") && evaluate_script.include?("platform/alias edits")
raise "evaluate step must classify Docker tags by matching stable prefix/suffix contexts instead of blindly taking the last numeric segment" unless evaluate_script.include?("extractVersionContexts") && evaluate_script.include?("for (const beforeParts of beforeContexts)") && evaluate_script.include?("for (const afterParts of afterContexts)")
raise "evaluate step must reject composite tags when their prefix or suffix changes" unless evaluate_script.include?("beforeParts.prefix !== afterParts.prefix") && evaluate_script.include?("beforeParts.suffix !== afterParts.suffix")
raise "evaluate step must reject unrelated edits inside allowlisted manifests" unless evaluate_script.include?("isPatchMetadataLine") && evaluate_script.include?("if (!reference) {") && evaluate_script.include?("return null;")
raise "evaluate step must stop relying on the PR title for non-npm version classification" if evaluate_script.include?("title.match(/^Bump .+ from (\\S+) to (\\S+)(?: in .+)?$/)")
raise "evaluate step must restrict non-npm auto-approval to patch updates" unless evaluate_script.include?('nonNpmVersionUpdateType !== "patch"') && evaluate_script.include?("only patch updates are auto-approved")
raise "evaluate step must no longer require package.json for non-npm candidates" if evaluate_script.include?("does not update package.json")
raise "evaluate step must output reviewed head sha" unless evaluate_script.include?('core.setOutput("head-sha", currentHeadSha);')
raise "evaluate step must output reviewed base sha" unless evaluate_script.include?('core.setOutput("base-sha", currentBaseSha);')
raise "evaluate step must query current head sha in GraphQL" unless evaluate_script.include?("headRefOid")
raise "evaluate step must query current base sha in GraphQL" unless evaluate_script.include?("baseRefOid")
raise "evaluate step must query rollup commit oid" unless evaluate_script.include?("statusCheckRollup") && evaluate_script.include?("commit {") && evaluate_script.include?("oid")
raise "evaluate step must query CheckRun timestamps" unless evaluate_script.include?("completedAt") && evaluate_script.include?("startedAt")
raise "evaluate step must query StatusContext timestamps" unless evaluate_script.include?("createdAt")
raise "evaluate step must reject head updates during required-check evaluation" unless evaluate_script.include?("const currentHeadSha = pullRequest.headRefOid ?? pr.head.sha;") && evaluate_script.include?("currentHeadSha !== pr.head.sha")
raise "evaluate step must reject base updates during required-check evaluation" unless evaluate_script.include?("const currentBaseSha = pullRequest.baseRefOid ?? pr.base.sha;") && evaluate_script.include?("currentBaseSha !== pr.base.sha")
raise "evaluate step must reject stale status rollups" unless evaluate_script.include?('const statusRollupCommitSha = pullRequest.statusCheckRollup?.commit?.oid ?? "";') && evaluate_script.include?("statusRollupCommitSha !== currentHeadSha")
raise "evaluate step must prioritize active reruns over stale successes" unless evaluate_script.include?("const statusPriority =") && evaluate_script.include?("status.isActive") && evaluate_script.include?("nextPriority > currentPriority")
raise "evaluate step must wait only for non-secret monitored check contexts on the non-npm route" unless evaluate_script.include?('const monitoredCheckContexts = [') && evaluate_script.include?('"build"') && !evaluate_script.include?('"upload"') && !evaluate_script.include?('"boot-circle-ci"') && evaluate_script.include?('"Analyze (javascript)"') && evaluate_script.include?('"restyled"') && !evaluate_script.include?('"claude-review",') && evaluate_script.include?("...monitoredCheckContexts") && !evaluate_script.include?("monitoredCheckContextsAllowedToBeMissing") && evaluate_script.include?("required or monitored checks")
raise "evaluate step must treat matrix-expanded Claude review check names as claude-review" unless evaluate_script.include?("const expandStatusNames = (statusName) => {") && evaluate_script.include?("/^claude-review \\(.+\\)$/") && evaluate_script.include?('[statusName, "claude-review"]')
raise "evaluate step must ignore unrelated non-required checks when collecting non-passing statuses" unless evaluate_script.include?("requiredOrMonitoredContexts.has(statusName) && !passing")
raise "evaluate step must require the shared Claude verdict for non-npm approvals" unless evaluate_script.include?("const requireCurrentHeadClaudeReviewVerdict = true;") && evaluate_script.include?("if (requireCurrentHeadClaudeReviewVerdict) {")
raise "evaluate step must still validate base-bound trusted Claude verdicts when that optional path is enabled" unless evaluate_script.include?("dependabot-claude-review-verdict") && evaluate_script.include?("issues.listComments") && evaluate_script.include?("resolveCollaboratorPermission") && evaluate_script.include?("getCollaboratorPermissionLevel") && evaluate_script.include?("trustedVerdictAuthorPermissions") && evaluate_script.include?("parsedVerdict.head_sha") && evaluate_script.include?("parsedVerdict.base_sha") && evaluate_script.include?('parsedVerdict.review_status !== "pass"') && evaluate_script.include?("workflow_run_id") && evaluate_script.include?("workflow_run_attempt")
raise "evaluate step must trust pull_request and scheduled Claude review runs, while keeping manual dispatches untrusted and same-head findings sticky across reruns when that optional path is enabled" unless evaluate_script.include?('const claudeReviewCommentPrefix = "【Claude review】"') && evaluate_script.include?("pulls.listReviewComments") && evaluate_script.include?("actions.getWorkflowRun") && evaluate_script.include?('candidateWorkflowRun.name ?? "") !==') && evaluate_script.include?("candidateWorkflowRun.path ?? \"\").includes(") && evaluate_script.include?('const trustedClaudeReviewEvents = new Set([') && evaluate_script.include?('"pull_request"') && evaluate_script.include?('"schedule"') && !evaluate_script.include?('"workflow_dispatch"') && evaluate_script.include?("trustedClaudeReviewEvents.has(") && evaluate_script.include?("candidateWorkflowRun.head_sha ?? \"\") !== currentHeadSha") && evaluate_script.include?("candidateVerdictCommentUpdatedAt < candidateWorkflowRunStartedAt") && evaluate_script.include?("candidateVerdictCommentUpdatedAt > candidateWorkflowRunCompletedAt") && !evaluate_script.include?("commentTimestamp >= workflowRunStartedAt") && evaluate_script.include?("commentTimestamp <= verdictCommentUpdatedAt") && evaluate_script.include?("currentHeadClaudeFindings.length > 0")
raise "evaluate step must stop after the newest trusted verdict for the current head/base reports findings or broken workflow metadata" unless evaluate_script.match?(/Claude review did not explicitly report zero findings for the current head\/base[\s\S]{0,400}break;/) && evaluate_script.match?(/Claude review workflow run metadata no longer matches the trusted verdict[\s\S]{0,200}break;/)
raise "evaluate step must match inline findings to the trusted verdict publisher instead of github-actions[bot]" unless evaluate_script.include?("const trustedVerdictAuthorLogin = String(") && evaluate_script.include?("comment.user?.login === trustedVerdictAuthorLogin")
raise "evaluate step must not hard-code github-actions[bot] for machine-readable verdict comments" if evaluate_script.match?(/comment\.user\?\.login === "github-actions\[bot\]"[\s\S]{0,200}comment\.body\.includes\(verdictCommentMarker\)/)

prepare_step = auto_approve_job.fetch("steps").find { |step| step["id"] == "prepare" }
raise "prepare step missing" unless prepare_step
prepare_env = prepare_step.fetch("env")
raise "prepare step must reuse the read-status PAT" unless prepare_env["GH_TOKEN"] == "${{ secrets.DEPENDABOT_AUTO_APPROVE_READ_BRANCH_PROTECTION_TOKEN }}"
raise "prepare step must pin the non-npm candidate kind" unless prepare_env["DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND"] == "non-npm"
raise "prepare step must require the shared Claude verdict for non-npm approvals" unless prepare_env["DEPENDABOT_AUTO_APPROVE_REQUIRE_CURRENT_HEAD_CLAUDE_REVIEW_VERDICT"] == "true"
raise "prepare step must revalidate candidate conditions before capturing the approval snapshot" if prepare_env.key?("SKIP_CANDIDATE_VALIDATION")
raise "prepare step must pin the evaluated head sha" unless prepare_env["EXPECTED_HEAD_SHA"] == "${{ steps.evaluate.outputs.head-sha }}"
raise "prepare step must pin the evaluated base sha" unless prepare_env["EXPECTED_BASE_SHA"] == "${{ steps.evaluate.outputs.base-sha }}"

approve_step = auto_approve_job.fetch("steps").find { |step| step["id"] == "approve" }
raise "approve step missing" unless approve_step
approve_run = approve_step.fetch("run").to_s
approve_env = approve_step.fetch("env")
raise "approve step must execute the shared approval script" unless approve_run.include?("bash tools/github-actions/approve_dependabot_pr.sh")
raise "approve step must pass the branch protection PAT separately" unless approve_env["DEPENDABOT_AUTO_APPROVE_READ_BRANCH_PROTECTION_TOKEN"] == "${{ secrets.DEPENDABOT_AUTO_APPROVE_READ_BRANCH_PROTECTION_TOKEN }}"
raise "approve step must revalidate the non-npm candidate kind" unless approve_env["DEPENDABOT_AUTO_APPROVE_CANDIDATE_KIND"] == "non-npm"
raise "approve step must require the shared Claude verdict for non-npm approvals" unless approve_env["DEPENDABOT_AUTO_APPROVE_REQUIRE_CURRENT_HEAD_CLAUDE_REVIEW_VERDICT"] == "true"
raise "approve step must pass REVIEWED_HEAD_SHA from prepare output" unless approve_env["REVIEWED_HEAD_SHA"] == "${{ steps.prepare.outputs.head-sha }}"
raise "approve step must pass REVIEWED_BASE_SHA from prepare output" unless approve_env["REVIEWED_BASE_SHA"] == "${{ steps.prepare.outputs.base-sha }}"
raise "approve step must pass REVIEWED_DIFF_PATH from prepare output" unless approve_env["REVIEWED_DIFF_PATH"] == "${{ steps.prepare.outputs.input-dir }}/diff.patch"
raise "approve step must enable dismissal fallback token" unless approve_env["REVIEW_DISMISSAL_FALLBACK_TOKEN"] == "${{ github.token }}"
raise "approve step must revalidate candidate conditions before approval" if approve_env.key?("SKIP_CANDIDATE_VALIDATION")

puts "dependabot_auto_approve_workflow tests passed"
RUBY
