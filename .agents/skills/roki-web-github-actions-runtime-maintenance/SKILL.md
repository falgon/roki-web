---
name: roki-web-github-actions-runtime-maintenance
description: "Use when working on falgon/roki-web GitHub Actions runtime maintenance: Node runtime deprecation warnings, action version upgrades, workflow template drift, action SHA pinning for secret-consuming steps, Actions cache safety, Skicka cache handling, and PR verification before merge decisions."
---

# Roki Web GitHub Actions Runtime Maintenance

Use this for `falgon/roki-web` workflow updates that can affect CI, deploy, generated workflows, Actions cache, or secret-bearing jobs.

## Workflow

1. Treat `develop` as the PR base and comparison branch unless the user explicitly asks otherwise. Do not run `git checkout`, `git switch`, `git fetch`, `git pull`, `git push`, or other Git state-changing commands unless the user explicitly asked for that Git/GitHub operation.
2. Inspect all workflow entrypoints, not only the failing file:

```bash
rg -n "uses:|runs-on:|actions/cache|cache-apt|skicka|deploy_key|GITHUB_TOKEN|ACTIONS_PAT" .github/workflows tools/scheduled_post
```

3. For each changed or suspicious action, verify upstream `action.yml` rather than assuming a major tag is current:

```bash
gh api 'repos/OWNER/REPO/contents/action.yml?ref=REF' --jq '.content' | base64 --decode | rg -n "runs:|using:" -A3 -B1
gh api 'repos/OWNER/REPO/contents/SUBPATH/action.yml?ref=REF' --jq '.content' | base64 --decode | rg -n "runs:|using:" -A3 -B1
```

4. If an action is `composite`, inspect its internal `uses:` references. Composite wrappers can hide deprecated `actions/cache@v4`, `actions/upload-artifact@v4`, or other old runtimes.
5. Update generated workflow sources with the workflow they generate. In this repo, always check `tools/scheduled_post/template.yml` when touching deploy or scheduled-post workflow patterns.
6. Treat cache changes as security-sensitive. Never cache token, OAuth, config, or credential files.
7. Validate locally. Push a branch or create/update a PR only when the user explicitly asks for GitHub publishing or PR updates. For existing PRs, read PR state and watch checks before recommending merge, but do not push/update without explicit user instruction.

Keep scope explicit. Fix old-runtime actions in the changed workflows, generated templates, and hidden composite paths that can explain the current warning. Report repository-wide old-runtime actions outside that scope as follow-up unless the user asks to make the PR comprehensive.

## Runtime Upgrade Rules

- Prefer Node 24-capable releases when resolving GitHub's Node runtime deprecation warnings.
- Verify `runs.using` from the exact ref you plan to use. Do not assume a major version tag or floating latest tag points to a Node 24 implementation.
- For GitHub-owned low-secret actions, semver tags are acceptable unless the repo policy says otherwise.
- For third-party actions that receive secrets, deploy keys, PATs, `GITHUB_TOKEN`/`github.token`, registry credentials, or can publish artifacts/images, pin to a full 40-character commit SHA and leave a version comment.
- Also pin third-party setup/toolchain actions that run earlier in the same job before secret-bearing or publishing steps. They can mutate runner state, PATH, Docker builders, or build inputs for later credentialed steps.

```yaml
uses: owner/action@0123456789abcdef0123456789abcdef01234567 # v1.2.3
```

- Resolve annotated tags to the peeled commit before pinning:

```bash
git ls-remote https://github.com/OWNER/REPO.git refs/tags/vX.Y.Z refs/tags/vX.Y.Z^{}
```

Use the `^{}` SHA when present.

## Cache Safety

- Assume PR workflows can restore caches from the base/default branch. Do not put secrets in `actions/cache` paths.
- If removing a secret-bearing cache path, also change the cache key prefix so old archives cannot be restored.
- Delete known old secret-bearing caches only when the user explicitly authorizes cache deletion or GitHub cache-remediation write actions, and record the operational follow-up:

```bash
gh cache list --repo falgon/roki-web --key '<prefix>' --limit 100
gh cache delete --repo falgon/roki-web '<cache-id>'
```

- For Skicka in `build_pr.yml`, cache only non-secret install/build data. Keep `~/.skicka.tokencache.json` and `~/.skicka.config` out of the cache and regenerate them from secrets after cache restore.
- Before deleting, record the cache id, key, ref, size, and timestamps in the work log or PR comment.
- If a known secret-bearing cache still exists but the user did not explicitly authorize cache deletion or GitHub cache-remediation write actions, do not delete it. Report it as a pre-merge blocker/remediation item instead.
- If explicit cache deletion/remediation write authorization is present, delete the known secret-bearing cache as a pre-merge remediation because it removes a live exposure path. Verify the repo-scoped list is empty after deletion.
- If a real secret may have been cached, treat rotation of the affected token/client secret and provider access-log review as blocking security follow-ups before recommending merge. If the agent cannot perform them, recommend `BLOCK` unless a maintainer explicitly accepts the residual risk.

## roki-web Patterns

- Replace `awalsh128/cache-apt-pkgs-action@v1` with direct apt install when the goal is removing old runtime warnings:

```yaml
- name: Install tools (apt)
  run: |
    sudo apt-get update
    sudo apt-get install -y --no-install-recommends <packages>
```

- Keep the package list explicit. Current examples include `libxml2-utils`, `parallel`, `jq`, and `bc`.
- Check both workflow files and templates for old pins:

```bash
rg -n "actions/cache@v4|actions/cache/(restore|save)@v4|actions/upload-artifact@v4|actions/download-artifact@v4|cache-apt-pkgs-action|actions/checkout@v2|runs-on: ubuntu-20\\.04" .github/workflows tools/scheduled_post
```

- Note remaining out-of-scope old-runtime actions separately instead of silently expanding scope.

## Verification

Run these before finalizing a workflow-runtime PR:

```bash
git diff --check
ruby -e 'require "yaml"; ARGV.each { |f| YAML.load_file(f); puts "ok #{f}" }' <changed-yaml-files>
go run github.com/rhysd/actionlint/cmd/actionlint@v1.7.12 -color=false -ignore 'shellcheck reported issue' <changed-workflows>
make check
npm test
```

If `actionlint` reports pre-existing shellcheck noise, do not hide new action/runtime findings. Use the ignore only after confirming the remaining findings are unrelated to the current diff.

## Review And PR Gate

- Before publishing or recommending merge, get independent code review with at least two reviewers and one security reviewer when code/workflow files changed.
- Confirm the PR targets `develop` unless the user explicitly requested another base.
- Check PR state with:

```bash
gh pr view <pr> --json baseRefName,mergeStateStatus,reviewDecision,statusCheckRollup
gh pr checks <pr>
```

- Recommend merge only when required checks pass, review requirements are satisfied or explicitly accounted for, the base branch is correct, blocking security follow-ups are resolved or risk-accepted by a maintainer, and non-blocking security follow-ups are documented.
- Do not recommend merge while `reviewDecision=REVIEW_REQUIRED` or `mergeStateStatus=BLOCKED`, even if all checks pass. The correct conclusion is "checks are green, but do not merge until required review is satisfied."
