---
name: git-commit-with-coauthor
description: Git commit operations with mandatory Claude Code co-author attribution and GPG signature bypass. Use this skill when committing staged changes to a Git repository. The skill analyzes changes, generates conventional commit messages in Japanese, and automatically includes Claude <noreply@anthropic.com> as co-author, and always uses the --no-gpg-sign option.
---

# Git Commit with Co-author

## Overview

Automate Git commit operations with specific constraints: always bypass GPG signing using `--no-gpg-sign` and always include Claude Code (Claude <noreply@anthropic.com>) as co-author in commit messages. The skill analyzes staged changes, generates appropriate commit messages following Conventional Commits format, and ensures all required constraints are met.

### Why This Skill Exists

This skill serves as an enforcement layer for project-specific Git commit constraints. While git-flow-manager provides general Git operations, this skill MUST use git-flow-manager and MUST ensure:

- **Consistent co-authorship**: Every commit automatically credits Claude Code, maintaining accurate contribution tracking
- **GPG bypass enforcement**: Prevents commit failures in environments where GPG signing is not configured
- **Standardized workflow**: Provides a single entry point for commits that guarantees compliance with project policies

Without this skill, users or agents would need to manually remember these constraints for every commit, increasing the risk of non-compliant commits.

## When to Use This Skill

Trigger this skill when:
- Committing staged changes to a Git repository
- Creating a commit with automatic Claude Code co-author attribution
- Ensuring GPG signing is disabled for the commit

## Workflow

### Step 1: Delegate to git-flow-manager Subagent

**MUST** use the Task tool to invoke the git-flow-manager subagent with the following parameters:

**Task tool invocation**:
- `subagent_type`: `git-flow-manager`
- `description`: "Git コミット実行"
- `prompt`: (下記のプロンプト)

**Prompt to git-flow-manager**:
```
ステージされた変更を以下の手順でコミットしてください：

1. ステージされた変更を確認してください（git status）
2. 変更内容を分析してください（git diff --cached）
3. Conventional Commits 形式で日本語のコミットメッセージを生成してください
   - 使用可能なタイプ: feat, fix, docs, refactor, test, chore, style, perf
   - 形式: <type>: <subject>
4. 以下の制約を守ってコミットを実行してください：
   - 必ず --no-gpg-sign オプションを使用すること（これは必須の環境制約です）
   - Co-authored-by: Claude <noreply@anthropic.com> を含めること
5. コミット結果を報告してください
```

> **Note**: The prompt above includes all mandatory constraints from the "Constraints" section. git-flow-manager must adhere to these without exception.

### Step 2: Report Results

After git-flow-manager completes the operation:

**Option A - Relay Output** (Preferred):
If git-flow-manager provides a complete summary, relay that output directly to the user without additional processing.

**Option B - Supplement Output**:
If git-flow-manager's output lacks key information (e.g., commit hash is not displayed, or co-author line is missing from the output), supplement with:
- Commit hash (if not shown in the original output)
- Co-author confirmation (if `Co-authored-by:` line is not displayed)

**Error Propagation**:
If errors occur, git-flow-manager handles them according to its error handling procedures. This skill propagates those error messages to the user unchanged.

## Error Handling

The git-flow-manager subagent handles common error scenarios gracefully. This skill propagates error messages from git-flow-manager to the user unchanged.

**Expected error scenarios handled by git-flow-manager**:

- **No staged changes**: "ステージされた変更がありません。`git add` を使用してファイルをステージしてください。"
- **Not a Git repository**: "現在のディレクトリはGitリポジトリではありません。"
- **Commit fails**: Analyzes the error message, explains the issue, and suggests specific solutions
- **Pre-commit hooks fail**: Shows hook output and asks user whether to bypass hooks or fix issues
- **Committed unintended changes**: Explains how to revert: "`git reset --soft HEAD~1` で直前のコミットを取り消し、変更をステージ状態に戻すことができます。"

## Constraints

**Mandatory requirements** that must never be violated:

1. **Always use `--no-gpg-sign`**: This option MUST be included in every commit command without exception
2. **Co-author is required**: Every commit MUST include the co-author line for Claude Code
3. **No automatic push**: MUST never run `git push` without explicit user permission
4. **Always use git-flow-manager**: This skill MUST delegate all git operations to the git-flow-manager subagent without exception

## Usage Example

### User Input
```
ステージされた変更をコミットしてください
```

### Expected Workflow

1. **Invoke git-flow-manager** (Step 1):

   **This skill execution**:
   ```
   Task tool を使用して git-flow-manager を呼び出します
   - subagent_type: git-flow-manager
   - description: "Git コミット実行"
   - prompt: "ステージされた変更を以下の手順でコミットしてください：..."
   ```

   **git-flow-manager output**:
   ```
   ✓ ステージされた変更を確認しています...
   ブランチ: feature/user-auth
   変更されたファイル: 3個

   ✓ 変更内容を分析しています...
   - src/auth/login.py (追加)
   - src/auth/validation.py (追加)
   - tests/test_auth.py (追加)

   ✓ コミットメッセージを生成しています...

   ✓ コミットを実行しています...
   [feature/user-auth a1b2c3d] feat: ユーザー認証機能を追加
    3 files changed, 150 insertions(+)

   Co-authored-by: Claude <noreply@anthropic.com>
   ```

   > **Note**: The output format above is illustrative. Actual output from git-flow-manager may vary depending on the repository state, branch name, and changed files.

2. **Report Results** (Step 2):
   
   Relay git-flow-manager output directly (applying Option A).

## Notes

**Skill Characteristics**:
- Delegates Git operations to the git-flow-manager subagent, ensuring consistency and reliability
- Integrates seamlessly with existing Git workflows

**Features Delegated to git-flow-manager**:
- Compatibility with pre-commit hooks (displays hook output if hooks fail)
- Support for simple and complex commit message structures
- Commit messages optimized for Japanese projects (adaptable to other languages)

**Mandatory Constraints**:
- Co-author is automatically set to Claude Code for all commits
- The `--no-gpg-sign` constraint is absolute and enforced by environment requirements
