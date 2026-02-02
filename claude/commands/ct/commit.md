---
description: Create a git commit with a well-crafted conventional commit message
argument-hint: [optional context about changes]
allowed-tools: Bash(git:*)
disable-model-invocation: true
---

# Create Git Commit

Create a git commit following your standard git commit workflow. The user has completed work and is ready to commit their changes.

## User Context About Changes

$ARGUMENTS

## Current Repository State

- **Current git status**: !`git status`
- **Current git diff** (staged and unstaged changes): !`git diff HEAD`
- **Current branch**: !`git branch --show-current`
- **Recent commits** (for style reference): !`git log --oneline -10`

## Your Task

Based on the above changes and user context, create a single git commit following these guidelines:

### Commit Message Guidelines

1. **Format**: Use conventional commits (feat:, fix:, docs:, refactor:, chore:, test:, style:)
2. **First line**: 50 characters or less, imperative mood
3. **Focus**: Explain "why" rather than "what" (code shows the what)
4. **Style**: Match the style of recent commits in this repository

### Quality Checks

- Exclude temporary files (.env, *.log, .DS_Store, build artifacts, etc.)
- Ensure message accurately reflects the changes
- Verify all relevant changes are included

### Execution Steps

1. Add relevant files with `git add` (exclude temp files)
2. Create commit with message using HEREDOC format:
   ```bash
   git commit -m "$(cat <<'EOF'
   Your commit message here
   EOF
   )"
   ```
3. Run `git status` after to verify success

### Pre-commit Hook Handling

- If hooks modify files and it's safe to amend (check authorship with `git log -1 --format='%an %ae'` and verify not pushed), amend the commit
- Otherwise create a new commit

**IMPORTANT**: Do not push to remote unless explicitly requested.
