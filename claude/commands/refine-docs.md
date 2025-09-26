---
description: Review and refine documentation based on code changes
argument-hint: [sha1] [sha2] ...
allowed-tools: Bash(git show:*), Bash(git diff:*), Bash(git status:*), Bash(git log:*), Read, Edit, MultiEdit
---

## Documentation files found in project

!`rg --files . | rg '(CLAUDE.md|README.md|AGENTS.md)' | head -20`

## Recent commits

!`git log --oneline -10`

## Your task

Review the project's documentation files and check if they accurately reflect the latest state of the code.

**Arguments provided**: $ARGUMENTS

### Instructions:

1. **If commit SHAs were provided** (in $ARGUMENTS):
   - Use `git show <sha>` for each SHA to examine what changed in those specific commits
   - Focus your documentation review on the changes introduced in those commits
   - This is for retrospectively updating docs after changes have been committed

2. **If no arguments provided**:
   - Use `git status` to see uncommitted changes
   - Use `git diff` to see uncommitted modifications
   - Use `git diff --cached` to see staged changes
   - Focus on documenting any work from the current session

### What to look for:

1. Any new features, tools, or scripts that were created
2. Any modifications to existing functionality
3. Any new patterns or conventions that were established
4. Any important configuration changes
5. Changes to dependencies or build processes
6. Updates to CLI commands or their arguments

### Guidelines:

- Focus only on significant changes that would help future developers understand the project better
- Do not update documents for trivial changes like formatting or minor refactoring
- Ensure technical accuracy and completeness
- Update inline documentation in CLAUDE.md files if they contain agent instructions
- Consider if README files need updates for new features or changed workflows
