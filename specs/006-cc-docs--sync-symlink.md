# Spec 006: cc-docs--sync-symlinks

## Status: Completed (with chained symlink enhancement)

## Brief

Similar to `@home/.local/bin/claude-symlink` but make it more bidirectional. The tool should find all CLAUDE.md and AGENTS.md in a project (using `rg` or `fd` cli). Then ensure there is an appropriate symlink where the CLAUDE.md is pointing to a neighbouring AGENTS.md.

Consider all test cases for this where one exists and the other doesn't, or both exist, one exists but is a symlink already, or a broken/invalid symlink etc. The behaviour should be that the AGENTS.md becomes the 'source of truth' and the CLAUDE.md symlinks to it. This may require renaming the files as appropriate.

If both files exist and neither are symlinks, warn in the output but don't change anything. This command should provide a 'dry-run' which is the DEFAULT and then have a `--run` flag to actually make the change happen.

The cli should also offer a `--force` flag to handle the warning case we mentioned above where both files exist - in this case pick the most recently edited of the two AGENTS.md or CLAUDE.md and that content is written to the final AGENTS.md.

For tests, setup up temporary files and folders, don't mock anything (so it may be helpful to give the cli a `--cwd` optional flag to set the working directory, which defaults to the PWD).

### Chained Symlink Use Case

**Scenario**: A shared repository of agentic files that is symlinked into multiple git worktrees.

**Example structure**:
```
# Project directory
/Users/adamhall/work/app/deals-light-ui/CLAUDE.md -> /Users/adamhall/dev/ai-tools/ai-files/claude/CLAUDE.md

# Shared repo directory
/Users/adamhall/dev/ai-tools/ai-files/claude/CLAUDE.md -> AGENTS.md
/Users/adamhall/dev/ai-tools/ai-files/claude/AGENTS.md (actual file)
```

**Desired behavior**:
1. When CLAUDE.md is a symlink pointing outside the scanned directory tree, follow the symlink chain to determine if it's valid
2. Check if the chain ultimately resolves to an AGENTS.md file
3. If the chain is valid (CLAUDE.md -> CLAUDE.md -> AGENTS.md), mark as "already correct (chained)"
4. If the intermediate CLAUDE.md is a regular file (not yet a symlink to AGENTS.md), report that the chain is incomplete
5. The tool can be run in the shared repo directory to fix the intermediate symlink

**New scenarios to handle**:
- CLAUDE.md is symlink to external path -> check if chain resolves to AGENTS.md
- CLAUDE.md is symlink to external CLAUDE.md -> check if that external CLAUDE.md links to AGENTS.md
- Handle chains of arbitrary depth (with a reasonable max depth like 10 to prevent infinite loops)

## Implementation Plan

### Tool Name
`cc-docs--sync-symlinks` (follows `cc` domain naming convention)

### CLI Interface

**Flags:**
- `--run` - Execute changes (default is dry-run mode)
- `--force` - Resolve conflicts by keeping most recently edited content
- `--cwd <path>` - Set working directory (defaults to PWD, useful for testing)
- `--help, -h` - Show help

**Default behavior:** Dry-run mode (safe by default)

### Core Logic

1. **Discover files**: Find all `CLAUDE.md` and `AGENTS.md` files using `rg --files | rg '(CLAUDE|AGENTS)\.md$'`

2. **Group by directory**: Organize files by their parent directory

3. **Handle each directory** with these scenarios:

   | AGENTS.md | CLAUDE.md | Action |
   |-----------|-----------|--------|
   | regular file | doesn't exist | Create symlink: CLAUDE.md → AGENTS.md |
   | regular file | valid symlink | ✓ Already correct |
   | regular file | broken symlink | Fix symlink: CLAUDE.md → AGENTS.md |
   | regular file | regular file | **Conflict**: warn (unless --force) |
   | regular file | symlink (wrong target) | Fix symlink: CLAUDE.md → AGENTS.md |
   | doesn't exist | regular file | Rename CLAUDE.md → AGENTS.md, create symlink |
   | doesn't exist | symlink | Remove broken CLAUDE.md symlink |
   | doesn't exist | doesn't exist | (Skip - shouldn't happen) |

4. **Force mode conflict resolution**:
   - Compare modification times using `stat`
   - Keep newest content → write to AGENTS.md
   - Create CLAUDE.md → AGENTS.md symlink

### Test Cases

1. **Basic scenarios**:
   - Only AGENTS.md exists → create symlink
   - Only CLAUDE.md exists → rename to AGENTS.md, create symlink
   - Both exist as regular files → warn (no --force)
   - Both exist as regular files + --force → use newest

2. **Symlink scenarios**:
   - CLAUDE.md is valid symlink → no action
   - CLAUDE.md is broken symlink → fix
   - CLAUDE.md points to wrong file → fix

3. **Chained symlink scenarios**:
   - CLAUDE.md → external CLAUDE.md → AGENTS.md (valid chain)
   - CLAUDE.md → external CLAUDE.md (regular file, not yet symlinked)
   - CLAUDE.md → external path → AGENTS.md (skip intermediate CLAUDE.md)
   - CLAUDE.md → circular reference (detect and warn)
   - CLAUDE.md → deep chain (>10 levels, warn)

4. **Edge cases**:
   - Empty directory
   - Files with same mtime
   - Permission errors
   - Invalid symlinks

### Output Format

Example dry-run output:
```
🔍 Scanning for CLAUDE.md and AGENTS.md files...

Found 3 directories with documentation files:

📁 /path/to/project
  ✓ Would create: CLAUDE.md -> AGENTS.md

📁 /path/to/another
  ⚠️  Conflict: Both files exist (use --force to resolve)
      AGENTS.md (modified: 2025-01-10)
      CLAUDE.md (modified: 2025-01-09)

📁 /path/to/third
  ✓ Already correct: CLAUDE.md -> AGENTS.md

Summary: 1 to create, 1 conflict, 1 correct
💡 Run with --run to apply changes
```

## Todo Items

- [x] Design CLI interface and plan implementation
- [x] Implement the core library function
  - [x] File discovery logic
  - [x] Directory grouping
  - [x] File state detection (regular, symlink, broken, etc)
  - [x] Action determination logic
  - [x] Force mode conflict resolution
  - [x] Dry-run vs run mode execution
- [x] Write comprehensive tests
  - [x] Basic scenarios (only AGENTS, only CLAUDE, both)
  - [x] Symlink scenarios (valid, broken, wrong target)
  - [x] Force mode tests
  - [x] Edge cases
- [x] Run biome check, bun test, and bun typecheck
- [x] Test with bun run for fast feedback
- [x] Run make to build final executable
- [ ] **Chained symlink enhancement**
  - [ ] Implement symlink chain resolution (follow links to final target)
  - [ ] Detect when CLAUDE.md points to external CLAUDE.md that should link to AGENTS.md
  - [ ] Add circular reference detection
  - [ ] Add max depth limit (10 levels)
  - [ ] Write tests for chained scenarios
  - [ ] Update output to show chain status
