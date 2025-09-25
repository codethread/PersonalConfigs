# Dotfiles Monorepo (Agent instructions via CLAUDE.md)

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Discovery

Use `cindex` to understand the codebase structure:

```bash
cindex --markdown    # Generate markdown index of entire project with descriptions
cindex --help        # See all cindex options
cindex --path <dir>  # List all files recursively at path (with less relavent files filtered out)
```

## Directories

- **boot/** - System setup scripts. Go here to bootstrap a new machine.
- **claude/** - Global Claude Code configurations and agent documentation. Go here for multi-agent specs and hooks.
- **config/** - Application dotfiles (vim, kitty, nushell, etc). Go here to modify tool configurations.
- **home/** - Files that belong in home directory. Go here for home-specific scripts and configs.
- **oven/** - TypeScript/Bun workspace for CLI tools. Go here for active development.
- **specs/** - Feature specifications. Go here to write or review specs before implementation.

## Build Commands

### Makefile (Root)

```bash
make         # Run link then build (default) - quiet output, errors only
make link    # Link dotfiles via dotty
make build   # Build oven executables
```

### Oven Development

```bash
cd oven
bun run fmt      # Format code
bun run lint     # Lint code
bun run check    # Check formatting and linting
bun run fix      # Fix issues (quiet by default)
bun run build    # Build executables to ~/.local/bin (quiet by default, use -v for verbose)
bun run sync-docs   # Update documentation (quiet by default, use -v for verbose)
bun run ./bin/<filename>  # Test before building
```

## Tool Development Workflow

### Development Hierarchy

Build tools in order of increasing complexity:

1. **Nushell alias** (1 line)
   - Location: `config/nushell/scripts/ct/alias/*.nu`
   - Example: `export alias ga = git add`
   - When: Simple command shortcuts

2. **Nushell function** (3-4 lines)
   - Location: `config/nushell/scripts/ct/<category>/mod.nu`
   - Example: Short functions like `git_current_branch`
   - When: Need parameters or simple logic

3. **Bash script** (≤200 lines)
   - Location: `home/.local/bin/`
   - Example: `home/.local/bin/nush`
   - When: Composition of multiple tools, and or needs to be globally available in PATH for non-tty usage
   - Notes:
     - `$ make link` links executables to `~/.local/bin/` (in PATH)
     - Always use bash (not zsh or other shells)
     - Add `:module:` comment for documentation
     - Use `-h` or `--help` flag for usage info

4. **TypeScript/Bun** (>200 lines or needs dependencies)
   - IMPORTANT: read `oven/AGENTS.md` docs for details when working here
   - Location: `oven/bin/*.ts`
   - When: Complex logic, dependencies, async task control or shared code
   - Notes:
     - `$ make build` builds executables to `~/.local/bin/` (in PATH)
     - Full development environment with testing
     - **Shared modules**: Use `oven/shared/*.ts` for reusable types and utilities
     - **Claude Code hooks**: Import types from `oven/shared/claude-hooks.ts` for hook development
     - **Quiet by default**: Build scripts support `--verbose` flag for detailed output

### Script Evolution Path

Start simple → Graduate as needed:

1. Try as nushell alias first
2. Expand to nushell function if needed
3. Create bash script in `home/.local/bin/` for standalone tools
4. Migrate to `oven/` when exceeding 200 lines or needing TypeScript
5. Build with `make build` (or `bun run build` inside `oven/`) to create executable

## Claude Code Integration

### Naming Convention for Claude Code Tools

Claude Code specific tools follow the `cc-<context>--<action>` naming pattern:

- **cc-hook--**: Tools that run as Claude Code hooks (e.g., `cc-hook--context-injector`, `cc-hook--session-logger`)
- **cc-logs--**: Tools that process Claude Code session logs (e.g., `cc-logs--analyze-subagents`, `cc-logs--extract-dialogue`)

### Available Claude Code Hooks

The project includes several Claude Code hooks in the `oven/bin/` directory:

- **`cc-hook--context-injector`** - Automatically provides relevant AGENTS.md documentation when files are read
  - Session-based deduplication to avoid spam
  - Recursive discovery from file location up to project root
  - Hooks into SessionStart, SessionEnd, and PostToolUse (Read) events
  - Provides contextual documentation in XML tags

- **`cc-hook--npm-redirect`** - Redirects npm/npx/node commands to the detected package manager
  - Detects project package manager from lock files (pnpm-lock.yaml, bun.lock, bun.lockb, yarn.lock, package-lock.json)
  - Redirects node commands to bun in Bun projects (with compatibility warnings)
  - Blocks mismatched commands with exit code 2 and suggests correct alternative
  - Supports commands with `cd` directory changes
  - Ignores package manager names inside quoted strings

- **`cc-hook--session-logger`** - Logs Claude Code session activities for analysis

All hooks are built as standalone executables to `~/.local/bin/` and follow the oven development patterns with comprehensive unit tests.
