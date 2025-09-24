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
make         # Run link then build (default)
make link    # Link dotfiles via dotty
make build   # Build oven executables
```

### Oven Development

```bash
cd oven
bun run fmt      # Format code
bun run lint     # Lint code
bun run check    # Check formatting and linting
bun run fix      # Fix issues
bun run build    # Build executables to ~/.local/bin
bun run sync-docs   # Update documentation
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

### Script Evolution Path

Start simple → Graduate as needed:

1. Try as nushell alias first
2. Expand to nushell function if needed
3. Create bash script in `home/.local/bin/` for standalone tools
4. Migrate to `oven/` when exceeding 200 lines or needing TypeScript
5. Build with `make build` (or `bun run build` inside `oven/`) to create executable

## Claude Code Integration

### Automatic Context Provision

The project includes an intelligent context system that automatically provides relevant AGENTS.md documentation to Claude Code:

- **`agent-context-provider`** - Hook-based system that discovers and provides contextual documentation
- **Session-based deduplication** - Tracks seen AGENTS.md files per session to avoid spam
- **Recursive discovery** - Finds all AGENTS.md files from current file location up to project root
- **Smart integration** - Hooks into SessionStart, SessionEnd, and PostToolUse (Read) events

When Claude Code reads any file, it automatically receives relevant AGENTS.md context in XML tags without duplication.
