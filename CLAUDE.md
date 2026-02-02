# Dotfiles Monorepo (Agent instructions via CLAUDE.md)

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Directories

- **boot/** - System setup scripts. Go here to bootstrap a new machine.
- **claude/** - Global Claude Code configurations and agent documentation. Go here for multi-agent specs and hooks.
- **config/** - Application dotfiles (vim, kitty, nushell, etc). Go here to modify tool configurations.
- **home/** - Files that belong in home directory. Go here for home-specific scripts and configs.
- **oven/** - TypeScript/Bun workspace for CLI tools. Go here for active development.
- **specs/** - Feature specifications. Go here to write or review specs before implementation.

### Makefile (Root)

```bash
make         # Run link then build (default) - quiet output, errors only
make link    # Link dotfiles via dotty
make build   # Build oven executables
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
   - Location: `oven/bin/*.ts`
   - When: Complex logic, dependencies, async task control or shared code
   - Notes:
     - `$ make build` builds executables to `~/.local/bin/` (in PATH)
     - Full development environment with testing

### Script Evolution Path

Start simple → Graduate as needed:

1. Try as nushell alias first
2. Expand to nushell function if needed
3. Create bash script in `home/.local/bin/` for standalone tools
4. Migrate to `oven/` when exceeding 200 lines or needing TypeScript
5. Build with `make build` (or `bun run build` inside `oven/`) to create executable

## Claude code integrations

This repo defined claude code configurations such as commands and agents at `claude/`. These include hooks, commands and agents, and the `claude/README.md` gives a compressive overview of all aspects, including the dependencies on any scripts from the `oven` module.
