# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Directories

- **boot/** - System setup scripts. Go here to bootstrap a new machine.
- **claude/** - Claude Code configurations and agent documentation. Go here for multi-agent specs and hooks.
- **config/** - Application dotfiles (vim, kitty, nushell, etc). Go here to modify tool configurations.
- **home/** - Files that belong in home directory. Go here for home-specific scripts and configs.
- **oven/** - TypeScript/Bun workspace for CLI tools. Go here for active development.
- **specs/** - Feature specifications. Go here to write or review specs before implementation.

## Oven Development Commands

```bash
cd oven
bun run fmt      # Format code
bun run lint     # Lint code
bun run check    # Check formatting and linting
bun run fix      # Fix issues
bun run build    # Build executables to ~/.local/bin
bun run syncup   # Update documentation
bun run ./bin/<filename>  # Test before building
```
