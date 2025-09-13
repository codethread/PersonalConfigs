# Oven Workspace

Bun workspace for managing TypeScript/JavaScript executables.

## Structure

- `bin/` - Source TypeScript files for CLI tools
- `bin/shared/` - Shared modules used by CLI tools
- `scripts/` - Build and utility scripts

## Commands

```bash
# Format all code with Biome
bun run format

# Lint code
bun run lint

# Check formatting and linting
bun run check

# Fix formatting and linting issues (with unsafe fixes)
bun run fix

# Build all executables to ~/.local/bin
bun run build
```

## Adding New Tools

1. Create a new `.ts` file in `bin/` directory
2. Add appropriate shebang (e.g., `#!/usr/bin/env bun`)
3. Run `bun run format` to format the code
4. Run `bun run build` to compile to executable

## Tools Included

- `analyze-subagents` - Analyze Claude Code subagents
- `bra` - Git branch switcher with fzf
- `claude-code-logger` - Claude Code session logger
- `fromPhrase` - Search phrases in locale files
- `ghub` - Open GitHub/GitLab repo in browser
- `gitlab-pipeline-watcher` - Monitor GitLab pipelines
- `gmr` - GitLab merge request helper
- `isPhrase` - Check if string is a valid phrase key
