# Oven Workspace

Bun workspace for managing TypeScript/JavaScript executables, all `bin` files will be built to `~/local/bin/*`

## Structure

- `bin/` - Source TypeScript files for CLI tools
- `bin/shared/` - Shared modules used by CLI tools
- `scripts/` - Build and utility scripts

## Commands

```bash
# Format all code with Biome
bun run fmt

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
2. Run `bun run fmt` to format the code
3. Run `bun run build` to compile to executable

## Tools Included

- **analyze-subagents** - Analyze Claude Code subagent usage from session logs
- **bra** - Git branch switcher with fzf
- **claude-code-logger** - Claude Code session hook for logging events
- **extract-commit-dialogue** - Extract Claude Code dialogue for a specific commit
- **extract-dialogue** - Extract Claude Code session dialogue from log files
- **fromPhrase** - Search for phrases in locale files
- **ghub** - Open GitHub/GitLab repository in browser
- **gitlab-pipeline-watcher** - Monitor GitLab pipelines and send notifications
- **gmr** - GitLab merge request helper
- **isPhrase** - Check if a string is a valid phrase key
- **notif** - Show macOS native notifications

### Quick Usage

All tools support the `-h` or `--help` flag to display usage information:

```bash
# Get help for any tool
analyze-subagents -h
bra --help
```
