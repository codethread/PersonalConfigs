# Oven Workspace

Bun workspace for managing TypeScript/JavaScript executables, all `bin` files will be built to `~/local/bin/*`

## Structure

- `bin/` - Source TypeScript files for CLI tools
- `shared/` - Shared modules used by CLI tools
- `tests/` - Tests for `bin` files, following rails convention of `bin/myBin.ts` -> `tests/myBin.test.ts`
- `scripts/` - Build and utility scripts

## Commands

```bash
# Format and lint all code with Biome
bun run fix

# Check type definitions are correct
bun run fix

# Build all executables to ~/.local/bin
bun run build

# Run the full verification and build process
bun run verify
```

## Adding New Tools

1. Create a new `.ts` file in `bin/` directory
2. Run `bun run fmt` to format the code
3. Run `bun run build` to compile to executable

## Tools Included

- **bra** - Git branch switcher with fzf
- **cc-hook--context-injector** - Claude Code hook that provides project context at session start
- **cc-hook--npm-redirect** - Claude Code hook that redirects npm/npx/node commands to detected package manager
- **cc-hook--session-logger** - Claude Code hook for logging session events
- **cc-logs--analyze-subagents** - Analyze Claude Code subagent usage from session logs
- **cc-logs--extract-commit-dialogue** - Extract Claude Code dialogue for a specific commit
- **cc-logs--extract-dialogue** - Extract Claude Code session dialogue from log files
- **cc-speak** - Advanced text-to-speech tool with file and section reading support
- **cc-statusline** - Process Claude Code statusline data and display custom status
- **cindex** - Generate an index of files in the current project
- **ghub** - Open GitHub/GitLab repository in browser
- **git-cleanup** - Clean up build artifacts and log files from git projects
- **git-session** - Interactive git project session switcher using fzf
- **gitlab-pipeline-watcher** - Monitor GitLab pipelines and send notifications
- **notif** - Show macOS native notifications
- **prepend-comment** - Add or update module documentation comments
- **record-and-transcribe** - Record audio and transcribe using Whisper
- **speak-ts** - Text-to-speech wrapper optimized for TTS markers and better audio output
- **tts** - Optimized for text-to-speech (adds spoken indicators)

### Quick Usage

All tools support the `-h` or `--help` flag to display usage information:

```bash
# Get help for any tool
analyze-subagents -h
bra --help
```
