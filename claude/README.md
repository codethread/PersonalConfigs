# Claude Code Global Settings

This project is symlinked to `~/.claude` and allows sharing global Claude Code settings between machines.

## Version Status

Currently pinned to **Claude Code v1.0.88** due to significant regressions in later versions. This version appears more stable, but we should be mindful of newer features we may be missing:

- v1.0.124: `CLAUDE_BASH_NO_LOGIN` environment variable, improved tool permissions
- v1.0.126: `/context` command for Bedrock/Vertex, mTLS OpenTelemetry support

Custom directories (i.e., those not carrying Claude Code significance like `agents/` or `commands/`) are prefixed with `x-` for clarity and to avoid accidental name collision.

## Hooks Architecture

Claude Code hooks are orchestrated through `settings.json` and implemented as either:

- **TypeScript/Bun scripts** in `oven/bin/cc-hook--*.ts` (compiled to `~/.local/bin/`)
- **Shell scripts** in `claude/x-hooks/` (local to claude directory)

### Hook Event Flow

```mermaid
graph LR
    Start[Session Start] -->|cc-hook--context-injector| Init[Initialize context tracking]
    UserPrompt[User Prompt]
    PreTool[Pre Tool Use] -->|cc-hook--npm-redirect| NPM[Redirect npm/npx/node commands]
    PostTool[Post Tool Use] -->|cc-hook--context-injector| Context[Inject AGENTS.md context]
    PostTool -->|prettier| Format[Format JSON/MD files]
    Stop[Stop Event]
    End[Session End] -->|cc-hook--context-injector| Cleanup[Cleanup context tracking]
```

### Session Logging

All hook events are captured by `cc-hook--session-logger`:

```mermaid
graph LR
    subgraph "All Events"
        Event[Session Start / User Prompt / ...etc]
    end

    Event --> SessionLogger[cc-hook--session-logger]

    SessionLogger --> HookLogs[Hook logs in<br/>~/.local/share/claude-logs/]
    ClaudeCode[Claude Code] --> ProjectLogs[Project logs in<br/>~/.claude/projects/]

    HookLogs --> Aggregation[Log aggregation tools<br/>oven/bin/cc-logs--*]
    ProjectLogs --> Aggregation
```

### Log Aggregation Tools

The `oven/bin/cc-logs--*` commands provide analysis and aggregation of both hook logs and Claude Code project logs:

- **cc-logs--extract-dialogue**: Extract conversation dialogues from session logs
- **cc-logs--analyze-subagents**: Analyze subagent usage patterns and performance
- **cc-logs--extract-commit-dialogue**: Extract dialogues related to git commits

### Active Hooks

#### 1. **cc-hook--session-logger**

- **Summary**: Logs all Claude Code events to structured JSON files for analysis and debugging
- **Events**: All events (SessionStart, SessionEnd, PreToolUse, PostToolUse, UserPromptSubmit, Stop, SubagentStop, PreCompact, Notification)
- **Purpose**: Creates audit trail, enables session replay, supports analytics

#### 2. **cc-hook--context-injector**

- **Summary**: Injects relevant AGENTS.md documentation when files are read, with session-based deduplication
- **Events**: SessionStart (initialize), PostToolUse[Read] (inject context), SessionEnd (cleanup)
- **Purpose**: Provides contextual documentation without repetition, tracks seen documentation per session

#### 3. **cc-hook--npm-redirect**

- **Summary**: Intercepts npm/npx/node commands and redirects to the appropriate package manager (pnpm/bun/yarn)
- **Events**: PreToolUse[Bash]
- **Purpose**: Prevents package manager conflicts, ensures correct tool usage based on project lock files

### Hook Interactions and Dependencies

```mermaid
graph LR
    subgraph "Session Lifecycle"
        session-logger
    end

    subgraph "Context Management"
        context-injector
    end

    subgraph "Tool Redirects"
        npm-redirect
    end

    session-logger -.->|tracks all events| context-injector
    session-logger -.->|tracks all events| npm-redirect
    context-injector -->|prevents duplicate docs| context-injector
    npm-redirect -->|blocks incorrect commands| npm-redirect
```

### Data Flow Between Hooks

1. **Session State Sharing**:
   - `cc-hook--session-logger` creates session directories and transcript files
   - `cc-hook--context-injector` uses session ID to track which documentation has been shown
   - Both hooks use environment variables like `CLAUDE_PROJECT_DIR` and session IDs

2. **Event Cascading**:
   - Hooks don't directly communicate but process events in sequence
   - Each hook can block further execution (exit code 2) or allow continuation

3. **File System Coordination**:
   - Session logs stored in `~/.local/share/claude-logs/sessions/<date>/<session-id>/`
   - Context state stored in `/tmp/claude-context-<session-id>.json`
   - All hooks respect the project directory structure

## Custom Slash Commands

### Naming Convention

To help find commands amongst, there are some common patterns:

- `prime-*`: Set up context for a specific task, allowing for more direct context loading than relying on a cross cutting CLAUDE.md file for implicit understanding
- `refine-*`: Have claude code undertake repetitive improvements like updating documentation

### Available Commands

#### /speak [optional initial message]

- **Location**: `claude/commands/speak.md`
- **Purpose**: Enable audio communication mode for the session
- **Usage**: Start an interactive audio conversation with Claude Code
- **Example**: `/speak` or `/speak Let's discuss the architecture`
- **Features**: Voice input/output, automatic transcription, continuous conversation mode

#### /prime-simple [feature you wish to build]

- **Location**: `claude/commands/prime-simple.md`
- **Purpose**: Prime the architect for a simple feature, but with clarity

#### /prime-spec [feature you wish to build]

- **Location**: `claude/commands/prime-spec.md`
- **Purpose**: Architect engages with user to define comprehensive feature specification

#### /prime-tech [specs/<spec>.md]

- **Location**: `claude/commands/prime-tech.md`
- **Purpose**: Architect leads technical design discussion to create implementation blueprint

#### /prime-build [specs/<spec>.md]

- **Location**: `claude/commands/prime-build.md`
- **Purpose**: Architect coordinates implementation team to build approved specification

#### /refine-docs [sha1] [sha2] ...

- **Location**: `claude/commands/refine-docs.md`
- **Purpose**: Review and refine documentation based on code changes
- **Usage**:
  - Without arguments: Reviews documentation against current session changes
  - With git SHAs: Reviews documentation against specific commits (for retrospective updates)
- **Example**: `/refine-docs cdaa1fb 79f8137` - reviews changes from those two commits
- **Created from**: Migrated from `cc-hook--check-claude-md` hook for manual invocation

#### /refine-spec [improvement to make]

- **Location**: `claude/commands/refine-spec.md`
- **Purpose**: Refine the prime commands for spec building
- **Example**: `/speak` or `/speak Let's discuss the architecture`
- **Features**: Voice input/output, automatic transcription, continuous conversation mode

#### /refine-docs [sha1] [sha2] ...

- **Location**: `claude/commands/refine-docs.md`
- **Purpose**: Review and refine documentation based on code changes
- **Usage**:
  - Without arguments: Reviews documentation against current session changes
  - With git SHAs: Reviews documentation against specific commits (for retrospective updates)
- **Example**: `/refine-docs cdaa1fb 79f8137` - reviews changes from those two commits
- **Created from**: Migrated from `cc-hook--check-claude-md` hook for manual invocation

## Spec-Driven Development

We use a multi-agent architecture with spec-driven development to build features systematically from requirements to verified implementation. The workflow follows these phases:

1. **Requirements Gathering** - Architect agent clarifies and documents user needs
2. **Specification Creation** - Detailed technical specs created in `specs/` directory
3. **Implementation** - Senior Dev agent builds according to specifications
4. **Verification** - QA agent validates against acceptance criteria

See `claude/x-agent-docs/feature-building/ways-of-working.md` for detailed architecture.

### Commands for Spec-Driven Development

#### Simple features:

- **`/prime-simple`** - Quick feature development with basic clarification

#### Complex features:

- **`/prime-spec`** - Full requirements gathering and specification creation
- **`/prime-tech`** - Technical design discussion from existing spec
- **`/prime-build`** - Coordinate implementation from approved specification

#### Iterative improvements to the workflow itself

- **`/refine-spec`** - Coordinate implementation from approved specification

### Hook Configuration Reference

Hooks are configured in `settings.json` with this structure:

```json
{
  "hooks": {
    "<EventName>": [
      {
        "matcher": "<ToolName>", // Optional: filter by tool
        "hooks": [
          {
            "type": "command",
            "command": "<command-to-execute>"
          }
        ]
      }
    ]
  }
}
```

### Development Guidelines

When developing new hooks:

1. Follow the naming convention: `cc-hook--<purpose>` for TypeScript hooks
2. Export a lib function for testing (e.g., `ccHookSessionLoggerLib`)
3. Use the shared types from `oven/shared/claude-hooks.ts`
4. Handle stdin for hook input and stdout for responses
5. Use exit code 2 to block operations with a reason
6. Consider session state and avoid infinite loops

## Experimental Features

### Project Context Injection with cindex

We're experimenting with using `cindex` to automatically inject project context into Claude Code sessions. This feature is under active development and the exact format is subject to heavy modification as we experiment with different approaches.

#### Current Approach

- **Tool**: `cindex` (located in `oven/bin/cindex.ts`)
- **Purpose**: Generate and inject project structure/context at session start
- **Format**: Currently experimenting with markdown-based project indices
- **Integration**: Exploring hook-based injection and command-based approaches

#### Usage Patterns Being Tested

```bash
# Generate project index with descriptions
cindex --markdown

# Generate focused index for specific paths
cindex --path <directory>

# Filter out less relevant files automatically
cindex --help  # See current experimental options
```

**Note**: The implementation details, output format, and integration methods are all experimental and will change frequently as we refine the approach for optimal context injection.
