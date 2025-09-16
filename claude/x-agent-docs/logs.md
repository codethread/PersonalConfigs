# Claude Code Logs Documentation

## Overview

Claude Code generates two types of log files that capture all interactions and operations:

1. **Hook Logger Output** (`.logs/claude-*.jsonl`) - Created by custom hooks, contains simplified event data
2. **Transcript Files** (`~/.claude/projects/*/[session-id].jsonl`) - Official Claude Code transcripts with full conversation and metadata

Both files use JSONL format (JSON Lines) where each line is a complete JSON object representing an event.

## File Locations

### Hook Logger Files

- Location: `.logs/` directory in the current working directory
- Naming:
  - `claude-session-current.jsonl` - Current session (symlink)
  - `claude-session-{timestamp}.jsonl` - Session-specific log
  - `claude-daily-{date}.jsonl` - Daily aggregate log

### Transcript Files

- Location: `~/.claude/projects/{project-path}/{session-id}.jsonl`
- Session ID: UUID format (e.g., `061ab4b2-77b6-4695-8648-a6927b058356`)
- Referenced in hook logger events as `transcript_path`

## Hook Logger Structure

Each line in the hook logger contains an event with this structure:

```json
{
	"timestamp": "2025-09-12T04:27:09.723Z", // ISO timestamp
	"unix_timestamp": 1757651229723, // Unix timestamp in milliseconds
	"event": "PreToolUse", // Event type (see Event Types below)
	"session_id": "061ab4b2-77b6-4695-8648-a6927b058356",
	"cwd": "/Users/adamhall/PersonalConfigs", // Current working directory
	"transcript_path": "/Users/adamhall/.claude/projects/...", // Path to full transcript
	"tool_name": "Task", // Tool being used (for tool events)
	"tool_input": {
		/* tool-specific parameters */
	},
	"tool_response": {
		/* tool execution results */
	}, // PostToolUse only
	"env": {
		"CLAUDE_PROJECT_DIR": "/Users/adamhall/PersonalConfigs",
		"USER": "adamhall",
		"PWD": "/Users/adamhall/PersonalConfigs"
	},
	"raw_data": {
		/* complete hook input data */
	}
}
```

### Event Types in Hook Logger

- `SessionStart` - Session initialization
- `SessionEnd` - Session termination
- `UserPromptSubmit` - User submits a prompt
- `PreToolUse` - Before tool execution
- `PostToolUse` - After tool execution
- `Stop` - Main agent stops responding
- `SubagentStop` - Subagent completes
- `Notification` - System notifications
- `PreCompact` - Before context compaction

## Transcript File Structure

The transcript file contains more detailed information about the conversation flow:

```json
{
	"parentUuid": "uuid-of-parent-event", // Links events in a chain
	"isSidechain": false, // true = subagent activity
	"userType": "external",
	"cwd": "/Users/adamhall/PersonalConfigs",
	"sessionId": "061ab4b2-77b6-4695-8648-a6927b058356",
	"version": "1.0.112", // Claude Code version
	"gitBranch": "main", // Current git branch
	"type": "assistant", // Message type (user/assistant/system)
	"message": {
		"id": "msg_01NEtuLQkug532vMmZKw9RnE",
		"type": "message",
		"role": "assistant",
		"model": "claude-sonnet-4-20250514", // Model used
		"content": [
			{
				"type": "text",
				"text": "I'll read a file first..."
			},
			{
				"type": "tool_use",
				"id": "toolu_01W3w2xcMDRJ28jeQPxHNpfM",
				"name": "Read",
				"input": { "file_path": "/path/to/file" }
			}
		],
		"stop_reason": null,
		"stop_sequence": null,
		"usage": {
			// Token usage statistics
			"input_tokens": 4,
			"cache_creation_input_tokens": 4686,
			"cache_read_input_tokens": 11357,
			"output_tokens": 88,
			"service_tier": "standard"
		}
	},
	"requestId": "req_011CT3sdX3aaFQnHrdB6bLZ6",
	"uuid": "9347e107-eeea-4002-9eef-1686a701bffe",
	"timestamp": "2025-09-12T04:27:03.812Z",
	"toolUseResult": {
		// Result of tool execution
		"type": "text",
		"file": {
			/* file content and metadata */
		},
		"totalDurationMs": 15893, // Subagent execution time
		"totalTokens": 15176, // Total tokens used by subagent
		"totalToolUseCount": 1 // Number of tools subagent used
	}
}
```

## Identifying Subagents

Subagents are identified by multiple indicators:

### In Hook Logger

1. **Task Tool Events**: Look for `tool_name: "Task"`

   ```json
   {
   	"event": "PreToolUse",
   	"tool_name": "Task",
   	"tool_input": {
   		"subagent_type": "general-purpose",
   		"description": "Read aerospace config file",
   		"prompt": "Please read the file..."
   	}
   }
   ```

2. **SubagentStop Events**: Mark completion of subagent execution
   ```json
   {
   	"event": "SubagentStop",
   	"stop_hook_active": false
   }
   ```

### In Transcript Files

1. **Sidechain Flag**: `"isSidechain": true` indicates subagent activity
2. **Parent UUID Chain**: Subagent events have their own UUID chain separate from main conversation
3. **Model Information**: Subagent model is specified in message.model field

## Key Data Patterns

### 1. Extracting Tool Usage

```bash
# Find all tools used in a session
jq -r 'select(.tool_name) | .tool_name' logfile.jsonl | sort | uniq -c

# Find specific tool usage with inputs
jq 'select(.tool_name == "Read") | {timestamp, file: .tool_input.file_path}' logfile.jsonl
```

### 2. Analyzing Subagents

```bash
# Find all subagent invocations
jq 'select(.tool_name == "Task" and .event == "PreToolUse")' logfile.jsonl

# Get subagent models from transcript
jq 'select(.isSidechain == true and .message.model) | .message.model' transcript.jsonl | uniq

# Calculate subagent execution time and tokens
jq 'select(.toolUseResult.totalDurationMs) | {
  duration_ms: .toolUseResult.totalDurationMs,
  tokens: .toolUseResult.totalTokens
}' transcript.jsonl
```

### 3. Session Analysis

```bash
# Get session timeline
jq -r '[.timestamp, .event, .tool_name // ""] | @tsv' logfile.jsonl

# Find errors or blocked operations
jq 'select(.raw_data.hook_event_name == "PreToolUse" and .blocked == true)' logfile.jsonl
```

### 4. Token Usage Analysis

```bash
# Total tokens per message from transcript
jq 'select(.message.usage) | {
  timestamp,
  total: (.message.usage.input_tokens + .message.usage.output_tokens)
}' transcript.jsonl

# Subagent token usage
jq 'select(.toolUseResult.totalTokens) | {
  timestamp,
  tokens: .toolUseResult.totalTokens,
  duration_ms: .toolUseResult.totalDurationMs
}' transcript.jsonl
```

## Important Fields Reference

### Tool Information

- `tool_name`: Name of the tool (Read, Write, Edit, Task, Bash, etc.)
- `tool_input`: Parameters passed to the tool
- `tool_response`: Results from tool execution (PostToolUse only)
- `toolUseID`: Unique identifier for tracking tool call through pre/post events

### Subagent Specific

- `isSidechain`: Boolean flag for subagent activity
- `subagent_type`: Type of subagent (general-purpose, statusline-setup, etc.)
- `totalDurationMs`: Execution time for subagent
- `totalTokens`: Total tokens consumed by subagent
- `totalToolUseCount`: Number of tools the subagent used

### Message Types

- `type`: "user" | "assistant" | "system"
- `subtype`: Additional categorization (e.g., "informational")
- `content`: Array of content blocks (text, tool_use, tool_result)

### Relationships

- `parentUuid`: Links events in execution chain
- `uuid`: Unique identifier for this event
- `sessionId`: Groups all events in a session
- `requestId`: Groups related API calls

## Common Analysis Tasks

### 1. List All Subagents Used

```bash
jq -r 'select(.tool_name == "Task" and .event == "PreToolUse") |
  "\(.timestamp) | \(.tool_input.subagent_type) | \(.tool_input.description)"' logfile.jsonl
```

### 2. Calculate Session Duration

```bash
jq -s '[.[] | select(.event == "SessionStart" or .event == "SessionEnd")] |
  {
    start: .[0].timestamp,
    end: .[1].timestamp,
    duration_minutes: ((.[1].unix_timestamp - .[0].unix_timestamp) / 60000 | floor)
  }' logfile.jsonl
```

### 3. Find Most Used Tools

```bash
jq -r 'select(.tool_name) | .tool_name' logfile.jsonl | sort | uniq -c | sort -rn
```

### 4. Extract Subagent Conversation

```bash
# Get all sidechain messages from transcript
jq 'select(.isSidechain == true)' transcript.jsonl
```

### 5. Track File Modifications

```bash
jq 'select(.tool_name == "Write" or .tool_name == "Edit") |
  {
    timestamp,
    action: .tool_name,
    file: .tool_input.file_path
  }' logfile.jsonl
```

## Notes for Developers

1. **JSONL Format**: Each line is independent JSON - parse line by line, not as array
2. **Timestamps**: Multiple formats available (ISO string, Unix milliseconds)
3. **Null Safety**: Many fields are optional - use `// ""` or `// null` for defaults in jq
4. **Event Ordering**: Events are chronological within a file
5. **Cross-Reference**: Use `transcript_path` from hook logger to access full conversation
6. **Sidechain Isolation**: Subagent events form separate conversation threads with `isSidechain: true`
7. **Token Tracking**: Token usage appears in multiple places - check both `message.usage` and `toolUseResult`

## Example: Complete Subagent Analysis Script

```bash
#!/bin/bash
# Analyze a specific subagent execution

LOGFILE="$1"
TRANSCRIPT=$(jq -r 'select(.tool_name == "Task" and .event == "PreToolUse") |
  .raw_data.transcript_path' "$LOGFILE" | head -1)

echo "=== Subagent Summary ==="
jq -r 'select(.tool_name == "Task" and .event == "PostToolUse") |
  "Type: \(.tool_input.subagent_type)",
  "Description: \(.tool_input.description)",
  "Duration: \(.raw_data.tool_response.totalDurationMs)ms",
  "Tokens: \(.raw_data.tool_response.totalTokens)"' "$LOGFILE"

if [[ -f "$TRANSCRIPT" ]]; then
  echo -e "\n=== Model Used ==="
  jq -r 'select(.isSidechain == true and .message.model) |
    .message.model' "$TRANSCRIPT" | head -1

  echo -e "\n=== Tools Called by Subagent ==="
  jq -r 'select(.isSidechain == true and .message.content[]?.type == "tool_use") |
    .message.content[].name' "$TRANSCRIPT" | sort | uniq -c
fi
```

## Troubleshooting

- **Missing transcript_path**: Older sessions may not have this field
- **Empty toolUseResult**: Not all events have results (check event type)
- **Parsing errors**: Ensure proper JSONL format - each line must be valid JSON
- **Permission issues**: Transcript files require read access to Claude config directory
- **Incomplete data**: Sessions may be interrupted - check for SessionEnd event
