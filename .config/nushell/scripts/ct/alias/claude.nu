export alias cl = with-env { DISABLE_AUTOUPDATER: 1 } { claude }

# Start up claude with access to heavy duty mcps like figma and playwright
export alias clui = with-env { DISABLE_AUTOUPDATER: 1 } { claude --strict-mcp-config --mcp-config ~/.claude/mcp/ui.json }

#
export alias _claude-session = jq 'select(.event == "PreToolUse")' .logs/claude-session-*.jsonl
export alias _claude-prompts = jq 'select(.event == "UserPromptSubmit") | {prompt: .raw_data.prompt, transcript: .raw_data.transcript_path, timestamp}' .logs/claude-session-*.jsonl


# Get tool usage statistics
export alias _claude-session-stats = jq -s 'group_by(.tool_name) | map({tool: .[0].tool_name, count: length})' .logs/claude-session-*.jsonl

