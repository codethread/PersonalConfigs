export alias cl = claude

# Start up claude with access to heavy duty mcps like figma and playwright
export def clui --wrapped [ ...rest ] {
	(claude
		--strict-mcp-config
		--mcp-config ~/.claude/x-mcp/ui.json
		...$rest
	)
}

#
export alias _claude-session = jq 'select(.event == "PreToolUse")' .logs/claude-session-*.jsonl
export alias _claude-prompts = jq 'select(.event == "UserPromptSubmit") | {prompt: .raw_data.prompt, transcript: .raw_data.transcript_path, timestamp}' .logs/claude-session-*.jsonl


# Get tool usage statistics
export alias _claude-session-stats = jq -s 'group_by(.tool_name) | map({tool: .[0].tool_name, count: length})' .logs/claude-session-*.jsonl

export alias _claude-stable = claude install 1.0.88
