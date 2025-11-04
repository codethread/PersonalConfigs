# claude code wrapper with extra args
export def cl --wrapped [
	--dangerously-skip-permissions(-d) # ahhhhh
	--ui # add a ui based mcp config
	...rest
] {
	let args = {
		ui: $ui,
		dangerously-skip-permissions: $dangerously_skip_permissions
	}

	let custom = $args
	| items {|key, value| match [$key $value] {
		["ui", true] => { $"--mcp-config=(echo ~/.claude/x-mcp/ui.json | path expand)" }
		["dangerously-skip-permissions", true] => { "--dangerously-skip-permissions" }
	}}
	| compact

	let all_args = $custom ++ $rest
	print $all_args
	claude ...$all_args
}

export alias _claude-session = jq 'select(.event == "PreToolUse")' .logs/claude-session-*.jsonl
export alias _claude-prompts = jq 'select(.event == "UserPromptSubmit") | {prompt: .raw_data.prompt, transcript: .raw_data.transcript_path, timestamp}' .logs/claude-session-*.jsonl


# Get tool usage statistics
export alias _claude-session-stats = jq -s 'group_by(.tool_name) | map({tool: .[0].tool_name, count: length})' .logs/claude-session-*.jsonl

export alias _claude-stable = claude install 1.0.88
