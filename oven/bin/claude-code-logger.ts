/** biome-ignore-all lint/correctness/noUnusedVariables: types kept as docs */
import {existsSync, mkdirSync} from "fs";
import {appendFile, symlink, unlink, readlink} from "fs/promises";
import {join} from "path";

// Claude Code Hook Event Types
interface BaseHookInput {
	session_id: string;
	transcript_path: string;
	cwd: string;
	hook_event_name: string;
}

interface PreToolUseInput extends BaseHookInput {
	hook_event_name: "PreToolUse";
	tool_name: string;
	tool_input: Record<string, any>; // Schema depends on the tool
}

interface PostToolUseInput extends BaseHookInput {
	hook_event_name: "PostToolUse";
	tool_name: string;
	tool_input: Record<string, any>; // Schema depends on the tool
	tool_response: Record<string, any>; // Schema depends on the tool
}

interface NotificationInput extends BaseHookInput {
	hook_event_name: "Notification";
	message: string;
}

interface UserPromptSubmitInput extends BaseHookInput {
	hook_event_name: "UserPromptSubmit";
	prompt: string;
}

interface StopInput extends BaseHookInput {
	hook_event_name: "Stop" | "SubagentStop";
	stop_hook_active: boolean; // True when Claude is already continuing from a stop hook
}

interface PreCompactInput extends BaseHookInput {
	hook_event_name: "PreCompact";
	trigger: "manual" | "auto";
	custom_instructions: string; // Empty for auto, user-provided for manual
}

interface SessionStartInput extends BaseHookInput {
	hook_event_name: "SessionStart";
	source: "startup" | "resume" | "clear" | "compact";
}

interface SessionEndInput extends BaseHookInput {
	hook_event_name: "SessionEnd";
	reason: "clear" | "logout" | "prompt_input_exit" | "other";
}

type HookInput =
	| PreToolUseInput
	| PostToolUseInput
	| NotificationInput
	| UserPromptSubmitInput
	| StopInput
	| PreCompactInput
	| SessionStartInput
	| SessionEndInput;

// Hook Output Types (JSON stdout)
interface BaseHookOutput {
	continue?: boolean; // Whether Claude should continue (default: true)
	stopReason?: string; // Message shown when continue is false
	suppressOutput?: boolean; // Hide stdout from transcript mode (default: false)
	systemMessage?: string; // Optional warning shown to user
}

interface PreToolUseOutput extends BaseHookOutput {
	decision?: "approve" | "block"; // Deprecated, use hookSpecificOutput
	reason?: string; // Deprecated
	hookSpecificOutput?: {
		hookEventName: "PreToolUse";
		permissionDecision: "allow" | "deny" | "ask";
		permissionDecisionReason: string;
	};
}

interface PostToolUseOutput extends BaseHookOutput {
	decision?: "block"; // Automatically prompts Claude with reason
	reason?: string; // Explanation for decision
	hookSpecificOutput?: {
		hookEventName: "PostToolUse";
		additionalContext: string; // Additional info for Claude
	};
}

interface UserPromptSubmitOutput extends BaseHookOutput {
	decision?: "block"; // Prevents prompt processing
	reason?: string; // Shown to user, not added to context
	hookSpecificOutput?: {
		hookEventName: "UserPromptSubmit";
		additionalContext: string; // Added to context if not blocked
	};
}

interface StopOutput extends BaseHookOutput {
	decision?: "block"; // Prevents Claude from stopping
	reason?: string; // Must be provided when blocking
}

interface SessionStartOutput extends BaseHookOutput {
	hookSpecificOutput?: {
		hookEventName: "SessionStart";
		additionalContext: string; // Added to context
	};
}

// Custom Log Entry Structure
interface LogEntry {
	timestamp: string; // ISO 8601 format
	unix_timestamp: number;
	event: string;
	session_id: string;
	cwd: string;
	transcript_path?: string;

	// Tool-specific fields (PreToolUse, PostToolUse)
	tool_name?: string;
	tool_input?: Record<string, any>;
	tool_response?: Record<string, any>;

	// Event-specific fields
	message?: string; // Notification
	prompt?: string; // UserPromptSubmit
	stop_hook_active?: boolean; // Stop, SubagentStop
	trigger?: "manual" | "auto"; // PreCompact
	custom_instructions?: string; // PreCompact
	source?: "startup" | "resume" | "clear" | "compact"; // SessionStart
	reason?: "clear" | "logout" | "prompt_input_exit" | "other"; // SessionEnd

	// Environment context
	env: {
		CLAUDE_PROJECT_DIR?: string;
		USER?: string;
		PWD?: string;
	};

	// Original hook data for debugging
	raw_data: HookInput | {error: string; raw: string};
}

function showHelp() {
	console.log(`
claude-code-logger - Claude Code session hook for logging events

Usage: claude-code-logger [options]

Options:
  -h, --help      Show this help message

Description:
  A Claude Code hook script that logs session events to JSONL files.
  Typically used as a hook in Claude Code settings, not called directly.
  
  Creates logs in .logs/ directory:
  - claude-session-<timestamp>.jsonl - Session-specific logs
  - claude-session-current.jsonl - Symlink to current session
  - claude-session-last.jsonl - Symlink to previous session
  - claude-daily-<date>.jsonl - Daily aggregate logs

Examples:
  # Usually configured as a Claude Code hook
  echo '{"hook_event_name":"SessionStart"}' | claude-code-logger
`);
}

async function main() {
	// Check for help flag in command line args
	const args = process.argv.slice(2);
	if (args.includes("-h") || args.includes("--help")) {
		showHelp();
		process.exit(0);
	}

	// Read stdin data
	const stdinData = await Bun.stdin.text();
	let hookData: HookInput | {error: string; raw: string};

	try {
		hookData = JSON.parse(stdinData) as HookInput;
	} catch (_error) {
		// If no stdin data or invalid JSON, create minimal log entry
		hookData = {
			error: "Failed to parse stdin",
			raw: stdinData,
		};
	}

	// Get current working directory from hook data or environment
	const cwd = "cwd" in hookData ? hookData.cwd : process.cwd();
	const sessionId = "session_id" in hookData ? hookData.session_id : "unknown";
	const hookEventName = "hook_event_name" in hookData ? hookData.hook_event_name : "unknown";

	// Create logs directory if it doesn't exist
	const logsDir = join(cwd, ".logs");
	if (!existsSync(logsDir)) {
		mkdirSync(logsDir, {recursive: true});
	}

	// Generate filename based on session start time
	// Use session_id as it typically contains timestamp info, or create new timestamp
	const timestamp = sessionId.includes("-") ? sessionId.split("-")[0] : Date.now().toString();
	const logFile = join(logsDir, `claude-session-${timestamp}.jsonl`);
	const logFileName = `claude-session-${timestamp}.jsonl`;

	// Symlink paths
	const currentSymlink = join(logsDir, "claude-session-current.jsonl");
	const lastSymlink = join(logsDir, "claude-session-last.jsonl");

	// Create log entry with all available information
	const logEntry: LogEntry = {
		timestamp: new Date().toISOString(),
		unix_timestamp: Date.now(),
		event: hookEventName,
		session_id: sessionId,
		cwd: cwd,
		transcript_path: "transcript_path" in hookData ? hookData.transcript_path : undefined,

		// Include tool-specific data if available
		tool_name: "tool_name" in hookData ? hookData.tool_name : undefined,
		tool_input: "tool_input" in hookData ? hookData.tool_input : undefined,
		tool_response: "tool_response" in hookData ? hookData.tool_response : undefined,

		// Include event-specific data
		message: "message" in hookData ? hookData.message : undefined,
		prompt: "prompt" in hookData ? hookData.prompt : undefined,
		stop_hook_active: "stop_hook_active" in hookData ? hookData.stop_hook_active : undefined,
		trigger: "trigger" in hookData ? hookData.trigger : undefined,
		custom_instructions:
			"custom_instructions" in hookData ? hookData.custom_instructions : undefined,
		source: "source" in hookData ? hookData.source : undefined,
		reason: "reason" in hookData ? hookData.reason : undefined,

		// Include environment info
		env: {
			CLAUDE_PROJECT_DIR: process.env.CLAUDE_PROJECT_DIR,
			USER: process.env.USER,
			PWD: process.env.PWD,
		},

		// Include full hook data for completeness
		raw_data: hookData,
	};

	// Helper function to safely update symlink
	async function updateSymlink(symlinkPath: string, targetFile: string) {
		try {
			// Check if symlink exists and remove it
			if (existsSync(symlinkPath)) {
				await unlink(symlinkPath);
			}
			// Create new symlink pointing to the target file (relative path for portability)
			await symlink(targetFile, symlinkPath);
		} catch (error) {
			// Silently fail to not interrupt Claude Code operation
			console.error(`Failed to update symlink ${symlinkPath}:`, error);
		}
	}

	// Write to JSONL file (one JSON object per line)
	try {
		await appendFile(logFile, `${JSON.stringify(logEntry)}\n`);

		// Optional: Also log to a daily aggregate file for easier analysis
		const dailyFile = join(logsDir, `claude-daily-${new Date().toISOString().split("T")[0]}.jsonl`);
		await appendFile(dailyFile, `${JSON.stringify(logEntry)}\n`);

		// Handle symlinks based on hook event
		if (hookEventName === "SessionStart") {
			// On session start, move current to last (if it exists) and create new current
			try {
				if (existsSync(currentSymlink)) {
					// Get the target of current symlink before updating
					const currentTarget = await readlink(currentSymlink);
					// Update last symlink to point to what current was pointing to
					await updateSymlink(lastSymlink, currentTarget);
				}
			} catch (error) {
				console.error("Failed to update last symlink:", error);
			}
			// Update current symlink to point to this session's log
			await updateSymlink(currentSymlink, logFileName);
		} else if (hookEventName === "SessionEnd") {
			// On session end, we could optionally move current to last
			// But keeping current pointing to the most recently active session is probably more useful
			// So we'll just ensure the symlinks are valid
			if (!existsSync(currentSymlink)) {
				// If current doesn't exist, create it pointing to this session
				await updateSymlink(currentSymlink, logFileName);
			}
		}
	} catch (error) {
		// Silently fail to not interrupt Claude Code operation
		console.error("Logging error:", error);
	}

	// Always exit successfully to not block Claude Code
	process.exit(0);
}

main();
