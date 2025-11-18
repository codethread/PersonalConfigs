// :module: Claude Code hook for logging session events and interactions

import {existsSync, mkdirSync, readdirSync} from "fs";
import {appendFile, readlink, symlink, unlink} from "fs/promises";
import {join} from "path";
import type {HookInput} from "../shared/claude-hooks";

// Logger-specific types and interfaces

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
	tool_input?: Record<string, unknown>;
	tool_response?: Record<string, unknown>;

	// Event-specific fields
	message?: string; // Notification
	prompt?: string; // UserPromptSubmit
	stop_hook_active?: boolean; // Stop, SubagentStop
	trigger?: "manual" | "auto"; // PreCompact
	custom_instructions?: string; // PreCompact
	source?: "startup" | "resume" | "clear" | "compact"; // SessionStart
	reason?: "clear" | "logout" | "prompt_input_exit" | "other"; // SessionEnd

	// Original hook data for debugging
	raw_data: HookInput | {error: string; raw: string};
}

function showHelp() {
	console.log(`
cc-hook--session-logger - Claude Code hook for logging session events

Usage: cc-hook--session-logger [options]

Options:
  -h, --help      Show this help message

Description:
  A Claude Code hook script that logs session events to JSONL files.
  Typically used as a hook in Claude Code settings, not called directly.

  Creates logs in .logs/ directory:
  - cc-session-<timestamp>-<session-id>.jsonl - Session-specific logs
  - cc-session-current.jsonl - Symlink to current session
  - cc-session-last.jsonl - Symlink to previous session

Examples:
  # Usually configured as a Claude Code hook
  echo '{"hook_event_name":"SessionStart"}' | cc-hook--session-logger
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

	// Generate filename - check if session file already exists
	let logFile: string;
	let logFileName: string;

	// Look for existing session file
	const _sessionPattern = `cc-session-*-${sessionId}.jsonl`;
	const existingFiles = existsSync(logsDir)
		? readdirSync(logsDir).filter((f) => f.endsWith(`-${sessionId}.jsonl`) && f.startsWith("cc-session-"))
		: [];

	if (existingFiles.length > 0) {
		// Use existing session file
		logFileName = existingFiles[0];
		logFile = join(logsDir, logFileName);
	} else {
		// Create new session file with timestamp for first event
		const now = new Date();
		const timestamp = now.toISOString().replace(/[:.]/g, "-").replace("T", "-").slice(0, -5); // Format: YYYY-MM-DD-HH-MM-SS
		logFileName = `cc-session-${timestamp}-${sessionId}.jsonl`;
		logFile = join(logsDir, logFileName);
	}

	// Symlink paths
	const currentSymlink = join(logsDir, "cc-session-current.jsonl");
	const lastSymlink = join(logsDir, "cc-session-last.jsonl");

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
		custom_instructions: "custom_instructions" in hookData ? hookData.custom_instructions : undefined,
		source: "source" in hookData ? hookData.source : undefined,
		reason: "reason" in hookData ? hookData.reason : undefined,

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

if (import.meta.main) {
	main();
}
