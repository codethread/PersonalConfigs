#!/usr/bin/env bun

import {existsSync, mkdirSync} from "fs";
import {appendFile, symlink, unlink, readlink} from "fs/promises";
import {join} from "path";

// Read stdin data
const stdinData = await Bun.stdin.text();
let hookData;

try {
	hookData = JSON.parse(stdinData);
} catch (_error) {
	// If no stdin data or invalid JSON, create minimal log entry
	hookData = {
		error: "Failed to parse stdin",
		raw: stdinData,
	};
}

// Get current working directory from hook data or environment
const cwd = hookData.cwd || process.cwd();
const sessionId = hookData.session_id || "unknown";
const hookEventName = hookData.hook_event_name || "unknown";

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
const logEntry = {
	timestamp: new Date().toISOString(),
	unix_timestamp: Date.now(),
	event: hookEventName,
	session_id: sessionId,
	cwd: cwd,
	transcript_path: hookData.transcript_path,

	// Include tool-specific data if available
	tool_name: hookData.tool_name,
	tool_input: hookData.tool_input,
	tool_output: hookData.tool_output,

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
