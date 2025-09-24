// :module: Extract dialogue context from transcripts

import {existsSync} from "fs";
import {readFile} from "fs/promises";
import {relative} from "path";

// Import types from logger (we'll export these from the logger file)
type LogEntry = {
	timestamp: string;
	unix_timestamp: number;
	event: string;
	session_id: string;
	cwd: string;
	transcript_path?: string;
	tool_name?: string;
	tool_input?: Record<string, any>;
	tool_response?: Record<string, any>;
	message?: string;
	prompt?: string;
	stop_hook_active?: boolean;
	trigger?: "manual" | "auto";
	custom_instructions?: string;
	source?: "startup" | "resume" | "clear" | "compact";
	reason?: "clear" | "logout" | "prompt_input_exit" | "other";
	env: {
		CLAUDE_PROJECT_DIR?: string;
		USER?: string;
		PWD?: string;
	};
	raw_data: any;
};

// Transcript entry types based on observed structure
interface TranscriptEntry {
	parentUuid?: string;
	isSidechain?: boolean;
	userType?: string;
	cwd?: string;
	sessionId?: string;
	version?: string;
	gitBranch?: string;
	type: "user" | "assistant" | "system" | "summary";
	message?: {
		role?: string;
		content?: string | Array<{type: string; text?: string; [key: string]: any}>;
		id?: string;
		model?: string;
		[key: string]: any;
	};
	uuid?: string;
	timestamp?: string;
	requestId?: string;
	[key: string]: any; // Allow additional fields
}

// Dialogue event interfaces
interface DialogueEvent {
	sender: "user" | "agent";
	timestamp?: string;
	content?: string;
	raw: TranscriptEntry | LogEntry;
}

interface DialogueInteraction {
	type: "discussion" | "interjection";
	events: DialogueEvent[];
}

// Main output interface
interface ExtractedDialogue {
	session_id: string;
	log_file: string;
	transcript_path: string;
	start_time: string;
	end_time: string;
	dialogue: DialogueInteraction[];
}

// Helper function to read and parse JSONL files
async function readJsonlFile<T>(filePath: string): Promise<T[]> {
	const content = await readFile(filePath, "utf-8");
	return content
		.split("\n")
		.filter((line) => line.trim())
		.map((line) => {
			try {
				return JSON.parse(line) as T;
			} catch (error) {
				console.error(`Failed to parse line: ${line.substring(0, 100)}...`);
				throw error;
			}
		});
}

// Extract text content from message
function extractMessageContent(message: TranscriptEntry["message"]): string {
	if (!message) return "";

	if (typeof message.content === "string") {
		return message.content;
	}

	if (Array.isArray(message.content)) {
		return message.content
			.map((item) => {
				if (item.type === "text") return item.text || "";
				if (item.type === "tool_use") return `[Tool: ${item.name || "unknown"}]`;
				return "";
			})
			.filter(Boolean)
			.join("\n");
	}

	return "";
}

// Main extraction function
async function extractDialogue(logFilePath: string): Promise<ExtractedDialogue> {
	// Validate input file exists
	if (!existsSync(logFilePath)) {
		throw new Error(`Log file not found: ${logFilePath}`);
	}

	// Read log entries
	const logEntries = await readJsonlFile<LogEntry>(logFilePath);
	if (logEntries.length === 0) {
		throw new Error("Log file is empty");
	}

	// Extract transcript path from first entry
	const transcriptPath = logEntries[0].transcript_path;
	if (!transcriptPath) {
		throw new Error("No transcript_path found in log file");
	}

	if (!existsSync(transcriptPath)) {
		throw new Error(`Transcript file not found: ${transcriptPath}`);
	}

	// Read transcript entries
	const transcriptEntries = await readJsonlFile<TranscriptEntry>(transcriptPath);

	// Extract session metadata
	const sessionId = logEntries[0].session_id;
	const startTime = logEntries[0].timestamp;
	const endTime = logEntries[logEntries.length - 1].timestamp;
	const logFileRelative = relative(process.cwd(), logFilePath);

	// Build dialogue interactions
	const dialogueInteractions: DialogueInteraction[] = [];

	// Find all UserPromptSubmit events
	const userPromptEvents = logEntries
		.map((entry, index) => ({entry, index}))
		.filter(({entry}) => entry.event === "UserPromptSubmit");

	for (const {entry: userPrompt, index: userPromptIndex} of userPromptEvents) {
		// Look backwards for "Claude is waiting for your input" notification
		let notificationEvent: LogEntry | null = null;
		let stopEvent: LogEntry | null = null;

		// Search backwards from user prompt
		for (let i = userPromptIndex - 1; i >= 0; i--) {
			const entry = logEntries[i];

			if (
				!notificationEvent &&
				entry.event === "Notification" &&
				entry.message === "Claude is waiting for your input"
			) {
				notificationEvent = entry;
			}

			if (
				notificationEvent &&
				entry.event === "Stop" &&
				entry.timestamp < notificationEvent.timestamp
			) {
				stopEvent = entry;
				break;
			}
		}

		// Find corresponding transcript entries
		const userTranscriptEntries = transcriptEntries.filter(
			(t) =>
				t.type === "user" &&
				t.timestamp &&
				Math.abs(new Date(t.timestamp).getTime() - new Date(userPrompt.timestamp).getTime()) < 5000,
		);

		const userTranscript = userTranscriptEntries[0];

		if (notificationEvent && stopEvent) {
			// This is a discussion - find agent message before stop
			const agentTranscript = transcriptEntries
				.filter(
					(t) =>
						t.type === "assistant" &&
						t.timestamp &&
						new Date(t.timestamp) < new Date(stopEvent.timestamp),
				)
				.sort((a, b) => new Date(b.timestamp!).getTime() - new Date(a.timestamp!).getTime())[0];

			const events: DialogueEvent[] = [];

			if (agentTranscript) {
				events.push({
					sender: "agent",
					timestamp: agentTranscript.timestamp,
					content: extractMessageContent(agentTranscript.message),
					raw: agentTranscript,
				});
			}

			events.push({
				sender: "agent",
				timestamp: notificationEvent.timestamp,
				content: notificationEvent.message,
				raw: notificationEvent,
			});

			if (userTranscript) {
				events.push({
					sender: "user",
					timestamp: userTranscript.timestamp,
					content: extractMessageContent(userTranscript.message),
					raw: userTranscript,
				});
			} else {
				// Fallback to log entry
				events.push({
					sender: "user",
					timestamp: userPrompt.timestamp,
					content: userPrompt.prompt || "",
					raw: userPrompt,
				});
			}

			dialogueInteractions.push({
				type: "discussion",
				events,
			});
		} else {
			// This is an interjection
			const events: DialogueEvent[] = [];

			if (userTranscript) {
				events.push({
					sender: "user",
					timestamp: userTranscript.timestamp,
					content: extractMessageContent(userTranscript.message),
					raw: userTranscript,
				});
			} else {
				// Fallback to log entry
				events.push({
					sender: "user",
					timestamp: userPrompt.timestamp,
					content: userPrompt.prompt || "",
					raw: userPrompt,
				});
			}

			// Find agent response after user prompt
			const agentResponse = transcriptEntries.find(
				(t) =>
					t.type === "assistant" &&
					t.timestamp &&
					new Date(t.timestamp) > new Date(userPrompt.timestamp),
			);

			if (agentResponse) {
				events.push({
					sender: "agent",
					timestamp: agentResponse.timestamp,
					content: extractMessageContent(agentResponse.message),
					raw: agentResponse,
				});
			}

			dialogueInteractions.push({
				type: "interjection",
				events,
			});
		}
	}

	return {
		session_id: sessionId,
		log_file: logFileRelative,
		transcript_path: transcriptPath,
		start_time: startTime,
		end_time: endTime,
		dialogue: dialogueInteractions,
	};
}

function showHelp() {
	console.log(`
extract-dialogue - Extract Claude Code session dialogue from log files

Usage: extract-dialogue <log-file> [options]

Arguments:
  log-file        Path to Claude Code session log file (JSONL format)

Options:
  -h, --help      Show this help message

Description:
  Parses Claude Code session logs and extracts structured dialogue information
  including user prompts, assistant responses, and tool usage.

Examples:
  extract-dialogue .logs/cc-session-current.jsonl
  extract-dialogue .logs/cc-session-2025-09-16-14-30-00-a53e19af.jsonl
  extract-dialogue .logs/claude-session-a53e19af.jsonl  # old format still supported
`);
}

// CLI entry point
async function main() {
	try {
		const args = process.argv.slice(2);

		// Check for help flag
		if (args.includes("-h") || args.includes("--help")) {
			showHelp();
			process.exit(0);
		}

		if (args.length === 0) {
			console.error("Usage: extract-dialogue.ts <log-file>");
			console.error("Example: extract-dialogue.ts .logs/cc-session-current.jsonl");
			console.error(
				"Example: extract-dialogue.ts .logs/cc-session-2025-09-16-14-30-00-a53e19af.jsonl",
			);
			process.exit(1);
		}

		const logFile = args[0];
		const result = await extractDialogue(logFile);

		// Output as formatted JSON
		console.log(JSON.stringify(result, null, 2));
	} catch (error) {
		console.error("Error:", error instanceof Error ? error.message : error);
		process.exit(1);
	}
}

// Run if called directly
if (import.meta.main) {
	main();
}
