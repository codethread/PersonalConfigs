// :module: Extract Claude Code dialogue from git commit messages and session logs

// CLI tool to extract and format Claude Code dialogue for a specific git commit

import {$} from "bun";
import {existsSync} from "fs";
import {readdir, readFile, writeFile} from "fs/promises";
import {join, relative} from "path";

// Import types from extract-dialogue
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
	[key: string]: any;
}

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

interface ExtractedDialogue {
	session_id: string;
	log_file: string;
	transcript_path: string;
	start_time: string;
	end_time: string;
	dialogue: DialogueInteraction[];
}

interface ExtractedCommitDialogue {
	commit_sha: string;
	commit_message: string;
	session_ids: string[];
	log_files: string[];
	transcript_paths: string[];
	start_time: string;
	end_time: string;
	dialogue: DialogueInteraction[];
}

interface CliArgs {
	commitSha: string;
	markdown: boolean;
	outputPath?: string;
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

// Extract dialogue from a single log file (based on extract-dialogue.ts)
async function extractDialogueFromLog(logFilePath: string): Promise<ExtractedDialogue> {
	if (!existsSync(logFilePath)) {
		throw new Error(`Log file not found: ${logFilePath}`);
	}

	const logEntries = await readJsonlFile<LogEntry>(logFilePath);
	if (logEntries.length === 0) {
		throw new Error("Log file is empty");
	}

	const transcriptPath = logEntries[0].transcript_path;
	if (!transcriptPath) {
		throw new Error("No transcript_path found in log file");
	}

	if (!existsSync(transcriptPath)) {
		throw new Error(`Transcript file not found: ${transcriptPath}`);
	}

	const transcriptEntries = await readJsonlFile<TranscriptEntry>(transcriptPath);

	const sessionId = logEntries[0].session_id;
	const startTime = logEntries[0].timestamp;
	const endTime = logEntries[logEntries.length - 1].timestamp;
	const logFileRelative = relative(process.cwd(), logFilePath);

	const dialogueInteractions: DialogueInteraction[] = [];

	const userPromptEvents = logEntries
		.map((entry, index) => ({entry, index}))
		.filter(({entry}) => entry.event === "UserPromptSubmit");

	for (const {entry: userPrompt, index: userPromptIndex} of userPromptEvents) {
		let notificationEvent: LogEntry | null = null;
		let stopEvent: LogEntry | null = null;

		for (let i = userPromptIndex - 1; i >= 0; i--) {
			const entry = logEntries[i];

			if (
				!notificationEvent &&
				entry.event === "Notification" &&
				entry.message === "Claude is waiting for your input"
			) {
				notificationEvent = entry;
			}

			if (notificationEvent && entry.event === "Stop" && entry.timestamp < notificationEvent.timestamp) {
				stopEvent = entry;
				break;
			}
		}

		const userTranscriptEntries = transcriptEntries.filter(
			(t) =>
				t.type === "user" &&
				t.timestamp &&
				Math.abs(new Date(t.timestamp).getTime() - new Date(userPrompt.timestamp).getTime()) < 5000,
		);

		const userTranscript = userTranscriptEntries[0];

		if (notificationEvent && stopEvent) {
			const agentTranscript = transcriptEntries
				.filter(
					(t) =>
						t.type === "assistant" && t.timestamp && new Date(t.timestamp) < new Date(stopEvent.timestamp),
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
			const events: DialogueEvent[] = [];

			if (userTranscript) {
				events.push({
					sender: "user",
					timestamp: userTranscript.timestamp,
					content: extractMessageContent(userTranscript.message),
					raw: userTranscript,
				});
			} else {
				events.push({
					sender: "user",
					timestamp: userPrompt.timestamp,
					content: userPrompt.prompt || "",
					raw: userPrompt,
				});
			}

			const agentResponse = transcriptEntries.find(
				(t) =>
					t.type === "assistant" && t.timestamp && new Date(t.timestamp) > new Date(userPrompt.timestamp),
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

// Get commit timestamp using git
async function getCommitTimestamp(commitSha: string): Promise<Date> {
	const result = await $`git show -s --format=%cI ${commitSha}`.text();
	return new Date(result.trim());
}

// Get commit message
async function getCommitMessage(commitSha: string): Promise<string> {
	const result = await $`git show -s --format=%B ${commitSha}`.text();
	return result.trim();
}

// Find log files within a time range
async function findLogFilesInTimeRange(startTime: Date, endTime: Date, logsDir: string): Promise<string[]> {
	if (!existsSync(logsDir)) {
		return [];
	}

	const files = await readdir(logsDir);
	const logFiles = files.filter(
		(f) =>
			(f.startsWith("cc-session-") || f.startsWith("claude-session-")) &&
			f.endsWith(".jsonl") &&
			!f.includes("current") &&
			!f.includes("last"),
	);

	const relevantLogs: string[] = [];

	for (const logFile of logFiles) {
		const logPath = join(logsDir, logFile);
		try {
			const logEntries = await readJsonlFile<LogEntry>(logPath);
			if (logEntries.length === 0) continue;

			const sessionStart = new Date(logEntries[0].timestamp);
			const sessionEnd = new Date(logEntries[logEntries.length - 1].timestamp);

			// Check if session overlaps with commit time range
			if (
				(sessionStart >= startTime && sessionStart <= endTime) ||
				(sessionEnd >= startTime && sessionEnd <= endTime) ||
				(sessionStart <= startTime && sessionEnd >= endTime)
			) {
				relevantLogs.push(logPath);
			}
		} catch (error) {
			console.error(`Error reading log file ${logFile}:`, error);
		}
	}

	return relevantLogs;
}

// Helper function to convert string to kebab-case
function toKebabCase(str: string): string {
	return str
		.toLowerCase()
		.replace(/[^\w\s-]/g, "") // Remove special characters
		.replace(/[\s_]+/g, "-") // Replace spaces and underscores with hyphens
		.replace(/^-+|-+$/g, ""); // Remove leading/trailing hyphens
}

// Helper function to format date as YY-MM-DD
function formatDateYYMMDD(date: Date): string {
	const year = date.getFullYear().toString().slice(-2);
	const month = (date.getMonth() + 1).toString().padStart(2, "0");
	const day = date.getDate().toString().padStart(2, "0");
	return `${year}-${month}-${day}`;
}

// Helper function to generate markdown filename
function generateMarkdownFilename(commitMessage: string, commitDate: Date, shortSha: string): string {
	const firstLine = commitMessage.split("\n")[0];
	const dateStr = formatDateYYMMDD(commitDate);
	const kebabTitle = toKebabCase(firstLine);
	return `${dateStr}-${kebabTitle}-${shortSha}.md`;
}

// Helper function to format timestamp for display
function formatTimestamp(timestamp: string): string {
	return new Date(timestamp).toLocaleString();
}

// Helper function to escape markdown characters
function escapeMarkdown(text: string): string {
	return text.replace(/([*_`~\\])/g, "\\$1");
}

// Helper function to clean content for markdown display
function cleanContentForMarkdown(content: string): string {
	if (!content) return "";

	// Remove command wrapper tags
	const cleaned = content
		.replace(/<command-message>.*?<\/command-message>/g, "")
		.replace(/<command-name>.*?<\/command-name>/g, "")
		.replace(/<command-args>(.*?)<\/command-args>/g, "$1")
		.replace(/\[Tool: [^\]]+\]/g, "")
		.trim();

	return escapeMarkdown(cleaned);
}

// Generate markdown content from ExtractedCommitDialogue
function generateMarkdown(data: ExtractedCommitDialogue): string {
	const shortSha = data.commit_sha.slice(0, 7);
	const _commitDate = new Date(data.end_time);
	const duration = Math.round(
		(new Date(data.end_time).getTime() - new Date(data.start_time).getTime()) / 1000 / 60,
	);

	let markdown = `# ${data.commit_message.split("\n")[0]}\n\n`;
	markdown += `**Commit:** \`${shortSha}\`  \n`;
	markdown += `**Date:** ${formatTimestamp(data.end_time)}  \n`;
	markdown += `**Duration:** ${duration} minutes  \n`;
	markdown += `**Sessions:** ${data.session_ids.length}  \n\n`;

	if (data.session_ids.length > 0) {
		markdown += "## Sessions\n\n";
		data.session_ids.forEach((sessionId, index) => {
			markdown += `- **Session ${index + 1}:** \`${sessionId}\`\n`;
		});
		markdown += "\n";
	}

	if (data.dialogue.length > 0) {
		markdown += "## Dialogue\n\n";

		data.dialogue.forEach((interaction, index) => {
			const isDiscussion = interaction.type === "discussion";
			markdown += `### ${isDiscussion ? "Discussion" : "Interjection"} ${index + 1}\n\n`;

			interaction.events.forEach((event, _eventIndex) => {
				const cleanContent = cleanContentForMarkdown(event.content || "");
				if (cleanContent.trim()) {
					const sender = event.sender === "user" ? "👤 **User**" : "🤖 **Agent**";
					const timestamp = event.timestamp ? ` _(${formatTimestamp(event.timestamp)})_` : "";

					markdown += `${sender}${timestamp}:\n`;
					markdown += `${cleanContent}\n\n`;
				}
			});

			markdown += "---\n\n";
		});
	}

	return markdown;
}

function showHelp() {
	console.log(`
cc-logs--extract-commit-dialogue - Extract Claude Code dialogue for a specific commit

Usage: cc-logs--extract-commit-dialogue [commit-sha] [options]

Arguments:
  commit-sha      Git commit SHA to extract dialogue for (default: HEAD)

Options:
  --markdown, -m  Output in markdown format instead of JSON
  --path <path>   Specify output path for markdown file
  -h, --help      Show this help message

Description:
  Extracts Claude Code session dialogue that led to a specific commit by finding
  log entries and transcripts within the commit's time window.

Examples:
  cc-logs--extract-commit-dialogue                    # Extract dialogue for HEAD commit (JSON)
  cc-logs--extract-commit-dialogue HEAD --markdown    # Extract dialogue for HEAD (markdown)
  cc-logs--extract-commit-dialogue abc123f -m         # Extract dialogue for specific commit
  cc-logs--extract-commit-dialogue HEAD -m --path ./commit.md  # Custom output path
`);
}

// Parse command line arguments
function parseArgs(args: string[]): CliArgs {
	// Check for help flag first
	if (args.includes("-h") || args.includes("--help")) {
		showHelp();
		process.exit(0);
	}

	const result: CliArgs = {
		commitSha: "",
		markdown: false,
		outputPath: undefined,
	};

	let i = 0;
	while (i < args.length) {
		const arg = args[i];

		if (arg === "--markdown" || arg === "-m") {
			result.markdown = true;
		} else if (arg === "--path") {
			if (i + 1 < args.length) {
				result.outputPath = args[i + 1];
				i++; // Skip next arg as it's the path value
			} else {
				throw new Error("--path flag requires a value");
			}
		} else if (!arg.startsWith("-")) {
			// Assume it's the commit SHA
			result.commitSha = arg;
		}

		i++;
	}

	return result;
}

// Main function to extract dialogue from a commit
async function extractCommitDialogue(commitSha: string): Promise<ExtractedCommitDialogue> {
	// Get commit timestamps
	const endTime = await getCommitTimestamp(commitSha);
	const commitMessage = await getCommitMessage(commitSha);

	// Get parent commit to determine start time
	let startTime: Date;
	try {
		const parentResult = await $`git rev-parse ${commitSha}^`.text();
		const parentSha = parentResult.trim();
		startTime = await getCommitTimestamp(parentSha);
	} catch (_error) {
		// If no parent (first commit), use a reasonable default (1 hour before)
		startTime = new Date(endTime.getTime() - 60 * 60 * 1000);
		console.warn(`No parent commit found for ${commitSha}, using 1 hour before as start time`);
	}

	// Find log files in the time range
	const logsDir = join(process.cwd(), ".logs");
	const logFiles = await findLogFilesInTimeRange(startTime, endTime, logsDir);

	if (logFiles.length === 0) {
		console.warn(
			`No log files found for commit ${commitSha} (${startTime.toISOString()} - ${endTime.toISOString()})`,
		);
	}

	// Extract dialogue from each log file
	const extractedDialogues: ExtractedDialogue[] = [];
	const sessionIds: string[] = [];
	const logFileRelatives: string[] = [];
	const transcriptPaths: string[] = [];
	const allDialogueInteractions: DialogueInteraction[] = [];

	for (const logFile of logFiles) {
		try {
			const extracted = await extractDialogueFromLog(logFile);
			extractedDialogues.push(extracted);
			sessionIds.push(extracted.session_id);
			logFileRelatives.push(extracted.log_file);
			transcriptPaths.push(extracted.transcript_path);
			allDialogueInteractions.push(...extracted.dialogue);
		} catch (error) {
			console.error(`Error extracting dialogue from ${logFile}:`, error);
		}
	}

	return {
		commit_sha: commitSha,
		commit_message: commitMessage,
		session_ids: sessionIds,
		log_files: logFileRelatives,
		transcript_paths: transcriptPaths,
		start_time: startTime.toISOString(),
		end_time: endTime.toISOString(),
		dialogue: allDialogueInteractions,
	};
}

// CLI entry point
async function main() {
	try {
		const args = process.argv.slice(2);

		// Parse command line arguments
		const cliArgs = parseArgs(args);

		// Default to HEAD if no commit specified
		if (!cliArgs.commitSha) {
			const headResult = await $`git rev-parse HEAD`.text();
			cliArgs.commitSha = headResult.trim();
		}

		// Validate it's a valid commit
		try {
			await $`git rev-parse ${cliArgs.commitSha}`.quiet();
		} catch (_error) {
			console.error(`Invalid commit: ${cliArgs.commitSha}`);
			console.error(
				"Usage: cc-logs--extract-commit-dialogue [commit-sha] [--markdown|-m] [--path <output-path>]",
			);
			console.error("Examples:");
			console.error("  cc-logs--extract-commit-dialogue HEAD");
			console.error("  cc-logs--extract-commit-dialogue abc123f --markdown");
			console.error("  cc-logs--extract-commit-dialogue HEAD -m --path ./my-commit.md");
			process.exit(1);
		}

		const result = await extractCommitDialogue(cliArgs.commitSha);

		if (cliArgs.markdown) {
			// Generate markdown output
			const markdown = generateMarkdown(result);

			// Determine output path
			let outputPath: string;
			if (cliArgs.outputPath) {
				outputPath = cliArgs.outputPath;
			} else {
				const commitDate = new Date(result.end_time);
				const shortSha = result.commit_sha.slice(0, 7);
				outputPath = generateMarkdownFilename(result.commit_message, commitDate, shortSha);
			}

			// Write markdown file
			await writeFile(outputPath, markdown, "utf-8");
			console.log(`Markdown written to: ${outputPath}`);
		} else {
			// Output as formatted JSON (original behavior)
			console.log(JSON.stringify(result, null, 2));
		}
	} catch (error) {
		console.error("Error:", error instanceof Error ? error.message : error);
		process.exit(1);
	}
}

// Run if called directly
if (import.meta.main) {
	main();
}
