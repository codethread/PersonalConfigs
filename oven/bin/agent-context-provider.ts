// :module: Provides contextual AGENTS.md documentation to Claude Code based on file reads with session-based deduplication

import {existsSync, readFileSync, unlinkSync, writeFileSync} from "fs";
import {dirname, join, resolve} from "path";
import {parseArgs} from "util";
import type {HookInput} from "../shared/claude-hooks";
import {report, reportError} from "../shared/report";

function showHelp() {
	console.log(`
agent-context-provider - Provide contextual AGENTS.md documentation to Claude Code

Usage: agent-context-provider <command> [options]

Commands:
  session-start    Initialize session tracking (called from SessionStart hook)
  session-end      Clean up session tracking (called from SessionEnd hook)
  read            Process file read and provide context (called from PostToolUse Read hook)

Options:
  --help, -h      Show this help message

This tool is designed to be called from Claude Code hooks to automatically provide
relevant AGENTS.md documentation when files are read, with intelligent deduplication
to avoid spamming Claude with repeated documentation.
`);
	process.exit(0);
}

export interface SessionState {
	sessionId: string;
	seenAgentsFiles: Set<string>;
	projectRoot: string;
}

export interface AgentContextOptions {
	command: "session-start" | "session-end" | "read";
	sessionId?: string;
	filePath?: string;
	projectRoot?: string;
}

async function main() {
	const {positionals, values} = parseArgs({
		args: Bun.argv.slice(2),
		options: {
			help: {type: "boolean", short: "h"},
		},
		allowPositionals: true,
	});

	if (values.help) {
		showHelp();
	}

	if (positionals.length === 0) {
		console.error("Error: command is required");
		showHelp();
	}

	const command = positionals[0] as "session-start" | "session-end" | "read";

	if (!["session-start", "session-end", "read"].includes(command)) {
		console.error("Error: Invalid command. Must be session-start, session-end, or read");
		process.exit(1);
	}

	try {
		// Read stdin data like claude-code-logger does
		const stdinData = await Bun.stdin.text();
		let hookInput: Partial<HookInput> = {};

		if (stdinData.trim()) {
			hookInput = JSON.parse(stdinData) as HookInput;
		}

		// Extract relevant data based on hook type
		const sessionId = hookInput.session_id;
		let filePath: string | undefined;
		const projectRoot = hookInput.cwd;

		// For read command, extract file path from PostToolUse Read events
		if (command === "read" && hookInput.hook_event_name === "PostToolUse") {
			const toolInput = (hookInput as any).tool_input;
			filePath = toolInput?.file_path;
		}

		const result = await agentContextProviderLib({
			command,
			sessionId,
			filePath,
			projectRoot,
		});

		report(result);
	} catch (err) {
		reportError(err);
		process.exit(1);
	}
}

export async function agentContextProviderLib(options: AgentContextOptions) {
	switch (options.command) {
		case "session-start":
			return await handleSessionStart(options);
		case "session-end":
			return await handleSessionEnd(options);
		case "read":
			return await handleRead(options);
		default:
			throw new Error(`Unknown command: ${options.command}`);
	}
}

async function handleSessionStart(
	options: AgentContextOptions,
): Promise<{success: boolean; message: string}> {
	if (!options.sessionId) {
		throw new Error("Session ID is required for session-start");
	}

	if (!options.projectRoot) {
		throw new Error("Project root is required for session-start");
	}

	const sessionState: SessionState = {
		sessionId: options.sessionId,
		seenAgentsFiles: new Set(),
		projectRoot: options.projectRoot,
	};

	const sessionFile = getSessionFilePath(options.sessionId);
	writeSessionState(sessionFile, sessionState);

	// Provide introductory context about the automatic documentation system
	const introMessage = `<project-context>
IMPORTANT: Ensure you read related documentation when working in nested areas of the codebase`;

	// Find all AGENTS.md files in the project to list them
	const agentsFiles: string[] = [];
	function findAllAgentsFiles(dir: string) {
		const glob = new Bun.Glob("**/AGENTS.md");
		for (const file of glob.scanSync({cwd: dir, absolute: false})) {
			if (!file.includes("node_modules") && !file.includes(".git")) {
				agentsFiles.push(file);
			}
		}
	}

	try {
		findAllAgentsFiles(options.projectRoot);
	} catch (_e) {
		// Ignore errors in finding files
	}

	if (agentsFiles.length > 0) {
		const sortedFiles = agentsFiles.sort();
		console.log(`${introMessage}
Project documentation:
${sortedFiles.map((f) => `- ${f}`).join("\n")}
</project-context>`);
	}

	return {
		success: true,
		message: `Initialized agent context session: ${options.sessionId}`,
	};
}

async function handleSessionEnd(
	options: AgentContextOptions,
): Promise<{success: boolean; message: string}> {
	if (!options.sessionId) {
		throw new Error("Session ID is required for session-end");
	}

	const sessionFile = getSessionFilePath(options.sessionId);
	if (existsSync(sessionFile)) {
		unlinkSync(sessionFile);
	}

	return {
		success: true,
		message: `Cleaned up agent context session: ${options.sessionId}`,
	};
}

async function handleRead(
	options: AgentContextOptions,
): Promise<{success: boolean; agentsFound: number}> {
	if (!options.sessionId) {
		throw new Error("Session ID is required for read command");
	}

	if (!options.filePath) {
		throw new Error("File path is required for read command");
	}

	const sessionFile = getSessionFilePath(options.sessionId);
	if (!existsSync(sessionFile)) {
		// Session not initialized, skip processing
		return {success: true, agentsFound: 0};
	}

	const sessionState = readSessionState(sessionFile);
	const agentsFiles = findAgentsFiles(options.filePath, sessionState.projectRoot);

	let agentsFound = 0;
	const newAgentsContent: string[] = [];

	for (const agentsFile of agentsFiles) {
		if (!sessionState.seenAgentsFiles.has(agentsFile)) {
			const content = readFileSync(agentsFile, "utf8");
			const relativePath = agentsFile.replace(sessionState.projectRoot, "").replace(/^\//, "");

			newAgentsContent.push(`<agent-documentation-context file="${relativePath}">
${content}
</agent-documentation-context>`);

			sessionState.seenAgentsFiles.add(agentsFile);
			agentsFound++;
		}
	}

	if (newAgentsContent.length > 0) {
		// Output just the XML-tagged content - the SessionStart already explained what this is
		const contextMessage = newAgentsContent.join("\n\n");

		// Use stderr with exit code 2 to ensure Claude sees the context
		// Exit code 2 for PostToolUse shows stderr to Claude
		console.error(contextMessage);

		// Update session state
		writeSessionState(sessionFile, sessionState);

		// Exit with code 2 to make this a "blocking" event that Claude will see
		// This ensures the context is actually delivered to Claude
		process.exit(2);
	}

	return {success: true, agentsFound};
}

function findAgentsFiles(filePath: string, projectRoot: string): string[] {
	const agentsFiles: string[] = [];
	const normalizedProjectRoot = resolve(projectRoot);
	let currentDir = resolve(dirname(filePath));

	// Traverse up the directory tree until we reach the project root
	while (currentDir.startsWith(normalizedProjectRoot)) {
		const agentsPath = join(currentDir, "AGENTS.md");
		if (existsSync(agentsPath)) {
			agentsFiles.push(agentsPath);
		}

		// Stop if we've reached the project root
		if (currentDir === normalizedProjectRoot) {
			break;
		}

		const parentDir = dirname(currentDir);
		if (parentDir === currentDir) {
			// Reached filesystem root
			break;
		}
		currentDir = parentDir;
	}

	// Return in order from deepest to shallowest (closest to file first)
	return agentsFiles;
}

function getSessionFilePath(sessionId: string): string {
	return join("/tmp", `claude-agent-context-${sessionId}.json`);
}

function readSessionState(sessionFile: string): SessionState {
	const data = readFileSync(sessionFile, "utf8");
	const parsed = JSON.parse(data);

	// Convert seenAgentsFiles array back to Set
	return {
		...parsed,
		seenAgentsFiles: new Set(parsed.seenAgentsFiles || []),
	};
}

function writeSessionState(sessionFile: string, sessionState: SessionState): void {
	// Convert Set to array for JSON serialization
	const serializable = {
		...sessionState,
		seenAgentsFiles: Array.from(sessionState.seenAgentsFiles),
	};

	writeFileSync(sessionFile, JSON.stringify(serializable, null, 2));
}

// Only run if executed directly
if (import.meta.main) {
	main();
}
