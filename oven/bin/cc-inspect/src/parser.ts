// Parser for Claude Code session logs

import {dirname, join} from "node:path";
import type {ZodError} from "zod";
import type {
	AgentNode,
	Event,
	EventType,
	LogEntry,
	SessionData,
	TextContent,
	ThinkingContent,
	ToolResultContent,
	ToolUseContent,
	ToolUseResult,
} from "./types";
import {LogEntrySchema} from "./types";

/**
 * Detailed error information for parse failures
 */
export class ParseError extends Error {
	public readonly filePath: string;
	public readonly lineNumber: number;
	public readonly rawLine: string;
	public readonly zodError?: ZodError;

	constructor(options: {
		message: string;
		filePath: string;
		lineNumber: number;
		rawLine: string;
		zodError?: ZodError;
	}) {
		super(options.message);
		this.name = "ParseError";
		this.filePath = options.filePath;
		this.lineNumber = options.lineNumber;
		this.rawLine = options.rawLine;
		this.zodError = options.zodError;
	}

	override toString(): string {
		const lines = [
			`${this.name}: ${this.message}`,
			`  File: ${this.filePath}`,
			`  Line: ${this.lineNumber}`,
			`  Raw content (first 200 chars): ${this.rawLine.substring(0, 200)}${this.rawLine.length > 200 ? "..." : ""}`,
		];

		if (this.zodError) {
			lines.push(`  Validation errors:`);

			// Parse the original JSON to show actual values
			let parsedData: unknown;
			try {
				parsedData = JSON.parse(this.rawLine);
			} catch {
				parsedData = null;
			}

			for (const issue of this.zodError.issues) {
				const pathStr = issue.path.length > 0 ? issue.path.join(" → ") : "(root)";
				lines.push(`\n    [${issue.code}] at ${pathStr}`);
				lines.push(`      Message: ${issue.message}`);

				// Show expected/received for type errors
				if (issue.code === "invalid_type" && "expected" in issue && "received" in issue) {
					lines.push(`      Expected: ${String(issue.expected)}`);
					lines.push(`      Received: ${String(issue.received)}`);
				}

				// Show actual value if we can extract it
				if (parsedData && issue.path.length > 0) {
					try {
						let value: unknown = parsedData;
						for (const key of issue.path) {
							if (value && typeof value === "object" && typeof key !== "symbol" && key in value) {
								value = (value as Record<string | number, unknown>)[key];
							}
						}
						const valueStr = JSON.stringify(value);
						lines.push(
							`      Actual value: ${valueStr.length > 100 ? `${valueStr.substring(0, 100)}...` : valueStr}`,
						);
					} catch {
						// Can't extract value, skip it
					}
				}

				// Show valid options for enums (checking property existence dynamically)
				if ("options" in issue && Array.isArray(issue.options)) {
					lines.push(`      Valid options: ${issue.options.join(", ")}`);
				}

				// Show constraints for size validations
				if (issue.code === "too_small" && "minimum" in issue && "inclusive" in issue && "type" in issue) {
					lines.push(
						`      Minimum ${issue.inclusive ? "(inclusive)" : "(exclusive)"}: ${String(issue.minimum)}`,
					);
					lines.push(`      Validation type: ${String(issue.type)}`);
				}
				if (issue.code === "too_big" && "maximum" in issue && "inclusive" in issue && "type" in issue) {
					lines.push(
						`      Maximum ${issue.inclusive ? "(inclusive)" : "(exclusive)"}: ${String(issue.maximum)}`,
					);
					lines.push(`      Validation type: ${String(issue.type)}`);
				}

				// Show unrecognized keys
				if (issue.code === "unrecognized_keys" && "keys" in issue && Array.isArray(issue.keys)) {
					lines.push(`      Unrecognized keys: ${issue.keys.join(", ")}`);
				}
			}
		}

		return lines.join("\n");
	}
}

export async function parseSessionLogs(sessionLogPath: string): Promise<SessionData> {
	const logDirectory = dirname(sessionLogPath);
	const sessionId = extractSessionId(sessionLogPath);

	// Parse main session log
	const mainLogEntries = await parseJsonlFile(sessionLogPath);

	// Find sub-agent logs that are referenced in this session
	const agentLogs = await findAgentLogs(logDirectory, mainLogEntries);

	// Build agent tree
	const mainAgent = await buildAgentTree({sessionId, mainLogEntries, agentLogs, logDirectory});

	// Extract all events chronologically
	const allEvents = extractAllEvents(mainAgent);

	return {
		sessionId,
		mainAgent,
		allEvents,
		logDirectory,
	};
}

async function parseJsonlFile(filePath: string): Promise<LogEntry[]> {
	const file = Bun.file(filePath);
	const content = await file.text();

	const lines = content.split("\n").filter((line) => line.trim());
	const entries: LogEntry[] = [];

	for (let i = 0; i < lines.length; i++) {
		const line = lines[i];
		if (!line) continue; // Type guard for TypeScript
		const lineNumber = i + 1;

		try {
			// First parse JSON
			const parsed = JSON.parse(line);

			// Then validate with Zod
			const result = LogEntrySchema.safeParse(parsed);

			if (!result.success) {
				throw new ParseError({
					message: `Failed to validate log entry against schema`,
					filePath,
					lineNumber,
					rawLine: line,
					zodError: result.error,
				});
			}

			entries.push(result.data);
		} catch (error: unknown) {
			// If it's already a ParseError, rethrow it
			if (error instanceof ParseError) {
				throw error;
			}

			// If it's a JSON parse error, wrap it
			if (error instanceof SyntaxError) {
				throw new ParseError({
					message: `Failed to parse JSON: ${error.message}`,
					filePath,
					lineNumber,
					rawLine: line,
				});
			}

			// Unknown error
			const errorMessage = error instanceof Error ? error.message : String(error);
			throw new ParseError({
				message: `Unexpected error: ${errorMessage}`,
				filePath,
				lineNumber,
				rawLine: line,
			});
		}
	}

	return entries;
}

function extractSessionId(logPath: string): string {
	const filename = logPath.split("/").pop() || "";
	return filename.replace(".jsonl", "");
}

/**
 * Normalizes toolUseResult to a single object.
 * If it's an array, returns the first element.
 * If it's a string, returns undefined.
 * If it's an object, returns it as-is.
 */
function normalizeToolUseResult(toolUseResult: LogEntry["toolUseResult"]): ToolUseResult | undefined {
	if (!toolUseResult || typeof toolUseResult === "string") {
		return undefined;
	}
	return Array.isArray(toolUseResult) ? toolUseResult[0] : toolUseResult;
}

async function findAgentLogs(logDirectory: string, mainLogEntries: LogEntry[]): Promise<Map<string, string>> {
	// Extract agent IDs that were actually spawned in this session
	const agentIds = new Set<string>();

	for (const entry of mainLogEntries) {
		// Check toolUseResult for agentId
		const result = normalizeToolUseResult(entry.toolUseResult);
		if (result?.agentId) {
			agentIds.add(result.agentId);
		}

		// Also check message content for Task tool results
		if (entry.type === "user" && entry.message?.content) {
			const content = Array.isArray(entry.message.content) ? entry.message.content : [];
			for (const item of content) {
				if (item.type === "tool_result") {
					const toolResult = item as ToolResultContent;
					// Check if content mentions agentId
					if (typeof toolResult.content === "string") {
						// For tool results from Task tool, agentId is in toolUseResult
						const result = normalizeToolUseResult(entry.toolUseResult);
						if (result?.agentId) {
							agentIds.add(result.agentId);
						}
					}
				}
			}
		}
	}

	// Only load agent logs for agents that were spawned in this session
	const agentLogs = new Map<string, string>();
	for (const agentId of agentIds) {
		const logPath = join(logDirectory, `agent-${agentId}.jsonl`);
		// Check if file exists by trying to read it
		try {
			const file = Bun.file(logPath);
			const exists = await file.exists();
			if (exists) {
				agentLogs.set(agentId, logPath);
			} else {
				console.warn(`Warning: Agent log file not found for agent ${agentId}`);
			}
		} catch {
			// Agent log file doesn't exist, skip it
			console.warn(`Warning: Could not access agent log file for agent ${agentId}`);
		}
	}

	return agentLogs;
}

async function buildAgentTree(options: {
	sessionId: string;
	mainLogEntries: LogEntry[];
	agentLogs: Map<string, string>;
	logDirectory: string;
}): Promise<AgentNode> {
	const {sessionId, mainLogEntries, agentLogs, logDirectory} = options;
	// Create main agent node
	const mainAgent: AgentNode = {
		id: sessionId,
		name: "Main Agent",
		parent: null,
		children: [],
		events: parseEvents(mainLogEntries, sessionId, null),
		logPath: join(logDirectory, `${sessionId}.jsonl`),
	};

	// Parse sub-agent logs and add as children
	for (const [agentId, logPath] of agentLogs) {
		const agentEntries = await parseJsonlFile(logPath);
		const agentInfo = extractAgentInfo(mainLogEntries, agentId);

		// Parse events from agent file
		const agentFileEvents = parseEvents(agentEntries, sessionId, agentId);

		// Also parse events from session log that belong to this agent (for resumed agents)
		const sessionAgentEvents = parseSessionEventsForAgent(mainLogEntries, sessionId, agentId);

		// Combine and sort events
		const allAgentEvents = [...agentFileEvents, ...sessionAgentEvents].sort(
			(a, b) => a.timestamp.getTime() - b.timestamp.getTime(),
		);

		const agentNode: AgentNode = {
			id: agentId,
			name: agentInfo.name,
			model: agentInfo.model,
			subagentType: agentInfo.subagentType,
			description: agentInfo.description,
			parent: sessionId,
			children: [],
			events: allAgentEvents,
			logPath,
			isResumed: agentInfo.isResumed,
			resumedFrom: agentInfo.resumedFrom,
		};

		mainAgent.children.push(agentNode);
	}

	return mainAgent;
}

interface AgentInfo {
	name: string | null;
	model?: string;
	subagentType?: string;
	description?: string;
}

interface ExtendedAgentInfo extends AgentInfo {
	isResumed?: boolean;
	resumedFrom?: string;
}

function extractAgentInfo(logEntries: LogEntry[], agentId: string): ExtendedAgentInfo {
	// First, find the result entry that contains this agentId
	const resultEntry = logEntries.find((e) => {
		if (e.type !== "user") return false;
		const result = normalizeToolUseResult(e.toolUseResult);
		return result?.agentId === agentId;
	});

	const normalizedResult = resultEntry ? normalizeToolUseResult(resultEntry.toolUseResult) : undefined;

	if (!resultEntry || !normalizedResult) {
		return {name: agentId};
	}

	// Find the tool_result in the message content to get the tool_use_id
	let toolUseId: string | undefined;
	if (resultEntry.message?.content && Array.isArray(resultEntry.message.content)) {
		for (const item of resultEntry.message.content) {
			if (item.type === "tool_result") {
				const toolResult = item as ToolResultContent;
				toolUseId = toolResult.tool_use_id;
				break;
			}
		}
	}

	if (!toolUseId) {
		// Fallback to basic info from toolUseResult
		return {
			name: normalizedResult.prompt?.substring(0, 50) || null,
			model: undefined,
			description: normalizedResult.prompt,
		};
	}

	// Now find the assistant message that contains the matching tool_use
	for (const entry of logEntries) {
		if (entry.type === "assistant" && entry.message?.content) {
			const content = Array.isArray(entry.message.content) ? entry.message.content : [];
			for (const item of content) {
				if (item.type === "tool_use" && item.id === toolUseId && item.name === "Task") {
					const toolUse = item as ToolUseContent;
					// Check if this is a resume call
					const isResume = "resume" in toolUse.input;
					const resumesAgentId = isResume ? (toolUse.input.resume as string) : undefined;

					return {
						name: (toolUse.input.description as string) || null,
						model: toolUse.input.model as string | undefined,
						subagentType: toolUse.input.subagent_type as string | undefined,
						description: toolUse.input.prompt as string | undefined,
						isResumed: isResume && resumesAgentId === agentId,
						resumedFrom: isResume && resumesAgentId === agentId ? toolUse.id : undefined,
					};
				}
			}
		}
	}

	// Fallback if we can't find the tool_use
	return {
		name: normalizedResult.prompt?.substring(0, 50) || null,
		model: undefined,
		description: normalizedResult.prompt,
	};
}

// Parse events from session log that belong to a specific agent (after resume)
function parseSessionEventsForAgent(logEntries: LogEntry[], sessionId: string, agentId: string): Event[] {
	const events: Event[] = [];

	// Look for tool results and assistant messages that belong to this agent
	for (const entry of logEntries) {
		// Skip events that would be in the agent's own log file
		if (entry.agentId === agentId) continue;

		// Check if this is a tool result for this agent (happens after resume)
		const result = normalizeToolUseResult(entry.toolUseResult);
		if (entry.type === "user" && result?.agentId === agentId) {
			// Skip entries missing required fields
			if (!entry.uuid || !entry.timestamp) continue;

			const content = entry.message?.content;
			if (Array.isArray(content)) {
				for (const item of content) {
					if (item.type === "tool_result") {
						const toolResult = item as ToolResultContent;
						let output = "";

						if (typeof toolResult.content === "string") {
							output = toolResult.content;
						} else if (Array.isArray(toolResult.content)) {
							output = toolResult.content.map((c) => (c.type === "text" ? c.text : "[Image]")).join("\n");
						}

						events.push({
							id: entry.uuid,
							parentId: entry.parentUuid ?? null,
							timestamp: new Date(entry.timestamp),
							sessionId,
							agentId,
							agentName: null,
							type: "tool-result" as EventType,
							data: {
								type: "tool-result",
								toolUseId: toolResult.tool_use_id,
								success: !toolResult.is_error,
								output,
								agentId,
							},
						});
					}
				}
			}
		}
	}

	return events;
}

function parseEvents(logEntries: LogEntry[], sessionId: string, agentId: string | null): Event[] {
	const events: Event[] = [];

	for (const entry of logEntries) {
		// Skip unknown log entry types (e.g., "queue-operation")
		if (entry.type !== "summary" && entry.type !== "user" && entry.type !== "assistant") {
			continue;
		}

		// Normalize toolUseResult at the start of each iteration
		const result = normalizeToolUseResult(entry.toolUseResult);

		// Handle summary type (which has minimal fields)
		if (entry.type === "summary") {
			events.push({
				id: entry.leafUuid || entry.uuid || "unknown",
				parentId: entry.parentUuid ?? null,
				timestamp: entry.timestamp ? new Date(entry.timestamp) : new Date(),
				sessionId,
				agentId: agentId || entry.agentId || sessionId,
				agentName: null,
				type: "summary" as EventType,
				data: {
					type: "summary",
					summary: entry.summary || "",
				},
			});
			continue;
		}

		// For non-summary entries, these fields should exist
		if (!entry.uuid || !entry.timestamp) {
			console.warn(`Skipping entry with missing required fields: ${JSON.stringify(entry)}`);
			continue;
		}

		const baseEvent = {
			id: entry.uuid,
			parentId: entry.parentUuid ?? null,
			timestamp: new Date(entry.timestamp),
			sessionId,
			// For main agent (agentId param is null), use sessionId as the agentId
			// This ensures events can be looked up by agent ID since main agent's ID is sessionId
			agentId: agentId || entry.agentId || sessionId,
			agentName: null,
		};

		// Handle user messages
		if (entry.type === "user" && entry.message) {
			const content = entry.message.content;

			// Check if it's a tool result
			if (Array.isArray(content)) {
				for (const item of content) {
					if (item.type === "tool_result") {
						const toolResult = item as ToolResultContent;
						let output = "";

						if (typeof toolResult.content === "string") {
							output = toolResult.content;
						} else if (Array.isArray(toolResult.content)) {
							output = toolResult.content.map((c) => (c.type === "text" ? c.text : "[Image]")).join("\n");
						}

						events.push({
							...baseEvent,
							type: "tool-result" as EventType,
							data: {
								type: "tool-result",
								toolUseId: toolResult.tool_use_id,
								success: !toolResult.is_error,
								output,
								agentId: result?.agentId,
							},
						});
					}
				}
			} else if (typeof content === "string") {
				events.push({
					...baseEvent,
					type: "user-message" as EventType,
					data: {
						type: "user-message",
						text: content,
					},
				});
			}
		}

		// Handle assistant messages
		if (entry.type === "assistant" && entry.message) {
			const content = entry.message.content;

			if (Array.isArray(content)) {
				for (const item of content) {
					if (item.type === "text") {
						const textContent = item as TextContent;
						events.push({
							...baseEvent,
							type: "assistant-message" as EventType,
							data: {
								type: "assistant-message",
								text: textContent.text,
							},
						});
					} else if (item.type === "thinking") {
						const thinkingContent = item as ThinkingContent;
						events.push({
							...baseEvent,
							type: "thinking" as EventType,
							data: {
								type: "thinking",
								content: thinkingContent.thinking,
							},
						});
					} else if (item.type === "tool_use") {
						const toolUse = item as ToolUseContent;
						const isResume = toolUse.name === "Task" && "resume" in toolUse.input;
						const resumesAgentId = isResume ? (toolUse.input.resume as string) : undefined;

						events.push({
							...baseEvent,
							type: "tool-use" as EventType,
							data: {
								type: "tool-use",
								toolName: toolUse.name,
								toolId: toolUse.id,
								input: toolUse.input,
								description: (toolUse.input.description as string) || undefined,
								isResume,
								resumesAgentId,
							},
						});
					}
				}
			}
		}
	}

	return events;
}

function extractAllEvents(agent: AgentNode): Event[] {
	const events = [...agent.events];

	for (const child of agent.children) {
		events.push(...extractAllEvents(child));
	}

	// Sort by timestamp
	events.sort((a, b) => a.timestamp.getTime() - b.timestamp.getTime());

	return events;
}
