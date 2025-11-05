// Parser for Claude Code session logs

import {dirname, join} from "node:path";
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
} from "./types";

export async function parseSessionLogs(sessionLogPath: string): Promise<SessionData> {
	const logDirectory = dirname(sessionLogPath);
	const sessionId = extractSessionId(sessionLogPath);

	// Parse main session log
	const mainLogEntries = await parseJsonlFile(sessionLogPath);

	// Find sub-agent logs that are referenced in this session
	const agentLogs = await findAgentLogs(logDirectory, mainLogEntries);

	// Build agent tree
	const mainAgent = await buildAgentTree(sessionId, mainLogEntries, agentLogs, logDirectory);

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

	return content
		.split("\n")
		.filter((line) => line.trim())
		.map((line) => JSON.parse(line) as LogEntry);
}

function extractSessionId(logPath: string): string {
	const filename = logPath.split("/").pop() || "";
	return filename.replace(".jsonl", "");
}

async function findAgentLogs(logDirectory: string, mainLogEntries: LogEntry[]): Promise<Map<string, string>> {
	// Extract agent IDs that were actually spawned in this session
	const agentIds = new Set<string>();

	for (const entry of mainLogEntries) {
		// Check toolUseResult for agentId
		if (entry.toolUseResult?.agentId) {
			agentIds.add(entry.toolUseResult.agentId);
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
						if (entry.toolUseResult?.agentId) {
							agentIds.add(entry.toolUseResult.agentId);
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

async function buildAgentTree(
	sessionId: string,
	mainLogEntries: LogEntry[],
	agentLogs: Map<string, string>,
	logDirectory: string,
): Promise<AgentNode> {
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

		const agentNode: AgentNode = {
			id: agentId,
			name: agentInfo.name,
			model: agentInfo.model,
			description: agentInfo.description,
			parent: sessionId,
			children: [],
			events: parseEvents(agentEntries, sessionId, agentId),
			logPath,
		};

		mainAgent.children.push(agentNode);
	}

	return mainAgent;
}

interface AgentInfo {
	name: string | null;
	model?: string;
	description?: string;
}

function extractAgentInfo(logEntries: LogEntry[], agentId: string): AgentInfo {
	// Find the Task tool use that spawned this agent
	for (const entry of logEntries) {
		if (entry.type === "user" && entry.toolUseResult?.agentId === agentId) {
			return {
				name: entry.toolUseResult.prompt?.substring(0, 50) || null,
				model: undefined,
				description: entry.toolUseResult.prompt,
			};
		}

		// Also check for Task tool use in assistant messages
		if (entry.type === "assistant" && entry.message?.content) {
			const content = Array.isArray(entry.message.content) ? entry.message.content : [];
			for (const item of content) {
				if (item.type === "tool_use" && item.name === "Task") {
					const toolUse = item as ToolUseContent;
					// Check if this tool use result contains our agentId
					const resultEntry = logEntries.find(
						(e) => e.type === "user" && e.toolUseResult?.agentId === agentId,
					);
					if (resultEntry) {
						return {
							name: (toolUse.input.description as string) || null,
							model: toolUse.input.model as string | undefined,
							description: toolUse.input.prompt as string | undefined,
						};
					}
				}
			}
		}
	}

	return {name: agentId};
}

function parseEvents(logEntries: LogEntry[], sessionId: string, agentId: string | null): Event[] {
	const events: Event[] = [];

	for (const entry of logEntries) {
		const baseEvent = {
			id: entry.uuid,
			parentId: entry.parentUuid,
			timestamp: new Date(entry.timestamp),
			sessionId,
			agentId: agentId || entry.agentId || null,
			agentName: null,
		};

		// Handle summary type
		if (entry.type === "summary") {
			events.push({
				...baseEvent,
				type: "summary" as EventType,
				data: {
					type: "summary",
					summary: entry.summary || "",
				},
			});
			continue;
		}

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
							output = toolResult.content.map((c) => c.text).join("\n");
						}

						events.push({
							...baseEvent,
							type: "tool-result" as EventType,
							data: {
								type: "tool-result",
								toolUseId: toolResult.tool_use_id,
								success: !toolResult.is_error,
								output,
								agentId: entry.toolUseResult?.agentId,
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
						events.push({
							...baseEvent,
							type: "tool-use" as EventType,
							data: {
								type: "tool-use",
								toolName: toolUse.name,
								toolId: toolUse.id,
								input: toolUse.input,
								description: (toolUse.input.description as string) || undefined,
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
