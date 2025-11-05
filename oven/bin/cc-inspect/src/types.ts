// Types for Claude Code session logs

export interface LogEntry {
	type: "summary" | "user" | "assistant";
	uuid: string;
	parentUuid: string | null;
	timestamp: string;
	sessionId: string;
	agentId?: string;
	isSidechain: boolean;
	userType?: string;
	cwd?: string;
	version?: string;
	gitBranch?: string;
	message?: Message;
	toolUseResult?: ToolUseResult;
	summary?: string;
	leafUuid?: string;
	thinkingMetadata?: ThinkingMetadata;
	requestId?: string;
}

export interface Message {
	role: "user" | "assistant";
	content: MessageContent[] | string;
	model?: string;
	id?: string;
	type?: string;
	stop_reason?: string | null;
	stop_sequence?: string | null;
	usage?: Usage;
}

export type MessageContent = TextContent | ThinkingContent | ToolUseContent | ToolResultContent;

export interface TextContent {
	type: "text";
	text: string;
}

export interface ThinkingContent {
	type: "thinking";
	thinking: string;
	signature?: string;
}

export interface ToolUseContent {
	type: "tool_use";
	id: string;
	name: string;
	input: Record<string, unknown>;
}

export interface ToolResultContent {
	type: "tool_result";
	tool_use_id: string;
	content: string | TextContent[];
	is_error?: boolean;
}

export interface ToolUseResult {
	status?: string;
	prompt?: string;
	agentId?: string;
	content?: TextContent[];
	totalDurationMs?: number;
	totalTokens?: number;
	totalToolUseCount?: number;
	usage?: Usage;
	stdout?: string;
	stderr?: string;
	interrupted?: boolean;
	isImage?: boolean;
	type?: string;
	file?: FileResult;
}

export interface FileResult {
	filePath: string;
	content: string;
	numLines: number;
	startLine: number;
	totalLines: number;
}

export interface Usage {
	input_tokens: number;
	cache_creation_input_tokens?: number;
	cache_read_input_tokens?: number;
	cache_creation?: {
		ephemeral_5m_input_tokens?: number;
		ephemeral_1h_input_tokens?: number;
	};
	output_tokens: number;
	service_tier?: string;
}

export interface ThinkingMetadata {
	level: string;
	disabled: boolean;
	triggers: unknown[];
}

// Processed event types for visualization
export interface Event {
	id: string;
	parentId: string | null;
	timestamp: Date;
	sessionId: string;
	agentId: string | null;
	agentName: string | null;
	type: EventType;
	data: EventData;
}

export type EventType =
	| "user-message"
	| "assistant-message"
	| "tool-use"
	| "tool-result"
	| "thinking"
	| "agent-spawn"
	| "summary";

export type EventData =
	| UserMessageData
	| AssistantMessageData
	| ToolUseData
	| ToolResultData
	| ThinkingData
	| AgentSpawnData
	| SummaryData;

export interface UserMessageData {
	type: "user-message";
	text: string;
}

export interface AssistantMessageData {
	type: "assistant-message";
	text: string;
}

export interface ToolUseData {
	type: "tool-use";
	toolName: string;
	toolId: string;
	input: Record<string, unknown>;
	description?: string;
}

export interface ToolResultData {
	type: "tool-result";
	toolUseId: string;
	success: boolean;
	output: string;
	agentId?: string;
}

export interface ThinkingData {
	type: "thinking";
	content: string;
}

export interface AgentSpawnData {
	type: "agent-spawn";
	agentId: string;
	description: string;
	prompt: string;
	model?: string;
}

export interface SummaryData {
	type: "summary";
	summary: string;
}

// Agent tree structure
export interface AgentNode {
	id: string;
	name: string | null;
	model?: string;
	description?: string;
	parent: string | null;
	children: AgentNode[];
	events: Event[];
	logPath: string;
}

export interface SessionData {
	sessionId: string;
	mainAgent: AgentNode;
	allEvents: Event[];
	logDirectory: string;
}
