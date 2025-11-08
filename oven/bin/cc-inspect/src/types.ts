// Types for Claude Code session logs

import {z} from "zod";

// Base Zod schemas
export const UsageSchema = z.object({
	input_tokens: z.number(),
	cache_creation_input_tokens: z.number().optional(),
	cache_read_input_tokens: z.number().optional(),
	cache_creation: z
		.object({
			ephemeral_5m_input_tokens: z.number().optional(),
			ephemeral_1h_input_tokens: z.number().optional(),
		})
		.optional(),
	output_tokens: z.number(),
	service_tier: z.string().nullable().optional(),
});

export const TextContentSchema = z.object({
	type: z.literal("text"),
	text: z.string(),
});

export const ThinkingContentSchema = z.object({
	type: z.literal("thinking"),
	thinking: z.string(),
	signature: z.string().optional(),
});

export const ToolUseContentSchema = z.object({
	type: z.literal("tool_use"),
	id: z.string(),
	name: z.string(),
	input: z.record(z.string(), z.unknown()),
});

export const ImageContentSchema = z.object({
	type: z.literal("image"),
	source: z.object({
		type: z.string(),
		media_type: z.string(),
		data: z.string(),
	}),
});

export const ToolResultContentSchema = z.object({
	type: z.literal("tool_result"),
	tool_use_id: z.string(),
	content: z.union([z.string(), z.array(z.union([TextContentSchema, ImageContentSchema]))]),
	is_error: z.boolean().optional(),
});

export const MessageContentSchema = z.discriminatedUnion("type", [
	TextContentSchema,
	ThinkingContentSchema,
	ToolUseContentSchema,
	ToolResultContentSchema,
	ImageContentSchema,
]);

export const FileResultSchema = z.object({
	filePath: z.string(),
	content: z.string(),
	numLines: z.number(),
	startLine: z.number(),
	totalLines: z.number(),
});

export const ImageFileResultSchema = z.object({
	base64: z.string(),
});

export const ToolUseResultSchema = z.object({
	status: z.string().optional(),
	prompt: z.string().optional(),
	agentId: z.string().optional(),
	content: z.union([z.array(TextContentSchema), z.string()]).optional(),
	totalDurationMs: z.number().optional(),
	totalTokens: z.number().optional(),
	totalToolUseCount: z.number().optional(),
	usage: UsageSchema.optional(),
	stdout: z.string().optional(),
	stderr: z.string().optional(),
	interrupted: z.boolean().optional(),
	isImage: z.boolean().optional(),
	type: z.string().optional(),
	file: z.union([FileResultSchema, ImageFileResultSchema]).optional(),
	// Grep tool result fields
	mode: z.string().optional(),
	numFiles: z.number().optional(),
	filenames: z.array(z.string()).optional(),
	numLines: z.number().optional(),
});

export const MessageSchema = z.object({
	role: z.enum(["user", "assistant"]),
	content: z.union([z.array(MessageContentSchema), z.string()]),
	model: z.string().optional(),
	id: z.string().optional(),
	type: z.string().optional(),
	stop_reason: z.string().nullable().optional(),
	stop_sequence: z.string().nullable().optional(),
	usage: UsageSchema.optional(),
});

export const ThinkingMetadataSchema = z.object({
	level: z.string(),
	disabled: z.boolean(),
	triggers: z.array(z.unknown()),
});

export const LogEntrySchema = z.object({
	type: z.string(),
	uuid: z.string().optional(),
	parentUuid: z.string().nullable().optional(),
	timestamp: z.string().optional(),
	sessionId: z.string().optional(),
	agentId: z.string().optional(),
	isSidechain: z.boolean().optional(),
	userType: z.string().optional(),
	cwd: z.string().optional(),
	version: z.string().optional(),
	gitBranch: z.string().optional(),
	message: MessageSchema.optional(),
	toolUseResult: z.union([ToolUseResultSchema, z.array(ToolUseResultSchema), z.string()]).optional(),
	summary: z.string().optional(),
	leafUuid: z.string().optional(),
	thinkingMetadata: ThinkingMetadataSchema.optional(),
	requestId: z.string().optional(),
});

// TypeScript types inferred from schemas
export type Usage = z.infer<typeof UsageSchema>;
export type TextContent = z.infer<typeof TextContentSchema>;
export type ThinkingContent = z.infer<typeof ThinkingContentSchema>;
export type ToolUseContent = z.infer<typeof ToolUseContentSchema>;
export type ToolResultContent = z.infer<typeof ToolResultContentSchema>;
export type ImageContent = z.infer<typeof ImageContentSchema>;
export type MessageContent = z.infer<typeof MessageContentSchema>;
export type FileResult = z.infer<typeof FileResultSchema>;
export type ImageFileResult = z.infer<typeof ImageFileResultSchema>;
export type ToolUseResult = z.infer<typeof ToolUseResultSchema>;
export type Message = z.infer<typeof MessageSchema>;
export type ThinkingMetadata = z.infer<typeof ThinkingMetadataSchema>;
export type LogEntry = z.infer<typeof LogEntrySchema>;

// Processed event types for visualization
export const UserMessageDataSchema = z.object({
	type: z.literal("user-message"),
	text: z.string(),
});

export const AssistantMessageDataSchema = z.object({
	type: z.literal("assistant-message"),
	text: z.string(),
});

export const ToolUseDataSchema = z.object({
	type: z.literal("tool-use"),
	toolName: z.string(),
	toolId: z.string(),
	input: z.record(z.string(), z.unknown()),
	description: z.string().optional(),
	isResume: z.boolean().optional(),
	resumesAgentId: z.string().optional(),
});

export const ToolResultDataSchema = z.object({
	type: z.literal("tool-result"),
	toolUseId: z.string(),
	success: z.boolean(),
	output: z.string(),
	agentId: z.string().optional(),
});

export const ThinkingDataSchema = z.object({
	type: z.literal("thinking"),
	content: z.string(),
});

export const AgentSpawnDataSchema = z.object({
	type: z.literal("agent-spawn"),
	agentId: z.string(),
	description: z.string(),
	prompt: z.string(),
	model: z.string().optional(),
});

export const SummaryDataSchema = z.object({
	type: z.literal("summary"),
	summary: z.string(),
});

export const EventDataSchema = z.discriminatedUnion("type", [
	UserMessageDataSchema,
	AssistantMessageDataSchema,
	ToolUseDataSchema,
	ToolResultDataSchema,
	ThinkingDataSchema,
	AgentSpawnDataSchema,
	SummaryDataSchema,
]);

export const EventTypeSchema = z.enum([
	"user-message",
	"assistant-message",
	"tool-use",
	"tool-result",
	"thinking",
	"agent-spawn",
	"summary",
]);

export const EventSchema = z.object({
	id: z.string(),
	parentId: z.string().nullable(),
	timestamp: z.date(),
	sessionId: z.string(),
	agentId: z.string().nullable(),
	agentName: z.string().nullable(),
	type: EventTypeSchema,
	data: EventDataSchema,
});

// Agent tree structure (recursive, so we need to use z.lazy)
export type AgentNode = {
	id: string;
	name: string | null;
	model?: string;
	subagentType?: string;
	description?: string;
	parent: string | null;
	children: AgentNode[];
	events: Event[];
	logPath: string;
	isResumed?: boolean;
	resumedFrom?: string; // Tool use ID that resumed this agent
};

export const AgentNodeSchema: z.ZodType<AgentNode> = z.lazy(() =>
	z.object({
		id: z.string(),
		name: z.string().nullable(),
		model: z.string().optional(),
		subagentType: z.string().optional(),
		description: z.string().optional(),
		parent: z.string().nullable(),
		children: z.array(AgentNodeSchema),
		events: z.array(EventSchema),
		logPath: z.string(),
		isResumed: z.boolean().optional(),
		resumedFrom: z.string().optional(),
	}),
);

export const SessionDataSchema = z.object({
	sessionId: z.string(),
	mainAgent: AgentNodeSchema,
	allEvents: z.array(EventSchema),
	logDirectory: z.string(),
});

export const SessionSchema = z.object({
	filename: z.string(),
	path: z.string(),
	sessionId: z.string(),
	modifiedAt: z.string(),
	size: z.number(),
});

// API Response Types (Discriminated Unions)
export const DirectoriesResponseSchema = z.discriminatedUnion("status", [
	z.object({status: z.literal("success"), directories: z.array(z.string())}),
	z.object({status: z.literal("error"), error: z.string()}),
]);

export const SessionsResponseSchema = z.discriminatedUnion("status", [
	z.object({status: z.literal("success"), sessions: z.array(SessionSchema)}),
	z.object({status: z.literal("error"), error: z.string()}),
]);

export const SessionDataResponseSchema = z.discriminatedUnion("status", [
	z.object({status: z.literal("success"), data: SessionDataSchema}),
	z.object({status: z.literal("error"), error: z.string()}),
]);

// TypeScript types inferred from schemas
export type UserMessageData = z.infer<typeof UserMessageDataSchema>;
export type AssistantMessageData = z.infer<typeof AssistantMessageDataSchema>;
export type ToolUseData = z.infer<typeof ToolUseDataSchema>;
export type ToolResultData = z.infer<typeof ToolResultDataSchema>;
export type ThinkingData = z.infer<typeof ThinkingDataSchema>;
export type AgentSpawnData = z.infer<typeof AgentSpawnDataSchema>;
export type SummaryData = z.infer<typeof SummaryDataSchema>;
export type EventData = z.infer<typeof EventDataSchema>;
export type EventType = z.infer<typeof EventTypeSchema>;
export type Event = z.infer<typeof EventSchema>;
export type SessionData = z.infer<typeof SessionDataSchema>;
export type Session = z.infer<typeof SessionSchema>;
export type DirectoriesResponse = z.infer<typeof DirectoriesResponseSchema>;
export type SessionsResponse = z.infer<typeof SessionsResponseSchema>;
export type SessionDataResponse = z.infer<typeof SessionDataResponseSchema>;
