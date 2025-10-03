// :module: Type definitions for Claude Code transcript JSONL format

/**
 * Base interface for all transcript entries
 */
export interface BaseTranscriptEntry {
	parentUuid: string;
	isSidechain: boolean;
	userType: string;
	cwd: string;
	sessionId: string;
	version: string;
	gitBranch: string;
	type: string;
	uuid: string;
	timestamp: string;
}

/**
 * User message entry in transcript
 */
export interface UserTranscriptEntry extends BaseTranscriptEntry {
	type: "user";
	message: {
		role: "user";
		content: string | UserMessageContent[];
	};
	thinkingMetadata?: {
		level: string;
		disabled: boolean;
		triggers: string[];
	};
}

/**
 * Content types in user messages
 */
export type UserMessageContent =
	| {type: "text"; text: string}
	| {type: "tool_result"; content: string; is_error: boolean; tool_use_id: string};

/**
 * Assistant message entry in transcript
 */
export interface AssistantTranscriptEntry extends BaseTranscriptEntry {
	type: "assistant";
	message: {
		id: string;
		type: "message";
		role: "assistant";
		model: string;
		content: AssistantMessageContent[];
		stop_reason: string | null;
		stop_sequence: string | null;
		usage: {
			input_tokens: number;
			cache_creation_input_tokens?: number;
			cache_read_input_tokens?: number;
			cache_creation?: {
				ephemeral_5m_input_tokens: number;
				ephemeral_1h_input_tokens: number;
			};
			output_tokens: number;
			service_tier: string;
		};
	};
	requestId: string;
}

/**
 * Content types in assistant messages
 */
export type AssistantMessageContent =
	| {type: "text"; text: string}
	| {
			type: "tool_use";
			id: string;
			name: string;
			input: Record<string, unknown>;
	  };

/**
 * System message entry in transcript
 */
export interface SystemTranscriptEntry extends BaseTranscriptEntry {
	type: "system";
	subtype: string;
	content: string;
	isMeta: boolean;
	toolUseID?: string;
	level?: string;
}

/**
 * Union type for all transcript entries
 */
export type TranscriptEntry = UserTranscriptEntry | AssistantTranscriptEntry | SystemTranscriptEntry;

/**
 * Extract text content from a user message
 */
export function extractUserText(entry: UserTranscriptEntry): string | null {
	const {content} = entry.message;

	// Handle string content directly
	if (typeof content === "string") {
		return content;
	}

	// Handle array content (look for text type)
	if (Array.isArray(content)) {
		const textContent = content.find((item) => item.type === "text");
		if (textContent && "text" in textContent) {
			return textContent.text;
		}
	}

	return null;
}

/**
 * Check if a transcript entry is a user message with text
 */
export function isUserTextMessage(entry: unknown): entry is UserTranscriptEntry {
	if (!entry || typeof entry !== "object") {
		return false;
	}

	const typedEntry = entry as Partial<TranscriptEntry>;
	return (
		typedEntry.type === "user" &&
		"message" in typedEntry &&
		typeof typedEntry.message === "object" &&
		typedEntry.message !== null &&
		"content" in typedEntry.message
	);
}

/**
 * Token usage summary
 */
export interface TokenUsage {
	totalInputTokens: number;
	totalOutputTokens: number;
	totalTokens: number;
}

/**
 * Context snapshot - total tokens at a point in time
 */
export interface ContextSnapshot {
	currentContextSize: number; // Current total context window size
	lastPromptDelta: number; // Tokens added since last user prompt
}

/**
 * Calculate total token usage from transcript entries
 * Only counts new tokens (input_tokens + output_tokens), not cached token reads
 */
export function calculateTokenUsage(entries: unknown[]): TokenUsage {
	let totalInputTokens = 0;
	let totalOutputTokens = 0;

	for (const entry of entries) {
		if (!entry || typeof entry !== "object") continue;

		const typedEntry = entry as Partial<AssistantTranscriptEntry>;
		if (typedEntry.type === "assistant" && typedEntry.message?.usage) {
			const usage = typedEntry.message.usage;
			// Only count new tokens, not cached reads (which are reused tokens)
			totalInputTokens += usage.input_tokens || 0;
			// Sum output tokens
			totalOutputTokens += usage.output_tokens || 0;
		}
	}

	return {
		totalInputTokens,
		totalOutputTokens,
		totalTokens: totalInputTokens + totalOutputTokens,
	};
}

/**
 * Calculate context snapshot - current size and last prompt delta
 */
export function calculateContextSnapshot(entries: unknown[]): ContextSnapshot {
	let currentContextSize = 0;
	let lastPromptDelta = 0;

	// Find latest assistant message for current context size
	let latestAssistantIndex = -1;
	let lastUserIndex = -1;
	let assistantBeforeLastUser = -1;

	for (let i = entries.length - 1; i >= 0; i--) {
		const entry = entries[i];
		if (!entry || typeof entry !== "object") continue;

		const typedEntry = entry as Partial<TranscriptEntry>;

		// Find latest assistant message
		if (latestAssistantIndex === -1 && typedEntry.type === "assistant") {
			latestAssistantIndex = i;
			const assistantEntry = typedEntry as Partial<AssistantTranscriptEntry>;
			if (assistantEntry.message?.usage) {
				const usage = assistantEntry.message.usage;
				currentContextSize =
					(usage.input_tokens || 0) +
					(usage.cache_creation_input_tokens || 0) +
					(usage.cache_read_input_tokens || 0);
			}
		}

		// Find last user text message
		if (lastUserIndex === -1 && isUserTextMessage(entry)) {
			const text = extractUserText(entry as UserTranscriptEntry);
			if (text) {
				lastUserIndex = i;
			}
		}

		// Find assistant message before last user message
		if (
			lastUserIndex !== -1 &&
			assistantBeforeLastUser === -1 &&
			typedEntry.type === "assistant" &&
			i < lastUserIndex
		) {
			assistantBeforeLastUser = i;
			const assistantEntry = typedEntry as Partial<AssistantTranscriptEntry>;
			if (assistantEntry.message?.usage) {
				const usage = assistantEntry.message.usage;
				const previousContextSize =
					(usage.input_tokens || 0) +
					(usage.cache_creation_input_tokens || 0) +
					(usage.cache_read_input_tokens || 0);
				lastPromptDelta = currentContextSize - previousContextSize;
			}
			break; // We have everything we need
		}
	}

	return {currentContextSize, lastPromptDelta};
}

/**
 * Format token count in thousands (e.g., 32000 -> "32K")
 */
export function formatTokenCount(tokens: number): string {
	const thousands = Math.round(tokens / 1000);
	return `${thousands}K`;
}
