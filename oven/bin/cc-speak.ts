// :module: Advanced text-to-speech tool with file and section reading support

import {$} from "bun";
import {parseArgs} from "util";
import {stripMarkdownTTS} from "./strip-markdown";

// Shell command executor interface for dependency injection
export interface ShellExecutor {
	tts: (text: string) => Promise<string>;
	speak: (audioFile: string) => Promise<void>;
}

// Default implementation using Bun's $ operator
export const defaultShellExecutor: ShellExecutor = {
	async tts(text: string): Promise<string> {
		const audioFile = await $`tts < ${new Response(text)}`.quiet().text();
		return audioFile.trim();
	},
	async speak(audioFile: string): Promise<void> {
		// Use external speak command and consume output with .text() to prevent hanging
		await $`speak ${audioFile}`.text();
	},
};

function showHelp() {
	console.log(`cc-speak - Advanced text-to-speech tool with file and section reading support

Usage: cc-speak [OPTIONS]

Enable audio communication mode with flexible input options.

OPTIONS:
    --text TEXT, -t TEXT     Speak the provided text
    --file FILE, -f FILE     Speak the entire file contents
    --start NUM              Start reading from line NUM (1-indexed, inclusive)
    --end NUM                End reading at line NUM (1-indexed, inclusive)
    --help, -h               Show this help message

EXAMPLES:
    cc-speak --text "Hello world"
    cc-speak --file README.md
    cc-speak --file README.md --start=5 --end=20
    cc-speak -f docs.md -s 1 -e 10
    cc-speak --text "Context explanation" --file README.md --start=1 --end=10

NOTES:
    - Line numbers are 1-indexed and inclusive
    - --start without --end reads from start line to end of file
    - --end without --start reads from beginning to end line
    - Both --text and --file can be used together for context + content
    - When both provided, text is spoken first, then file content
    - Markdown formatting is automatically stripped for better TTS
    - Processing happens in parallel for improved performance
`);
	process.exit(0);
}

export interface CcSpeakOptions {
	text?: string;
	file?: string;
	startLine?: number;
	endLine?: number;
	executor?: ShellExecutor; // Optional executor for testing
}

export interface ProcessResult {
	success: boolean;
	audioFiles: string[];
	errors?: string[];
}

async function main() {
	const {values} = parseArgs({
		args: Bun.argv.slice(2),
		options: {
			text: {type: "string", short: "t"},
			file: {type: "string", short: "f"},
			start: {type: "string", short: "s"},
			end: {type: "string", short: "e"},
			help: {type: "boolean", short: "h"},
		},
		strict: false,
		allowPositionals: false,
	});

	if (values.help) {
		showHelp();
	}

	// Validate input
	if (!values.text && !values.file) {
		console.error("Error: Must provide either --text or --file option (or both)");
		console.error("Use --help for usage information");
		process.exit(1);
	}

	// Parse line numbers
	let startLine: number | undefined;
	let endLine: number | undefined;

	if (values.start && typeof values.start === "string") {
		startLine = parseInt(values.start, 10);
		if (Number.isNaN(startLine) || startLine < 1) {
			console.error("Error: --start must be a positive integer");
			process.exit(1);
		}
	}

	if (values.end && typeof values.end === "string") {
		endLine = parseInt(values.end, 10);
		if (Number.isNaN(endLine) || endLine < 1) {
			console.error("Error: --end must be a positive integer");
			process.exit(1);
		}
	}

	// Validate that line range options are only used with --file
	if ((startLine || endLine) && !values.file) {
		console.error("Error: --start and --end can only be used with --file");
		process.exit(1);
	}

	try {
		const result = await ccSpeakLib({
			text: typeof values.text === "string" ? values.text : undefined,
			file: typeof values.file === "string" ? values.file : undefined,
			startLine,
			endLine,
			executor: defaultShellExecutor,
		});

		if (!result.success) {
			console.error("Errors occurred:");
			result.errors?.forEach((error) => {
				console.error(`  ${error}`);
			});
			process.exit(1);
		}
	} catch (error) {
		console.error(`Fatal error: ${error instanceof Error ? error.message : String(error)}`);
		process.exit(1);
	}
}

export async function ccSpeakLib(options: CcSpeakOptions): Promise<ProcessResult> {
	const {text, file, startLine, endLine, executor = defaultShellExecutor} = options;
	const audioFiles: string[] = [];
	const errors: string[] = [];

	// Validate input
	if (!text && !file) {
		return {
			success: false,
			audioFiles: [],
			errors: ["Must provide either --text or --file option (or both)"],
		};
	}

	// Process and play audio in streaming fashion
	try {
		// Process text first if provided
		if (text) {
			const audioFile = await processTextToAudio(text, executor);
			if (audioFile) {
				audioFiles.push(audioFile);
				await playAudio(audioFile, executor);
				await cleanupAudioFile(audioFile);
			}
		}

		// Process file second if provided
		if (file) {
			const audioFile = await processFileToAudio({filePath: file, executor, startLine, endLine});
			if (audioFile) {
				audioFiles.push(audioFile);
				await playAudio(audioFile, executor);
				await cleanupAudioFile(audioFile);
			}
		}

		return {success: true, audioFiles};
	} catch (error) {
		errors.push(`Processing failed: ${error instanceof Error ? error.message : String(error)}`);
		return {success: false, audioFiles, errors};
	}
}

async function processTextToAudio(text: string, executor: ShellExecutor): Promise<string | null> {
	try {
		const strippedText = stripMarkdownTTS(text, false);
		const audioFile = await executor.tts(strippedText);
		return audioFile;
	} catch (error) {
		console.error(error);
		throw new Error(`Text processing failed: ${error instanceof Error ? error.message : String(error)}`);
	}
}

async function processFileToAudio(options: {
	filePath: string;
	executor: ShellExecutor;
	startLine?: number;
	endLine?: number;
}): Promise<string | null> {
	const {filePath, executor, startLine, endLine} = options;
	try {
		// Check if file exists
		const file = Bun.file(filePath);
		if (!(await file.exists())) {
			throw new Error(`File not found: ${filePath}`);
		}

		let content = await file.text();

		// Apply line range if specified
		if (startLine || endLine) {
			const lines = content.split("\n");
			const totalLines = lines.length;

			// Set defaults
			const start = startLine ?? 1;
			const end = endLine ?? totalLines;

			// Validate range
			if (start < 1) {
				throw new Error("--start must be >= 1");
			}
			if (end > totalLines) {
				throw new Error(`--end (${end}) exceeds file length (${totalLines} lines)`);
			}
			if (start > end) {
				throw new Error(`--start (${start}) cannot be greater than --end (${end})`);
			}

			// Extract the specified range (convert to 0-based indexing)
			content = lines.slice(start - 1, end).join("\n");
		}

		const strippedContent = stripMarkdownTTS(content, false);
		const audioFile = await executor.tts(strippedContent);
		return audioFile;
	} catch (error) {
		console.error(error);
		throw new Error(`File processing failed: ${error instanceof Error ? error.message : String(error)}`);
	}
}

async function playAudio(audioFilePath: string, executor: ShellExecutor): Promise<void> {
	try {
		await executor.speak(audioFilePath);
	} catch (error) {
		console.error(error);
		throw new Error(`Audio playback failed: ${error instanceof Error ? error.message : String(error)}`);
	}
}

async function cleanupAudioFile(audioFilePath: string): Promise<void> {
	try {
		await Bun.file(audioFilePath).delete();
	} catch (err) {
		console.warn(err);
	}
}

if (import.meta.main) {
	main();
}
