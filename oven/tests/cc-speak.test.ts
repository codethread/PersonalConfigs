import {afterEach, beforeEach, describe, expect, test} from "bun:test";
import {mkdir, rmdir, writeFile} from "fs/promises";
import {dirname, resolve} from "path";
import {type CcSpeakOptions, ccSpeakLib, type ShellExecutor} from "../bin/cc-speak";

const testDir = resolve(dirname(import.meta.dir), "test-tmp");
const testFile = resolve(testDir, "test.md");

beforeEach(async () => {
	await mkdir(testDir, {recursive: true});
});

afterEach(async () => {
	try {
		await rmdir(testDir, {recursive: true});
	} catch (_error) {
		// Ignore cleanup errors
	}
});

describe("ccSpeakLib", () => {
	test("should reject empty options", async () => {
		const options: CcSpeakOptions = {};

		const result = await ccSpeakLib(options);
		expect(result.success).toBe(false);
		expect(result.errors).toBeDefined();
		expect(
			result.errors?.some((error) => error.includes("Must provide either --text or --file option")),
		).toBe(true);
	});

	test("should process text-only successfully", async () => {
		const mockExecutor: ShellExecutor = {
			async tts(text: string): Promise<string> {
				expect(text).toBe("Hello world! This is italicized text.");
				return "/tmp/audio1.wav";
			},
			async speak(audioFile: string): Promise<void> {
				expect(audioFile).toBe("/tmp/audio1.wav");
			},
		};

		const options: CcSpeakOptions = {
			text: "Hello **world**! This is *italicized* text.",
			executor: mockExecutor,
		};

		const result = await ccSpeakLib(options);

		expect(result.success).toBe(true);
		expect(result.audioFiles).toHaveLength(1);
		expect(result.audioFiles[0]).toBe("/tmp/audio1.wav");
	});

	test("should validate line range bounds", async () => {
		const testContent = `Line 1
Line 2
Line 3`;

		await writeFile(testFile, testContent);

		const mockExecutor: ShellExecutor = {
			async tts(): Promise<string> {
				return "/tmp/audio.wav";
			},
			async speak(): Promise<void> {},
		};

		const options: CcSpeakOptions = {
			file: testFile,
			startLine: 1,
			endLine: 10, // Exceeds file length
			executor: mockExecutor,
		};

		const result = await ccSpeakLib(options);
		expect(result.success).toBe(false);
		expect(result.errors).toBeDefined();
		expect(result.errors?.some((error) => error.includes("exceeds file length"))).toBe(true);
	});

	test("should validate start > end", async () => {
		const testContent = `Line 1
Line 2
Line 3`;

		await writeFile(testFile, testContent);

		const mockExecutor: ShellExecutor = {
			async tts(): Promise<string> {
				return "/tmp/audio.wav";
			},
			async speak(): Promise<void> {},
		};

		const options: CcSpeakOptions = {
			file: testFile,
			startLine: 3,
			endLine: 1, // Invalid range
			executor: mockExecutor,
		};

		const result = await ccSpeakLib(options);
		expect(result.success).toBe(false);
		expect(result.errors).toBeDefined();
		expect(result.errors?.some((error) => error.includes("cannot be greater than"))).toBe(true);
	});

	test("should handle non-existent file", async () => {
		const mockExecutor: ShellExecutor = {
			async tts(): Promise<string> {
				return "/tmp/audio.wav";
			},
			async speak(): Promise<void> {},
		};

		const options: CcSpeakOptions = {
			file: "/path/to/nonexistent/file.txt",
			executor: mockExecutor,
		};

		const result = await ccSpeakLib(options);
		expect(result.success).toBe(false);
		expect(result.errors).toBeDefined();
		expect(result.errors?.some((error) => error.includes("File not found"))).toBe(true);
	});

	test("should process file-only successfully", async () => {
		const testContent = `# Test Document\n\n**Bold text** and *italic text*.\n\nSome regular content.`;
		await writeFile(testFile, testContent);

		const mockExecutor: ShellExecutor = {
			async tts(text: string): Promise<string> {
				// Verify markdown stripping occurred - note: stripMarkdownTTS converts newlines to periods for TTS
				expect(text).toBe("Test Document.. Bold text and italic text.. Some regular content.");
				return "/tmp/file-audio.wav";
			},
			async speak(audioFile: string): Promise<void> {
				expect(audioFile).toBe("/tmp/file-audio.wav");
			},
		};

		const options: CcSpeakOptions = {
			file: testFile,
			executor: mockExecutor,
		};

		const result = await ccSpeakLib(options);

		expect(result.success).toBe(true);
		expect(result.audioFiles).toHaveLength(1);
		expect(result.audioFiles[0]).toBe("/tmp/file-audio.wav");
	});

	test("should process both text and file in correct order", async () => {
		const testContent = `# File Content\n\nThis is from the file.`;
		await writeFile(testFile, testContent);

		const callOrder: string[] = [];

		const mockExecutor: ShellExecutor = {
			async tts(text: string): Promise<string> {
				if (text === "Text content first") {
					callOrder.push("tts-text");
					return "/tmp/text-audio.wav";
				} else {
					callOrder.push("tts-file");
					return "/tmp/file-audio.wav";
				}
			},
			async speak(audioFile: string): Promise<void> {
				if (audioFile === "/tmp/text-audio.wav") {
					callOrder.push("speak-text");
				} else {
					callOrder.push("speak-file");
				}
			},
		};

		const options: CcSpeakOptions = {
			text: "Text content first",
			file: testFile,
			executor: mockExecutor,
		};

		const result = await ccSpeakLib(options);

		expect(result.success).toBe(true);
		expect(result.audioFiles).toHaveLength(2);

		// Verify correct order: text processed and played first, then file
		expect(callOrder).toEqual(["tts-text", "speak-text", "tts-file", "speak-file"]);
	});

	test("should handle file with line range", async () => {
		const testContent = `Line 1\nLine 2\nLine 3\nLine 4\nLine 5`;
		await writeFile(testFile, testContent);

		let processedText = "";
		const mockExecutor: ShellExecutor = {
			async tts(text: string): Promise<string> {
				processedText = text;
				return "/tmp/range-audio.wav";
			},
			async speak(): Promise<void> {},
		};

		const options: CcSpeakOptions = {
			file: testFile,
			startLine: 2,
			endLine: 4,
			executor: mockExecutor,
		};

		const result = await ccSpeakLib(options);

		expect(result.success).toBe(true);
		// stripMarkdownTTS converts newlines to periods for better TTS
		expect(processedText).toBe("Line 2. Line 3. Line 4");
	});

	test("should handle tts command failure", async () => {
		const mockExecutor: ShellExecutor = {
			async tts(): Promise<string> {
				throw new Error("TTS failed");
			},
			async speak(): Promise<void> {},
		};

		const options: CcSpeakOptions = {
			text: "This should fail",
			executor: mockExecutor,
		};

		const result = await ccSpeakLib(options);

		expect(result.success).toBe(false);
		expect(result.errors).toBeDefined();
		expect(result.errors?.some((error) => error.includes("Text processing failed"))).toBe(true);
	});

	test("should handle speak command failure", async () => {
		const mockExecutor: ShellExecutor = {
			async tts(): Promise<string> {
				return "/tmp/audio.wav";
			},
			async speak(): Promise<void> {
				throw new Error("Speaker not available");
			},
		};

		const options: CcSpeakOptions = {
			text: "Test text",
			executor: mockExecutor,
		};

		const result = await ccSpeakLib(options);

		expect(result.success).toBe(false);
		expect(result.errors).toBeDefined();
		expect(result.errors?.some((error) => error.includes("Audio playback failed"))).toBe(true);
	});

	test("should handle only start line", async () => {
		const testContent = `Line 1\nLine 2\nLine 3\nLine 4\nLine 5`;
		await writeFile(testFile, testContent);

		let processedText = "";
		const mockExecutor: ShellExecutor = {
			async tts(text: string): Promise<string> {
				processedText = text;
				return "/tmp/audio.wav";
			},
			async speak(): Promise<void> {},
		};

		const options: CcSpeakOptions = {
			file: testFile,
			startLine: 3,
			executor: mockExecutor,
		};

		const result = await ccSpeakLib(options);

		expect(result.success).toBe(true);
		// stripMarkdownTTS converts newlines to periods for better TTS
		expect(processedText).toBe("Line 3. Line 4. Line 5");
	});

	test("should handle only end line", async () => {
		const testContent = `Line 1\nLine 2\nLine 3\nLine 4\nLine 5`;
		await writeFile(testFile, testContent);

		let processedText = "";
		const mockExecutor: ShellExecutor = {
			async tts(text: string): Promise<string> {
				processedText = text;
				return "/tmp/audio.wav";
			},
			async speak(): Promise<void> {},
		};

		const options: CcSpeakOptions = {
			file: testFile,
			endLine: 3,
			executor: mockExecutor,
		};

		const result = await ccSpeakLib(options);

		expect(result.success).toBe(true);
		// stripMarkdownTTS converts newlines to periods for better TTS
		expect(processedText).toBe("Line 1. Line 2. Line 3");
	});

	test("should strip markdown from text input", async () => {
		let capturedText = "";
		const mockExecutor: ShellExecutor = {
			async tts(text: string): Promise<string> {
				capturedText = text;
				return "/tmp/audio.wav";
			},
			async speak(): Promise<void> {},
		};

		const options: CcSpeakOptions = {
			text: "# Header\n\n**Bold** and *italic* and `code` text.",
			executor: mockExecutor,
		};

		const result = await ccSpeakLib(options);

		expect(result.success).toBe(true);
		// stripMarkdownTTS converts double newlines to periods for better TTS
		expect(capturedText).toBe("Header.. Bold and italic and code text.");
	});

	test("should process complex markdown file", async () => {
		const testContent = `# Main Title

## Subsection

- List item 1
- **Bold list item**
- *Italic list item*

\`\`\`javascript
const code = "example";
\`\`\`

Some [link text](https://example.com) here.

> Blockquote text

Final paragraph.`;

		await writeFile(testFile, testContent);

		let processedText = "";
		const mockExecutor: ShellExecutor = {
			async tts(text: string): Promise<string> {
				processedText = text;
				return "/tmp/audio.wav";
			},
			async speak(): Promise<void> {},
		};

		const options: CcSpeakOptions = {
			file: testFile,
			executor: mockExecutor,
		};

		const result = await ccSpeakLib(options);

		expect(result.success).toBe(true);
		// Verify markdown stripping occurred
		expect(processedText).toContain("Main Title");
		expect(processedText).toContain("Bold list item");
		expect(processedText).not.toContain("**Bold");
		expect(processedText).not.toContain("```");
	});
});
