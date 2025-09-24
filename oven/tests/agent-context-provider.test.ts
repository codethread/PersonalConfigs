import {afterEach, beforeEach, describe, expect, test} from "bun:test";
import {existsSync, mkdirSync, rmSync, unlinkSync, writeFileSync} from "fs";
import {join} from "path";
import {agentContextProviderLib} from "../bin/agent-context-provider";

describe("agentContextProviderLib", () => {
	const testSessionId = "test-session-123";
	const testProjectRoot = "/tmp/test-project-agent-context";
	const sessionFile = join("/tmp", `claude-agent-context-${testSessionId}.json`);

	beforeEach(() => {
		// Clean up any existing test files
		if (existsSync(sessionFile)) {
			unlinkSync(sessionFile);
		}
		if (existsSync(testProjectRoot)) {
			rmSync(testProjectRoot, {recursive: true, force: true});
		}

		// Create test project structure
		mkdirSync(testProjectRoot, {recursive: true});
		mkdirSync(join(testProjectRoot, "src"), {recursive: true});
		mkdirSync(join(testProjectRoot, "src", "components"), {recursive: true});
		mkdirSync(join(testProjectRoot, "docs"), {recursive: true});

		// Create AGENTS.md files at different levels
		writeFileSync(
			join(testProjectRoot, "AGENTS.md"),
			"# Root Agent Documentation\nRoot level guidance.",
		);
		writeFileSync(
			join(testProjectRoot, "src", "AGENTS.md"),
			"# Src Agent Documentation\nSource code guidance.",
		);
		writeFileSync(
			join(testProjectRoot, "src", "components", "AGENTS.md"),
			"# Components Agent Documentation\nComponent specific guidance.",
		);

		// Create some test files
		writeFileSync(join(testProjectRoot, "README.md"), "# Test Project");
		writeFileSync(join(testProjectRoot, "src", "index.ts"), "export * from './components';");
		writeFileSync(
			join(testProjectRoot, "src", "components", "Button.tsx"),
			"export function Button() {}",
		);
	});

	afterEach(() => {
		// Clean up test files
		if (existsSync(sessionFile)) {
			unlinkSync(sessionFile);
		}
		if (existsSync(testProjectRoot)) {
			rmSync(testProjectRoot, {recursive: true, force: true});
		}
	});

	describe("session-start", () => {
		test("should create session state file", async () => {
			const result = await agentContextProviderLib({
				command: "session-start",
				sessionId: testSessionId,
				projectRoot: testProjectRoot,
			});

			expect(result.success).toBe(true);
			expect(result.message).toContain(testSessionId);
			expect(existsSync(sessionFile)).toBe(true);

			const sessionData = await Bun.file(sessionFile).json();
			expect(sessionData.sessionId).toBe(testSessionId);
			expect(sessionData.projectRoot).toBe(testProjectRoot);
			expect(sessionData.seenAgentsFiles).toEqual([]);
		});

		test("should require session ID", async () => {
			await expect(
				agentContextProviderLib({
					command: "session-start",
					projectRoot: testProjectRoot,
				}),
			).rejects.toThrow("Session ID is required");
		});

		test("should require project root", async () => {
			await expect(
				agentContextProviderLib({
					command: "session-start",
					sessionId: testSessionId,
				}),
			).rejects.toThrow("Project root is required");
		});
	});

	describe("session-end", () => {
		test("should clean up session state file", async () => {
			// First create a session
			await agentContextProviderLib({
				command: "session-start",
				sessionId: testSessionId,
				projectRoot: testProjectRoot,
			});

			expect(existsSync(sessionFile)).toBe(true);

			// Then end it
			const result = await agentContextProviderLib({
				command: "session-end",
				sessionId: testSessionId,
			});

			expect(result.success).toBe(true);
			expect(result.message).toContain(testSessionId);
			expect(existsSync(sessionFile)).toBe(false);
		});

		test("should handle missing session file gracefully", async () => {
			const result = await agentContextProviderLib({
				command: "session-end",
				sessionId: testSessionId,
			});

			expect(result.success).toBe(true);
			expect(result.message).toContain(testSessionId);
		});

		test("should require session ID", async () => {
			await expect(
				agentContextProviderLib({
					command: "session-end",
				}),
			).rejects.toThrow("Session ID is required");
		});
	});

	describe("read", () => {
		beforeEach(async () => {
			// Initialize session for read tests
			await agentContextProviderLib({
				command: "session-start",
				sessionId: testSessionId,
				projectRoot: testProjectRoot,
			});
		});

		test("should find and return AGENTS.md files for deeply nested file", async () => {
			// Mock stderr and process.exit to capture output
			const originalError = console.error;
			const originalExit = process.exit;
			let output = "";
			let exitCode: number | undefined;

			console.error = (message: string) => {
				output += `${message}\n`;
			};
			process.exit = ((code?: number) => {
				exitCode = code;
				throw new Error(`Process.exit(${code})`);
			}) as never;

			try {
				await agentContextProviderLib({
					command: "read",
					sessionId: testSessionId,
					filePath: join(testProjectRoot, "src", "components", "Button.tsx"),
				});
			} catch (e) {
				// Expected to throw due to process.exit
			}

			console.error = originalError;
			process.exit = originalExit;

			expect(exitCode).toBe(2);
			expect(output).toMatchSnapshot();
		});

		test("should find AGENTS.md files for file in src directory", async () => {
			const originalError = console.error;
			const originalExit = process.exit;
			let output = "";
			let exitCode: number | undefined;

			console.error = (message: string) => {
				output += message;
			};
			process.exit = ((code?: number) => {
				exitCode = code;
				throw new Error(`Process.exit(${code})`);
			}) as never;

			try {
				await agentContextProviderLib({
					command: "read",
					sessionId: testSessionId,
					filePath: join(testProjectRoot, "src", "index.ts"),
				});
			} catch (e) {
				// Expected to throw due to process.exit
			}

			console.error = originalError;
			process.exit = originalExit;

			expect(exitCode).toBe(2);
			expect(output).toMatchSnapshot();
		});

		test("should find only root AGENTS.md for file in project root", async () => {
			const originalError = console.error;
			const originalExit = process.exit;
			let output = "";
			let exitCode: number | undefined;

			console.error = (message: string) => {
				output += message;
			};
			process.exit = ((code?: number) => {
				exitCode = code;
				throw new Error(`Process.exit(${code})`);
			}) as never;

			try {
				await agentContextProviderLib({
					command: "read",
					sessionId: testSessionId,
					filePath: join(testProjectRoot, "README.md"),
				});
			} catch (e) {
				// Expected to throw due to process.exit
			}

			console.error = originalError;
			process.exit = originalExit;

			expect(exitCode).toBe(2);
			expect(output).toMatchSnapshot();
		});

		test("should not return same AGENTS.md files twice", async () => {
			const originalError = console.error;
			const originalExit = process.exit;
			let output = "";
			let exitCode: number | undefined;

			console.error = (message: string) => {
				output += message;
			};
			process.exit = ((code?: number) => {
				exitCode = code;
				throw new Error(`Process.exit(${code})`);
			}) as never;

			// First read
			try {
				await agentContextProviderLib({
					command: "read",
					sessionId: testSessionId,
					filePath: join(testProjectRoot, "src", "components", "Button.tsx"),
				});
			} catch (e) {
				// Expected to throw due to process.exit
			}

			expect(exitCode).toBe(2);
			expect(output).toMatchSnapshot();
			const firstOutput = output;
			output = ""; // Reset output capture
			exitCode = undefined;

			// Second read of the same file - should NOT exit(2) since no new files
			const result2 = await agentContextProviderLib({
				command: "read",
				sessionId: testSessionId,
				filePath: join(testProjectRoot, "src", "components", "Button.tsx"),
			});

			console.error = originalError;
			process.exit = originalExit;

			expect(result2.success).toBe(true);
			expect(result2.agentsFound).toBe(0); // No new files found
			expect(exitCode).toBeUndefined(); // Should not have called exit
			expect(output.trim()).toBe("");
		});

		test("should track seen files correctly across different file reads", async () => {
			const originalError = console.error;
			const originalExit = process.exit;
			let output = "";
			let exitCode: number | undefined;

			console.error = (message: string) => {
				output += message;
			};
			process.exit = ((code?: number) => {
				exitCode = code;
				throw new Error(`Process.exit(${code})`);
			}) as never;

			// Read a deeply nested file first
			try {
				await agentContextProviderLib({
					command: "read",
					sessionId: testSessionId,
					filePath: join(testProjectRoot, "src", "components", "Button.tsx"),
				});
			} catch (e) {
				// Expected to throw due to process.exit
			}

			expect(exitCode).toBe(2);
			output = ""; // Reset
			exitCode = undefined;

			// Read a file in src - should not show root or src AGENTS.md again
			const result2 = await agentContextProviderLib({
				command: "read",
				sessionId: testSessionId,
				filePath: join(testProjectRoot, "src", "index.ts"),
			});

			console.error = originalError;
			process.exit = originalExit;

			expect(result2.agentsFound).toBe(0); // All already seen
			expect(output.trim()).toBe(""); // No new output
			expect(exitCode).toBeUndefined(); // Should not have called exit
		});

		test("should handle session not initialized", async () => {
			// Clean up session to simulate uninitialized state
			if (existsSync(sessionFile)) {
				unlinkSync(sessionFile);
			}

			const result = await agentContextProviderLib({
				command: "read",
				sessionId: testSessionId,
				filePath: join(testProjectRoot, "src", "components", "Button.tsx"),
			});

			expect(result.success).toBe(true);
			expect(result.agentsFound).toBe(0);
		});

		test("should handle files outside project root", async () => {
			const result = await agentContextProviderLib({
				command: "read",
				sessionId: testSessionId,
				filePath: "/tmp/outside-project/file.txt",
			});

			expect(result.success).toBe(true);
			expect(result.agentsFound).toBe(0);
		});

		test("should require session ID", async () => {
			await expect(
				agentContextProviderLib({
					command: "read",
					filePath: join(testProjectRoot, "src", "index.ts"),
				}),
			).rejects.toThrow("Session ID is required");
		});

		test("should require file path", async () => {
			await expect(
				agentContextProviderLib({
					command: "read",
					sessionId: testSessionId,
				}),
			).rejects.toThrow("File path is required");
		});
	});

	describe("invalid command", () => {
		test("should throw error for unknown command", async () => {
			await expect(
				agentContextProviderLib({
					command: "invalid" as any,
					sessionId: testSessionId,
				}),
			).rejects.toThrow("Unknown command: invalid");
		});
	});
});
