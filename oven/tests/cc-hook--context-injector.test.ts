import {afterEach, beforeEach, describe, expect, test} from "bun:test";
import {existsSync, mkdirSync, rmSync, unlinkSync, writeFileSync} from "fs";
import {join} from "path";
import {ccHookContextInjectorLib} from "../bin/cc-hook--context-injector";

describe("ccHookContextInjectorLib", () => {
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
		writeFileSync(join(testProjectRoot, "AGENTS.md"), "# Root Agent Documentation\nRoot level guidance.");
		writeFileSync(
			join(testProjectRoot, "src", "AGENTS.md"),
			"# Src Agent Documentation\nSource code guidance.",
		);
		writeFileSync(
			join(testProjectRoot, "src", "components", "AGENTS.md"),
			"# Components Agent Documentation\nComponent specific guidance.",
		);

		// Create some test files including additional README.md files
		writeFileSync(join(testProjectRoot, "README.md"), "# Test Project");
		writeFileSync(join(testProjectRoot, "src", "README.md"), "# Src Documentation");
		writeFileSync(join(testProjectRoot, "docs", "README.md"), "# Docs Section");
		writeFileSync(join(testProjectRoot, "src", "index.ts"), "export * from './components';");
		writeFileSync(join(testProjectRoot, "src", "components", "Button.tsx"), "export function Button() {}");
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
			const result = await ccHookContextInjectorLib({
				command: "session-start",
				sessionId: testSessionId,
				projectRoot: testProjectRoot,
			});

			expect(result.success).toBe(true);
			if ("message" in result) {
				expect(result.message).toContain(testSessionId);
			}

			if ("contextOutput" in result) {
				expect(result.contextOutput).toContain("Project documentation:");
			}
			expect(existsSync(sessionFile)).toBe(true);

			const sessionData = await Bun.file(sessionFile).json();
			expect(sessionData.sessionId).toBe(testSessionId);
			expect(sessionData.projectRoot).toBe(testProjectRoot);
		});

		test("should require session ID", async () => {
			await expect(
				ccHookContextInjectorLib({
					command: "session-start",
					projectRoot: testProjectRoot,
				}),
			).rejects.toThrow("Session ID is required");
		});

		test("should require project root", async () => {
			await expect(
				ccHookContextInjectorLib({
					command: "session-start",
					sessionId: testSessionId,
				}),
			).rejects.toThrow("Project root is required");
		});

		test("should include only README.md files in context output", async () => {
			const result = await ccHookContextInjectorLib({
				command: "session-start",
				sessionId: testSessionId,
				projectRoot: testProjectRoot,
			});

			expect(result.success).toBe(true);

			// Type guard: session-start returns SessionStartResult which has contextOutput
			if (!("contextOutput" in result)) {
				throw new Error("Expected contextOutput in result");
			}

			const contextOutput = result.contextOutput as string;
			expect(contextOutput).toContain("Project documentation:");
			expect(contextOutput).toContain("README.md");
			expect(contextOutput).toContain("src/README.md");
			expect(contextOutput).toContain("docs/README.md");
			// Should NOT contain AGENTS.md files (they are loaded automatically by Claude Code)
			expect(contextOutput).not.toContain("AGENTS.md");
			expect(contextOutput).not.toContain("src/AGENTS.md");
			expect(contextOutput).not.toContain("src/components/AGENTS.md");
		});
	});

	describe("session-end", () => {
		test("should clean up session state file", async () => {
			// First create a session
			await ccHookContextInjectorLib({
				command: "session-start",
				sessionId: testSessionId,
				projectRoot: testProjectRoot,
			});

			expect(existsSync(sessionFile)).toBe(true);

			// Then end it
			const result = await ccHookContextInjectorLib({
				command: "session-end",
				sessionId: testSessionId,
			});

			expect(result.success).toBe(true);
			if ("message" in result) {
				expect(result.message).toContain(testSessionId);
			}
			expect(existsSync(sessionFile)).toBe(false);
		});

		test("should handle missing session file gracefully", async () => {
			const result = await ccHookContextInjectorLib({
				command: "session-end",
				sessionId: testSessionId,
			});

			expect(result.success).toBe(true);
			if ("message" in result) {
				expect(result.message).toContain(testSessionId);
			}
		});

		test("should require session ID", async () => {
			await expect(
				ccHookContextInjectorLib({
					command: "session-end",
				}),
			).rejects.toThrow("Session ID is required");
		});
	});

	describe("invalid command", () => {
		test("should throw error for unknown command", async () => {
			await expect(
				ccHookContextInjectorLib({
					// @ts-expect-error - intentionally passing invalid type to test error handling
					command: "invalid",
					sessionId: testSessionId,
				}),
			).rejects.toThrow("Unknown command: invalid");
		});
	});
});
