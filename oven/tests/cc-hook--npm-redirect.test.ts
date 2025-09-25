import {afterEach, beforeEach, describe, expect, test} from "bun:test";
import {existsSync, rmSync} from "fs";
import {mkdir, writeFile} from "fs/promises";
import {join} from "path";
import {ccHookNpmRedirectLib} from "../bin/cc-hook--npm-redirect";

describe("ccHookNpmRedirectLib", () => {
	const testProjectRoot = "/tmp/test-npm-redirect";

	beforeEach(() => {
		// Clean up any existing test files
		if (existsSync(testProjectRoot)) {
			rmSync(testProjectRoot, {recursive: true, force: true});
		}
	});

	afterEach(() => {
		// Clean up test files
		if (existsSync(testProjectRoot)) {
			rmSync(testProjectRoot, {recursive: true, force: true});
		}
	});

	describe("package manager detection", () => {
		test.each([
			{
				lockFile: "bun.lock",
				expectedManager: "bun",
				command: "npm install lodash",
				expectedSuggestion: "bun install lodash",
				shouldBlock: true,
			},
			{
				lockFile: "bun.lockb",
				expectedManager: "bun",
				command: "npm install lodash",
				expectedSuggestion: "bun install lodash",
				shouldBlock: true,
			},
			{
				lockFile: "pnpm-lock.yaml",
				expectedManager: "pnpm",
				command: "npm run build",
				expectedSuggestion: "pnpm run build",
				shouldBlock: true,
			},
			{
				lockFile: "yarn.lock",
				expectedManager: "yarn",
				command: "npm test",
				expectedSuggestion: "yarn test",
				shouldBlock: true,
			},
			{
				lockFile: "package-lock.json",
				lockFileContent: "{}",
				expectedManager: "npm",
				command: "npm install",
				expectedSuggestion: null,
				shouldBlock: false,
			},
		])(
			"should detect $expectedManager project from $lockFile",
			async ({lockFile, lockFileContent = "", expectedManager, command, expectedSuggestion, shouldBlock}) => {
				await mkdir(testProjectRoot, {recursive: true});
				await Promise.all([
					writeFile(join(testProjectRoot, lockFile), lockFileContent),
					writeFile(join(testProjectRoot, "package.json"), "{}"),
				]);

				const result = await ccHookNpmRedirectLib({
					command,
					cwd: testProjectRoot,
				});

				expect(result.detectedPackageManager).toBe(expectedManager);
				expect(result.shouldBlock).toBe(shouldBlock);
				if (shouldBlock) {
					expect(result.blockedCommand).toBe(command);
					expect(result.suggestedCommand).toBe(expectedSuggestion);
				} else {
					expect(result.blockedCommand).toBeNull();
					expect(result.suggestedCommand).toBeNull();
				}
			},
		);

		test("should prioritize bun.lock over bun.lockb", async () => {
			await mkdir(testProjectRoot, {recursive: true});
			await Promise.all([
				writeFile(join(testProjectRoot, "bun.lock"), ""),
				writeFile(join(testProjectRoot, "bun.lockb"), ""),
				writeFile(join(testProjectRoot, "package.json"), "{}"),
			]);

			const result = await ccHookNpmRedirectLib({
				command: "npm install lodash",
				cwd: testProjectRoot,
			});

			expect(result.detectedPackageManager).toBe("bun");
			expect(result.shouldBlock).toBe(true);
		});

		test("should default to npm when no lock files found", async () => {
			await mkdir(testProjectRoot, {recursive: true});
			await writeFile(join(testProjectRoot, "package.json"), "{}");

			const result = await ccHookNpmRedirectLib({
				command: "npm install",
				cwd: testProjectRoot,
			});

			expect(result.detectedPackageManager).toBe("npm");
			expect(result.shouldBlock).toBe(false);
		});

		test("should prioritize pnpm over other package managers", async () => {
			await mkdir(testProjectRoot, {recursive: true});
			await Promise.all([
				writeFile(join(testProjectRoot, "pnpm-lock.yaml"), ""),
				writeFile(join(testProjectRoot, "yarn.lock"), ""),
				writeFile(join(testProjectRoot, "package-lock.json"), "{}"),
				writeFile(join(testProjectRoot, "package.json"), "{}"),
			]);

			const result = await ccHookNpmRedirectLib({
				command: "npm install",
				cwd: testProjectRoot,
			});

			expect(result.detectedPackageManager).toBe("pnpm");
		});
	});

	describe("npx command redirection", () => {
		test.each([
			{
				lockFile: "bun.lockb",
				lockFileContent: "",
				command: "npx eslint .",
				expectedSuggestion: "bunx eslint .",
				shouldBlock: true,
				projectType: "bun",
			},
			{
				lockFile: "pnpm-lock.yaml",
				lockFileContent: "",
				command: "npx prettier --write .",
				expectedSuggestion: "pnpm exec prettier --write .",
				shouldBlock: true,
				projectType: "pnpm",
			},
			{
				lockFile: "yarn.lock",
				lockFileContent: "",
				command: "npx create-react-app my-app",
				expectedSuggestion: "yarn dlx create-react-app my-app",
				shouldBlock: true,
				projectType: "yarn",
			},
			{
				lockFile: "package-lock.json",
				lockFileContent: "{}",
				command: "npx eslint .",
				expectedSuggestion: null,
				shouldBlock: false,
				projectType: "npm",
			},
		])(
			"should redirect npx commands in $projectType project",
			async ({lockFile, lockFileContent, command, expectedSuggestion, shouldBlock}) => {
				await mkdir(testProjectRoot, {recursive: true});
				await writeFile(join(testProjectRoot, lockFile), lockFileContent);

				const result = await ccHookNpmRedirectLib({
					command,
					cwd: testProjectRoot,
				});

				expect(result.shouldBlock).toBe(shouldBlock);
				if (shouldBlock) {
					expect(result.blockedCommand).toBe(command);
					expect(result.suggestedCommand).toBe(expectedSuggestion);
				}
			},
		);
	});

	describe("cross-package-manager command redirection", () => {
		test.each([
			{
				lockFile: "bun.lockb",
				lockFileContent: "",
				command: "yarn add lodash",
				expectedSuggestion: "bun add lodash",
				sourceManager: "yarn",
				targetManager: "bun",
			},
			{
				lockFile: "pnpm-lock.yaml",
				lockFileContent: "",
				command: "yarn build",
				expectedSuggestion: "pnpm build",
				sourceManager: "yarn",
				targetManager: "pnpm",
			},
			{
				lockFile: "bun.lockb",
				lockFileContent: "",
				command: "pnpm install",
				expectedSuggestion: "bun install",
				sourceManager: "pnpm",
				targetManager: "bun",
			},
			{
				lockFile: "yarn.lock",
				lockFileContent: "",
				command: "pnpm test",
				expectedSuggestion: "yarn test",
				sourceManager: "pnpm",
				targetManager: "yarn",
			},
		])(
			"should redirect $sourceManager to $targetManager in $targetManager project",
			async ({lockFile, lockFileContent, command, expectedSuggestion}) => {
				await mkdir(testProjectRoot, {recursive: true});
				await writeFile(join(testProjectRoot, lockFile), lockFileContent);

				const result = await ccHookNpmRedirectLib({
					command,
					cwd: testProjectRoot,
				});

				expect(result.shouldBlock).toBe(true);
				expect(result.suggestedCommand).toBe(expectedSuggestion);
			},
		);
	});

	describe("node command redirection", () => {
		test.each([
			{
				lockFile: "bun.lock",
				command: "node script.js",
				expectedSuggestion: "bun script.js",
				testDescription: "basic node command",
				shouldHaveSpecificWarning: true,
			},
			{
				lockFile: "bun.lockb",
				command: "node --inspect script.js",
				expectedSuggestion: "bun --inspect script.js",
				testDescription: "node command with flags",
				shouldHaveSpecificWarning: false,
			},
		])(
			"should redirect $testDescription to bun in bun project with warning",
			async ({lockFile, command, expectedSuggestion, shouldHaveSpecificWarning}) => {
				await mkdir(testProjectRoot, {recursive: true});
				await writeFile(join(testProjectRoot, lockFile), "");

				const result = await ccHookNpmRedirectLib({
					command,
					cwd: testProjectRoot,
				});

				expect(result.shouldBlock).toBe(true);
				expect(result.blockedCommand).toBe(command);
				expect(result.suggestedCommand).toBe(expectedSuggestion);
				expect(result.detectedPackageManager).toBe("bun");
				expect(result.warning).toBeDefined();
				if (shouldHaveSpecificWarning) {
					expect(result.warning).toContain("Node.js and Bun have different CLI flags");
				}
			},
		);

		test.each([
			{
				lockFile: "package-lock.json",
				lockFileContent: "{}",
				expectedManager: "npm",
			},
			{
				lockFile: "pnpm-lock.yaml",
				lockFileContent: "",
				expectedManager: "pnpm",
			},
			{
				lockFile: "yarn.lock",
				lockFileContent: "",
				expectedManager: "yarn",
			},
		])(
			"should not redirect node in $expectedManager project",
			async ({lockFile, lockFileContent, expectedManager}) => {
				await mkdir(testProjectRoot, {recursive: true});
				await writeFile(join(testProjectRoot, lockFile), lockFileContent);

				const result = await ccHookNpmRedirectLib({
					command: "node script.js",
					cwd: testProjectRoot,
				});

				expect(result.shouldBlock).toBe(false);
				expect(result.detectedPackageManager).toBe(expectedManager);
			},
		);

		test("should handle complex node commands", async () => {
			await mkdir(testProjectRoot, {recursive: true});
			await writeFile(join(testProjectRoot, "bun.lock"), "");

			const result = await ccHookNpmRedirectLib({
				command: "cd src && node --max-old-space-size=4096 build.js --prod",
				cwd: testProjectRoot,
			});

			expect(result.shouldBlock).toBe(true);
			expect(result.suggestedCommand).toBe("cd src && bun --max-old-space-size=4096 build.js --prod");
		});

		test("should not redirect node inside quoted strings", async () => {
			await mkdir(testProjectRoot, {recursive: true});
			await writeFile(join(testProjectRoot, "bun.lock"), "");

			const result = await ccHookNpmRedirectLib({
				command: 'echo "use node script.js"',
				cwd: testProjectRoot,
			});

			expect(result.shouldBlock).toBe(false);
		});
	});

	describe("directory traversal", () => {
		test("should detect package manager from parent directories", async () => {
			const nestedDir = join(testProjectRoot, "src", "components");
			await mkdir(nestedDir, {recursive: true});
			await writeFile(join(testProjectRoot, "bun.lockb"), "");

			const result = await ccHookNpmRedirectLib({
				command: "npm install",
				cwd: nestedDir,
			});

			expect(result.detectedPackageManager).toBe("bun");
			expect(result.shouldBlock).toBe(true);
		});

		test("should handle cd command in command string", async () => {
			await mkdir(testProjectRoot, {recursive: true});
			await writeFile(join(testProjectRoot, "pnpm-lock.yaml"), "");

			const commandToTest = `cd ${testProjectRoot} && npm install`;
			const expectedSuggestion = `cd ${testProjectRoot} && pnpm install`;

			const result = await ccHookNpmRedirectLib({
				command: commandToTest,
				// Don't pass cwd so it uses the cd parsing logic
			});

			expect(result.detectedPackageManager).toBe("pnpm");
			expect(result.shouldBlock).toBe(true);
			expect(result.suggestedCommand).toBe(expectedSuggestion);
		});
	});

	describe("commands that should not be redirected", () => {
		test("should not redirect non-package-manager commands", async () => {
			await mkdir(testProjectRoot, {recursive: true});
			await writeFile(join(testProjectRoot, "bun.lockb"), "");

			const result = await ccHookNpmRedirectLib({
				command: "git status",
				cwd: testProjectRoot,
			});

			expect(result.shouldBlock).toBe(false);
		});

		test("should not redirect commands that contain package manager names but are not package manager commands", async () => {
			await mkdir(testProjectRoot, {recursive: true});
			await writeFile(join(testProjectRoot, "bun.lockb"), "");

			const result = await ccHookNpmRedirectLib({
				command: "echo 'npm is great'",
				cwd: testProjectRoot,
			});

			expect(result.shouldBlock).toBe(false);
		});

		test("should handle empty commands gracefully", async () => {
			const result = await ccHookNpmRedirectLib({
				command: "",
				cwd: testProjectRoot,
			});

			expect(result.shouldBlock).toBe(false);
		});
	});

	describe("edge cases", () => {
		test("should handle missing cwd parameter", async () => {
			// This should use process.cwd() - in our case, the oven directory has bun.lock
			const result = await ccHookNpmRedirectLib({
				command: "npm install",
			});

			expect(result.detectedPackageManager).toBe("bun");
			expect(result.shouldBlock).toBe(true);
		});

		test("should handle relative paths in cd commands", async () => {
			const subDir = join(testProjectRoot, "subdir");
			await mkdir(subDir, {recursive: true});
			await writeFile(join(testProjectRoot, "yarn.lock"), "");

			// Mock process.cwd() to return testProjectRoot
			const originalCwd = process.cwd;
			process.cwd = () => testProjectRoot;

			try {
				const result = await ccHookNpmRedirectLib({
					command: "cd subdir && npm test",
				});

				expect(result.detectedPackageManager).toBe("yarn");
				expect(result.shouldBlock).toBe(true);
			} finally {
				process.cwd = originalCwd;
			}
		});
	});

	describe("return type validation", () => {
		test("should return proper NpmRedirectResult interface", async () => {
			await mkdir(testProjectRoot, {recursive: true});
			await writeFile(join(testProjectRoot, "bun.lockb"), "");

			const result = await ccHookNpmRedirectLib({
				command: "npm install",
				cwd: testProjectRoot,
			});

			// Verify the result has all expected properties
			expect(result).toHaveProperty("shouldBlock");
			expect(result).toHaveProperty("blockedCommand");
			expect(result).toHaveProperty("suggestedCommand");
			expect(result).toHaveProperty("detectedPackageManager");

			expect(typeof result.shouldBlock).toBe("boolean");
			expect(typeof result.detectedPackageManager).toBe("string");
		});
	});
});
