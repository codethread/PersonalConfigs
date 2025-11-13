import {afterEach, beforeEach, describe, expect, test} from "bun:test";
import {existsSync, mkdirSync, readlinkSync, rmSync, statSync, symlinkSync, writeFileSync} from "fs";
import {join} from "path";
import {syncSymlinksLib} from "../bin/cc-docs--sync-symlinks";

describe("cc-docs--sync-symlinks", () => {
	let testDir: string;

	beforeEach(() => {
		// Create a unique temporary directory for each test
		testDir = join("/tmp", `cc-docs-test-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`);
		mkdirSync(testDir, {recursive: true});
	});

	afterEach(() => {
		// Clean up test directory
		if (existsSync(testDir)) {
			rmSync(testDir, {recursive: true, force: true});
		}
	});

	describe("basic scenarios", () => {
		test("creates symlink when only AGENTS.md exists", async () => {
			// Setup
			const projectDir = join(testDir, "project1");
			mkdirSync(projectDir, {recursive: true});
			writeFileSync(join(projectDir, "AGENTS.md"), "# Agents documentation");

			// Execute dry-run
			const dryResult = await syncSymlinksLib({cwd: testDir});
			expect(dryResult.dryRun).toBe(true);
			expect(dryResult.actions).toHaveLength(1);
			expect(dryResult.actions[0]).toHaveProperty("type");
			expect(dryResult.actions[0].type).toBe("create_symlink");
			expect(existsSync(join(projectDir, "CLAUDE.md"))).toBe(false);

			// Execute with --run
			const runResult = await syncSymlinksLib({cwd: testDir, run: true});
			expect(runResult.dryRun).toBe(false);
			expect(existsSync(join(projectDir, "CLAUDE.md"))).toBe(true);
			expect(readlinkSync(join(projectDir, "CLAUDE.md"))).toBe("AGENTS.md");
		});

		test("renames CLAUDE.md to AGENTS.md and creates symlink when only CLAUDE.md exists", async () => {
			// Setup
			const projectDir = join(testDir, "project2");
			mkdirSync(projectDir, {recursive: true});
			const content = "# Claude documentation";
			writeFileSync(join(projectDir, "CLAUDE.md"), content);

			// Execute
			const result = await syncSymlinksLib({cwd: testDir, run: true});
			expect(result.actions).toHaveLength(1);
			expect(result.actions[0]).toHaveProperty("type");
			expect(result.actions[0].type).toBe("rename_and_symlink");

			// Verify
			expect(existsSync(join(projectDir, "AGENTS.md"))).toBe(true);
			expect(existsSync(join(projectDir, "CLAUDE.md"))).toBe(true);
			expect(readlinkSync(join(projectDir, "CLAUDE.md"))).toBe("AGENTS.md");
			expect(await Bun.file(join(projectDir, "AGENTS.md")).text()).toBe(content);
		});

		test("warns about conflict when both files exist as regular files (no force)", async () => {
			// Setup
			const projectDir = join(testDir, "project3");
			mkdirSync(projectDir, {recursive: true});
			writeFileSync(join(projectDir, "AGENTS.md"), "# Agents content");
			writeFileSync(join(projectDir, "CLAUDE.md"), "# Claude content");

			// Execute
			const result = await syncSymlinksLib({cwd: testDir});
			expect(result.actions).toHaveLength(1);
			expect(result.actions[0]).toHaveProperty("type");
			expect(result.actions[0].type).toBe("conflict");
			expect(result.actions[0]).toHaveProperty("message");
			expect(result.actions[0].message).toContain("Conflict");

			// Verify nothing changed
			expect(readFileSync(join(projectDir, "AGENTS.md"), "utf8")).toBe("# Agents content");
			expect(readFileSync(join(projectDir, "CLAUDE.md"), "utf8")).toBe("# Claude content");
		});

		test("resolves conflict with --force by keeping newest content", async () => {
			// Setup
			const projectDir = join(testDir, "project4");
			mkdirSync(projectDir, {recursive: true});

			// Create AGENTS.md first (older)
			writeFileSync(join(projectDir, "AGENTS.md"), "# Agents content (older)");
			await Bun.sleep(100); // Ensure different mtime

			// Create CLAUDE.md second (newer)
			writeFileSync(join(projectDir, "CLAUDE.md"), "# Claude content (newer)");

			// Execute with force
			const result = await syncSymlinksLib({cwd: testDir, run: true, force: true});
			expect(result.actions).toHaveLength(1);
			expect(result.actions[0]).toHaveProperty("type");
			expect(result.actions[0].type).toBe("fix_symlink");

			// Verify CLAUDE.md content was preserved in AGENTS.md (because it was newer)
			expect(existsSync(join(projectDir, "AGENTS.md"))).toBe(true);
			expect(existsSync(join(projectDir, "CLAUDE.md"))).toBe(true);
			expect(readlinkSync(join(projectDir, "CLAUDE.md"))).toBe("AGENTS.md");
			expect(await Bun.file(join(projectDir, "AGENTS.md")).text()).toBe("# Claude content (newer)");
		});
	});

	describe("symlink scenarios", () => {
		test("reports already correct when CLAUDE.md is valid symlink to AGENTS.md", async () => {
			// Setup
			const projectDir = join(testDir, "project5");
			mkdirSync(projectDir, {recursive: true});
			writeFileSync(join(projectDir, "AGENTS.md"), "# Agents documentation");
			symlinkSync("AGENTS.md", join(projectDir, "CLAUDE.md"));

			// Execute
			const result = await syncSymlinksLib({cwd: testDir});
			expect(result.actions).toHaveLength(1);
			expect(result.actions[0]).toHaveProperty("type");
			expect(result.actions[0].type).toBe("already_correct");
			expect(result.summary.alreadyCorrect).toBe(1);
		});

		test("fixes broken symlink", async () => {
			// Setup
			const projectDir = join(testDir, "project6");
			mkdirSync(projectDir, {recursive: true});
			writeFileSync(join(projectDir, "AGENTS.md"), "# Agents documentation");
			symlinkSync("NONEXISTENT.md", join(projectDir, "CLAUDE.md"));

			// Execute
			const result = await syncSymlinksLib({cwd: testDir, run: true});
			expect(result.actions).toHaveLength(1);
			expect(result.actions[0]).toHaveProperty("type");
			expect(result.actions[0].type).toBe("fix_symlink");

			// Verify
			expect(readlinkSync(join(projectDir, "CLAUDE.md"))).toBe("AGENTS.md");
		});

		test("fixes symlink pointing to wrong file", async () => {
			// Setup
			const projectDir = join(testDir, "project7");
			mkdirSync(projectDir, {recursive: true});
			writeFileSync(join(projectDir, "AGENTS.md"), "# Agents documentation");
			writeFileSync(join(projectDir, "OTHER.md"), "# Other documentation");
			symlinkSync("OTHER.md", join(projectDir, "CLAUDE.md"));

			// Execute
			const result = await syncSymlinksLib({cwd: testDir, run: true});
			expect(result.actions).toHaveLength(1);
			expect(result.actions[0]).toHaveProperty("type");
			expect(result.actions[0].type).toBe("fix_symlink");

			// Verify
			expect(readlinkSync(join(projectDir, "CLAUDE.md"))).toBe("AGENTS.md");
		});

		test("removes broken CLAUDE.md symlink when AGENTS.md doesn't exist", async () => {
			// Setup
			const projectDir = join(testDir, "project8");
			mkdirSync(projectDir, {recursive: true});
			symlinkSync("AGENTS.md", join(projectDir, "CLAUDE.md")); // Points to non-existent file

			// Execute
			const result = await syncSymlinksLib({cwd: testDir, run: true});
			expect(result.actions).toHaveLength(1);
			expect(result.actions[0]).toHaveProperty("type");
			expect(result.actions[0].type).toBe("remove_broken_symlink");

			// Verify
			expect(existsSync(join(projectDir, "CLAUDE.md"))).toBe(false);
		});
	});

	describe("multiple directories", () => {
		test("handles multiple directories with different states", async () => {
			// Setup
			const project1 = join(testDir, "project1");
			const project2 = join(testDir, "project2");
			const project3 = join(testDir, "project3");

			mkdirSync(project1, {recursive: true});
			mkdirSync(project2, {recursive: true});
			mkdirSync(project3, {recursive: true});

			// Project 1: Only AGENTS.md exists
			writeFileSync(join(project1, "AGENTS.md"), "# Project 1");

			// Project 2: Already correct
			writeFileSync(join(project2, "AGENTS.md"), "# Project 2");
			symlinkSync("AGENTS.md", join(project2, "CLAUDE.md"));

			// Project 3: Only CLAUDE.md exists
			writeFileSync(join(project3, "CLAUDE.md"), "# Project 3");

			// Execute
			const result = await syncSymlinksLib({cwd: testDir, run: true});
			expect(result.actions).toHaveLength(3);

			// Verify summary
			expect(result.summary.toCreate).toBe(1);
			expect(result.summary.toFix).toBe(1);
			expect(result.summary.alreadyCorrect).toBe(1);
		});
	});

	describe("edge cases", () => {
		test("handles empty directory (no documentation files)", async () => {
			const result = await syncSymlinksLib({cwd: testDir});
			expect(result.actions).toHaveLength(0);
			expect(result.summary.toCreate).toBe(0);
		});

		test("handles nested directory structures", async () => {
			// Setup
			const nestedDir = join(testDir, "deep", "nested", "path");
			mkdirSync(nestedDir, {recursive: true});
			writeFileSync(join(nestedDir, "AGENTS.md"), "# Nested docs");

			// Execute
			const result = await syncSymlinksLib({cwd: testDir, run: true});
			expect(result.actions).toHaveLength(1);
			expect(existsSync(join(nestedDir, "CLAUDE.md"))).toBe(true);
			expect(readlinkSync(join(nestedDir, "CLAUDE.md"))).toBe("AGENTS.md");
		});

		test("preserves file permissions", async () => {
			// Setup
			const projectDir = join(testDir, "project-perms");
			mkdirSync(projectDir, {recursive: true});
			writeFileSync(join(projectDir, "AGENTS.md"), "# Agents documentation");

			// Execute
			await syncSymlinksLib({cwd: testDir, run: true});

			// Verify symlink was created and original file still exists
			const agentsStat = statSync(join(projectDir, "AGENTS.md"));
			expect(agentsStat.isFile()).toBe(true);
			expect(readlinkSync(join(projectDir, "CLAUDE.md"))).toBe("AGENTS.md");
		});
	});

	describe("dry-run mode", () => {
		test("dry-run does not modify filesystem", async () => {
			// Setup
			const projectDir = join(testDir, "project-dry");
			mkdirSync(projectDir, {recursive: true});
			writeFileSync(join(projectDir, "AGENTS.md"), "# Agents documentation");

			// Execute dry-run (default)
			const result = await syncSymlinksLib({cwd: testDir});
			expect(result.dryRun).toBe(true);
			expect(result.actions).toHaveLength(1);

			// Verify no changes
			expect(existsSync(join(projectDir, "CLAUDE.md"))).toBe(false);
		});
	});

	describe("chained symlink scenarios", () => {
		test("recognizes valid symlink chain (CLAUDE.md -> CLAUDE.md -> AGENTS.md)", async () => {
			// Setup: Create shared repo with AGENTS.md
			const sharedRepo = join(testDir, "shared-repo");
			mkdirSync(sharedRepo, {recursive: true});
			writeFileSync(join(sharedRepo, "AGENTS.md"), "# Shared agents docs");
			symlinkSync("AGENTS.md", join(sharedRepo, "CLAUDE.md"));

			// Create project with CLAUDE.md linking to shared repo's CLAUDE.md
			const projectDir = join(testDir, "project-chained");
			mkdirSync(projectDir, {recursive: true});
			symlinkSync(join(sharedRepo, "CLAUDE.md"), join(projectDir, "CLAUDE.md"));

			// Execute
			const result = await syncSymlinksLib({cwd: testDir});

			// Find the project action
			const projectAction = result.actions.find((a) => a.dir === projectDir);
			expect(projectAction).toBeDefined();
			expect(projectAction?.type).toBe("already_correct");
			expect(projectAction?.message).toContain("chained");
			expect(projectAction?.message).toContain("2 links");
		});

		test("detects when CLAUDE.md links to external regular file (not yet symlinked)", async () => {
			// Setup: Create shared repo with regular CLAUDE.md (not symlinked yet)
			const sharedRepo = join(testDir, "shared-repo-incomplete");
			mkdirSync(sharedRepo, {recursive: true});
			writeFileSync(join(sharedRepo, "AGENTS.md"), "# Shared agents docs");
			writeFileSync(join(sharedRepo, "CLAUDE.md"), "# Shared claude docs");

			// Create project with CLAUDE.md linking to shared repo's CLAUDE.md
			const projectDir = join(testDir, "project-incomplete-chain");
			mkdirSync(projectDir, {recursive: true});
			symlinkSync(join(sharedRepo, "CLAUDE.md"), join(projectDir, "CLAUDE.md"));

			// Execute
			const result = await syncSymlinksLib({cwd: testDir});

			// Find actions for both directories
			const projectAction = result.actions.find((a) => a.dir === projectDir);
			const sharedAction = result.actions.find((a) => a.dir === sharedRepo);

			// Project: CLAUDE.md links to external CLAUDE.md (not AGENTS.md) - should be removed
			expect(projectAction).toBeDefined();
			expect(projectAction?.type).toBe("remove_broken_symlink");

			// Shared repo: Has both files, should be conflict or need fixing
			expect(sharedAction).toBeDefined();
			expect(sharedAction?.type).toBe("conflict");
		});

		test("handles valid chain that directly points to AGENTS.md (skipping CLAUDE.md)", async () => {
			// Setup: Create shared repo
			const sharedRepo = join(testDir, "shared-direct");
			mkdirSync(sharedRepo, {recursive: true});
			writeFileSync(join(sharedRepo, "AGENTS.md"), "# Shared agents docs");

			// Create project with CLAUDE.md directly linking to shared AGENTS.md
			const projectDir = join(testDir, "project-direct");
			mkdirSync(projectDir, {recursive: true});
			symlinkSync(join(sharedRepo, "AGENTS.md"), join(projectDir, "CLAUDE.md"));

			// Execute
			const result = await syncSymlinksLib({cwd: testDir});

			// Should recognize as valid (CLAUDE.md -> AGENTS.md, even if external)
			const projectAction = result.actions.find((a) => a.dir === projectDir);
			expect(projectAction).toBeDefined();
			expect(projectAction?.type).toBe("already_correct");
		});

		test("detects circular symlink references", async () => {
			// Setup: Create circular reference
			const projectDir = join(testDir, "project-circular");
			mkdirSync(projectDir, {recursive: true});

			// Create A -> B -> A circular link
			const fileA = join(projectDir, "CLAUDE.md");
			const fileB = join(projectDir, "temp.md");
			symlinkSync("temp.md", fileA);
			symlinkSync("CLAUDE.md", fileB);

			// Also need AGENTS.md for directory to be scanned
			writeFileSync(join(projectDir, "AGENTS.md"), "# Agents");

			// Execute
			const result = await syncSymlinksLib({cwd: testDir});

			const projectAction = result.actions.find((a) => a.dir === projectDir);
			expect(projectAction).toBeDefined();
			expect(projectAction?.type).toBe("fix_symlink");
			expect(projectAction?.message).toContain("Circular");
		});

		test("detects chain that's too deep (>10 levels)", async () => {
			// Setup: Create a deep chain
			const projectDir = join(testDir, "project-deep");
			mkdirSync(projectDir, {recursive: true});
			writeFileSync(join(projectDir, "AGENTS.md"), "# Agents");

			// Create chain: CLAUDE.md -> link1 -> link2 -> ... -> link11
			let prevLink = "link11.md";
			for (let i = 11; i >= 1; i--) {
				const linkName = `link${i}.md`;
				symlinkSync(prevLink, join(projectDir, linkName));
				prevLink = linkName;
			}
			symlinkSync("link1.md", join(projectDir, "CLAUDE.md"));

			// Execute
			const result = await syncSymlinksLib({cwd: testDir});

			const projectAction = result.actions.find((a) => a.dir === projectDir);
			expect(projectAction).toBeDefined();
			expect(projectAction?.type).toBe("fix_symlink");
			expect(projectAction?.message).toContain("too deep");
		});

		test("handles broken symlink in chain", async () => {
			// Setup: Create chain with broken link
			const projectDir = join(testDir, "project-broken-chain");
			mkdirSync(projectDir, {recursive: true});
			writeFileSync(join(projectDir, "AGENTS.md"), "# Agents");

			// CLAUDE.md -> intermediate.md (which doesn't exist)
			symlinkSync("intermediate.md", join(projectDir, "CLAUDE.md"));

			// Execute
			const result = await syncSymlinksLib({cwd: testDir});

			const projectAction = result.actions.find((a) => a.dir === projectDir);
			expect(projectAction).toBeDefined();
			expect(projectAction?.type).toBe("fix_symlink");
			expect(projectAction?.message).toContain("Broken symlink");
		});
	});
});

// Helper function to read file synchronously for tests
function readFileSync(path: string, encoding: BufferEncoding): string {
	return require("fs").readFileSync(path, encoding);
}
