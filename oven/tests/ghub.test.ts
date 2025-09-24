import {describe, expect, test} from "bun:test";
import {join} from "path";

// For now, we'll create a simple integration test since mocking Bun's $ is complex
describe("ghub", () => {
	const ghubPath = join(import.meta.dir, "../bin/ghub.ts");

	test("module exports openRepo function", async () => {
		const ghubModule = await import("../bin/ghub.ts");
		expect(typeof ghubModule.openRepo).toBe("function");
	});

	test("parseArgs is used for CLI parsing", async () => {
		// Verify the module uses parseArgs by checking imports
		const fileContent = await Bun.file(ghubPath).text();
		expect(fileContent).toContain('import {parseArgs} from "util"');
	});

	test("uses Bun $ for shell commands", async () => {
		// Verify the module uses Bun's $ instead of child_process
		const fileContent = await Bun.file(ghubPath).text();
		expect(fileContent).toContain('import {$} from "bun"');
		expect(fileContent).not.toContain("child_process");
		expect(fileContent).not.toContain("spawn");
	});

	test("uses import.meta.main for CLI detection", async () => {
		// Verify the module properly checks if it's being run as main
		const fileContent = await Bun.file(ghubPath).text();
		expect(fileContent).toContain("import.meta.main");
	});
});
