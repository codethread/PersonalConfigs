#!/usr/bin/env bun
import {$} from "bun";
import {readdir, readFile, writeFile, mkdtemp, rm} from "fs/promises";
import {join, basename} from "path";
import {tmpdir} from "os";
import {cleanBuilds} from "./clean";

interface ToolInfo {
	name: string;
	description: string;
	usage?: string;
}

async function extractHelpInfo(toolPath: string, tempDir: string): Promise<ToolInfo | null> {
	const toolName = basename(toolPath, ".ts");
	// Add a random suffix to avoid collisions when running in parallel
	const randomSuffix = Math.random().toString(36).substring(7);
	const tempBuildPath = join(tempDir, `${toolName}-${randomSuffix}`);

	try {
		// Build the tool to a temporary location
		await $`bun build ${toolPath} --compile --outfile ${tempBuildPath}`.quiet();

		// Run the tool with -h flag to get help text
		// Use nothrow() to prevent errors from stopping execution
		const result = await $`${tempBuildPath} -h`.nothrow();

		// Get output regardless of exit code (some tools might exit with 1 on help)
		const helpOutput = result.stdout?.toString() || result.stderr?.toString() || "";

		// Parse the help output
		// Extract the first line which should be "toolname - description"
		const lines = helpOutput.trim().split("\n");
		const firstLine = lines.find((line) => line.includes(" - "));

		if (firstLine) {
			const [name, ...descParts] = firstLine.split(" - ");
			const description = descParts.join(" - ").trim();

			// Extract usage if available
			const usageIndex = lines.findIndex((line) => line.toLowerCase().includes("usage:"));
			let usage = undefined;
			if (usageIndex !== -1 && usageIndex + 1 < lines.length) {
				usage = lines[usageIndex + 1].trim();
			}

			return {
				name: name.trim(),
				description,
				usage,
			};
		}

		// Fallback: just use the tool name
		return {
			name: toolName,
			description: "No description available",
		};
	} catch (error) {
		// Silently handle errors - tool might not support -h
		return {
			name: toolName,
			description: "No description available",
		};
	}
}

async function updateReadme(tools: ToolInfo[]): Promise<void> {
	const readmePath = join(process.cwd(), "README.md");

	// Read current README
	const readmeContent = await readFile(readmePath, "utf-8");

	// Find the Tools Included section
	const toolsSectionStart = readmeContent.indexOf("## Tools Included");
	if (toolsSectionStart === -1) {
		throw new Error("Could not find '## Tools Included' section in README.md");
	}

	// Find the next section (or end of file)
	let toolsSectionEnd = readmeContent.indexOf("\n## ", toolsSectionStart + 1);
	if (toolsSectionEnd === -1) {
		toolsSectionEnd = readmeContent.length;
	}

	// Generate new tools section
	let newToolsSection = "## Tools Included\n\n";

	// Sort tools alphabetically by name
	tools.sort((a, b) => a.name.localeCompare(b.name));

	// Create the tools list
	tools.forEach((tool) => {
		newToolsSection += `- **${tool.name}** - ${tool.description}\n`;
	});

	// Add additional section for usage examples if needed
	newToolsSection += "\n### Quick Usage\n\n";
	newToolsSection +=
		"All tools support the `-h` or `--help` flag to display usage information:\n\n";
	newToolsSection += "```bash\n";
	newToolsSection += "# Get help for any tool\n";
	newToolsSection += "analyze-subagents -h\n";
	newToolsSection += "bra --help\n";
	newToolsSection += "```\n";

	// Replace the tools section in README
	const newReadmeContent =
		readmeContent.substring(0, toolsSectionStart) +
		newToolsSection +
		readmeContent.substring(toolsSectionEnd);

	// Write updated README
	await writeFile(readmePath, newReadmeContent, "utf-8");

	console.log(`✅ Updated README.md with ${tools.length} tools`);
}

async function main() {
	try {
		// Create a temporary directory for builds
		const tempDir = await mkdtemp(join(tmpdir(), "sync-readme-"));

		try {
			// Get all TypeScript files in bin directory
			const binDir = join(process.cwd(), "bin");
			const files = await readdir(binDir);
			const tsFiles = files.filter((file) => file.endsWith(".ts"));

			console.log(`Found ${tsFiles.length} TypeScript files in bin/`);
			console.log("Building and extracting help information in parallel...");

			// Extract help information from all tools in parallel
			const toolPromises = tsFiles.map((file) => {
				const toolPath = join(binDir, file);
				return extractHelpInfo(toolPath, tempDir);
			});

			// Wait for all tools to be processed
			const toolResults = await Promise.all(toolPromises);

			// Filter out null results
			const validTools = toolResults.filter((tool): tool is ToolInfo => tool !== null);

			// Deduplicate tools by name (in case multiple files produce the same tool)
			const toolMap = new Map<string, ToolInfo>();
			validTools.forEach((tool) => {
				// Only keep the first occurrence of each tool name
				if (!toolMap.has(tool.name)) {
					toolMap.set(tool.name, tool);
				}
			});

			const tools = Array.from(toolMap.values());

			console.log("\nExtracted help information:");
			tools.forEach((tool) => {
				console.log(`  ✓ ${tool.name}: ${tool.description}`);
			});

			// Update README with the extracted information
			await updateReadme(tools);
		} finally {
			// Clean up temporary directory
			await rm(tempDir, {recursive: true, force: true});
		}
	} catch (error) {
		console.error("Error:", error);
		process.exit(1);
	} finally {
		await cleanBuilds();
	}
}

// Run if called directly
if (import.meta.main) {
	main();
}
