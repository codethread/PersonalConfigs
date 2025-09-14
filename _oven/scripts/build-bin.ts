#!/usr/bin/env bun

import {$} from "bun";
import {chmod, readdir} from "fs/promises";
import {basename, join} from "path";
import {cleanBuilds} from "./clean";

const BIN_SRC_DIR = join(import.meta.dir, "..", "bin");
const BIN_DEST_DIR = join(import.meta.dir, "..", "..", "..", ".local", "bin");

async function buildExecutables() {
	console.log("Building executables from _oven/bin to ~/.local/bin...");

	// Get all .ts files from bin directory
	const files = await readdir(BIN_SRC_DIR);
	const tsFiles = files.filter((f) => f.endsWith(".ts"));

	console.log(`Found ${tsFiles.length} TypeScript files to build\n`);

	// Build all files in parallel
	const buildPromises = tsFiles.map(async (file) => {
		const srcPath = join(BIN_SRC_DIR, file);
		const destName = basename(file, ".ts"); // Remove .ts extension
		const destPath = join(BIN_DEST_DIR, destName);

		try {
			// Use Bun.build API to create standalone executable
			const result = await Bun.build({
				entrypoints: [srcPath],
				compile: {
					target:
						process.platform === "darwin"
							? process.arch === "arm64"
								? "bun-darwin-arm64"
								: "bun-darwin-x64"
							: process.platform === "linux"
								? "bun-linux-x64"
								: "bun-linux-x64", // fallback
					outfile: destPath,
				},
				// Optimization options
				minify: true,
				bytecode: true, // Faster startup
				sourcemap: "inline",
			});

			if (!result.success) {
				const errors = result.logs.map((msg) => `  ${msg}`).join("\n");
				return {
					file,
					destName,
					success: false,
					error: `Build failed:\n${errors}`,
				};
			}

			// Make the file executable
			await chmod(destPath, 0o755);
			return {file, destName, success: true};
		} catch (error) {
			return {
				file,
				destName,
				success: false,
				error: `Exception: ${error}`,
			};
		}
	});

	// Wait for all builds to complete
	const results = await Promise.all(buildPromises);

	// Separate successes and failures
	const successes = results.filter((r) => r.success);
	const failures = results.filter((r) => !r.success);

	// Display results
	if (successes.length > 0) {
		console.log("✅ Successfully built:");
		for (const {file, destName} of successes) {
			console.log(`  • ${file} -> ${destName}`);
		}
	}

	if (failures.length > 0) {
		console.log("\n❌ Failed to build:");
		for (const {file, destName, error} of failures) {
			console.log(`  • ${file} -> ${destName}`);
			console.error(`    ${error}`);
		}
	}

	console.log(`\nSummary: ${successes.length} succeeded, ${failures.length} failed`);

	await cleanBuilds();

	console.log("Build complete!");
}

// Run the build
await buildExecutables();
