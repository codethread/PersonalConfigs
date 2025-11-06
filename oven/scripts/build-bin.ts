#!/usr/bin/env bun

import {chmod, readdir} from "fs/promises";
import {basename, join} from "path";
import {parseArgs} from "util";
import {cleanBuilds} from "./clean";

const BIN_SRC_DIR = join(import.meta.dir, "..", "bin");
const BIN_DEST_DIR = join(import.meta.dir, "..", "..", "..", ".local", "bin");
const CC_INSPECT_BUILD_SCRIPT = join(BIN_SRC_DIR, "cc-inspect", "build.ts");

async function buildExecutables(verbose = false) {
	if (verbose) {
		console.log("Building executables from oven/bin to ~/.local/bin...");
	}

	// Get all .ts files from bin directory (exclude workspace directories)
	const files = await readdir(BIN_SRC_DIR, {withFileTypes: true});
	const tsFiles = files.filter((f) => f.isFile() && f.name.endsWith(".ts")).map((f) => f.name);

	if (verbose) {
		console.log(`Found ${tsFiles.length} TypeScript files to build\n`);
	}

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
	if (verbose && successes.length > 0) {
		console.log("✅ Successfully built:");
		for (const {file, destName} of successes) {
			console.log(`  • ${file} -> ${destName}`);
		}
	}

	if (failures.length > 0) {
		console.log(failures.length > 0 && !verbose ? "❌ Failed to build:" : "\n❌ Failed to build:");
		for (const {file, destName, error} of failures) {
			console.log(`  • ${file} -> ${destName}`);
			console.error(`    ${error}`);
		}
	}

	if (verbose || failures.length > 0) {
		console.log(
			`${failures.length > 0 && !verbose ? "" : "\n"}Summary: ${successes.length} succeeded, ${failures.length} failed`,
		);
	}

	// Build cc-inspect workspace executable
	if (verbose) {
		console.log("\nBuilding cc-inspect workspace...");
	}

	try {
		const {buildExecutable} = await import(CC_INSPECT_BUILD_SCRIPT);
		await buildExecutable(verbose);
		if (verbose) {
			console.log("✅ cc-inspect workspace built successfully");
		}
	} catch (error) {
		console.error("\n❌ Failed to build cc-inspect workspace:");
		console.error(`  ${error instanceof Error ? error.message : String(error)}`);
	}

	await cleanBuilds(verbose);

	if (verbose) {
		console.log("\nBuild complete!");
	}
}

// Parse command line arguments
if (import.meta.main) {
	const {values} = parseArgs({
		args: Bun.argv.slice(2),
		options: {
			verbose: {type: "boolean", short: "v"},
		},
	});

	// Run the build
	await buildExecutables(values.verbose);
}
