#!/usr/bin/env bun
// :module: Build script for cc-inspect - compiles TypeScript/React app into standalone executable

import plugin from "bun-plugin-tailwind";
import {chmod} from "fs/promises";
import {join} from "path";
import {parseArgs} from "util";

const BIN_DEST_DIR = join(import.meta.dir, "..", "..", "..", "..", ".local", "bin");
const ENTRY_POINT = join(import.meta.dir, "src", "index.tsx");
const DEST_PATH = join(BIN_DEST_DIR, "cc-inspect");

async function buildExecutable(verbose = false) {
	if (verbose) {
		console.log("Building cc-inspect executable...");
		console.log(`  Entry: ${ENTRY_POINT}`);
		console.log(`  Output: ${DEST_PATH}`);
	}

	try {
		if (verbose) {
			console.log("  Starting Bun.build...");
		}

		const result = await Bun.build({
			entrypoints: [ENTRY_POINT],
			plugins: [plugin],
			compile: {
				target:
					process.platform === "darwin"
						? process.arch === "arm64"
							? "bun-darwin-arm64"
							: "bun-darwin-x64"
						: process.platform === "linux"
							? "bun-linux-x64"
							: "bun-linux-x64", // fallback
				outfile: DEST_PATH,
			},
			minify: true,
			bytecode: true,
			sourcemap: "inline",
			define: {
				"process.env.NODE_ENV": JSON.stringify("production"),
			},
		});

		if (verbose) {
			console.log(`  Build result success: ${result.success}`);
			console.log(`  Build logs count: ${result.logs.length}`);
		}

		if (!result.success) {
			const errors = result.logs
				.map((log) => {
					if (typeof log === "string") return `  ${log}`;
					return `  ${log.message || JSON.stringify(log)}`;
				})
				.join("\n");
			console.error("Build failed with errors:");
			console.error(errors);
			throw new Error(`Bundle failed${errors ? `:\n${errors}` : ""}`);
		}

		// Make the file executable
		await chmod(DEST_PATH, 0o755);

		if (verbose) {
			console.log("✅ cc-inspect built successfully");
		}

		return {success: true, outputPath: DEST_PATH};
	} catch (error) {
		if (verbose) {
			console.error("  Exception caught during build:");
			console.error(error);
		}
		throw new Error(`Failed to build cc-inspect: ${error instanceof Error ? error.message : String(error)}`);
	}
}

// Only run if executed directly
if (import.meta.main) {
	const {values} = parseArgs({
		args: Bun.argv.slice(2),
		options: {
			verbose: {type: "boolean", short: "v"},
		},
		strict: false,
	});

	try {
		await buildExecutable(values.verbose);
	} catch (error) {
		console.error(error instanceof Error ? error.message : String(error));
		process.exit(1);
	}
}

export {buildExecutable};
