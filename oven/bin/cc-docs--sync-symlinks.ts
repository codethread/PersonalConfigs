// :module: Bidirectional symlink management for CLAUDE.md and AGENTS.md files

import {$} from "bun";
import {existsSync, lstatSync, readlinkSync, renameSync, statSync, symlinkSync, unlinkSync} from "fs";
import {dirname, join, relative} from "path";
import {parseArgs} from "util";
import {report, reportError} from "../shared/report";

function showHelp() {
	console.log(`cc-docs--sync-symlinks - Bidirectional symlink management for CLAUDE.md and AGENTS.md files

Usage: cc-docs--sync-symlinks [options]

Description:
  Ensures CLAUDE.md files are symlinks pointing to AGENTS.md (the source of truth).
  By default runs in dry-run mode to preview changes safely.

Options:
  --run              Execute changes (default is dry-run)
  --force            Resolve conflicts by keeping most recently edited content
  --cwd <path>       Set working directory (defaults to current directory)
  --help, -h         Show this help message

Behavior:
  - AGENTS.md is the source of truth
  - CLAUDE.md should be a symlink to AGENTS.md
  - Discovers all CLAUDE.md and AGENTS.md files in the project
  - Handles conflicts, broken symlinks, and missing files

Examples:
  cc-docs--sync-symlinks                 # Preview changes (dry-run)
  cc-docs--sync-symlinks --run           # Apply changes
  cc-docs--sync-symlinks --run --force   # Apply changes, resolving conflicts
  cc-docs--sync-symlinks --cwd /path     # Run in specific directory
`);
	process.exit(0);
}

export interface SyncSymlinksOptions {
	run?: boolean;
	force?: boolean;
	cwd?: string;
}

interface FileInfo {
	path: string;
	exists: boolean;
	isSymlink: boolean;
	isRegularFile: boolean;
	symlinkTarget?: string;
	mtime?: Date;
}

interface DirectoryState {
	dir: string;
	agentsFile?: FileInfo;
	claudeFile?: FileInfo;
}

type ActionType =
	| "create_symlink"
	| "fix_symlink"
	| "rename_and_symlink"
	| "remove_broken_symlink"
	| "conflict"
	| "already_correct";

interface Action {
	type: ActionType;
	dir: string;
	message: string;
	details?: string;
}

export interface SyncResult {
	actions: Action[];
	summary: {
		toCreate: number;
		toFix: number;
		conflicts: number;
		alreadyCorrect: number;
		other: number;
	};
	dryRun: boolean;
}

async function main() {
	const {values} = parseArgs({
		args: Bun.argv.slice(2),
		options: {
			run: {type: "boolean"},
			force: {type: "boolean"},
			cwd: {type: "string"},
			help: {type: "boolean", short: "h"},
		},
		strict: false,
	});

	if (values.help) {
		showHelp();
	}

	try {
		const result = await syncSymlinksLib({
			run: values.run === true,
			force: values.force === true,
			cwd: typeof values.cwd === "string" ? values.cwd : undefined,
		});
		report(formatResult(result));
	} catch (err) {
		reportError(err);
		process.exit(1);
	}
}

export async function syncSymlinksLib(options: SyncSymlinksOptions): Promise<SyncResult> {
	const cwd = options.cwd || process.cwd();
	const dryRun = !options.run;

	// Discover all CLAUDE.md and AGENTS.md files
	const files = await discoverFiles(cwd);

	// Group by directory
	const directories = groupByDirectory(files);

	// Determine actions for each directory
	const actions: Action[] = [];
	for (const dirState of directories) {
		const action = determineAction(dirState, options.force || false);
		actions.push(action);

		// Execute action if not dry-run
		if (!dryRun && action.type !== "conflict" && action.type !== "already_correct") {
			await executeAction(action, dirState);
		}
	}

	// Build summary
	const summary = {
		toCreate: actions.filter((a) => a.type === "create_symlink").length,
		toFix:
			actions.filter((a) => a.type === "fix_symlink" || a.type === "rename_and_symlink").length +
			actions.filter((a) => a.type === "remove_broken_symlink").length,
		conflicts: actions.filter((a) => a.type === "conflict").length,
		alreadyCorrect: actions.filter((a) => a.type === "already_correct").length,
		other: 0,
	};

	return {actions, summary, dryRun};
}

async function discoverFiles(cwd: string): Promise<string[]> {
	try {
		// Use fd to find files - it handles symlinks (including broken ones) properly
		// -u: unrestricted (finds broken symlinks), -H: include hidden, -I: no-ignore
		// Use simple glob patterns matching the basename
		const claudeOutput = await $`cd ${cwd} && fd -u -H -I 'CLAUDE.md'`.text();
		const agentsOutput = await $`cd ${cwd} && fd -u -H -I 'AGENTS.md'`.text();

		const files: string[] = [];
		for (const line of [...claudeOutput.split("\n"), ...agentsOutput.split("\n")]) {
			if (line.trim().length > 0) {
				files.push(join(cwd, line.trim()));
			}
		}
		return files;
	} catch (_error) {
		// fd returns non-zero if no matches found
		return [];
	}
}

function groupByDirectory(files: string[]): DirectoryState[] {
	const dirMap = new Map<string, DirectoryState>();

	for (const file of files) {
		const dir = dirname(file);
		const filename = file.split("/").pop()!;

		if (!dirMap.has(dir)) {
			dirMap.set(dir, {dir});
		}

		const state = dirMap.get(dir)!;
		const fileInfo = getFileInfo(file);

		if (filename === "AGENTS.md") {
			state.agentsFile = fileInfo;
		} else if (filename === "CLAUDE.md") {
			state.claudeFile = fileInfo;
		}
	}

	return Array.from(dirMap.values());
}

function getFileInfo(path: string): FileInfo {
	const info: FileInfo = {
		path,
		exists: false,
		isSymlink: false,
		isRegularFile: false,
	};

	try {
		// Use lstat first to detect symlinks (even broken ones)
		const lstat = lstatSync(path);
		info.exists = true;
		info.isSymlink = lstat.isSymbolicLink();

		if (info.isSymlink) {
			try {
				info.symlinkTarget = readlinkSync(path);
				// Verify the symlink target exists
				if (!existsSync(path)) {
					// Broken symlink - still mark as exists but note it's broken
					info.exists = true;
				}
			} catch {
				// Broken symlink
				info.exists = true;
			}
		} else {
			info.isRegularFile = lstat.isFile();
			if (info.isRegularFile) {
				info.mtime = statSync(path).mtime;
			}
		}
	} catch {
		// File doesn't exist at all
		info.exists = false;
	}

	return info;
}

interface SymlinkChainResult {
	isValid: boolean;
	finalTarget?: string;
	chainLength: number;
	isChained: boolean; // True if chain goes through intermediate CLAUDE.md files
	message?: string;
}

function resolveSymlinkChain(path: string, maxDepth = 10): SymlinkChainResult {
	const visited = new Set<string>();
	let current = path;
	let depth = 0;

	while (depth < maxDepth) {
		// Detect circular reference
		if (visited.has(current)) {
			return {
				isValid: false,
				chainLength: depth,
				isChained: false,
				message: "Circular symlink reference detected",
			};
		}
		visited.add(current);

		try {
			const lstat = lstatSync(current);

			// If it's not a symlink, we've reached the end
			if (!lstat.isSymbolicLink()) {
				const basename = current.split("/").pop();
				return {
					isValid: basename === "AGENTS.md",
					finalTarget: current,
					chainLength: depth,
					isChained: depth > 0,
				};
			}

			// Follow the symlink
			const target = readlinkSync(current);
			// Resolve relative paths
			current = target.startsWith("/") ? target : join(dirname(current), target);
			depth++;
		} catch {
			// Can't read the symlink or target doesn't exist
			return {
				isValid: false,
				chainLength: depth,
				isChained: false,
				message: "Broken symlink in chain",
			};
		}
	}

	return {
		isValid: false,
		chainLength: depth,
		isChained: false,
		message: `Symlink chain too deep (>${maxDepth} levels)`,
	};
}

function determineAction(dirState: DirectoryState, force: boolean): Action {
	const {dir, agentsFile, claudeFile} = dirState;
	const hasAgents = agentsFile?.exists && agentsFile.isRegularFile;
	const hasClaude = claudeFile?.exists;
	const claudeIsSymlink = claudeFile?.isSymlink;
	const claudeIsRegular = claudeFile?.isRegularFile;

	// AGENTS.md exists as regular file
	if (hasAgents) {
		if (!hasClaude) {
			// No CLAUDE.md - create symlink
			return {
				type: "create_symlink",
				dir,
				message: "Would create: CLAUDE.md -> AGENTS.md",
			};
		}

		if (claudeIsSymlink) {
			const target = claudeFile?.symlinkTarget;
			if (target === "AGENTS.md") {
				// Already correct - direct link
				return {
					type: "already_correct",
					dir,
					message: "Already correct: CLAUDE.md -> AGENTS.md",
				};
			}

			// Check if it's a valid chain that resolves to AGENTS.md
			if (claudeFile?.path) {
				const chainResult = resolveSymlinkChain(claudeFile.path);
				if (chainResult.isValid) {
					return {
						type: "already_correct",
						dir,
						message: chainResult.isChained
							? `Already correct (chained): CLAUDE.md -> ... -> AGENTS.md (${chainResult.chainLength} links)`
							: "Already correct: CLAUDE.md -> AGENTS.md",
					};
				}
				if (chainResult.message) {
					return {
						type: "fix_symlink",
						dir,
						message: `Would fix: ${chainResult.message}`,
					};
				}
			}

			// Wrong target or broken symlink
			return {
				type: "fix_symlink",
				dir,
				message: `Would fix: CLAUDE.md -> AGENTS.md (currently: ${target || "broken"})`,
			};
		}

		if (claudeIsRegular) {
			// Both are regular files - conflict
			if (!force) {
				return {
					type: "conflict",
					dir,
					message: "Conflict: Both files exist (use --force to resolve)",
					details: `AGENTS.md (modified: ${agentsFile.mtime?.toISOString().split("T")[0]})\nCLAUDE.md (modified: ${claudeFile.mtime?.toISOString().split("T")[0]})`,
				};
			}
			// Force mode - keep newest
			return {
				type: "fix_symlink",
				dir,
				message: "Would resolve conflict: keep newest content in AGENTS.md, create symlink",
				details: `Newest: ${agentsFile.mtime! > claudeFile.mtime! ? "AGENTS.md" : "CLAUDE.md"}`,
			};
		}
	}

	// Only CLAUDE.md exists (no local AGENTS.md)
	if (hasClaude && !hasAgents) {
		if (claudeIsRegular) {
			return {
				type: "rename_and_symlink",
				dir,
				message: "Would rename: CLAUDE.md -> AGENTS.md, create symlink",
			};
		}
		if (claudeIsSymlink && claudeFile?.path) {
			// Check if it's a valid chain pointing to external AGENTS.md
			const chainResult = resolveSymlinkChain(claudeFile.path);
			if (chainResult.isValid) {
				return {
					type: "already_correct",
					dir,
					message: chainResult.isChained
						? `Already correct (chained, external): CLAUDE.md -> ... -> AGENTS.md (${chainResult.chainLength} links)`
						: "Already correct (external): CLAUDE.md -> AGENTS.md",
				};
			}
			// Invalid chain - broken or doesn't resolve to AGENTS.md
			return {
				type: "remove_broken_symlink",
				dir,
				message: chainResult.message
					? `Would remove: ${chainResult.message}`
					: "Would remove: broken CLAUDE.md symlink",
			};
		}
	}

	// Neither exists (shouldn't happen in normal flow)
	return {
		type: "already_correct",
		dir,
		message: "No action needed",
	};
}

async function executeAction(action: Action, dirState: DirectoryState): Promise<void> {
	const claudePath = join(action.dir, "CLAUDE.md");
	const agentsPath = join(action.dir, "AGENTS.md");

	switch (action.type) {
		case "create_symlink":
			// Just in case - remove if exists (shouldn't happen in normal flow)
			try {
				if (existsSync(claudePath)) {
					unlinkSync(claudePath);
				}
			} catch {
				// Ignore errors
			}
			symlinkSync("AGENTS.md", claudePath);
			break;

		case "fix_symlink": {
			// If force mode with conflict, read content before removing files
			let claudeContent: string | undefined;
			if (action.details?.includes("Newest:")) {
				const claudeFile = dirState.claudeFile;
				const agentsFile = dirState.agentsFile;
				if (
					claudeFile?.isRegularFile &&
					agentsFile?.isRegularFile &&
					claudeFile.mtime &&
					agentsFile.mtime &&
					claudeFile.mtime > agentsFile.mtime
				) {
					// CLAUDE.md is newer - read its content before removing
					claudeContent = await Bun.file(claudePath).text();
				}
			}

			// Remove existing CLAUDE.md (symlink or regular file)
			try {
				unlinkSync(claudePath);
			} catch {
				// Might not exist or already removed
			}

			// Write newest content to AGENTS.md if needed
			if (claudeContent) {
				await Bun.write(agentsPath, claudeContent);
			}

			// Create symlink
			symlinkSync("AGENTS.md", claudePath);
			break;
		}

		case "rename_and_symlink":
			renameSync(claudePath, agentsPath);
			symlinkSync("AGENTS.md", claudePath);
			break;

		case "remove_broken_symlink":
			try {
				unlinkSync(claudePath);
			} catch {
				// Already removed or doesn't exist
			}
			break;
	}
}

function formatResult(result: SyncResult): string {
	const {actions, summary, dryRun} = result;
	const lines: string[] = [];

	lines.push(
		dryRun ? "🔍 Scanning for CLAUDE.md and AGENTS.md files..." : "🔄 Syncing documentation files...",
	);
	lines.push("");

	if (actions.length === 0) {
		lines.push("No documentation files found.");
		return lines.join("\n");
	}

	lines.push(
		`Found ${actions.length} ${actions.length === 1 ? "directory" : "directories"} with documentation files:`,
	);
	lines.push("");

	// Group and display actions
	for (const action of actions) {
		const icon =
			action.type === "conflict" ? "⚠️ " : action.type === "already_correct" ? "✓" : dryRun ? "✓" : "✅";
		const relDir = relative(process.cwd(), action.dir) || ".";
		lines.push(`📁 ${relDir}`);
		lines.push(`  ${icon} ${action.message}`);
		if (action.details) {
			for (const detail of action.details.split("\n")) {
				lines.push(`      ${detail}`);
			}
		}
		lines.push("");
	}

	// Summary
	const parts: string[] = [];
	if (summary.toCreate > 0) parts.push(`${summary.toCreate} to create`);
	if (summary.toFix > 0) parts.push(`${summary.toFix} to fix`);
	if (summary.conflicts > 0) parts.push(`${summary.conflicts} conflict${summary.conflicts > 1 ? "s" : ""}`);
	if (summary.alreadyCorrect > 0) parts.push(`${summary.alreadyCorrect} correct`);

	lines.push(`Summary: ${parts.join(", ")}`);

	if (dryRun && (summary.toCreate > 0 || summary.toFix > 0)) {
		lines.push("💡 Run with --run to apply changes");
	}

	if (summary.conflicts > 0 && dryRun) {
		lines.push("⚠️  Use --force to automatically resolve conflicts");
	}

	return lines.join("\n");
}

// Only run if executed directly
if (import.meta.main) {
	main();
}
