#!/usr/bin/env bun

import {Glob} from "bun";
import {spawn} from "child_process";
import {existsSync, mkdirSync, readdirSync, statSync, writeFileSync} from "fs";
import {basename, join} from "path";

interface Options {
	debug?: boolean;
	test?: boolean;
	verbose?: boolean;
	maxDepth?: number;
	searchRoot?: string;
	includeGlobs?: boolean;
}

// Directories to skip for performance
const SKIP_DIRS = new Set([
	"node_modules",
	"target",
	".cargo",
	".next",
	"dist",
	"build",
	"coverage",
	".git", // Don't recurse into .git dirs themselves
]);

// Directory globs to search for additional sessions (from interactive-session.sh)
const SEARCH_GLOBS = [
	"~/PersonalConfigs",
	"~/workfiles",
	"~/.local/share/nvim/lazy/*",
	"~/.local/share/nvim/mason/packages/*",
	"~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes",
];

class GitProjectFinder {
	public searchRoot: string;
	private maxDepth: number;
	private debug: boolean;
	private verbose: boolean;
	private includeGlobs: boolean;
	private projects: string[] = [];
	private visitedDirs = 0;
	private maxDepthReached = 0;

	constructor(options: Options = {}) {
		this.searchRoot = options.searchRoot || join(process.env.HOME || "", "dev");
		this.maxDepth = options.maxDepth || 4;
		this.debug = options.debug || false;
		this.verbose = options.verbose || false;
		this.includeGlobs = options.includeGlobs ?? true; // Default to true
	}

	private log(message: string) {
		if (this.debug || this.verbose) {
			console.error(`[DEBUG] ${message}`);
		}
	}

	private isGitProject(dir: string): boolean {
		return existsSync(join(dir, ".git"));
	}

	private shouldSkipDir(dirName: string): boolean {
		return SKIP_DIRS.has(dirName);
	}

	private async findGlobDirectories(): Promise<string[]> {
		if (!this.includeGlobs) {
			return [];
		}

		const directories: string[] = [];
		const homeDir = process.env.HOME || "";

		for (const globPattern of SEARCH_GLOBS) {
			try {
				// Expand tilde to home directory
				const expandedPattern = globPattern.startsWith("~/")
					? join(homeDir, globPattern.slice(2))
					: globPattern;

				this.log(`Searching glob: ${expandedPattern}`);

				const glob = new Glob(expandedPattern);

				// Use Bun's glob scan from root directory
				for await (const match of glob.scan("/")) {
					try {
						const stat = statSync(match);
						if (stat.isDirectory()) {
							directories.push(match);
							this.log(`Found glob directory: ${match}`);
						}
					} catch (error) {
						this.log(`Cannot stat glob match: ${match} - ${error}`);
					}
				}
			} catch (error) {
				this.log(`Error processing glob ${globPattern}: ${error}`);
			}
		}

		return directories;
	}

	private walkDirectory(dir: string, currentDepth = 0): void {
		this.visitedDirs++;
		this.maxDepthReached = Math.max(this.maxDepthReached, currentDepth);

		if (this.verbose) {
			this.log(`Visiting: ${dir} (depth: ${currentDepth})`);
		}

		// Check if current directory is a git project
		if (this.isGitProject(dir)) {
			this.projects.push(dir);
			this.log(`Found git project: ${dir}`);
			return; // Stop recursing - no nested projects
		}

		// Don't recurse too deep
		if (currentDepth >= this.maxDepth) {
			this.log(`Max depth reached at: ${dir}`);
			return;
		}

		try {
			const entries = readdirSync(dir);

			for (const entry of entries) {
				if (this.shouldSkipDir(entry)) {
					this.log(`Skipping: ${join(dir, entry)}`);
					continue;
				}

				const fullPath = join(dir, entry);

				try {
					const stat = statSync(fullPath);
					if (stat.isDirectory()) {
						this.walkDirectory(fullPath, currentDepth + 1);
					}
				} catch (error) {
					// Skip directories we can't read (permissions, etc.)
					this.log(`Cannot read: ${fullPath} - ${error}`);
				}
			}
		} catch (error) {
			this.log(`Cannot read directory: ${dir} - ${error}`);
		}
	}

	public async findProjects(): Promise<string[]> {
		this.projects = [];
		this.visitedDirs = 0;
		this.maxDepthReached = 0;

		if (!existsSync(this.searchRoot)) {
			throw new Error(`Search root does not exist: ${this.searchRoot}`);
		}

		const startTime = Date.now();

		// Find git projects by walking directory tree
		this.walkDirectory(this.searchRoot);
		const gitProjects = [...this.projects];

		// Find additional directories from globs
		const globDirectories = await this.findGlobDirectories();

		// Merge and deduplicate
		const allProjects = [...new Set([...gitProjects, ...globDirectories])];

		const endTime = Date.now();

		if (this.debug) {
			console.error(
				`Found ${gitProjects.length} git projects + ${globDirectories.length} glob directories = ${allProjects.length} total in ${endTime - startTime}ms`,
			);
			console.error(`Visited ${this.visitedDirs} directories`);
			console.error(`Max depth reached: ${this.maxDepthReached}`);
		}

		return allProjects.sort();
	}
}

function toKebabCase(input: string): string {
	return basename(input)
		.toLowerCase()
		.replace(/[^a-z0-9]/g, "-")
		.replace(/-+/g, "-")
		.replace(/^-|-$/g, "");
}

async function createAndSwitchSession(projectPath: string): Promise<void> {
	const sessionName = toKebabCase(projectPath);
	const sessionsDir = join(process.env.HOME || "", ".config", "kitty", "sessions");
	const sessionFile = join(sessionsDir, `${sessionName}.session`);

	// Create sessions directory if it doesn't exist
	if (!existsSync(sessionsDir)) {
		mkdirSync(sessionsDir, {recursive: true});
	}

	// Create session file
	const sessionContent = `launch --title ${sessionName} --cwd ${projectPath}\n`;
	writeFileSync(sessionFile, sessionContent);

	console.log(`Created session: ${sessionName}`);
	console.log(`Directory: ${projectPath}`);
	console.log(`Session file: ${sessionFile}`);

	// Switch to the session
	try {
		const kittenProcess = spawn("kitten", ["@", "action", "goto_session", sessionFile]);

		kittenProcess.stderr?.on("data", (data) => {
			console.error(`Warning: ${data.toString()}`);
		});

		await new Promise((resolve, reject) => {
			kittenProcess.on("close", (code) => {
				if (code === 0) {
					resolve(code);
				} else {
					reject(new Error(`Process exited with code ${code}`));
				}
			});
		});
	} catch (error) {
		console.error(`Error switching to session: ${error}`);
		process.exit(1);
	}
}

async function runFzf(projects: string[]): Promise<string | null> {
	return new Promise((resolve) => {
		const fzf = spawn("fzf", ["--prompt=Select git project: ", "--height=40%", "--reverse"], {
			stdio: ["pipe", "pipe", "inherit"],
		});

		let selection = "";

		fzf.stdout.on("data", (data) => {
			selection += data.toString();
		});

		fzf.on("close", (code) => {
			if (code === 0 && selection.trim()) {
				resolve(selection.trim());
			} else {
				resolve(null);
			}
		});

		// Write projects to fzf stdin
		fzf.stdin.write(projects.join("\n"));
		fzf.stdin.end();
	});
}

function showHelp() {
	console.log(`Usage: git-session.ts [options]
Interactive git project session switcher

Finds projects by:
1. Walking ~/dev (or --search-root) recursively for git repositories
2. Searching predefined glob patterns for additional directories

Options:
  -h, --help        Show this help message
  --test           Test discovery speed without fzf
  --debug          Show debug information during search
  --verbose        Show verbose output (visited directories)
  --max-depth N    Maximum search depth (default: 4)
  --search-root P  Search root directory (default: ~/dev)
  --no-globs       Disable glob pattern searching (git projects only)

Examples:
  git-session.ts                    # Interactive mode (git + globs)
  git-session.ts --test             # Benchmark mode
  git-session.ts --debug --test     # Debug benchmark
  git-session.ts --no-globs         # Git projects only
  git-session.ts --max-depth 2      # Limit search depth`);
}

async function main() {
	const args = process.argv.slice(2);
	const options: Options = {};

	// Parse arguments
	for (let i = 0; i < args.length; i++) {
		switch (args[i]) {
			case "-h":
			case "--help":
				showHelp();
				process.exit(0);
				break;
			case "--test":
				options.test = true;
				break;
			case "--debug":
				options.debug = true;
				break;
			case "--verbose":
				options.verbose = true;
				break;
			case "--max-depth":
				options.maxDepth = parseInt(args[++i], 10);
				break;
			case "--search-root":
				options.searchRoot = args[++i];
				break;
			case "--no-globs":
				options.includeGlobs = false;
				break;
		}
	}

	const finder = new GitProjectFinder(options);

	try {
		if (options.test) {
			console.log("Testing git project discovery speed...");
			console.log(`Search root: ${finder.searchRoot}`);

			const startTime = Date.now();
			const projects = await finder.findProjects();
			const endTime = Date.now();

			console.log(`Found ${projects.length} projects in ${endTime - startTime}ms`);

			if (options.debug) {
				console.log("\nFirst 10 projects:");
				for (const p of projects.slice(0, 10)) {
					console.log(`  ${p}`);
				}
				if (projects.length > 10) {
					console.log(`  ... and ${projects.length - 10} more`);
				}
			}

			process.exit(0);
		}

		// Interactive mode
		console.error("Discovering projects...");
		const projects = await finder.findProjects();

		if (projects.length === 0) {
			console.error(`No git projects found in ${finder.searchRoot}`);
			process.exit(1);
		}

		const selected = await runFzf(projects);

		if (!selected) {
			process.exit(0); // User cancelled
		}

		await createAndSwitchSession(selected);
	} catch (error) {
		console.error(`Error: ${error}`);
		process.exit(1);
	}
}

if (import.meta.main) {
	main();
}
