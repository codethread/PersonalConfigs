#!/usr/bin/env bun
import {readFileSync, writeFileSync} from "fs";
import {resolve, extname} from "path";

// Map file extensions to their comment syntax
const COMMENT_SYNTAX: Record<string, string> = {
	// Shell scripts and configs
	".sh": "#",
	".bash": "#",
	".zsh": "#",
	".fish": "#",
	".nu": "#",
	".env": "#",
	".gitignore": "#",
	".dockerignore": "#",

	// JavaScript/TypeScript
	".js": "//",
	".jsx": "//",
	".ts": "//",
	".tsx": "//",
	".mjs": "//",
	".cjs": "//",

	// Rust
	".rs": "//",

	// Go
	".go": "//",

	// C/C++
	".c": "//",
	".cpp": "//",
	".cc": "//",
	".cxx": "//",
	".h": "//",
	".hpp": "//",

	// Java/Kotlin/Scala
	".java": "//",
	".kt": "//",
	".scala": "//",

	// Python
	".py": "#",
	".pyi": "#",

	// Ruby
	".rb": "#",

	// PHP
	".php": "//",

	// CSS/SCSS/LESS
	".css": "/*",
	".scss": "//",
	".sass": "//",
	".less": "//",

	// SQL
	".sql": "--",

	// YAML/TOML
	".yml": "#",
	".yaml": "#",
	".toml": "#",

	// Configuration files
	".conf": "#",
	".config": "#",
	".ini": "#",

	// Web
	".html": "<!--",
	".htm": "<!--",
	".xml": "<!--",

	// Markdown
	".md": "<!--",
	".markdown": "<!--",

	// Vim
	".vim": '"',
	".vimrc": '"',

	// Lua
	".lua": "--",
};

function showHelp() {
	console.log(`
prepend-comment - Add a module documentation comment to a file

Usage: prepend-comment [options] <file-path> <comment-text>

Arguments:
  file-path     Path to the file (absolute or relative to current directory)
  comment-text  Text to add as a comment (will be quoted appropriately)

Options:
  -h, --help    Show this help message
  -m, --module  Replace first comment line even without :module: marker
                (treats existing comment as module documentation)

Description:
  Adds a comment with ':module:' keyword identifier to the top of files.
  Preserves shebang lines and replaces existing :module: comments.

  With --module flag: Replaces ANY first comment line, treating it as
  module documentation. Useful for files with pre-existing module comments
  in languages like Nushell, Python, etc.

Examples:
  prepend-comment script.sh "Shell script for automation"
    → # :module: Shell script for automation

  prepend-comment --module file.nu "Updated module description"
    → Replaces first comment line with: # :module: Updated module description

  prepend-comment main.rs "Entry point for the application"
    → // :module: Entry point for the application

Search for module comments:
  rg ":module:"              # Find all module documentation
  grep -r ":module:" .       # Alternative using grep

Supported file types:
  - Shell scripts (.sh, .bash, .zsh, .fish, .nu)
  - JavaScript/TypeScript (.js, .ts, .jsx, .tsx)
  - Rust (.rs)
  - Python (.py)
  - Go (.go)
  - C/C++ (.c, .cpp, .h, .hpp)
  - And many more...

Files without extensions are treated as shell scripts (#).
`);
}

function getCommentPrefix(filePath: string): string {
	const ext = extname(filePath).toLowerCase();

	// If no extension, assume shell script
	if (!ext) {
		return "#";
	}

	const prefix = COMMENT_SYNTAX[ext];
	if (!prefix) {
		console.warn(`Warning: Unknown file extension '${ext}', using '#' comment syntax`);
		return "#";
	}

	return prefix;
}

function formatComment(prefix: string, text: string): string {
	// Add the :module: keyword identifier before the text
	const commentWithKeyword = `:module: ${text}`;

	switch (prefix) {
		case "/*":
			return `/* ${commentWithKeyword} */`;
		case "<!--":
			return `<!-- ${commentWithKeyword} -->`;
		case "--":
			return `-- ${commentWithKeyword}`;
		case '"':
			return `" ${commentWithKeyword}`;
		default:
			return `${prefix} ${commentWithKeyword}`;
	}
}

function isCommentLine(line: string, prefix: string): boolean {
	const trimmed = line.trim();

	// Check if the line contains our :module: keyword identifier
	const hasModuleKeyword = trimmed.includes(":module:");

	switch (prefix) {
		case "/*":
			return trimmed.startsWith("/*") && trimmed.endsWith("*/") && hasModuleKeyword;
		case "<!--":
			return trimmed.startsWith("<!--") && trimmed.endsWith("-->") && hasModuleKeyword;
		case "--":
			return trimmed.startsWith("--") && hasModuleKeyword;
		case '"':
			return trimmed.startsWith('"') && hasModuleKeyword;
		default:
			return trimmed.startsWith(prefix) && hasModuleKeyword;
	}
}

function isAnyCommentLine(line: string, prefix: string): boolean {
	const trimmed = line.trim();

	switch (prefix) {
		case "/*":
			return trimmed.startsWith("/*") && trimmed.endsWith("*/");
		case "<!--":
			return trimmed.startsWith("<!--") && trimmed.endsWith("-->");
		case "--":
			return trimmed.startsWith("--");
		case '"':
			return trimmed.startsWith('"');
		default:
			return trimmed.startsWith(prefix);
	}
}

function main() {
	const args = process.argv.slice(2);

	// Check for help flag
	if (args.length === 0 || args.includes("-h") || args.includes("--help")) {
		showHelp();
		process.exit(args.length === 0 ? 1 : 0);
	}

	// Parse --module flag
	let moduleFlag = false;
	let filteredArgs = args;

	if (args.includes("--module") || args.includes("-m")) {
		moduleFlag = true;
		filteredArgs = args.filter((arg) => arg !== "--module" && arg !== "-m");
	}

	if (filteredArgs.length !== 2) {
		console.error(
			`Error: Expected exactly 2 arguments (plus optional --module flag), got ${filteredArgs.length}`,
		);
		showHelp();
		process.exit(1);
	}

	const [filePath, commentText] = filteredArgs;
	const resolvedPath = resolve(filePath);

	try {
		// Read the existing file content
		const originalContent = readFileSync(resolvedPath, "utf-8");
		const lines = originalContent.split("\n");

		// Get the appropriate comment syntax
		const commentPrefix = getCommentPrefix(resolvedPath);
		const commentLine = formatComment(commentPrefix, commentText);

		let newContent: string;
		let action: string;
		let insertIndex = 0;

		// Check if the first line is a shebang
		const hasShebang = lines.length > 0 && lines[0].startsWith("#!");
		if (hasShebang) {
			insertIndex = 1;
		}

		// Determine if we should replace the existing comment
		let shouldReplace = false;

		if (lines.length > insertIndex) {
			// Check if line has :module: marker
			if (isCommentLine(lines[insertIndex], commentPrefix)) {
				shouldReplace = true;
			}
			// With --module flag, replace any comment line (treating it as module comment)
			else if (moduleFlag && isAnyCommentLine(lines[insertIndex], commentPrefix)) {
				shouldReplace = true;
			}
		}

		if (shouldReplace) {
			// Replace the existing comment
			lines[insertIndex] = commentLine;
			newContent = lines.join("\n");
			action = "Replaced comment in";
		} else {
			// Insert the comment after shebang (if present) or at the beginning
			if (hasShebang) {
				lines.splice(1, 0, commentLine);
				newContent = lines.join("\n");
			} else {
				newContent = commentLine + "\n" + originalContent;
			}
			action = "Added comment to";
		}

		// Write back to the file
		writeFileSync(resolvedPath, newContent, "utf-8");
	} catch (error) {
		if (error instanceof Error) {
			console.error(`Error: ${error.message}`);
		} else {
			console.error("An unknown error occurred");
		}
		process.exit(1);
	}
}

main();
