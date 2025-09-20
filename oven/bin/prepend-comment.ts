#!/usr/bin/env bun
import { readFileSync, writeFileSync } from "fs";
import { resolve, extname } from "path";

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
prepend-comment - Add a comment line to the beginning of a file

Usage: prepend-comment <file-path> <comment-text>

Arguments:
  file-path     Path to the file (absolute or relative to current directory)
  comment-text  Text to add as a comment (will be quoted appropriately)

Examples:
  prepend-comment script.sh "Shell script for automation"
  prepend-comment config/app.js "Main application configuration"
  prepend-comment main.rs "Entry point for the application"

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
  switch (prefix) {
    case "/*":
      return `/* ${text} */`;
    case "<!--":
      return `<!-- ${text} -->`;
    case "--":
      return `-- ${text}`;
    case '"':
      return `" ${text}`;
    default:
      return `${prefix} ${text}`;
  }
}

function isCommentLine(line: string, prefix: string): boolean {
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

  if (args.length !== 2) {
    console.error("Error: Expected exactly 2 arguments");
    showHelp();
    process.exit(1);
  }

  const [filePath, commentText] = args;
  const resolvedPath = resolve(filePath);

  try {
    // Read the existing file content
    const originalContent = readFileSync(resolvedPath, "utf-8");
    const lines = originalContent.split("\n");

    // Get the appropriate comment syntax
    const commentPrefix = getCommentPrefix(resolvedPath);
    const commentLine = formatComment(commentPrefix, commentText);

    // Check if the first line is already a comment of the same type
    let newContent: string;
    let action: string;

    if (lines.length > 0 && isCommentLine(lines[0], commentPrefix)) {
      // Replace the existing comment
      lines[0] = commentLine;
      newContent = lines.join("\n");
      action = "Replaced comment in";
    } else {
      // Prepend the comment
      newContent = commentLine + "\n" + originalContent;
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
