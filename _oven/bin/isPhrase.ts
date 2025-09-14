#!/usr/bin/env bun

import {parseArgs} from "util";

function showHelp() {
	console.log(`
isPhrase - Check if a string is a valid phrase key

Usage: isPhrase <key> [options]

Arguments:
  key             The string to check if it's a valid phrase key

Options:
  -h, --help      Show this help message

Description:
  Validates if a string matches the phrase key format (e.g., "section.subsection.key").
  Returns exit code 0 if valid, 1 if invalid.

Examples:
  isPhrase "user.settings.title"    # Valid phrase key (exit 0)
  isPhrase "invalid string"         # Invalid phrase key (exit 1)
`);
}

// Check for help flag first
const args = Bun.argv.slice(2);
if (args.includes("-h") || args.includes("--help")) {
	showHelp();
	process.exit(0);
}

const {
	positionals: [key],
} = parseArgs({
	args: args,
	strict: true,
	allowPositionals: true,
});

function isKey(str: string) {
	const isLangKeyRegex = /^[a-z]+(\.[a-z0-9-_]+)+$/i;

	return isLangKeyRegex.test(str);
}

process.exit(isKey(key) ? 0 : 1);
