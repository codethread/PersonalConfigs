#!/usr/bin/env bun

import {parseArgs} from "util";

const {
	positionals: [key],
} = parseArgs({
	args: Bun.argv.slice(2),
	strict: true,
	allowPositionals: true,
});

function isKey(str: string) {
	const isLangKeyRegex = /^[a-z]+(\.[a-z0-9-_]+)+$/i;

	return isLangKeyRegex.test(str);
}

process.exit(isKey(key) ? 0 : 1);
