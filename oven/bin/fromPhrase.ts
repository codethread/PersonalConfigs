#!/usr/bin/env bun

import {$} from "bun";
import {parseArgs} from "util";

function showHelp() {
	console.log(`
fromPhrase - Search for phrases in locale files

Usage: fromPhrase <search-term> [options]

Arguments:
  search-term     The phrase or text to search for in locale files

Options:
  --web           Open the found phrase in vim (for web locale files)
  -h, --help      Show this help message

Description:
  Searches for phrases in locale files (apps/web/app/public/locale) and can
  optionally open the result in vim.

Examples:
  fromPhrase "user settings"       # Search for phrase in locale files
  fromPhrase "login" --web         # Search and open in vim
`);
}

async function main() {
	// Check for help first
	const args = Bun.argv.slice(2);
	if (args.includes("-h") || args.includes("--help")) {
		showHelp();
		process.exit(0);
	}

	const {positionals, values} = parseArgs({
		args: args,
		strict: true,
		options: {
			web: {
				type: "boolean",
			},
		},
		allowPositionals: true,
	});

	const search = positionals.length === 1 ? positionals[0].split(" ") : positionals;

	const lines = (await $`rg "${search}" -F apps/web/app/public/locale -i --vimgrep`.text())
		.trim()
		.split("\n");

	const out = lines.filter((line) => line.includes("en-gb"));

	if (values.web) {
		const [found] = out.filter((line) => line.includes("web/en-gb"));
		if (!found) {
			console.error("not found");
		} else {
			const [, , , key] = found.split(":");
			const searches = (await $`rg ${key.trim()} -F -i --vimgrep`.text()).trim().split("\n");
			const res = searches.filter((line) => line.includes("apps/web"));
			if (res.length === 1) {
				const [goto] = res[0].split(":  ");
				await $`openInVim ${goto}`;
			} else {
				const formatted = res.map((l) => l.split(":")[0].trim());
				const out = (
					await $`echo ${formatted.join("\n")} | fzf --preview="bat --color=always {}" --preview-window=up,80%`.text()
				).trim();
				const [goto] = res.find((line) => line.includes(out)).split(":  ");
				await $`openInVim ${goto}`;
			}
		}
	} else {
		console.log(out);
		console.log("only web");
	}
}

main();
