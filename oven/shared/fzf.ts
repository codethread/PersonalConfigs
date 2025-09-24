// Provides a fuzzy-finder function for interactive selection of strings using fzf
interface Options {
	multi?: boolean;
	tmux?: boolean;
}

export async function fzf<Opts extends Options>(
	lines: string[],
	opts?: Opts,
): Promise<Opts["multi"] extends true ? string[] : string> {
	const flags = Object.entries(opts || {})
		.filter(([, v]) => !!v)
		.map(([flag]) => `--${flag}`);

	const fzf = Bun.spawn(["fzf", ...flags], {
		stdio: ["pipe", "pipe", "inherit"],
	});

	fzf.stdin.write(lines.join("\n"));
	fzf.stdin.end();

	const output = await new Response(fzf.stdout).text();
	if (opts?.multi) {
		const lines: string[] = output.split("\n").map((l) => l.trim());
		return lines as any;
	} else return output.trim() as any;
}
