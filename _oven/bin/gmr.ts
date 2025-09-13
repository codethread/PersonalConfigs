#!/usr/bin/env node
const {assert} = require("console");

const tokenUrl =
	"https://git.perkbox.io/-/profile/personal_access_tokens?name=Example+Access+token&scopes=read_api,read_user";

main().catch((e) => {
	console.log("FAILED\n\n");
	console.error(e);
	process.exit(1);
});

async function main() {
	const token = process.env.CLI_GITLAB_TOKEN;

	assert(
		Boolean(token),
		`env 'CLI_GITLAB_TOKEN' is required, create a new one with ${tokenUrl}\nsee https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html for more`,
	);

	const [remoteStr, branch] = await Promise.all([
		shell("git remote -v").then(returnOrErr),
		shell("git rev-parse --abbrev-ref HEAD").then(returnOrErr),
	]);

	const remote = remoteStr.split("\n")[0].split("\t")[1].split(" ")[0];

	if (!remote.startsWith("git@git.perkbox.io")) {
		throw new Error(`remote not supported: ${remote}`);
	}

	const [, repo] = remote.split(":");
	const repoName = repo.replace(".git", "");
	const body = {
		query: `query MRsBranch {
currentUser { name }
project(fullPath: "${repoName}") {
name
mergeRequests(state: opened, sourceBranches: "${branch}") {
count
nodes {
title
webUrl
}
}
}
}`,
	};

	const d = await fetch("https://git.perkbox.io/api/graphql", {
		method: "POST",
		headers: {
			Authorization: `Bearer ${process.env.CLI_GITLAB_TOKEN}`,
			"content-type": "application/json",
		},
		body: JSON.stringify(body),
	})
		.then((d) => {
			return d.json();
		})
		.then(({data, errors}) => {
			if (errors) throw errors;
			if (!data.currentUser)
				throw new Error(`Look like your token has expired, create a new one with ${tokenUrl}`);
			return data;
		});

	const mergeRequests = d?.project?.mergeRequests;
	if (mergeRequests?.count === 0) {
		console.log("no open MRs");
		process.exit(1);
	}

	const mrs = mergeRequests?.nodes;

	if (!mrs) throw new Error(`no mrs in payload\n\n${JSON.stringify(d, null, 2)}`);

	mrs.forEach(({webUrl}) => {
		console.log(webUrl);
		shell(`open ${webUrl}`);
	});
}

/* --------------------------------------------------------------- */
/*                      NOTIFCATIONS                               */
/* --------------------------------------------------------------- */
function _notify(msg, title) {
	shell(`osascript -e 'display notification "${msg}"${title ? ` with title "${title}"` : ""}'`);
}

/* --------------------------------------------------------------- */
/*                      HELPERS                                    */
/* --------------------------------------------------------------- */

function returnOrErr({stderr, stdout}) {
	if (stderr) throw stderr;
	return stdout;
}

/* --------------------------------------------------------------- */
/*                      SHELL                                      */
/* --------------------------------------------------------------- */

/**
 * @param {string} cmd
 *
 * @returns {Promise<{ stdout: string; stderr: string }>}
 */
async function shell(cmd) {
	const {spawn} = require("child_process");
	return new Promise((resolve, reject) => {
		const [exe, ...args] = cmd.split(" ");
		const spawned = spawn(exe, args, {shell: true});

		let stdout = "";
		let stderr = "";

		spawned.stdout.on("data", (data) => {
			const str = data.toString();
			stdout += str;
		});

		spawned.stderr.on("data", (data) => {
			const str = data.toString();
			stderr += str;
		});

		spawned.on("close", (code) => {
			if (code === 0) {
				resolve({stdout: stdout.trim(), stderr});
			} else {
				reject(new Error(`spawned process ${cmd} exited with code ${code}, stderr ${stderr}`));
			}
		});

		spawned.on("error", (e) => {
			reject(new Error(e.message));
		});
	});
}
