import { parseArgs } from "jsr:@std/cli/parse-args";
import $ from "jsr:@david/dax";

// add open / copy arg
async function main(filepath?: string) {
  const [remoteStr, branch] = await Promise.all([
    $("git remote -v").then(returnOrErr),
    $("git rev-parse --abbrev-ref HEAD").then(returnOrErr),
  ]);

  const remote = remoteStr.split("\n")[0].split("\t")[1].split(" ")[0];
  const { domain, repo } = /^git@(?<domain>.*):(?<repo>.*).git$/.exec(
    remote,
  ).groups;

  if (domain === "github.com") {
    await shell(`open https://${domain}/${repo}`);
  } else if (domain.startsWith("git.")) {
    // gitlab
    await shell(`open https://${domain}/${repo}/-/tree/${branch}`);
  } else {
    console.log(`remote not supported: ${remote} (expected github or gitlab)`);
  }
}

function returnOrErr({ stderr, stdout }) {
  if (stderr) throw stderr;
  return stdout;
}

main().catch(console.error);
