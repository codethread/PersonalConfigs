import { Command } from "npm:@commander-js/extra-typings";
// import { Command } from "npm:commander@12.1.0";
import { z } from "npm:zod@^3";

export function getArgs() {
  return yargs(Deno.args)
    .usage(
      `Log into your slack account and pull out tokens to run slack from the terminal!

// export function getArgs() {
//   return yargs(Deno.args)
//     .usage(
//       `Log into your slack account and pull out tokens to run slack from the terminal!
//
// Example usage:
// $ deno run \\
//       --allow-env \\
//       --allow-read \\
//       --allow-write=/var/folders \\
//       --allow-net=127.0.0.1 \\
//       --allow-run="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \\
//       ./getSlackToken.ts \\
//       --email <email> \\
//       --domain https://<domain>.slack.com`,
//     )
//     .wrap(null)
//     .option("email", {
//       alias: "e",
//       type: "string",
//       description: "Slack account email",
//       demandOption: true,
//     })
//     .option("password", {
//       alias: "p",
//       type: "string",
//       description:
//         "Slack account password (optional), also set with env.SLACK_PASS",
//     })
//     .option("domain", {
//       alias: "d",
//       type: "string",
//       description: "Slack workspace domain, e.g https://work.slack.com",
//       demandOption: true,
//     })
//     .option("browser-path", {
//       alias: "b",
//       type: "string",
//       description: "Path to installed Chrome browser",
//       default: "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
//     })
//     .env(false)
//     .locale("en")
//     .version("1.0.4")
//     .parse();
// }

export async function getArgs() {
  const program = new Command();

  program
    .name("getSlackToken")
    .description(
      `Log into your slack account and pull out tokens to run slack from the terminal!

Example usage:
$ deno run \\
      --allow-env \\
      --allow-read \\
      --allow-write=/var/folders \\
      --allow-net=127.0.0.1 \\
      --allow-run="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \\
      ./getSlackToken.ts \\
      --email <email> \\
      --domain https://<domain>.slack.com`,
    )
    .version("1.0.4")
    .requiredOption("-e, --email <email>", "Slack account email", "")
    .requiredOption(
      "-d, --domain <domain>",
      "Slack workspace domain, e.g https://work.slack.com",
      "",
    )
    .option(
      "-p, --password [password]",
      "Slack account password (optional), also set with env.SLACK_PASS",
    )
    .option(
      "-b, --browser-path <path>",
      "Path to installed Chrome browser",
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    )
    .parse(Deno.args);

  const options = program.opts();

  // Manually check required options
  if (!options.email) {
    console.error("Error: --email is required");
    // process.exit(1);
  }
  if (!options.domain) {
    console.error("Error: --domain is required");
    // process.exit(1);
  }

  return options;
}
