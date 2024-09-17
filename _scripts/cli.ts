// @deno-types="npm:@types/yargs@^17"
import yargs from "npm:yargs@^17";
import { z } from "npm:zod@^3";

export const SlackSchema = z
  .object({
    email: z.string().email(),
    password: z.string().optional(),
    domain: z.string().url(),
    browserPath: z.string().optional(),
  })
  .strict();

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
