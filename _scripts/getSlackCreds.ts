// @deno-types="npm:@types/yargs@^17"
import yargs from "npm:yargs@^17";
import puppeteer from "npm:puppeteer-core@^23";
import process from "node:process";
import tty from "node:tty";

let wasOk = true;
let browser: puppeteer.Browser;
let page: puppeteer.Page;

(async () => {
  const isRunningTerminal = !isScript();
  const log = isRunningTerminal ? console.log : () => {};
  if (!isRunningTerminal) {
    console.warn = () => {};
    console.error = () => {};
  }

  const { email, password, domain, browserPath } = await yargs(Deno.args)
    .usage(
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
    .wrap(null)
    .option("email", {
      alias: "e",
      type: "string",
      description: "Slack account email",
      demandOption: true,
    })
    .option("password", {
      alias: "p",
      type: "string",
      description:
        "Slack account password (optional), also set with env.SLACK_PASS",
    })
    .option("domain", {
      alias: "d",
      type: "string",
      description: "Slack workspace domain, e.g https://work.slack.com",
      demandOption: true,
    })
    .option("browser-path", {
      alias: "b",
      type: "string",
      description: "Path to installed Chrome browser",
      default: "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    })
    .env(false)
    .locale("en")
    .version("1.0.0")
    .parse();

  if (!/^https:\/\/\w+.slack.\w+$/.test(domain))
    throw new Error("domain should be a full url like https://foo.slack.com");

  log("Getting a slack token...");

  const path = "/customize/emoji";
  const target = `${domain}${path}`;
  browser = await puppeteer.launch({
    browser: "chrome",
    executablePath: browserPath,
    headless: false,
  });
  page = await browser.newPage();

  // naviagate
  {
    log(`navigating ${target}`);
    await page.goto(target);
    await page.locator("#index_google_sign_in_with_google").click();
  }

  // input email
  {
    log(`logging in as ${email}`);
    await page.waitForSelector("#identifierId");
    await page.type("#identifierId", email, { delay: 150 });
    await delay(1000);
    await page.keyboard.press("Enter");
  }

  // input password
  {
    await page.waitForNavigation();
    await page.waitForSelector('input[name="Passwd"]');
    // input password or wait on user
    const userPassword = password ?? Deno.env.get("SLACK_PASS");
    if (userPassword) {
      log(`typing password`);
      await delay(1500);
      await page.type('input[name="Passwd"]', userPassword, { delay: 150 });
      await delay(1000);
      await page.keyboard.press("Enter");
    } else {
      log("Please input your password");
    }
  }

  // then google 2fa
  log(`waiting on 2fa`);
  await page.waitForNavigation();
  await are_we_there_yet("/customize/emoji", log);

  // grab important bits
  log(`logging out`);
  const token = await page.evaluate("TS.boot_data.api_token");
  const [d, ds] = await Promise.all([getCookie("d"), getCookie("d-s")]);
  const json = { d: d?.value, ds: ds?.value, token };
  const out = new TextEncoder().encode(JSON.stringify(json));
  Deno.stdout.writeSync(out);
})()
  .catch((e) => {
    wasOk = false;
    const out = new TextEncoder().encode("❌ Error!\n" + e.message);
    Deno.stderr.writeSync(out);
  })
  .finally(async () => {
    if (browser) {
      await browser.close();
    }
    Deno.exit(wasOk ? 0 : 1);
  });

//*****************************************************************************//
// HELPERS
//*****************************************************************************//
function delay(time: number) {
  return new Promise(function (resolve) {
    setTimeout(resolve, time);
  });
} //

async function are_we_there_yet(path: string, log: Console["log"]) {
  while (true) {
    try {
      log("checking");
      await page.waitForNavigation({
        timeout: 0,
      });

      await page.waitForFunction(`window.location.pathname == '${path}'`);

      break;
    } catch (_) {
      continue;
    }
  }
}

async function getCookie(name: string) {
  const cookies = await page.cookies();
  return cookies.find((c) => c.name === name);
}
/** if true this is running via another script, false means user running in a terminal */
function isScript() {
  if (tty.isatty(process.stdout.fd)) {
    return false;
  } else {
    return true;
  }
}
