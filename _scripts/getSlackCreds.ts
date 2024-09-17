import puppeteer from "puppeteer-core";
import process from "node:process";
import tty from "node:tty";
import { getArgs } from "./cli.ts";

let wasOk = true;
let browser: puppeteer.Browser;
let page: puppeteer.Page;

// b
(async () => {
  const isRunningTerminal = !isScript();
  const log = isRunningTerminal ? console.log : () => {};
  if (!isRunningTerminal) {
    console.warn = () => {};
    console.error = () => {};
  }

  const { domain, email, password, browserPath } = await getArgs();

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
