import { $ } from "bun";

const logFile = Bun.file("/tmp/log.txt");
const loggerWriter = logFile.writer();

const IDS = { kitty: "kitty" } as const;

async function main() {
  const main: Win[] = (
    await $`aerospace list-windows --workspace 1 --json`.json()
  )
    .filter(Boolean)
    .filter((w) => !["pomo", "com.apple.finder"].includes(w["app-name"]));
  const alt: Win[] = (
    await $`aerospace list-windows --workspace 2 --json`.json()
  )
    .filter(Boolean)
    .filter((w) => !["pomo", "com.apple.finder"].includes(w["app-name"]));
  console.log(main);
  console.log(alt);

  if (isKittyFull(main)) {
    console.log("full");
    await shareMainSpace(main, alt);
  } else {
    console.log("not full");
    await focusKitty(main, alt);
  }

  loggerWriter.end();
}

function isKittyFull(wins: Win[]) {
  return wins.length === 1 && wins.some((w) => w["app-name"] === IDS.kitty);
}

async function focusKitty(main: Win[], alt: Win[]) {
  const others = main.filter((w) => w["app-name"] !== IDS.kitty);
  await Promise.all(
    others.map(async (win) => {
      const id = win["window-id"];
      console.log("move to 2", win, id);
      await $`aerospace move-node-to-workspace --window-id ${id} 2`;
    }),
  );
}

async function shareMainSpace(main: Win[], alt: Win[]) {
  await Promise.all(
    alt.map(async (win) => {
      await $`aerospace move-node-to-workspace --window-id ${win["window-id"]} 1`;
    }),
  );
}

function log(...msg: any) {
  loggerWriter.write(JSON.stringify(msg));
}

main();

interface Win {
  ["app-name"]: string;
  ["window-id"]: number;
  ["window-title"]: string;
}
