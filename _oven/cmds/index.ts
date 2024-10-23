#!/usr/bin/env bun

import { bar, foo } from "lib";

console.log(foo());
console.log(bar());

let cmds = [foo, bar];

// @ts-ignore
await import("@ct/work")
  .then((mods) => {
    // do whatever to add these commands to the global scope
    console.log(mods);
    console.log(Object.values(mods));
    for (const mod of Object.values(mods)) {
      cmds.push(mod);
    }
  })
  .catch(() => {
    // ignore, not available
  });

console.log(cmds.map((cmd) => cmd()));
