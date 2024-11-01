#!/usr/bin/env bun

import { parseArgs } from "util";
import { bar, foo } from "lib";

console.log(foo());
console.log(bar());

let cmds = [foo, bar];

import { $ } from "bun";

await $`aerospace resize smart -200`;

// @ts-ignore
// await import("@ct/work")
//   .then((mods) => {
//     const {
//       values,
//       positionals: [title, url],
//     } = parseArgs({
//       args: Bun.argv.slice(2),
//       allowPositionals: true,
//     });
//
//     // do whatever to add these commands to the global scope
//     if (mods.teamsStatus) {
//       // mods.postMr().then((res) => console.log(res));
//       mods
//         // .teamsStatus("back in a mo", { free: false, pinned: true })
//         // .teamsStatus("hi there", { free: true, pinned: false })
//         .teamsMr({ title, url })
//         .then((r) => console.log(r))
//         .catch((r) => console.log(r));
//     }
//     console.log(mods);
//     console.log(Object.values(mods));
//     for (const mod of Object.values(mods)) {
//       cmds.push(mod);
//     }
//   })
//   .catch(() => {
//     // ignore, not available
//   });
//
// // console.log(cmds.map((cmd) => cmd()));
