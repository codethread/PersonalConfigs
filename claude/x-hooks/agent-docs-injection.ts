#!/usr/bin/env bun

import { $ } from "bun";

// generate a bullet list of all docs in the codebase
async function main() {
  const files = await $`rg --files --follow | rg "(AGENTS|README)"`.text();
  console.log(`<project-context>
IMPORTANT: Ensure you read related documentation when working in nested areas of the codebase
Project documentation:
${files
  .trim()
  .split("\n")
  .map((f) => "- " + f)
  .join("\n")}
</project-context>`);
}

main();
