---
description: New bin tool description
disable-model-invocation: true
---

## Variables

- `FEATURE_SPEC`: $ARGUMENTS

## Brief

Please read @oven/AGENTS.md and then build a new 'bin' command at oven/bin/ following `FEATURE_SPEC`

- Explore other bin commands as an example.
- Consider the cli interface, focussing on parameters you **need**, not what might be useful.
- Ensure to consider comprehensive test cases to add to `oven/tests/`
- Plan the feature and present it to the user before proceeding
- Ensure you run `biome check <file>` , `bun test <test-file>` and `bun typecheck` as you go to verify your work
- Test the command with `bun run <file>` for fast feedback
- Finally ensure `make` passes and builds
