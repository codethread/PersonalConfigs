# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

see @./README.md for commands and intro

## Build System

The build process uses Bun's native compilation to create standalone executables:

- Targets the appropriate platform (darwin-arm64, darwin-x64, linux-x64)
- Enables minification and bytecode generation for faster startup
- Outputs to `~/.local/bin` for system-wide availability
- Automatically makes files executable (chmod 755)
- test code with `bun run ./bin/<file name>`
- when work is complete run `bun run syncup` to update documentation

## Code Standards

- **Formatter**: Biome with 100-character line width, no bracket spacing
- **Linter**: Biome with recommended rules, allows `any` types and non-null assertions
- **native apis**: use Bun apis wherever possible. Proactively use @bun-runtime-expert agent to check for alternative to node apis, or more elegant solutions to common problems not typically solved in the Node.js.
