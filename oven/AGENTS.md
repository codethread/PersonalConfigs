# Bun cli tools - Agentic documentation

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

IMPORTANT: see `./README.md` for commands and intro

## Build System

The build process uses Bun's native compilation to create standalone executables:

- Targets the appropriate platform (darwin-arm64, darwin-x64, linux-x64)
- Enables minification and bytecode generation for faster startup
- Outputs to `~/.local/bin` for system-wide availability
- Automatically makes files executable (chmod 755)
- test code with `bun run ./bin/<file name>`
- when work is complete run `bun run verify` to validate all checks, build the binaries and update the docs

## Code Standards

### Formatting, Linting & Type checking

- **Formatter**: Biome with 100-character line width, no bracket spacing
- **Linter**: Biome with recommended rules, allows `any` types and non-null assertions
- **Types**: Typescript via `tsc`, relatively strict

### Best Practices

1. **Use Bun native APIs**
   - Use `import {$} from "bun"` for shell commands instead of Node's child_process
   - Use Bun's built-in APIs wherever possible
   - Proactively use @bun-runtime-expert agent to check for alternatives to Node APIs

2. **Argument parsing**
   - Use `import {parseArgs} from "util"` for CLI argument parsing
   - Define clear option types and provide help documentation

3. **Testability**
   - Export core logic as function separate from CLI wrapper
   - Use `if (import.meta.main)` to conditionally run CLI code
   - This allows function to be imported and tested independently

4. **Test structure**
   - Tests live in `oven/tests/` with matching names (e.g., `ghub.ts` → `ghub.test.ts`)
   - Use Bun's built-in test runner: `import {describe, test, expect} from "bun:test"`
   - Mock external dependencies when needed but favour fake implementations, e.g create temporary directories and use real IO calls wherever possible
   - Make use of bun snapshot tests to verify outputs
   - Test the exported lib function directly, not the CLI wrapper
   - Example test structure:

   ```typescript
   import { describe, test, expect } from "bun:test";
   import { myToolLib } from "../bin/my-tool";

   describe("myToolLib", () => {
     test("should handle valid input", async () => {
       const result = await myToolLib({ option: "value" });
       expect(result.success).toBe(true);
     });

     test("should throw on invalid input", async () => {
       expect(() => myToolLib({ invalid: true })).toThrow("Expected error message");
     });
   });
   ```

5. **Output**
   - All cli functions (unless using interactive tty utils) should capture information and then pass this to a final `report` function which is common across `bin` modules
   - `report` can then write to stdout in typical scenarios, but be extended to support other outputs if appropriate.
   - this allows for consistent testing and a more functional style of avoiding IO

6. **Error handling patterns**
   - Use specific error types when possible: `throw new Error("Descriptive message")`
   - For async operations, always catch and re-throw with context
   - Use `reportError()` to ensure consistent error formatting
   - Example patterns:

   ```typescript
   try {
     const result = await riskyOperation();
     return result;
   } catch (error) {
     throw new Error(`Failed to complete operation: ${error.message}`);
   }
   ```

7. **Script structure**
   - follows 'clean code' style of high level functions first, going down to low level
   - this is an organizational pattern for readability, not relying on hoisting
   - help > CoreInterface > const definitions > main > lib > functions > execute main
   - main function should focus on cli parsing, lib invocation, reporting and error reporting. All 'logic' belongs in the 'lib' function

### Example pseudo script

```typescript
// :module: Description of what this tool does

import { $ } from "bun";
import { parseArgs } from "util";
import { report, reportError } from "../shared/report";

// always first for easy discoverability
function showHelp() {
  console.log(`
Usage: my-tool [options]

Options:
  --input, -i    Input file path (required)
  --output, -o   Output file path (optional)
  --verbose, -v  Enable verbose logging
  --help, -h     Show this help message

Examples:
  my-tool -i input.txt -o output.txt
  my-tool --input data.json --verbose
`);
  process.exit(0);
}

// interface to core logic if needed
export interface CoreLogicOptions {
  input: string;
  output?: string;
  verbose?: boolean;
}

// constants if needed
const OUTPUT_FILE = "some constant";

// CLI wrapper next
async function main() {
  const { values } = parseArgs({
    args: Bun.argv.slice(2),
    options: {
      input: { type: "string", short: "i" },
      output: { type: "string", short: "o" },
      verbose: { type: "boolean", short: "v" },
      help: { type: "boolean", short: "h" },
    },
  });

  if (values.help) {
    showHelp();
  }

  if (!values.input) {
    console.error("Error: --input is required");
    showHelp();
  }

  // Handle CLI logic
  try {
    const result = await doBinLib({
      input: values.input!,
      output: values.output,
      verbose: values.verbose,
    });
    report(result);
  } catch (err) {
    reportError(err);
    process.exit(1);
  }
}

// Exported lib functionality matching format `<bin name camelCased>Lib`
export async function doBinLib(options: CoreLogicOptions) {
  if (options.verbose) {
    console.log(`Processing input: ${options.input}`);
  }

  const data = await readInputFile(options.input);
  const processed = processData(data);

  if (options.output) {
    await writeOutputFile(options.output, processed);
    return { success: true, outputPath: options.output };
  }

  return { success: true, data: processed };
}

// implementation functions defined **after** call site
async function readInputFile(path: string): Promise<string> {
  try {
    return await Bun.file(path).text();
  } catch (error) {
    throw new Error(`Failed to read input file: ${error.message}`);
  }
}

function processData(data: string): string {
  // Process the data according to tool logic
  return data.trim().toUpperCase();
}

async function writeOutputFile(path: string, data: string): Promise<void> {
  try {
    await Bun.write(path, data);
  } catch (error) {
    throw new Error(`Failed to write output file: ${error.message}`);
  }
}

// Only run if executed directly
if (import.meta.main) {
  main();
}
```
