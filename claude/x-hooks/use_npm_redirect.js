const fs = require("node:fs");
const path = require("node:path");

function main() {
  try {
    // Read input data from stdin
    let inputData = "";
    process.stdin.setEncoding("utf8");

    process.stdin.on("data", (chunk) => {
      inputData += chunk;
    });

    process.stdin.on("end", () => {
      try {
        const data = JSON.parse(inputData);

        const toolInput = data.tool_input || {};
        const command = toolInput.command || "";

        if (!command) {
          process.exit(0);
        }

        // Detect the appropriate package manager based on lock files
        function detectPackageManager(targetDir = null) {
          // If command has cd, extract the target directory
          let searchDir = process.cwd();
          if (targetDir) {
            searchDir = targetDir;
          } else if (command.includes("cd ")) {
            const cdMatch = command.match(/cd\s+([^\s&&]+)/);
            if (cdMatch) {
              searchDir = cdMatch[1];
              // Handle relative paths
              if (!path.isAbsolute(searchDir)) {
                searchDir = path.resolve(process.cwd(), searchDir);
              }
            }
          }

          // Walk up the directory tree to find lock files
          let currentDir = searchDir;
          while (currentDir !== path.dirname(currentDir)) {
            if (fs.existsSync(path.join(currentDir, "pnpm-lock.yaml"))) {
              return "pnpm";
            } else if (fs.existsSync(path.join(currentDir, "bun.lockb"))) {
              return "bun";
            } else if (fs.existsSync(path.join(currentDir, "yarn.lock"))) {
              return "yarn";
            } else if (
              fs.existsSync(path.join(currentDir, "package-lock.json"))
            ) {
              return "npm";
            }
            currentDir = path.dirname(currentDir);
          }
          return "npm"; // default fallback
        }

        const detectedPM = detectPackageManager();

        // Check for package manager commands
        const npmPattern = /\bnpm\s+/;
        const npxPattern = /\bnpx\s+/;
        const yarnPattern = /\byarn\s+/;
        const pnpmPattern = /\bpnpm\s+/;

        let blockedCommand = null;
        let suggestedCommand = null;

        if (npmPattern.test(command) && detectedPM !== "npm") {
          blockedCommand = command;
          suggestedCommand = command.replace(/\bnpm\b/, detectedPM);
        } else if (npxPattern.test(command) && detectedPM !== "npm") {
          blockedCommand = command;
          const execCommand =
            detectedPM === "bun"
              ? "bunx"
              : detectedPM === "pnpm"
              ? "pnpm exec"
              : detectedPM === "yarn"
              ? "yarn dlx"
              : "npx";
          suggestedCommand = command.replace(/\bnpx\b/, execCommand);
        } else if (yarnPattern.test(command) && detectedPM !== "yarn") {
          blockedCommand = command;
          suggestedCommand = command.replace(/\byarn\b/, detectedPM);
        } else if (pnpmPattern.test(command) && detectedPM !== "pnpm") {
          blockedCommand = command;
          suggestedCommand = command.replace(/\bpnpm\b/, detectedPM);
        }

        if (blockedCommand) {
          // Send error message to stderr for LLM to see
          console.error(
            `Error: Use '${suggestedCommand}' instead of '${blockedCommand}'`
          );
          // Exit with code 2 to signal LLM to correct
          process.exit(2);
        }
      } catch (parseError) {
        console.error(`Error parsing JSON input: ${parseError.message}`);
        process.exit(1);
      }
    });
  } catch (error) {
    console.error(`Error in use_npm_redirect hook: ${error.message}`);
    process.exit(1);
  }
}

main();
