import { serve } from "bun";
import { parseArgs } from "util";
import { join, basename, resolve } from "node:path";
import { readdir, stat } from "node:fs/promises";
import index from "./index.html";
import { parseSessionLogs, ParseError } from "./parser";
import type { SessionData, DirectoriesResponse, SessionsResponse, SessionDataResponse } from "./types";

// Get the default Claude projects directory
const CLAUDE_PROJECTS_DIR = join(process.env.HOME || "~", ".claude", "projects");

// Security: Validate directory parameter to prevent path traversal
function isValidDirectory(directory: string): boolean {
	// Reject if contains ".." or path separators
	return !directory.includes("..") && !directory.includes("/") && !directory.includes("\\");
}

// Security: Validate session path to ensure it's within CLAUDE_PROJECTS_DIR
function isValidSessionPath(sessionPath: string): boolean {
	const resolvedPath = resolve(sessionPath);
	const resolvedBaseDir = resolve(CLAUDE_PROJECTS_DIR);
	return resolvedPath.startsWith(resolvedBaseDir);
}

async function main() {
  // Parse CLI arguments
  const { values } = parseArgs({
    args: Bun.argv.slice(2),
    options: {
      session: { type: "string", short: "s" },
      help: { type: "boolean", short: "h" },
    },
    strict: false,
  });

  if (values.help) {
    console.log(`cc-inspect - Visualize Claude Code session logs

Usage: cc-inspect [--session <path-to-session.jsonl>]

Options:
  --session, -s  Path to session log file (optional - can select via UI)
  --help, -h     Show this help message

Examples:
  cc-inspect                                        # Start server with UI selector
  cc-inspect -s ~/.claude/projects/-Users-foo/session-id.jsonl  # Load specific session
`);
    process.exit(0);
  }

  // Track CLI-provided session path
  let cliSessionPath: string | undefined;

  // Pre-load session if provided via CLI (for validation)
  if (values.session) {
    cliSessionPath = values.session as string;
    console.log(`📖 Validating session logs: ${cliSessionPath}`);
    try {
      const sessionData = await parseSessionLogs(cliSessionPath);
      console.log(`✅ Validated ${sessionData.allEvents.length} events from ${sessionData.mainAgent.children.length + 1} agents`);
    } catch (error) {
      console.error("Failed to parse session logs:", error);
      process.exit(1);
    }
  }

  const server = serve({
    port: Number.parseInt(process.env.PORT || "5555"),
    routes: {
      // API endpoint to get list of project directories
      "/api/directories": async (): Promise<Response> => {
        try {
          const entries = await readdir(CLAUDE_PROJECTS_DIR, { withFileTypes: true });
          const directories = entries
            .filter((entry) => entry.isDirectory())
            .map((entry) => entry.name)
            .sort();

          // Filter to only directories that contain session files
          const validDirectories: string[] = [];
          for (const dir of directories) {
            const dirPath = join(CLAUDE_PROJECTS_DIR, dir);
            try {
              const files = await readdir(dirPath);
              const hasSessionFiles = files.some((file) => file.endsWith(".jsonl") && !file.startsWith("agent-"));
              if (hasSessionFiles) {
                validDirectories.push(dir);
              }
            } catch {
              // Skip directories we can't read
              continue;
            }
          }

          const response: DirectoriesResponse = { status: "success", directories: validDirectories };
          return new Response(JSON.stringify(response), {
            headers: { "Content-Type": "application/json" },
          });
        } catch (err) {
          const message = err instanceof Error ? err.message : String(err);
          const response: DirectoriesResponse = { status: "error", error: `Failed to read directories: ${message}` };
          return new Response(JSON.stringify(response), {
            status: 500,
            headers: { "Content-Type": "application/json" },
          });
        }
      },

      // API endpoint to get list of session files in a directory
      "/api/sessions": async (req): Promise<Response> => {
        const url = new URL(req.url);
        const directory = url.searchParams.get("directory");

        if (!directory) {
          const response: SessionsResponse = { status: "error", error: "Missing directory parameter" };
          return new Response(JSON.stringify(response), {
            status: 400,
            headers: { "Content-Type": "application/json" },
          });
        }

        // Security: Validate directory parameter to prevent path traversal
        if (!isValidDirectory(directory)) {
          const response: SessionsResponse = { status: "error", error: "Invalid directory parameter" };
          return new Response(JSON.stringify(response), {
            status: 400,
            headers: { "Content-Type": "application/json" },
          });
        }

        try {
          const dirPath = join(CLAUDE_PROJECTS_DIR, directory);
          const files = await readdir(dirPath);

          // Filter for session files (exclude agent logs)
          const sessionFiles = files.filter((file) => file.endsWith(".jsonl") && !file.startsWith("agent-"));

          // Get file stats for each session
          const sessions = await Promise.all(
            sessionFiles.map(async (file) => {
              const filePath = join(dirPath, file);
              const stats = await stat(filePath);
              return {
                filename: file,
                path: filePath,
                sessionId: file.replace(".jsonl", ""),
                modifiedAt: stats.mtime.toISOString(),
                size: stats.size,
              };
            })
          );

          // Sort by modification time, most recent first
          sessions.sort((a, b) => new Date(b.modifiedAt).getTime() - new Date(a.modifiedAt).getTime());

          const response: SessionsResponse = { status: "success", sessions };
          return new Response(JSON.stringify(response), {
            headers: { "Content-Type": "application/json" },
          });
        } catch (err) {
          const message = err instanceof Error ? err.message : String(err);
          const response: SessionsResponse = { status: "error", error: `Failed to read sessions: ${message}` };
          return new Response(JSON.stringify(response), {
            status: 500,
            headers: { "Content-Type": "application/json" },
          });
        }
      },

      // API endpoint to load and parse a specific session
      "/api/session": async (req): Promise<Response> => {
        const url = new URL(req.url);
        const sessionPath = url.searchParams.get("path") || cliSessionPath;

        if (!sessionPath) {
          const response: SessionDataResponse = { status: "error", error: "Missing path parameter" };
          return new Response(JSON.stringify(response), {
            status: 400,
            headers: { "Content-Type": "application/json" },
          });
        }

        // Security: Validate session path to ensure it's within CLAUDE_PROJECTS_DIR
        if (!isValidSessionPath(sessionPath)) {
          const response: SessionDataResponse = { status: "error", error: "Invalid session path" };
          return new Response(JSON.stringify(response), {
            status: 400,
            headers: { "Content-Type": "application/json" },
          });
        }

        try {
          // Parse the session
          console.log(`📖 Parsing session logs: ${sessionPath}`);
          const sessionData = await parseSessionLogs(sessionPath);
          console.log(`✅ Parsed ${sessionData.allEvents.length} events from ${sessionData.mainAgent.children.length + 1} agents`);

          const response: SessionDataResponse = { status: "success", data: sessionData };
          return new Response(JSON.stringify(response), {
            headers: { "Content-Type": "application/json" },
          });
        } catch (err) {
          // Log detailed error information to console
          if (err instanceof ParseError) {
            console.error("❌ Parse error with detailed information:");
            console.error(err.toString());
            console.error("\n📄 Full raw log line:");
            console.error(err.rawLine);
          } else {
            const message = err instanceof Error ? err.message : String(err);
            console.error("❌ Failed to parse session:", message);
            if (err instanceof Error && err.stack) {
              console.error(err.stack);
            }
          }

          // Send simple error message to frontend
          const message = err instanceof Error ? err.message : String(err);
          const response: SessionDataResponse = { status: "error", error: `Failed to parse session: ${message}` };
          return new Response(JSON.stringify(response), {
            status: 500,
            headers: { "Content-Type": "application/json" },
          });
        }
      },

      // Serve index.html for all other routes
      "/*": index,
    },

    development: false,
  });

  console.log(`🚀 Server running at ${server.url}`);
  if (!values.session) {
    console.log(`📁 Select a session from the UI to view`);
  }
}

main();
