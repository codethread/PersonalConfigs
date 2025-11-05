import { serve } from "bun";
import { parseArgs } from "util";
import index from "./index.html";
import { parseSessionLogs } from "./parser";
import type { SessionData } from "./types";

// Parse CLI arguments
const { values } = parseArgs({
  args: Bun.argv.slice(2),
  options: {
    session: { type: "string", short: "s" },
    help: { type: "boolean", short: "h" },
  },
  strict: false,
});

if (values.help || !values.session) {
  console.log(`cc-inspect - Visualize Claude Code session logs

Usage: cc-inspect --session <path-to-session.jsonl>

Options:
  --session, -s  Path to session log file (required)
  --help, -h     Show this help message

Examples:
  cc-inspect -s ~/.claude/projects/-Users-foo/session-id.jsonl
`);
  process.exit(values.help ? 0 : 1);
}

let sessionData: SessionData | null = null;

// Parse session logs on startup
const sessionPath = values.session as string;
console.log(`📖 Parsing session logs: ${sessionPath}`);
try {
  sessionData = await parseSessionLogs(sessionPath);
  console.log(`✅ Parsed ${sessionData.allEvents.length} events from ${sessionData.mainAgent.children.length + 1} agents`);
} catch (error) {
  console.error("Failed to parse session logs:", error);
  process.exit(1);
}

const server = serve({
  port: Number.parseInt(process.env.PORT || "5555"),
  routes: {
    // API endpoint to get session data
    "/api/session": () => {
      if (!sessionData) {
        return new Response(JSON.stringify({ error: "No session data loaded" }), {
          status: 500,
          headers: { "Content-Type": "application/json" },
        });
      }

      return new Response(JSON.stringify(sessionData), {
        headers: { "Content-Type": "application/json" },
      });
    },

    // Serve index.html for all other routes
    "/*": index,
  },

  development: process.env.NODE_ENV !== "production" && {
    // Enable browser hot reloading in development
    hmr: true,

    // Echo console logs from the browser to the server
    console: true,
  },
});

console.log(`🚀 Server running at ${server.url}`);
