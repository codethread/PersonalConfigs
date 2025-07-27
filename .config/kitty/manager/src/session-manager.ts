import { basename } from "node:path";
import { Command } from "commander";
import { KittySessionManager } from "./KittySessionManager.ts";
import { dumpLog, listLogFile } from "./utils.ts";

// CLI interface
async function main() {
  const program = new Command();

  // Lazy initialization of manager to avoid config loading for help commands
  const getManager = () => new KittySessionManager();

  program
    .name("ksm")
    .description(
      "Kitty Session Manager - tmux-style session management for kitty"
    )
    .version("1.0.0");

  // Default command - list sessions
  program
    .command("list")
    .alias("ls")
    .description("List all available sessions")
    .action(async () => {
      try {
        const manager = getManager();
        console.log("Available sessions:");
        const sessions = await manager.listSessions();

        for (const session of sessions) {
          const status = session.hasSession ? "✓ (active)" : "○ (available)";
          const tabInfo = session.tabId ? ` [tab:${session.tabId}]` : "";
          console.log(`  ${status} ${session.project}${tabInfo}`);
        }
      } catch (error) {
        console.error(
          `Error: ${error instanceof Error ? error.message : String(error)}`
        );
        process.exit(1);
      }
    });

  // Switch to specific session
  program
    .command("switch")
    .alias("sw")
    .description("Switch to or create session for project")
    .argument("<project>", "Project name to switch to")
    .action(async (projectName: string) => {
      try {
        const manager = getManager();
        await manager.switchToSession(projectName);
        console.log(`Switched to session: ${projectName}`);
      } catch (error) {
        console.error(
          `Error: ${error instanceof Error ? error.message : String(error)}`
        );
        process.exit(1);
      }
    });

  // List project names only
  program
    .command("projects")
    .description("List available project names only")
    .action(async () => {
      try {
        const manager = getManager();
        const projects = await manager.getProjects();
        for (const project of projects) {
          console.log(project);
        }
      } catch (error) {
        console.error(
          `Error: ${error instanceof Error ? error.message : String(error)}`
        );
        process.exit(1);
      }
    });

  // List config-based projects
  program
    .command("config-projects")
    .alias("config")
    .description("List projects from configuration")
    .option("-w, --work", "Use work context instead of personal")
    .action(async (options) => {
      try {
        const manager = getManager();
        const projects = await manager.getConfigBasedProjects(options.work);
        for (const project of projects) {
          const indicator = project.isGitRepo ? "📁" : "📄";
          console.log(`${indicator} ${project.name} (${project.path})`);
        }
      } catch (error) {
        console.error(
          `Error: ${error instanceof Error ? error.message : String(error)}`
        );
        process.exit(1);
      }
    });

  // List keyed projects
  program
    .command("keyed-projects")
    .alias("keys")
    .description("List keyed projects (P0-P9)")
    .option("-w, --work", "Use work context instead of personal")
    .action(async (options) => {
      try {
        const manager = getManager();
        const projects = await manager.getKeyedProjects(options.work);
        for (const [key, path] of projects) {
          console.log(`${key}: ${path}`);
        }
      } catch (error) {
        console.error(
          `Error: ${error instanceof Error ? error.message : String(error)}`
        );
        process.exit(1);
      }
    });

  // Interactive project selection
  program
    .command("select")
    .alias("s")
    .description("Interactive project selection with fzf")
    .option("-w, --work", "Use work context instead of personal")
    .action(async (options) => {
      try {
        const manager = getManager();
        const selectedPath = await manager.selectProjectInteractively(
          options.work
        );
        if (selectedPath) {
          const projectName = basename(selectedPath);
          await manager.switchToSessionByPath(selectedPath, projectName);
          console.log(`Switched to session: ${projectName} (${selectedPath})`);
        } else {
          console.log("No project selected");
        }
      } catch (error) {
        console.error(
          `Error: ${error instanceof Error ? error.message : String(error)}`
        );
        process.exit(1);
      }
    });

  // Switch by key
  program
    .command("key")
    .alias("k")
    .description("Switch to project by key (P0, P1, etc.)")
    .argument("<key>", "Project key (P0-P9)")
    .option("-w, --work", "Use work context instead of personal")
    .action(async (key: string, options) => {
      try {
        const manager = getManager();
        await manager.switchToProjectByKey(key, options.work);
        console.log(`Switched to session by key: ${key}`);
      } catch (error) {
        console.error(
          `Error: ${error instanceof Error ? error.message : String(error)}`
        );
        process.exit(1);
      }
    });

  program
    .command("logs")
    .option("-p, --path", "Show path")
    .action(async (_, options) => {
      if (options.path) {
        console.log(listLogFile());
      } else {
        console.log(dumpLog());
      }
    });

  // Set default action to list if no command provided
  program.action(async () => {
    try {
      const manager = getManager();
      console.log("Available sessions:");
      const sessions = await manager.listSessions();

      for (const session of sessions) {
        const status = session.hasSession ? "✓ (active)" : "○ (available)";
        const tabInfo = session.tabId ? ` [tab:${session.tabId}]` : "";
        console.log(`  ${status} ${session.project}${tabInfo}`);
      }
    } catch (error) {
      console.error(
        `Error: ${error instanceof Error ? error.message : String(error)}`
      );
      process.exit(1);
    }
  });

  await program.parseAsync(process.argv);
}

main();
