import { execSync } from "node:child_process";
import { join, basename } from "node:path";
import {
  DirectoryScanner,
  type ProjectDirectory,
} from "./directory-scanner.ts";
import {
  SessionConfigLoader,
  type KeyedProject,
} from "./SessionConfigLoader.ts";
import { createSessionName, log } from "./utils.ts";
import { KittyListResponseSchema, type KittyTab } from "./kitty.types.ts";

export class KittySessionManager {
  private readonly projectsDir = join(
    process.env.HOME ?? "",
    "dev",
    "projects"
  );
  private readonly sessionEnvVar = "KITTY_SESSION_PROJECT";
  private readonly configLoader: SessionConfigLoader;
  private readonly directoryScanner: DirectoryScanner;

  constructor(configPath?: string) {
    this.configLoader = new SessionConfigLoader(configPath);
    this.directoryScanner = new DirectoryScanner();
  }

  async getProjects(): Promise<string[]> {
    try {
      const result = execSync(
        `fd --type=d --max-depth=1 . "${this.projectsDir}"`,
        {
          encoding: "utf-8",
          timeout: 5000,
        }
      ).trim();

      const projects = result
        ? result
            .split("\n")
            .map((path) => basename(path))
            .filter((name) => name !== basename(this.projectsDir))
            .sort()
        : [];

      log("info", "Found projects", { count: projects.length, projects });
      return projects;
    } catch (error) {
      log("error", "Failed to read projects directory", {
        error: String(error),
      });
      throw error;
    }
  }

  async getConfigBasedProjects(isWork = false): Promise<ProjectDirectory[]> {
    try {
      const config = this.configLoader.loadConfig();
      const expandedConfig = this.configLoader.expandConfig(config);
      const directories = this.configLoader.getAllDirectories(expandedConfig);

      const projects = await this.directoryScanner.findProjectsInDirectories(
        directories
      );

      log("info", "Found config-based projects", {
        count: projects.length,
        directories: directories.length,
        isWork,
      });

      return projects;
    } catch (error) {
      log("error", "Failed to get config-based projects", {
        error: String(error),
        isWork,
      });
      return [];
    }
  }

  async getKeyedProjects(isWork = false): Promise<KeyedProject[]> {
    try {
      const config = this.configLoader.loadConfig();
      const expandedConfig = this.configLoader.expandConfig(config);
      const keyedProjects = this.configLoader.getKeyedProjects(
        expandedConfig,
        isWork
      );

      log("info", "Found keyed projects", {
        count: keyedProjects.length,
        isWork,
      });

      return keyedProjects;
    } catch (error) {
      log("error", "Failed to get keyed projects", {
        error: String(error),
        isWork,
      });
      return [];
    }
  }

  async selectProjectInteractively(isWork = false): Promise<string | null> {
    try {
      const config = this.configLoader.loadConfig();
      const expandedConfig = this.configLoader.expandConfig(config);
      const directories = this.configLoader.getAllDirectories(expandedConfig);

      const selectedPath = await this.directoryScanner.findProjectsWithFzf(
        directories
      );

      if (selectedPath) {
        log("info", "Project selected interactively", { selectedPath });
      }

      return selectedPath;
    } catch (error) {
      log("error", "Failed to select project interactively", {
        error: String(error),
        isWork,
      });
      return null;
    }
  }

  async switchToProjectByKey(key: string, isWork = false): Promise<void> {
    try {
      const keyedProjects = await this.getKeyedProjects(isWork);
      const project = keyedProjects.find(([k]) => k === key);

      if (!project) {
        throw new Error(`No project found for key: ${key}`);
      }

      const [, projectPath] = project;
      const expandedPath = this.expandTilde(projectPath);
      const projectName = basename(expandedPath);

      log("info", "Switching to project by key", {
        key,
        projectPath: expandedPath,
        projectName,
      });

      await this.switchToSessionByPath(expandedPath, projectName);
    } catch (error) {
      log("error", "Failed to switch to project by key", {
        key,
        isWork,
        error: String(error),
      });
      throw error;
    }
  }

  private expandTilde(path: string): string {
    if (path.startsWith("~/")) {
      return join(process.env.HOME ?? "", path.slice(2));
    }
    return path;
  }

  async matchSessionTab(projectName: string): Promise<KittyTab | null> {
    try {
      const socket = await this.getKittySocket();
      const output = execSync(
        `kitten @ --to="${socket}" ls --match=env:${this.sessionEnvVar}=${projectName}`,
        { encoding: "utf8" }
      );
      const parsed = JSON.parse(output);
      const validated = KittyListResponseSchema.parse(parsed);
      log("debug", "Listed --match kitty windows", {
        windowCount: validated.length,
      });

      // If --match found results, return the first tab
      for (const osWindow of validated) {
        const firstTab = osWindow.tabs[0];
        if (firstTab) {
          return firstTab;
        }
      }
      return null;
    } catch (_) {
      log("debug", `no match for ${projectName}`);
      return null;
    }
  }

  async getKittySocket(): Promise<string> {
    // Background processes don't inherit KITTY_LISTEN_ON, so we find the socket
    let socket = process.env.KITTY_LISTEN_ON;
    if (!socket) {
      try {
        // Find the actual socket file
        const socketFile = execSync("ls /tmp/mykitty* 2>/dev/null | head -1", {
          encoding: "utf8",
        }).trim();
        if (socketFile) {
          socket = `unix:${socketFile}`;
        } else {
          socket = "unix:/tmp/mykitty";
        }
      } catch {
        socket = "unix:/tmp/mykitty";
      }
    }
    log("debug", "Using kitty socket", { socket });
    return socket;
  }

  async focusTab(tabId: number): Promise<void> {
    try {
      const socket = await this.getKittySocket();
      execSync(`kitten @ --to="${socket}" focus-tab --match="id:${tabId}"`, {
        encoding: "utf8",
      });
      log("info", "Focused tab", { tabId });
    } catch (error) {
      log("error", "Failed to focus tab", {
        tabId,
        error: String(error),
      });
      throw error;
    }
  }

  async createSessionTab(projectName: string): Promise<void> {
    try {
      const projectPath = join(this.projectsDir, projectName);
      const socket = await this.getKittySocket();

      // Create new tab with project directory and session environment variable
      const cmd = [
        "kitten",
        "@",
        `--to="${socket}"`,
        "launch",
        "--type=tab",
        `--cwd=${projectPath}`,
        `--env=${this.sessionEnvVar}=${projectName}`,
        "--tab-title",
        `"${createSessionName(projectName)}"`,
      ].join(" ");

      log("info", "Creating new session tab", {
        projectName,
        projectPath,
        cmd,
      });

      execSync(cmd, { encoding: "utf8" });

      log("info", "Created new session tab", { projectName });
    } catch (error) {
      log("error", "Failed to create session tab", {
        projectName,
        error: String(error),
      });
      throw error;
    }
  }

  async createSessionTabByPath(
    projectPath: string,
    projectName: string
  ): Promise<void> {
    try {
      const socket = await this.getKittySocket();

      // Create new tab with project directory and session environment variable
      const cmd = [
        "kitten",
        "@",
        `--to="${socket}"`,
        "launch",
        "--type=tab",
        `--cwd=${projectPath}`,
        `--env=${this.sessionEnvVar}=${projectName}`,
        "--tab-title",
        `"${createSessionName(projectName)}"`,
      ].join(" ");

      log("info", "Creating new session tab by path", {
        projectName,
        projectPath,
        cmd,
      });

      execSync(cmd, { encoding: "utf8" });

      log("info", "Created new session tab by path", { projectName });
    } catch (error) {
      log("error", "Failed to create session tab by path", {
        projectName,
        projectPath,
        error: String(error),
      });
      throw error;
    }
  }

  async switchToSessionByPath(
    projectPath: string,
    projectName: string
  ): Promise<void> {
    log("info", "Switching to session by path", { projectPath, projectName });

    try {
      // First, try to find existing session
      const existingTab = await this.matchSessionTab(projectName);

      if (existingTab) {
        // Focus existing session
        log("debug", "existing tab found!", { tabId: existingTab.id });
        await this.focusTab(existingTab.id);
        log("info", "Switched to existing session by path", {
          projectName,
          projectPath,
          tabId: existingTab.id,
        });
      } else {
        // Create new session
        await this.createSessionTabByPath(projectPath, projectName);
        log("info", "Created and switched to new session by path", {
          projectName,
          projectPath,
        });
      }
    } catch (error) {
      log("error", "Failed to switch to session by path", {
        projectName,
        projectPath,
        error: String(error),
      });
      throw error;
    }
  }

  async switchToSession(projectName: string): Promise<void> {
    log("info", "Switching to session", { projectName });

    try {
      // First, try to find existing session
      const existingTab = await this.matchSessionTab(projectName);

      if (existingTab) {
        // Focus existing session
        log("debug", "existing tab found!", { tabId: existingTab.id });
        await this.focusTab(existingTab.id);
        log("info", "Switched to existing session", {
          projectName,
          tabId: existingTab.id,
        });
      } else {
        // Create new session
        await this.createSessionTab(projectName);
        log("info", "Created and switched to new session", { projectName });
      }
    } catch (error) {
      log("error", "Failed to switch to session", {
        projectName,
        error: String(error),
      });
      throw error;
    }
  }

  async listSessions(): Promise<
    Array<{ project: string; hasSession: boolean; tabId?: number }>
  > {
    try {
      const projects = await this.getProjects();
      const sessions = [];

      for (const project of projects) {
        const tab = await this.matchSessionTab(project);
        sessions.push({
          project,
          hasSession: !!tab,
          ...(tab?.id !== undefined && { tabId: tab.id }),
        });
      }

      log("info", "Listed all sessions", { sessionCount: sessions.length });
      return sessions;
    } catch (error) {
      log("error", "Failed to list sessions", { error: String(error) });
      throw error;
    }
  }
}
