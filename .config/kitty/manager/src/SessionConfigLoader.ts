import { z } from "zod";
import { execSync } from "node:child_process";
import { readFileSync, existsSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const KeyedProjectSchema = z.tuple([z.string(), z.string()]);

const DirSpecialSchema = z.tuple([z.string(), z.array(z.string())]);

export const SessionConfigSchema = z.object({
  dirs: z.array(z.string()),
  dirs_special: z.array(DirSpecialSchema).optional(),
  base: z.array(KeyedProjectSchema),
  personal: z.array(KeyedProjectSchema),
  work: z.array(KeyedProjectSchema),
});

export type SessionConfig = z.infer<typeof SessionConfigSchema>;
export type KeyedProject = z.infer<typeof KeyedProjectSchema>;
export type DirSpecial = z.infer<typeof DirSpecialSchema>;

export interface ExpandedConfig {
  dirs: string[];
  base: KeyedProject[];
  personal: KeyedProject[];
  work: KeyedProject[];
}

export class SessionConfigLoader {
  private readonly configPath: string;

  constructor(configPath?: string) {
    this.configPath =
      configPath ??
      join(process.env.HOME ?? "", ".local", "data", "sessions.json");
  }

  loadConfig(): SessionConfig {
    try {
      const configContent = readFileSync(this.configPath, "utf-8");
      const parsed = JSON.parse(configContent);
      return SessionConfigSchema.parse(parsed);
    } catch (error) {
      throw new Error(
        `Failed to load config from ${this.configPath}: ${error}`
      );
    }
  }

  expandGlobs(dirs: string[]): string[] {
    const expanded: string[] = [];

    for (const dir of dirs) {
      const expandedPath = this.expandTilde(dir);

      if (expandedPath.includes("*")) {
        try {
          // Use fd command for glob expansion since it's available
          const result = execSync(
            `fd --type=d --max-depth=1 . "${expandedPath.replace("*", "")}"`,
            {
              encoding: "utf-8",
              timeout: 5000,
            }
          ).trim();

          if (result) {
            const matches = result.split("\n").filter(Boolean);
            expanded.push(...matches);
          }
        } catch {
          // Fallback: try without glob expansion
          const baseDir = expandedPath.replace("/*", "");
          if (existsSync(baseDir)) {
            expanded.push(baseDir);
          }
        }
      } else {
        expanded.push(expandedPath);
      }
    }

    return expanded;
  }

  executeSpecialDirs(dirsSpecial: DirSpecial[]): string[] {
    const results: string[] = [];

    for (const [cmd, args] of dirsSpecial) {
      try {
        const output = execSync(`${cmd} ${args.join(" ")}`, {
          encoding: "utf-8",
          timeout: 10000,
        }).trim();

        if (output) {
          results.push(output);
        }
      } catch {
        // Ignore command errors, continue with other commands
      }
    }

    return results;
  }

  expandConfig(config: SessionConfig): ExpandedConfig {
    // Check cache first
    const cacheFile = process.env.CACHE_FILE;
    if (cacheFile && existsSync(cacheFile)) {
      try {
        const cacheContent = readFileSync(cacheFile, "utf-8");
        const cachedConfig = JSON.parse(cacheContent) as ExpandedConfig;
        return cachedConfig;
      } catch {
        // Cache is corrupted or invalid, continue with fresh expansion
      }
    }

    // Perform expansion
    let expandedDirs = this.expandGlobs(config.dirs);

    if (config.dirs_special) {
      const specialDirs = this.executeSpecialDirs(config.dirs_special);
      expandedDirs = [...expandedDirs, ...specialDirs];
    }

    const expandedConfig: ExpandedConfig = {
      dirs: expandedDirs,
      base: config.base,
      personal: config.personal,
      work: config.work,
    };

    // Write to cache
    if (cacheFile) {
      try {
        writeFileSync(
          cacheFile,
          JSON.stringify(expandedConfig, null, 2),
          "utf-8"
        );
      } catch {
        // Ignore cache write errors
      }
    }

    return expandedConfig;
  }

  getAllDirectories(config: ExpandedConfig): string[] {
    return config.dirs
      .map((dir) => this.expandTilde(dir))
      .filter((dir) => existsSync(dir));
  }

  getKeyedProjects(config: ExpandedConfig, isWork = false): KeyedProject[] {
    const baseProjects = config.base;
    const contextProjects = isWork ? config.work : config.personal;

    return [...baseProjects, ...contextProjects];
  }

  private expandTilde(path: string): string {
    if (path.startsWith("~/")) {
      return join(process.env.HOME ?? "", path.slice(2));
    }
    if (path.startsWith("`") && path.endsWith("`")) {
      // Handle backtick-quoted paths (spaces in names)
      return this.expandTilde(path.slice(1, -1));
    }
    return path;
  }
}
