import { execSync } from "node:child_process";
import { stat } from "node:fs/promises";
import { join, basename } from "node:path";

export interface ProjectDirectory {
  name: string;
  path: string;
  isGitRepo?: boolean;
}

export class DirectoryScanner {
  async findProjectsInDirectories(
    directories: string[]
  ): Promise<ProjectDirectory[]> {
    const processDirectory = async (
      dir: string
    ): Promise<ProjectDirectory[]> => {
      try {
        const result = execSync(`fd --type=d --max-depth=1 . "${dir}"`, {
          encoding: "utf-8",
          timeout: 5000,
        }).trim();

        if (!result) return [];

        const paths = result.split("\n");

        const projects = await Promise.all(
          paths
            .filter((path) => basename(path) !== basename(dir)) // Exclude parent directory
            .map(async (fullPath) => {
              const isGitRepo = await this.isGitRepository(fullPath);
              return {
                name: basename(fullPath),
                path: fullPath,
                isGitRepo,
              };
            })
        );

        return projects;
      } catch {
        // Skip directories that can't be read
        return [];
      }
    };

    const allProjects = await Promise.all(
      directories.map((dir) => processDirectory(dir))
    );

    const projects = allProjects.flat();
    return projects.sort((a, b) => a.name.localeCompare(b.name));
  }

  async findProjectsWithFzf(directories: string[]): Promise<string | null> {
    try {
      const projects = await this.findProjectsInDirectories(directories);
      const projectPaths = projects.map((p) => p.path).join("\n");

      if (!projectPaths) {
        return null;
      }

      const result = execSync("fzf", {
        input: projectPaths,
        encoding: "utf-8",
        timeout: 30000,
      }).trim();

      return result || null;
    } catch {
      return null;
    }
  }

  private async isGitRepository(path: string): Promise<boolean> {
    try {
      const gitPath = join(path, ".git");
      const stats = await stat(gitPath);
      return stats.isDirectory();
    } catch {
      return false;
    }
  }
}
