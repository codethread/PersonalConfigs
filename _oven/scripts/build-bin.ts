#!/usr/bin/env bun

import { $ } from "bun";
import { chmod, readdir, readFile, writeFile } from "fs/promises";
import { basename, join } from "path";

const BIN_SRC_DIR = join(import.meta.dir, "..", "bin");
const BIN_DEST_DIR = join(import.meta.dir, "..", "..", "..", ".local", "bin");

async function buildExecutables() {
  console.log("Building executables from _oven/bin to ~/.local/bin...");

  // Get all .ts files from bin directory
  const files = await readdir(BIN_SRC_DIR);
  const tsFiles = files.filter((f) => f.endsWith(".ts"));

  for (const file of tsFiles) {
    const srcPath = join(BIN_SRC_DIR, file);
    const destName = basename(file, ".ts"); // Remove .ts extension
    const destPath = join(BIN_DEST_DIR, destName);

    console.log(`Building ${file} -> ${destName}`);

    try {
      // Read the source file
      const content = await readFile(srcPath, "utf-8");

      // Check the shebang to determine the runtime
      let shebang = "#!/usr/bin/env bun";
      const firstLine = content.split("\n")[0];
      if (firstLine.startsWith("#!")) {
        shebang = firstLine;
      }

      // For bun files, we can use bun build to create a standalone executable
      if (shebang.includes("bun")) {
        // Build with bun
        await $`bun build ${srcPath} --compile --outfile ${destPath}`.quiet();
      } else if (shebang.includes("node")) {
        // For node files, just copy them as-is (they're already JavaScript compatible)
        await writeFile(destPath, content);
      } else {
        // Default to copying the file
        await writeFile(destPath, content);
      }

      // Make the file executable
      await chmod(destPath, 0o755);
    } catch (error) {
      console.error(`Error building ${file}:`, error);
    }
  }

  // Also copy any shared modules that might be needed
  const sharedSrcDir = join(BIN_SRC_DIR, "shared");
  const sharedDestDir = join(BIN_DEST_DIR, "shared");

  try {
    await $`mkdir -p ${sharedDestDir}`.quiet();
    await $`cp -r ${sharedSrcDir}/* ${sharedDestDir}/`.quiet();
    console.log("Copied shared modules");
  } catch (error) {
    console.log("No shared modules to copy or error copying:", error.message);
  }

  console.log("Cleaning build artifacts");
  const ovenDir = join(import.meta.dir, "..");
  const ovenFiles = await readdir(ovenDir);
  const buildArtifacts = ovenFiles.filter((f) => f.endsWith(".bun-build"));
  
  for (const artifact of buildArtifacts) {
    const artifactPath = join(ovenDir, artifact);
    await $`rm ${artifactPath}`.quiet();
    console.log(`  Removed: ${artifact}`);
  }
  
  if (buildArtifacts.length === 0) {
    console.log("  No build artifacts to clean");
  }

  console.log("Build complete!");
}

// Run the build
await buildExecutables();
