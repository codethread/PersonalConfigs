const { spawn } = require("child_process");

/**
 * @param {string} cmd
 *
 * @returns {Promise<{ stdout: string; stderr: string }>}
 */
async function shell(cmd) {
  return new Promise((resolve, reject) => {
    const [exe, ...args] = cmd.split(" ");
    const spawned = spawn(exe, args, { shell: true });

    let stdout = "";
    let stderr = "";

    spawned.stdout.on("data", (data) => {
      const str = data.toString();
      stdout += str;
    });

    spawned.stderr.on("data", (data) => {
      const str = data.toString();
      stderr += str;
    });

    spawned.on("close", (code) => {
      if (code === 0) {
        resolve({ stdout: stdout.trim(), stderr });
      } else {
        reject(
          new Error(
            `spawned process ${cmd} exited with code ${code}, stderr ${stderr}`,
          ),
        );
      }
    });

    spawned.on("error", (e) => {
      reject(new Error(e.message));
    });
  });
}

module.exports = { shell };
