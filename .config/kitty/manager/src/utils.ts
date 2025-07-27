import { join, dirname } from "node:path";
import { appendFileSync, readFileSync, mkdirSync } from "node:fs";

const logFile = process.env.LOG_TS_FILE;

export function log(
  level: "info" | "error" | "debug",
  message: string,
  data?: unknown,
) {
  const entry = {
    timestamp: new Date().toISOString(),
    level,
    message,
    data,
  };
  appendFileSync(logFile, `${JSON.stringify(entry)}\n`);
}

export function dumpLog() {
  return readFileSync(logFile, "utf-8");
}

export function listLogFile() {
  return logFile;
}

export function createSessionName(projectName: string): string {
  return `${projectName}`;
}
