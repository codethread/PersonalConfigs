#!/usr/bin/env bun

import { existsSync, mkdirSync } from "fs";
import { appendFile, readFile, writeFile } from "fs/promises";
import { join } from "path";
import { $ } from "bun";

/**
 * Claude Code Smart Notification System
 *
 * This script provides intelligent desktop notifications for Claude Code hook events,
 * helping you stay informed about task completion, errors, and when input is needed
 * without being overwhelmed by notification spam.
 *
 * PURPOSE:
 * - Sends contextual notifications based on Claude Code hook events
 * - Prevents duplicate notifications through intelligent caching
 * - Uses appropriate urgency levels and icons for different event types
 * - Auto-expires success messages while persisting error notifications
 * - Provides clear feedback when Claude Code needs intervention or completes tasks
 *
 * INTEGRATION:
 * This script is called by Claude Code hooks configured in .claude/settings.json
 * for events like PreToolUse, PostToolUse, Stop, SubagentStop, etc.
 *
 * NOTIFICATION TYPES:
 * - Critical (errors, blocked actions): Red icon, critical urgency, persistent
 * - Normal (all other notifications): Default icon, normal urgency, timed/auto-expire
 *
 * NOTE: Low urgency notifications are NOT used as they go to macOS notification
 * tray which isn't visible, defeating the purpose of desktop notifications.
 *
 * TESTING:
 * Test the script with these example payloads (run from project root):
 *
 * # Test task completion notification
 * echo '{"hook_event_name":"Stop","session_id":"test-123","cwd":"'$(pwd)'"}' | bun ~/.config/claude/claude-notifier.js
 *
 * # Test error notification
 * echo '{"hook_event_name":"PostToolUse","session_id":"test-456","tool_name":"Bash","tool_output":{"error":"Permission denied","exit_code":1},"cwd":"'$(pwd)'"}' | bun ~/.config/claude/claude-notifier.js
 *
 * # Test blocked tool notification
 * echo '{"hook_event_name":"PreToolUse","session_id":"test-789","tool_name":"Edit","exit_code":2,"cwd":"'$(pwd)'"}' | bun ~/.config/claude/claude-notifier.js
 *
 * # Test subagent completion
 * echo '{"hook_event_name":"SubagentStop","session_id":"test-999","subagent_type":"librarian","cwd":"'$(pwd)'"}' | bun ~/.config/claude/claude-notifier.js
 *
 * # Test session start notification
 * echo '{"hook_event_name":"SessionStart","session_id":"test-000","cwd":"'$(pwd)'"}' | bun ~/.config/claude/claude-notifier.js
 *
 * # Test Claude notification passthrough
 * echo '{"hook_event_name":"Notification","session_id":"test-111","notification_text":"Error: Something failed","cwd":"'$(pwd)'"}' | bun ~/.config/claude/claude-notifier.js
 *
 * DEPENDENCIES:
 * - Bun runtime
 * - Kitty terminal with notify kitten
 * - Write access to /tmp for notification cache
 */

// Configuration
const NOTIFICATION_CACHE_FILE = "/tmp/claude-notifier-cache.json";
const NOTIFICATION_DEBOUNCE_MS = 2000; // Prevent duplicate notifications within 2 seconds
const AUTO_EXPIRE_SUCCESS = "10s";
const AUTO_EXPIRE_INFO = "15s";

// Sound mappings for different notification types
const NOTIFICATION_SOUNDS = {
  error: "Basso", // Deep, attention-grabbing for critical errors
  warning: "Funk", // Noticeable but less alarming for warnings
  success: "success", // Custom YouTube-sourced success sound
  info: "Ping", // Neutral for general information
  blocked: "Submarine", // Distinctive alert for blocked actions
  silent: "silent", // No sound
};

// Logging configuration
const LOG_DIR = ".logs";
const LOG_FILE = "notifier.log";

// Simple logging function
async function log(level, message, data = null) {
  try {
    const timestamp = new Date().toISOString();
    const logEntry = {
      timestamp,
      level,
      message,
      ...(data && { data }),
    };

    // Ensure log directory exists
    if (!existsSync(LOG_DIR)) {
      mkdirSync(LOG_DIR, { recursive: true });
    }

    // Append to log file
    const logLine = JSON.stringify(logEntry) + "\n";
    await appendFile(join(LOG_DIR, LOG_FILE), logLine);
  } catch (error) {
    // Silently fail logging to not interrupt main functionality
  }
}

// Read stdin data
const stdinData = await Bun.stdin.text();
let hookData;

try {
  hookData = JSON.parse(stdinData);
  await log("DEBUG", "Received hook data", {
    hookData,
    stdinLength: stdinData.length,
  });
} catch (error) {
  await log("ERROR", "Failed to parse stdin data", {
    error: error.message,
    stdin: stdinData,
  });
  process.exit(0);
}

// Notification cache to prevent duplicates
class NotificationCache {
  constructor() {
    this.cache = {};
    this.loadCache();
  }

  async loadCache() {
    try {
      if (existsSync(NOTIFICATION_CACHE_FILE)) {
        const data = await readFile(NOTIFICATION_CACHE_FILE, "utf8");
        this.cache = JSON.parse(data);
        // Clean old entries
        const now = Date.now();
        for (const key in this.cache) {
          if (now - this.cache[key] > 60000) {
            // Remove entries older than 1 minute
            delete this.cache[key];
          }
        }
        await this.saveCache();
      }
    } catch (error) {
      this.cache = {};
    }
  }

  async saveCache() {
    try {
      await writeFile(NOTIFICATION_CACHE_FILE, JSON.stringify(this.cache));
    } catch (error) {
      // Silently fail
    }
  }

  shouldNotify(key, dedupeMs = NOTIFICATION_DEBOUNCE_MS) {
    const now = Date.now();
    const lastNotified = this.cache[key];

    if (!lastNotified || now - lastNotified > dedupeMs) {
      this.cache[key] = now;
      this.saveCache();
      return true;
    }
    return false;
  }
}

const notificationCache = new NotificationCache();

// Function to play sound on macOS using osascript
async function playMacOSSound(soundName) {
  try {
    if (!soundName || soundName === "silent" || soundName === "system") {
      return;
    }

    // Check for custom sounds in user's Library/Sounds directory
    const userSoundsDir = `${process.env.HOME}/Library/Sounds`;
    const customSoundPath = `${userSoundsDir}/${soundName}.aiff`;

    // Check if custom sound file exists
    if (existsSync(customSoundPath)) {
      // Play custom sound using afplay
      await $`osascript -e 'do shell script "afplay ${customSoundPath}"'`.quiet();
      await log("DEBUG", "Played custom macOS sound", {
        soundName,
        path: customSoundPath,
      });
    } else {
      // Fallback to system sound by name
      await $`osascript -e 'display notification "" sound name "${soundName}"'`.quiet();
      await log("DEBUG", "Played system macOS sound", { soundName });
    }
  } catch (error) {
    await log("WARN", "Failed to play macOS sound", {
      soundName,
      error: error.message,
    });
  }
}

// Function to send notification using kitten notify only
async function sendNotification(title, body, options = {}) {
  try {
    const {
      icon = "info",
      urgency = "normal",
      expireAfter = null,
      waitForCompletion = false,
      buttons = [],
      soundName = "system",
    } = options;

    // Play sound on macOS first (non-blocking)
    if (soundName && soundName !== "silent") {
      playMacOSSound(soundName); // Don't await to keep it non-blocking
    }

    // Build kitten notify command (always silent)
    let notifyCmd = ["kitten", "notify"];

    // Add icon
    notifyCmd.push(`--icon=${icon}`);

    // Add urgency
    if (urgency) {
      notifyCmd.push(`--urgency=${urgency}`);
    }

    // Always use silent for kitten notify since we handle sound separately
    notifyCmd.push("--sound-name=silent");

    // Add expiration
    if (expireAfter) {
      notifyCmd.push(`--expire-after=${expireAfter}`);
    }

    // Add wait flag
    if (waitForCompletion) {
      notifyCmd.push("--wait-for-completion");
    }

    // Add buttons
    for (const button of buttons) {
      notifyCmd.push(`--button=${button}`);
    }

    // Add title and body
    notifyCmd.push(title, body);

    await log("INFO", "Sending kitten notification", {
      title,
      body,
      options,
      command: notifyCmd,
    });

    await log("INFO", "env", { listen: process.env.KITTY_LISTEN_ON });
    // Execute notification
    const result = await $`${notifyCmd}`.text();
    await log("INFO", "Kitten notification sent successfully", {
      exitCode: result.exitCode,
    });
  } catch (error) {
    await log("ERROR", "Kitten notification failed", {
      error: error.message,
      title,
      body,
      options,
    });
  }
}

// Analyze hook event and determine notification strategy
async function processHookEvent() {
  const eventName = hookData.hook_event_name;
  const sessionId = hookData.session_id || "unknown";
  const toolName = hookData.tool_name;
  const toolOutput = hookData.tool_output;
  const toolInput = hookData.tool_input;

  // Create notification key for deduplication
  const notificationKey = `${sessionId}-${eventName}-${toolName || "none"}`;

  switch (eventName) {
    case "PreToolUse":
      // Check if tool use was blocked
      if (hookData.exit_code === 2 || hookData.exit_code === 1) {
        if (
          notificationCache.shouldNotify(`${notificationKey}-blocked`, 5000)
        ) {
          await sendNotification(
            "Claude Code: Action Blocked",
            `Tool "${toolName}" was blocked. Check your hooks configuration.`,
            {
              icon: "error",
              urgency: "critical",
              soundName: NOTIFICATION_SOUNDS.blocked,
            },
          );
        }
      }
      break;

    case "PostToolUse":
      // Check for errors in tool execution
      if (toolOutput && typeof toolOutput === "object") {
        if (
          toolOutput.error ||
          (toolOutput.exit_code && toolOutput.exit_code !== 0)
        ) {
          const errorMsg =
            toolOutput.error || toolOutput.stderr || "Unknown error";
          const truncatedError =
            errorMsg.length > 100
              ? errorMsg.substring(0, 100) + "..."
              : errorMsg;

          if (
            notificationCache.shouldNotify(`${notificationKey}-error`, 10000)
          ) {
            await sendNotification(
              "Claude Code: Tool Error",
              `Error in ${toolName}: ${truncatedError}`,
              {
                icon: "error",
                urgency: "critical",
                soundName: NOTIFICATION_SOUNDS.error,
              },
            );
          }
        }
      }
      break;

    case "Stop":
      // Notify when Claude finishes responding
      if (notificationCache.shouldNotify(`${sessionId}-stop`, 1000)) {
        // Check if there were any errors in the session
        const hasErrors = hookData.has_errors || false;

        if (hasErrors) {
          await sendNotification(
            "Claude Code: Task Complete (with errors)",
            "Ready for new input. Some errors occurred during execution.",
            {
              icon: "warning",
              urgency: "normal",
              expireAfter: AUTO_EXPIRE_INFO,
              soundName: NOTIFICATION_SOUNDS.warning,
            },
          );
        } else {
          await sendNotification(
            "Claude Code: Ready",
            "Task complete. Ready for new input.",
            {
              icon: "info",
              urgency: "normal",
              expireAfter: AUTO_EXPIRE_SUCCESS,
              soundName: NOTIFICATION_SOUNDS.success,
            },
          );
        }
      }
      break;

    case "SubagentStop":
      break; // i don't care
      // Notify when subagent completes
      const subagentType = hookData.subagent_type || "unknown";
      if (
        notificationCache.shouldNotify(
          `${sessionId}-subagent-${subagentType}`,
          5000,
        )
      ) {
        await sendNotification(
          "Claude Code: Subagent Complete",
          `${subagentType} agent has finished its task`,
          {
            icon: "info",
            urgency: "normal",
            expireAfter: AUTO_EXPIRE_INFO,
            soundName: NOTIFICATION_SOUNDS.info,
          },
        );
      }
      break;

    case "UserPromptSubmit":
      // Optional: Track when user submits prompt (usually not needed for notifications)
      break;

    case "Notification":
      // Handle Claude's own notifications
      const notificationText = hookData.notification_text || "";

      // Check for specific notification patterns
      if (
        notificationText.includes("error") ||
        notificationText.includes("failed")
      ) {
        if (
          notificationCache.shouldNotify(
            `${sessionId}-notification-error`,
            5000,
          )
        ) {
          await sendNotification("Claude Code: Error", notificationText, {
            icon: "error",
            urgency: "critical",
            soundName: NOTIFICATION_SOUNDS.error,
          });
        }
      } else if (
        notificationText.includes("waiting") ||
        notificationText.includes("input needed")
      ) {
        if (
          notificationCache.shouldNotify(
            `${sessionId}-notification-waiting`,
            10000,
          )
        ) {
          await sendNotification(
            "Claude Code: Input Needed",
            notificationText,
            {
              icon: "warning",
              urgency: "normal",
              soundName: NOTIFICATION_SOUNDS.warning,
            },
          );
        }
      }
      break;

    case "SessionStart":
      break; // i don't care
      // Notify when a new session starts
      if (notificationCache.shouldNotify(`${sessionId}-start`, 60000)) {
        await sendNotification(
          "Claude Code: Session Started",
          "New Claude Code session initiated",
          {
            icon: "info",
            urgency: "normal",
            expireAfter: "5s",
            soundName: NOTIFICATION_SOUNDS.info,
          },
        );
      }
      break;

    case "SessionEnd":
      break; // i don't care
      // Notify when session ends
      if (notificationCache.shouldNotify(`${sessionId}-end`, 60000)) {
        await sendNotification(
          "Claude Code: Session Ended",
          "Claude Code session has ended",
          {
            icon: "info",
            urgency: "normal",
            expireAfter: AUTO_EXPIRE_INFO,
            soundName: NOTIFICATION_SOUNDS.info,
          },
        );
      }
      break;

    case "PreCompact":
      // Usually not needed for notifications
      break;
  }

  // Special handling for specific tools that might need user attention
  if (toolName === "ExitPlanMode" && eventName === "PostToolUse") {
    if (notificationCache.shouldNotify(`${sessionId}-plan-ready`, 5000)) {
      await sendNotification(
        "Claude Code: Plan Ready",
        "Review the proposed plan and confirm to proceed",
        {
          icon: "warning",
          urgency: "normal",
          soundName: NOTIFICATION_SOUNDS.info,
        },
      );
    }
  }

  // Check for specific error patterns in tool output
  if (toolOutput && typeof toolOutput === "string") {
    const lowerOutput = toolOutput.toLowerCase();

    if (lowerOutput.includes("permission denied")) {
      if (
        notificationCache.shouldNotify(`${notificationKey}-permission`, 10000)
      ) {
        await sendNotification(
          "Claude Code: Permission Denied",
          `${toolName} encountered a permission issue`,
          {
            icon: "error",
            urgency: "critical",
            soundName: NOTIFICATION_SOUNDS.error,
          },
        );
      }
    } else if (lowerOutput.includes("not found") && toolName !== "Read") {
      if (
        notificationCache.shouldNotify(`${notificationKey}-notfound`, 10000)
      ) {
        await sendNotification(
          "Claude Code: Resource Not Found",
          `${toolName} could not find the requested resource`,
          {
            icon: "warning",
            urgency: "normal",
            expireAfter: AUTO_EXPIRE_INFO,
            soundName: NOTIFICATION_SOUNDS.warning,
          },
        );
      }
    }
  }
}

// Main execution
try {
  await processHookEvent();
  await log("DEBUG", "Hook processing completed successfully");
} catch (error) {
  await log("ERROR", "Hook processing failed", {
    error: error.message,
    stack: error.stack,
  });
}

// Always exit successfully to not block Claude Code
process.exit(0);
