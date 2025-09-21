#!/usr/bin/env node

/**
 * Claude Code Hook: Project Index Provider
 *
 * This hook automatically provides project structure context to Claude
 * at the start of each session using the cindex tool.
 *
 * Installation:
 * 1. Ensure cindex is available in PATH: `which cindex`
 * 2. Add to Claude settings.json under x-hooks
 * 3. The index will be included in session context
 */

const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

// Configuration
const MAX_INDEX_LENGTH = 8000; // Max characters to include
const CACHE_FILE = '/tmp/cindex-cache.md';
const CACHE_MAX_AGE_MS = 1000 * 60 * 15; // 15 minutes

function generateIndex() {
  try {
    // Check if cindex exists
    execSync('which cindex', { stdio: 'ignore' });

    // Generate index
    const index = execSync('cindex -m', {
      encoding: 'utf-8',
      maxBuffer: 1024 * 1024 * 10 // 10MB buffer
    });

    return index;
  } catch (error) {
    console.error('Failed to generate project index:', error.message);
    return null;
  }
}

function getCachedIndex() {
  try {
    if (fs.existsSync(CACHE_FILE)) {
      const stats = fs.statSync(CACHE_FILE);
      const age = Date.now() - stats.mtimeMs;

      if (age < CACHE_MAX_AGE_MS) {
        return fs.readFileSync(CACHE_FILE, 'utf-8');
      }
    }
  } catch (error) {
    // Ignore cache errors
  }
  return null;
}

function saveCache(index) {
  try {
    fs.writeFileSync(CACHE_FILE, index);
  } catch (error) {
    // Ignore cache write errors
  }
}

function main() {
  // Try cache first
  let index = getCachedIndex();

  if (!index) {
    // Generate fresh index
    index = generateIndex();

    if (index) {
      saveCache(index);
    }
  }

  if (!index) {
    return {
      success: false,
      message: 'Project index unavailable'
    };
  }

  // Truncate if too long
  if (index.length > MAX_INDEX_LENGTH) {
    index = index.substring(0, MAX_INDEX_LENGTH) + '\n\n[... truncated ...]';
  }

  // Extract key insights from index
  const directories = index.match(/^## `[^`]+`$/gm) || [];
  const summaries = index.match(/\[summary\][^\n]+/g) || [];

  return {
    success: true,
    context: `
# Project Structure Overview

This project uses the cindex tool for intelligent project indexing.
Run \`cindex -m\` to see the full project structure.

## Key Directories
${directories.slice(0, 10).map(d => d.replace(/^## /, '- ')).join('\n')}

## Notable Components
${summaries.slice(0, 10).map(s => '- ' + s.replace('[summary] ', '')).join('\n')}

## Full Index Preview
\`\`\`
${index.substring(0, 2000)}
\`\`\`

For the complete index, run: \`cindex -m\`
`,
    metadata: {
      indexLength: index.length,
      directoriesFound: directories.length,
      summariesFound: summaries.length,
      cached: !!getCachedIndex()
    }
  };
}

// Export for Claude Code hooks
if (require.main === module) {
  const result = main();
  console.log(JSON.stringify(result, null, 2));
} else {
  module.exports = main;
}