#!/usr/bin/env bash
#:module: kitty session manager - go to session by number
#:
#: Usage: ksm <number> [--print]
#:
#: Maps a number (0-9) to a kitty session based on profile context.
#: Reads mappings from sessions.toml and either switches to the session
#: or prints the project directory path.
#:
#: Options:
#:   --print    Print the project directory path instead of switching sessions
#:              Useful for: cd $(ksm 1 --print)

set -euo pipefail

if [[ $# -lt 1 ]] || [[ $# -gt 2 ]]; then
  echo "Usage: ksm <number> [--print]" >&2
  exit 1
fi

# Parse arguments
NUM=""
PRINT_MODE=false

for arg in "$@"; do
  if [[ "$arg" == "--print" ]]; then
    PRINT_MODE=true
  elif [[ "$arg" =~ ^[0-9]$ ]]; then
    NUM="$arg"
  else
    echo "Error: Invalid argument '$arg'" >&2
    echo "Usage: ksm <number> [--print]" >&2
    exit 1
  fi
done

if [[ -z "$NUM" ]]; then
  echo "Error: No session number provided" >&2
  echo "Usage: ksm <number> [--print]" >&2
  exit 1
fi
SESSIONS_TOML="$XDG_CONFIG_HOME/kitty/sessions.toml"
SESSIONS_DIR="$XDG_CONFIG_HOME/kitty/sessions"

if [[ ! -f "$SESSIONS_TOML" ]]; then
  echo "Error: sessions.toml not found at $SESSIONS_TOML" >&2
  exit 1
fi

# Determine which profile to use based on hostname or environment
HOSTNAME=$(hostname)
KEYS_SECTION="keys"
PROJECTS_SECTION="projects"

if [[ "$HOSTNAME" =~ ^PB-.* ]] || [[ "${CT_USER:-}" == "work" ]]; then
  KEYS_SECTION="profiles.work.keys"
  PROJECTS_SECTION="profiles.work.projects"
fi

# Get the project name from the keys section
PROJECT=$(cat "$SESSIONS_TOML" | fx --toml ".$KEYS_SECTION.P$NUM" 2>/dev/null || echo "")

if [[ -z "${PROJECT:-}" ]]; then
  echo "Error: No mapping found for P$NUM" >&2
  exit 1
fi

# If print mode, get and print the project directory path
if [[ "$PRINT_MODE" == true ]]; then
  PROJECT_PATH=$(cat "$SESSIONS_TOML" | fx --toml ".$PROJECTS_SECTION.$PROJECT" 2>/dev/null || echo "")

  # Fall back to main projects section if not found in profile-specific section
  if [[ -z "${PROJECT_PATH:-}" ]] && [[ "$PROJECTS_SECTION" != "projects" ]]; then
    PROJECT_PATH=$(cat "$SESSIONS_TOML" | fx --toml ".projects.$PROJECT" 2>/dev/null || echo "")
  fi

  if [[ -z "${PROJECT_PATH:-}" ]]; then
    echo "Error: No project path found for '$PROJECT'" >&2
    exit 1
  fi

  # Expand tilde to home directory
  PROJECT_PATH="${PROJECT_PATH/#\~/$HOME}"

  echo "$PROJECT_PATH"
  exit 0
fi

# Session switching mode
SESSION_FILE="$SESSIONS_DIR/$PROJECT.session"

# Create session file if it doesn't exist
if [[ ! -f "$SESSION_FILE" ]]; then
  # Get the project path from TOML
  PROJECT_PATH=$(cat "$SESSIONS_TOML" | fx --toml ".$PROJECTS_SECTION.$PROJECT" 2>/dev/null || echo "")

  # Fall back to main projects section if not found in profile-specific section
  if [[ -z "${PROJECT_PATH:-}" ]] && [[ "$PROJECTS_SECTION" != "projects" ]]; then
    PROJECT_PATH=$(cat "$SESSIONS_TOML" | fx --toml ".projects.$PROJECT" 2>/dev/null || echo "")
  fi

  if [[ -z "${PROJECT_PATH:-}" ]]; then
    echo "Error: No project path found for '$PROJECT'" >&2
    exit 1
  fi

  # Expand tilde to home directory
  PROJECT_PATH="${PROJECT_PATH/#\~/$HOME}"

  # Ensure sessions directory exists
  mkdir -p "$SESSIONS_DIR"

  # Create session file with launch command
  echo "launch --title $PROJECT --cwd $PROJECT_PATH" > "$SESSION_FILE"
  echo "Created session file: $SESSION_FILE" >&2
fi

# Switch to the session
kitty @ action goto_session "sessions/$PROJECT.session"
