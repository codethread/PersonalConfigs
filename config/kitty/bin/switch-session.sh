#!/usr/bin/env bash
#:module: kitty session manager - go to session by number
#:
#: Usage: ksm-goto-session <number>
#:
#: Maps a number (0-9) to a kitty session based on profile context.
#: Reads mappings from sessions.toml and switches to the corresponding session.

set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "Usage: ksm-goto-session <number>" >&2
  exit 1
fi

NUM="$1"
SESSIONS_TOML="$XDG_CONFIG_HOME/kitty/sessions.toml"
SESSIONS_DIR="$XDG_CONFIG_HOME/kitty/sessions"

if [[ ! -f "$SESSIONS_TOML" ]]; then
  echo "Error: sessions.toml not found at $SESSIONS_TOML" >&2
  exit 1
fi

# Determine which section to use based on hostname or environment
HOSTNAME=$(hostname)
SECTION="projects"

if [[ "$HOSTNAME" =~ ^PB-.* ]] || [[ "${CT_USER:-}" == "work" ]]; then
  SECTION="work_projects"
fi

# Get the project name from the appropriate section
PROJECT=$(cat "$SESSIONS_TOML" | fx --toml ".$SECTION.keys.P$NUM" 2>/dev/null || echo "")

if [[ -z "${PROJECT:-}" ]]; then
  echo "Error: No mapping found for P$NUM" >&2
  exit 1
fi

SESSION_FILE="$SESSIONS_DIR/$PROJECT.session"

if [[ ! -f "$SESSION_FILE" ]]; then
  echo "Error: Session file not found: $SESSION_FILE" >&2
  exit 1
fi

# Switch to the session
kitty @ action goto_session "sessions/$PROJECT.session"
