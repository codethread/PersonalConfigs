#!/usr/bin/env bash
set -euo pipefail

PORT="${PORT:-9222}"
ROOT="${CHROME_DEBUG_ROOT:-$HOME/.cache/chrome-debug}"
PROFILE="${CHROME_PROFILE:-Profile Dev}"

mkdir -p "$ROOT"

/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome \
  --remote-debugging-port="$PORT" \
  --user-data-dir="$ROOT" \
  --profile-directory="$PROFILE" \
  --no-first-run \
  --no-default-browser-check \
  --password-store=basic >/dev/null 2>&1 &

CHROME_PID=$!
echo "Launched Chrome (pid=$CHROME_PID) with profile '$PROFILE' root '$ROOT' on port $PORT"
