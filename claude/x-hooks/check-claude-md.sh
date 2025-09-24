#!/bin/bash

# Check if CLAUDE.md accurately reflects the latest changes from the session
# This hook runs once per Stop event to avoid endless loops

# Read input from stdin
input=$(cat)

# Parse the JSON input to check if this is already from a stop hook
stop_hook_active=$(echo "$input" | jq -r '.stop_hook_active // false')

# If we're already in a stop hook, exit silently to prevent loops
if [ "$stop_hook_active" = "true" ]; then
    exit 0
fi

# Check if CLAUDE.md exists in the project
if [ ! -f "$CLAUDE_PROJECT_DIR/CLAUDE.md" ]; then
    exit 0
fi

# Define the prompt message
read -r -d '' PROMPT << 'EOF'
Please review the changes made in this session and check if CLAUDE.md accurately reflects them. Look for:

1. Any new features, tools, or scripts that were created
2. Any modifications to existing functionality
3. Any new patterns or conventions that were established
4. Any important configuration changes

If CLAUDE.md needs updates, please update it to accurately reflect the current state of the project.

Focus only on significant changes that would help future Claude sessions understand the project better. Do not update CLAUDE.md for trivial changes.

Make any changes if appropriate and then stop without any kind message back to the user
EOF

# Output JSON with the prompt embedded using jq for proper escaping
echo "$PROMPT" | jq -R -s '{decision: "block", reason: .}'

exit 0
