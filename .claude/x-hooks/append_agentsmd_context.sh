#!/bin/bash

# Find all CLAUDE.md files in current directory and subdirectories
# for each, rename it to AGENTS.md and then create a CLAUDE.local.md symlinked to the AGENTS.md file

# Get the current working directory and expand ~ to full path
cwd=$(pwd)
personal_configs="$HOME/PersonalConfigs"
claude_dir="$HOME/.claude"

# Exit early if we're in restricted directories
if [[ "$cwd" == "$personal_configs"* ]] || [[ "$cwd" == "$claude_dir"* ]]; then
    echo "Skipping: Current directory ($cwd) is in restricted path" > /tmp/append_agentsmd_context.log 2>&1
    exit 0
fi

fd -t f "CLAUDE.md" | while read -r claude_file; do
    dir=$(dirname "$claude_file")
    agents_file="$dir/AGENTS.md"
    
    echo "Processing: $claude_file"
    
    mv "$claude_file" "$agents_file"
    echo "  Renamed $claude_file -> $agents_file"
    
    claude_local_file="$dir/CLAUDE.local.md"
    
    ln -sf AGENTS.md "$claude_local_file"
    echo "  Created symlink $claude_local_file -> AGENTS.md"
    echo
done > /tmp/append_agentsmd_context.log 2>&1
