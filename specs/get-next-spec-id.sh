#!/bin/bash

# get-next-spec-id.sh
# Determines the next available specification ID number by scanning existing spec files
# Returns a 3-digit padded ID (001, 002, etc.)

get_next_spec_id() {
    local specs_dir="${1:-$(dirname "$0")}"

    # Find all spec files matching the pattern XXX-*.md (but not template files)
    # Extract the numerical IDs from filenames
    local highest_id=0

    # Use find to get spec files, then extract IDs
    while IFS= read -r -d '' file; do
        local filename=$(basename "$file")

        # Skip template files and other non-spec files
        if [[ "$filename" =~ ^[0-9]{3}-.*\.md$ ]] && [[ ! "$filename" =~ \.(tech|notes)\.md$ ]]; then
            # Extract the 3-digit ID from the beginning of the filename
            local id="${filename:0:3}"

            # Convert to integer for comparison (remove leading zeros)
            local id_int=$((10#$id))

            if [[ $id_int -gt $highest_id ]]; then
                highest_id=$id_int
            fi
        fi
    done < <(find "$specs_dir" -maxdepth 1 -name "*.md" -print0 2>/dev/null)

    # Calculate next ID and format with leading zeros
    local next_id=$((highest_id + 1))
    printf "%03d" $next_id
}

# If script is executed directly (not sourced), call the function
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    get_next_spec_id "$@"
fi