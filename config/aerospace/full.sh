#!/bin/bash

KITTY_ID="kitty"

is_kitty_full() {
    local main_windows="$1"
    local count=$(echo "$main_windows" | jq 'length')
    local kitty_count=$(echo "$main_windows" | jq --arg kitty "$KITTY_ID" '[.[] | select(.["app-name"] == $kitty)] | length')
    
    [[ $count -eq 1 && $kitty_count -eq 1 ]]
}

focus_kitty() {
    local main_windows="$1"
    
    echo "$main_windows" | jq -r --arg kitty "$KITTY_ID" '.[] | select(.["app-name"] != $kitty and .["app-name"] != "com.pomo.dev" and .["app-name"] != "com.apple.finder") | .["window-id"]' | while read -r window_id; do
        echo "move to 2 window_id: $window_id"
        aerospace move-node-to-workspace --window-id "$window_id" 2 
    done
}

share_main_space() {
    local alt_windows="$1"
    
    echo "$alt_windows" | jq -r '.[] | select(.["app-name"] != "com.pomo.dev" and .["app-name"] != "com.apple.finder") | .["window-id"]' | while read -r window_id; do
        aerospace move-node-to-workspace --window-id "$window_id" 1 
    done
}

main() {
    local main_windows=$(aerospace list-windows --workspace 1 --json | jq 'map(select(. != null))')
    local alt_windows=$(aerospace list-windows --workspace 2 --json | jq 'map(select(. != null))')
    
    echo "$main_windows"
    echo "$alt_windows"
    
    if is_kitty_full "$main_windows"; then
        echo "full"
        share_main_space "$alt_windows"
    else
        echo "not full"
        focus_kitty "$main_windows"
    fi
}

main
