#!/bin/bash

# Browse Ramda documentation in Terminal
# Requires jq and a tool such as fzf or peco for interactive filtering

ramda() {
    LATEST="http://raine.github.io/ramda-json-docs/latest.json"
    DOCS_URL="http://ramdajs.com/docs/"

    json=$(curl -s $LATEST)
    functions=$(echo "$json" | jq -r '.[] | if .sig and (.sig | length > 0) then .name + " :: " + .sig else .name end')
    fn=$(echo "$functions" | fzf) || exit 0
    fn_name=$(echo $fn | awk '{print $1}')
    desc=$(echo "$json" | jq -r ".[] | select(.name == \"$fn_name\") | .description" | fmt)
    docs_url="$DOCS_URL#$fn_name"

    echo "$fn"
    echo
    echo "$desc"
    echo
    echo "$docs_url"

# Open URL in browser
# open $docs_url     # mac
# xdg-open $docs_url # linux
}
