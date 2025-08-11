#!/bin/bash

OPTIONAL_ORIGIN=$1

if ! thumbs --regexp "[^:]+:\d+:\d+:" -u -t /tmp/thumbs.txt; then
  # no selection was made
  if [ -n "$OPTIONAL_ORIGIN" ]; then
    kitten @ focus-window --match "id:$OPTIONAL_ORIGIN"
  fi
  exit 0
fi

pbcopy </tmp/thumbs.txt &

~/.config/kitty/bin/open-in-vim.sh "$(</tmp/thumbs.txt)"
