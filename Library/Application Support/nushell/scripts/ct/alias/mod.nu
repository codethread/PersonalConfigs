export use git.nu *
export use node.nu *
export use tmux.nu *

#---------------------------------------------#
# EDITOR-ish
# -------------------------------------------#

export alias v = nvim
# see https://github.com/wbthomason/packer.nvim/issues/180 for MACOSX_DEPLOYMENT_TARGET=10.15

export alias nvim-boot = MACOSX_DEPLOYMENT_TARGET=10.15 nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'

# rg --json hi | lines | each {|| from json } | where type == 'match' | get data

export alias vo = ls

#---------------------------------------------#
# SEARCH
# -------------------------------------------#

export def pj [...deps: string] {
  let search = ($deps | str join "|" | $"\"\(($in)\)\"")
  print $search
  rg --glob "**/package.json" $search
}

#---------------------------------------------#
# OS
#---------------------------------------------#

export alias mac-dark-toggle = osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to not dark mode'
export alias l = ls -a
export alias finder = ^open -a 'Finder' .
export alias ports = lsof -i tcp:3000

export def alert [] {
  osascript -e 'display notification "Task Finished" with title "CMD"'
  afplay /System/Library/Sounds/Glass.aiff
}

#---------------------------------------------#
# HOMEBREW
#---------------------------------------------#

# update brewfile
export alias bbd = brew bundle dump
# ensure all installed
export alias bbc = brew bundle check
# removed unlisted
export alias bbx = brew bundle cleanup
export alias bbi = brew bundle install

def brewclean [] {
  brew cleanup
  brew autoremove
}

export alias brewdeps = brew deps --graph --installed

#---------------------------------------------#
# KITTY
# -------------------------------------------#

export alias ktt = kitty +kitten themes
export alias ktt-dark = kitty +kitten themes --reload-in=all Tokyo Night Storm
export alias ktt-light = kitty +kitten themes --reload-in=all Tokyo Night Day
