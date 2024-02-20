use git.nu *
use node.nu *
use tmux.nu *

#---------------------------------------------#
# EDITOR-ish
# -------------------------------------------#

alias v = nvim
# see https://github.com/wbthomason/packer.nvim/issues/180 for MACOSX_DEPLOYMENT_TARGET=10.15

alias nvim-boot = MACOSX_DEPLOYMENT_TARGET=10.15 nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'

# rg --json hi | lines | each {|| from json } | where type == 'match' | get data

alias vo = ls

#---------------------------------------------#
# SEARCH
# -------------------------------------------#

def pj [...deps: string] {
  let search = ($deps | str join "|" | $"\"\(($in)\)\"")
  print $search
  rg --glob "**/package.json" $search
}

#---------------------------------------------#
# OS
#---------------------------------------------#

alias mac-dark-toggle = osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to not dark mode'
alias l = ls -a
alias finder = ^open -a 'Finder' .
alias ports = lsof -i tcp:3000

def alert [msg = "Task Finished"] {
  osascript -e $'display notification "($msg)" with title "CMD"'
  afplay /System/Library/Sounds/Glass.aiff
}

# I always forget
def symlink [original: path, symbolic: path] {
  ln -s $original $symbolic
}

#---------------------------------------------#
# HOMEBREW
#---------------------------------------------#

# update brewfile
alias bbd = brew bundle dump
# ensure all installed
alias bbc = brew bundle check
# removed unlisted
alias bbx = brew bundle cleanup
alias bbi = brew bundle install

def brewclean [] {
  brew cleanup
  brew autoremove
}

alias brewdeps = brew deps --graph --installed

#---------------------------------------------#
# KITTY
# -------------------------------------------#

alias ktt = kitty +kitten themes
alias ktt-dark = kitty +kitten themes --reload-in=all Tokyo Night Storm
alias ktt-light = kitty +kitten themes --reload-in=all Tokyo Night Day

#---------------------------------------------#
# KITTY
# -------------------------------------------#
alias loggy = cd `~/Library/Mobile Documents/iCloud~com~logseq~logseq/`
