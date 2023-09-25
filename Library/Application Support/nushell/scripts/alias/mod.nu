export use git.nu *
export use node.nu *

#---------------------------------------------#
# nushell
# -------------------------------------------#

export alias als = scope aliases

def nuopen [arg, --raw (-r)] { if $raw { open -r $arg } else { open $arg } }

alias open = ^open

export def plugs [] {  
  help commands | where command_type == "plugin"
}

export def cmds [] {  
  help commands | where command_type == "custom" | reject params
}

#---------------------------------------------#
# EDITOR-ish
# -------------------------------------------#

export alias v = nvim
# see https://github.com/wbthomason/packer.nvim/issues/180 for MACOSX_DEPLOYMENT_TARGET=10.15

export alias nvim-boot = MACOSX_DEPLOYMENT_TARGET=10.15 nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'

# rg --json hi | lines | each {|| from json } | where type == 'match' | get data

export alias vo = ls

#---------------------------------------------#
# HELPERS
# -------------------------------------------#

export def pathis [] {
  $env.PATH
}

#---------------------------------------------#
# OS
#---------------------------------------------#

export alias mac-dark-toggle = osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to not dark mode'
alias l = ls -a
export alias finder = open -a 'Finder' .
export alias ports = lsof -i tcp:3000

export def alert [] {
  osascript -e 'display notification "Task Finished" with title "CMD"'
  afplay /System/Library/Sounds/Glass.aiff
}

#---------------------------------------------#
# OS
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

#---------------------------------------------#
# TMUX
# -------------------------------------------#
export alias tmux-delete-resurrect = rm -f ~/.local/share/tmux/resurrect

