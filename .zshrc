#------------------------------------------
#--- ZPlugin
#-----------------------------------------

### Added by zplugin's installer https://github.com/zdharma/zplugin/blob/master/doc/INSTALLATION.adoc
source "$HOME/.zplugin/bin/zplugin.zsh"
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin
### End of zplugin's installer chunk
autoload -Uz compinit
compinit

# pure https://github.com/sindresorhus/pure#zplugin
zplugin ice pick"async.zsh" src"pure.zsh"
zplugin light sindresorhus/pure

# prevent repeated work for direnv
# https://github.com/zdharma/zplugin/wiki/Direnv-explanation
zplugin ice as"program" make'!' atclone'./direnv hook zsh > zhook.zsh' atpull'%atclone' pick"direnv" src"zhook.zsh"
zplugin light direnv/direnv

# git aliases
zplugin snippet OMZ::plugins/git/git.plugin.zsh

# fzf keybindings
zplugin snippet https://github.com/junegunn/fzf/blob/master/shell/key-bindings.zsh

zplugin ice blockf
zplugin light zsh-users/zsh-completions

# pretty colors
zplugin load zsh-users/zsh-syntax-highlighting

#------------------------------------------
#--- Exports
#-----------------------------------------
export EDITOR='vim'
source "$HOME/.aliases.zsh"

export PATH="$PATH:$HOME/.nodenv/shims"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/.bin"

# source $HOME/.asdf/asdf.sh
source $HOME/.cargo/env # TODO needed?

if which nodenv > /dev/null; then eval "$(nodenv init -)"; fi

export JQ_COLORS="1;30:0;31:0;32:0;35:0;33:1;35:1;35"

source $HOME/enhancd/init.sh
#------------------------------------------
#--- History
#-----------------------------------------
HISTFILE=$HOME/.zsh_history
HISTSIZE=999999999
SAVEHIST=$HISTSIZE
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.

setopt auto_menu
bindkey -e # emacs key bindings
if [[ "${terminfo[kcbt]}" != "" ]]; then
  bindkey "${terminfo[kcbt]}" reverse-menu-complete   # [Shift-Tab] - move through the completion menu backwards
fi

#------------------------------------------
#--- Sky Stuff
#-----------------------------------------
if [[ $(whoami) =~ 'adh23' ]]; then
    export TOOLKIT_PATH="$HOME/Sky/toolkit"
    export SKY_SERVICE_FOLDER="$HOME/Service"
    export SKYPORT_GRAPHQL_DIR="$SKY_SERVICE_FOLDER/skyport-graphql"
    export SKY_SERVICE_DEV_TOOLS=$SKY_SERVICE_FOLDER/skymobile-service/dev-tools
    [ -r $SKY_SERVICE_DEV_TOOLS/.sky.sh ] && source $SKY_SERVICE_DEV_TOOLS/.sky.sh
fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
# export SDKMAN_DIR="/Users/adh23/.sdkman"
# [[ -s "/Users/adh23/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/adh23/.sdkman/bin/sdkman-init.sh"

