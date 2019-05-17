bindkey -e # emacs key bindings
if [[ "${terminfo[kcbt]}" != "" ]]; then
  bindkey "${terminfo[kcbt]}" reverse-menu-complete   # [Shift-Tab] - move through the completion menu backwards
fi

source ~/.zsh_plugins.sh

#------------------------------------------
#--- History
#-----------------------------------------
HISTFILE=$HOME/.zsh_history
HISTSIZE=999999999
SAVEHIST=$HISTSIZE
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.

# highlight tab
zstyle ':completion:*' menu selecto

#------------------------------------------
#--- Exports
#-----------------------------------------
export EDITOR='vim'
source "$HOME/.aliases.zsh"

export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/.bin"
source $HOME/.cargo/env # TODO needed?

export JQ_COLORS="1;30:0;31:0;32:0;35:0;33:1;35:1;35"

source $HOME/enhancd/init.sh
eval "$(fnm env --multi)"

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
