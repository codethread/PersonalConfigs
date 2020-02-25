# Only read by interactive shells
# i.e only put in stuff that helps with typing commands etc
# https://blog.flowblok.id.au/2013-02/shell-startup-scripts.html

# this has some tips on speeding up zsh
# https://htr3n.github.io/2018/07/faster-zsh/

#------------------------------------------
#--- ZSH settings
#-----------------------------------------
bindkey -e # emacs key bindings
if [[ "${terminfo[kcbt]}" != "" ]]; then
    bindkey "${terminfo[kcbt]}" reverse-menu-complete   # [Shift-Tab] - move through the completion menu backwards
fi

# highlight tab
zstyle ':completion:*' menu selecto

#------------------------------------------
#--- ZSH History
#-----------------------------------------
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.

HISTFILE=$HOME/.zsh_history
HISTSIZE=999999999
SAVEHIST=$HISTSIZE

#------------------------------------------
#--- terminal specific envs and aliases
#-----------------------------------------
# Prompt https://scriptingosx.com/2019/07/moving-to-zsh-06-customizing-the-zsh-prompt/
NEWLINE=$'\n' # couldn't get the newline to behave without this
PROMPT="${NEWLINE}%F{blue}%~${NEWLINE}%(?.%F{magenta}ᕕ(ᐛ)ᕗ.%F{red}(╯°□°%)╯︵ ┻━┻) %F{normal}"
# time
# RPROMPT='%F{yellow}%*'

ssource ~/.zsh_plugins.sh
ssource ~/.fzf.zsh
ssource "$HOME/.aliases.zsh" # TODO: move?

# set up colors for ls, fd, tree etc https://github.com/sharkdp/vivid
ssource ~/.config/vivid/built/snazzy.sh
export JQ_COLORS="1;30:0;31:0;32:0;35:0;33:1;35:1;35"

# quickest way to cd around
FZF_ALT_C_COMMAND="fd --type d --exclude '{Library,Music,Applications,Pictures,Unity,VirtualBox VMs,WebstormProjects,Tools,node_modules,.git}' . ${HOME}"

#------------------------------------------
#--- Language specifi
#-----------------------------------------
# haskell
# source ~/.ghcup/env
# source $HOME/.cargo/env # TODO needed?

#------------------------------------------
#--- Sky Stuff
#-----------------------------------------
if [[ $(whoami) =~ 'adh23' ]]; then
    export TOOLKIT_PATH="$HOME/sky/toolkit"
    export SKY_SERVICE_FOLDER="$HOME/service"
    export SKYPORT_GRAPHQL_DIR="$SKY_SERVICE_FOLDER/skyport-graphql"
    export SKY_SERVICE_DEV_TOOLS="$SKY_SERVICE_FOLDER/skymobile-service/dev-tools"
    ssource $SKY_SERVICE_DEV_TOOLS/.sky.sh
fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
    # export SDKMAN_DIR="/Users/adh23/.sdkman"
    # [[ -s "/Users/adh23/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/adh23/.sdkman/bin/sdkman-init.sh"
# fnm
