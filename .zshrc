#------------------------------------------
#--- ZPlugin
#-----------------------------------------

### Added by Zplugin's installer https://github.com/zdharma/zplugin/blob/master/doc/INSTALLATION.adoc
source "$HOME/.zplugin/bin/zplugin.zsh"
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin
### End of Zplugin's installer chunk

# pure https://github.com/sindresorhus/pure#zplugin
zplugin ice pick"async.zsh" src"pure.zsh"
zplugin light sindresorhus/pure

# prevent repeated work for direnv
# https://github.com/zdharma/zplugin/wiki/Direnv-explanation
zplugin ice as"program" make'!' atclone'./direnv hook zsh > zhook.zsh' atpull'%atclone' pick"direnv" src"zhook.zsh"
zplugin light direnv/direnv

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

# adds keybindings - fzf installed by vim-plug
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

#------------------------------------------
#--- Tmux
#-----------------------------------------
# _tmuxinator() {
#   local commands projects
#   commands=(${(f)"$(tmuxinator commands zsh)"})
#   projects=(${(f)"$(tmuxinator completions start)"})
#
#   if (( CURRENT == 2 )); then
#     _describe -t commands "tmuxinator subcommands" commands
#     _describe -t projects "tmuxinator projects" projects
#   elif (( CURRENT == 3)); then
#     case $words[2] in
#       copy|debug|delete|open|start)
#         _arguments '*:projects:($projects)'
#         ;;
#     esac
#   fi
#
#   return
# }
#
# compdef _tmuxinator tmuxinator mux
# alias mux="tmuxinator"

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

