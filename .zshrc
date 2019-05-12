#------------------------------------------
#--- Exports
#-----------------------------------------
export ZSH=$HOME/.oh-my-zsh
source $ZSH/oh-my-zsh.sh

export EDITOR='vim'
source "$HOME/.aliases.zsh"

export PATH="$PATH:$HOME/.nodenv/shims"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/.bin"

# source $HOME/.asdf/asdf.sh
source $HOME/.cargo/env # TODO needed?

# source zsh
# fpath=(~/PersonalConfigs/zsh/bin "${fpath[@]}")
# typeset -U PATH fpath # dedupe fpath

#------------------------------------------
#--- Settings
#-----------------------------------------
DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"
HIST_IGNORE_SPACE=true
DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(git)

if which nodenv > /dev/null; then eval "$(nodenv init -)"; fi

export JQ_COLORS="1;30:0;31:0;32:0;35:0;33:1;35:1;35"

source $HOME/enhancd/init.sh
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
autoload -U promptinit; promptinit
prompt pure

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='fd --type d --exclude "{Library,Music,Applications,Pictures,Unity,VirtualBox VMs,WebstormProjects,Tools,node_modules,.git}"'
export FZF_CTRL_T_COMMAND='rg --files --no-messages --hidden --follow --glob "!.git/*" --glob "!**/*.lock"'

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

#------------------------------------------
#------------------------------------------
#--- DEPRECATED
#-----------------------------------------

_tmuxinator() {
  local commands projects
  commands=(${(f)"$(tmuxinator commands zsh)"})
  projects=(${(f)"$(tmuxinator completions start)"})

  if (( CURRENT == 2 )); then
    _describe -t commands "tmuxinator subcommands" commands
    _describe -t projects "tmuxinator projects" projects
  elif (( CURRENT == 3)); then
    case $words[2] in
      copy|debug|delete|open|start)
        _arguments '*:projects:($projects)'
        ;;
    esac
  fi

  return
}

compdef _tmuxinator tmuxinator mux
alias mux="tmuxinator"

# ZSH_THEME="spaceship"
# ZSH_CUSTOM=$HOME/PersonalConfigs/zsh_custom
# source $HOME/PersonalConfigs/spaceship-config.zsh

