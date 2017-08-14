# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

set guifont=Liberation\ Mono\ for\ Powerline\ 10 

ZSH_CUSTOM=$HOME/PersonalConfigs/zsh_custom

# ZSH_THEME="robbyrussell"
# ZSH_THEME="michelebologna"
ZSH_THEME="simple"
# ZSH_THEME="random"
#
# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

HIST_IGNORE_SPACE=true
source $ZSH/oh-my-zsh.sh

# source $HOME/.bin/tmuxinator.zsh
export EDITOR='vim'
# source $HOME/Service/skymobile-service/scripts/useful_scripts.sh

set diffopt+=vertical

export PATH="$PATH:./node_modules/.bin"
export PATH="$PATH:$HOME/elixir/bin"
export PATH="/usr/local/opt/node@6/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
#----export NVM_DIR="$HOME/.nvm"
#----. "/usr/local/opt/nvm/nvm.sh"

if [ -r ~/.zshrc_private ]
then
  source ~/.zshrc_private
fi

if which nodenv > /dev/null; then eval "$(nodenv init -)"; fi

. $HOME/.asdf/asdf.sh

source $HOME/.aliases


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# Feed the output of ag into fzf
# ag -g "" | fzf
#
# # Setting ag as the default source for fzf
# export FZF_DEFAULT_COMMAND='ag -g ""'
#
# # Now fzf (w/o pipe) will use ag instead of find
# fzf
#
# # To apply the command to CTRL-T as well
# export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
# export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

