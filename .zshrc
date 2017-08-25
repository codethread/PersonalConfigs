# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

export ZSH=$HOME/.oh-my-zsh
export EDITOR='vim'

set guifont=Liberation\ Mono\ for\ Powerline\ 10

ZSH_CUSTOM=$HOME/PersonalConfigs/zsh_custom
ZSH_THEME="simple"
DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"

plugins=(git)

# export MANPATH="/usr/local/man:$MANPATH"
HIST_IGNORE_SPACE=true
source $ZSH/oh-my-zsh.sh


set diffopt+=vertical

export SKY_SERVICE_FOLDER='/Users/adh23/Service'

export PATH="$PATH:./node_modules/.bin"
export PATH="$PATH:$HOME/elixir/bin"
export PATH="/usr/local/opt/node@6/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="$PATH:$HOME/Service/skymobile-service/scripts"
# export PATH="$PATH:$HOME/PersonalConfigs/bin"
#----export NVM_DIR="$HOME/.nvm"
#----. "/usr/local/opt/nvm/nvm.sh"

if [ -r ~/.zshrc_private ]
then
  source ~/.zshrc_private
fi

if [ -r ~/.sky_private ]; then
  source ~/.sky_private
fi

if which nodenv > /dev/null; then eval "$(nodenv init -)"; fi

. $HOME/.asdf/asdf.sh

# source all personal scripts
for f in $HOME/.personal-scripts/*; do source $f; done


#tmuxinator stuff
source "$HOME/PersonalConfigs/bin/tmuxinator.zsh"
for f in $HOME/PersonalConfigs/.tmuxinator/*; do ln -s -f $f ~/.tmuxinator; done
for f in $HOME/Service/skymobile-service/tmuxinator/*; do ln -s -f $f ~/.tmuxinator; done
#
#
# source all sky scripts
# brew install tmux
# gem install tmuxinator
# ln -s ~/Service/skymobile-service/scripts/tmuxinator/spages.yml ~/PersonalConfigs/.tmuxinator
# for f in $SKY_SERVICE_FOLDER/skymobile-service/scripts/*; do source $f; done

source $HOME/.aliases.zsh

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
