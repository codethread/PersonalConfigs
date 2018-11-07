#------------------------------------------
#--- Exports
#-----------------------------------------
export ZSH=$HOME/.oh-my-zsh

export PATH="$PATH:$HOME/elixir/bin"
# export PATH="/usr/local/opt/node@6/bin:$PATH"
# export PATH="/usr/local/sbin:$PATH"
export PATH="$PATH:$HOME/PersonalConfigs/bin"
export PATH="$HOME/.nodenv/shims:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

export EDITOR='vim'
# export KEYTIMEOUT=100

#------------------------------------------
#--- Settings
#-----------------------------------------
# ZSH_THEME="spaceship"
# ZSH_CUSTOM=$HOME/PersonalConfigs/zsh_custom
DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"
HIST_IGNORE_SPACE=true
DISABLE_UNTRACKED_FILES_DIRTY="true"


# plugins=(bundler gem git mix sudo tmux web-search)
plugins=(git tmux)

# set guifont=monofur\ for\ Powerline\ 10
if which nodenv > /dev/null; then eval "$(nodenv init -)"; fi

#------------------------------------------
#--- Sources
#-----------------------------------------
source $ZSH/oh-my-zsh.sh
source $HOME/.asdf/asdf.sh
source $HOME/.aliases.zsh
source $HOME/.cargo/env

# source $HOME/PersonalConfigs/spaceship-config.zsh

for f in $HOME/PersonalConfigs/scripts/*; do source $f; done
# for f in $HOME/PersonalConfigs/bin/*; do ln -s -f $f /usr/local/bin; done
for f in $HOME/PersonalConfigs/tmuxinator/*; do ln -s -f $f ~/.tmuxinator; done

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
source $HOME/enhancd/init.sh

autoload -U promptinit; promptinit
prompt pure

source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
#------------------------------------------
#--- Sky Stuff
#-----------------------------------------
export SKY_SERVICE_FOLDER='/Users/adh23/Service'
export SKY_SERVICE_DEV_TOOLS=$SKY_SERVICE_FOLDER/skymobile-service/dev-tools
[ -r $SKY_SERVICE_DEV_TOOLS/.sky.zsh ] && source $SKY_SERVICE_DEV_TOOLS/.sky.zsh
