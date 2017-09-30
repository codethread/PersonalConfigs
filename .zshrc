#------------------------------------------
#--- Exports
#-----------------------------------------
export PATH="$PATH:$HOME/elixir/bin"
export PATH="/usr/local/opt/node@6/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="$PATH:$HOME/PersonalConfigs/bin"

export ZSH=$HOME/.oh-my-zsh
export EDITOR='vim'

#------------------------------------------
#--- Plugins
#-----------------------------------------
plugins=(vi-mode)
plugins=(git)
plugins=(docker)
plugins=(ruby)
plugins=(rvm)

#------------------------------------------
#--- Sources
#-----------------------------------------
source $ZSH/oh-my-zsh.sh
source $HOME/.asdf/asdf.sh
source $HOME/.aliases.zsh
source $HOME/PersonalConfigs/spaceship-config.zsh

for f in $HOME/.personal-scripts/*; do source $f; done
for f in $HOME/PersonalConfigs/.tmuxinator/*; do ln -s -f $f ~/.tmuxinator; done

bindkey -v
# load these after vi-mnode or they wont work
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -r ~/.zshrc_private ] && source ~/.zshrc_private


#------------------------------------------
#--- Settings
#-----------------------------------------
set guifont=Liberation\ Mono\ for\ Powerline\ 10
set diffopt+=vertical

ZSH_CUSTOM=$HOME/PersonalConfigs/zsh_custom
ZSH_THEME="spaceship"
DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"
HIST_IGNORE_SPACE=true
DISABLE_UNTRACKED_FILES_DIRTY="true"

bindkey -M viins 'jk' vi-cmd-mode
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word

export KEYTIMEOUT=100

if which nodenv > /dev/null; then eval "$(nodenv init -)"; fi

#------------------------------------------
#--- Sky Stuff
#-----------------------------------------
export SKY_SERVICE_FOLDER='/Users/adh23/Service'
export SKY_SERVICE_DEV_TOOLS=$SKY_SERVICE_FOLDER/skymobile-service/dev-tools
[ -r $SKY_SERVICE_DEV_TOOLS/.sky.zsh ] && source $SKY_SERVICE_DEV_TOOLS/.sky.zsh

