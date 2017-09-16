#------------------------------------------
#--- Exports
#-----------------------------------------
export PATH="$PATH:./node_modules/.bin"
export PATH="$PATH:$HOME/elixir/bin"
export PATH="/usr/local/opt/node@6/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="$PATH:$HOME/PersonalConfigs/bin"

export ZSH=$HOME/.oh-my-zsh
export EDITOR='vim'

#------------------------------------------
#--- Settings
#-----------------------------------------
set guifont=Liberation\ Mono\ for\ Powerline\ 10
ZSH_CUSTOM=$HOME/PersonalConfigs/zsh_custom
ZSH_THEME="simple"
DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"
HIST_IGNORE_SPACE=true
DISABLE_UNTRACKED_FILES_DIRTY="true"
plugins=(git)
set diffopt+=vertical

#------------------------------------------
#--- Sources
#-----------------------------------------
source $ZSH/oh-my-zsh.sh
source $HOME/.asdf/asdf.sh
source $HOME/.aliases.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -r ~/.zshrc_private ] && source ~/.zshrc_private

for f in $HOME/.personal-scripts/*; do source $f; done
for f in $HOME/PersonalConfigs/.tmuxinator/*; do ln -s -f $f ~/.tmuxinator; done

#------------------------------------------
#--- Misc
#-----------------------------------------
if which nodenv > /dev/null; then eval "$(nodenv init -)"; fi

#------------------------------------------
#--- Sky Stuff
#-----------------------------------------
export SKY_SERVICE_FOLDER='/Users/adh23/Service'
export SKY_SERVICE_DEV_TOOLS=$SKY_SERVICE_FOLDER/skymobile-service/dev-tools
[ -r $SKY_SERVICE_DEV_TOOLS/.sky.zsh ] && source $SKY_SERVICE_DEV_TOOLS/.sky.zsh
