#------------------------------------------
#--- Exports
#-----------------------------------------
export ZSH=$HOME/.oh-my-zsh

export PATH="$PATH:$HOME/elixir/bin"
# export PATH="/usr/local/opt/node@6/bin:$PATH"
# export PATH="/usr/local/sbin:$PATH"
export PATH="$PATH:$HOME/PersonalConfigs/bin"
export PATH="$HOME/.nodenv/shims:$PATH"

export EDITOR='vim'
# export KEYTIMEOUT=100

#------------------------------------------
#--- Settings
#-----------------------------------------
ZSH_THEME="spaceship"
ZSH_CUSTOM=$HOME/PersonalConfigs/zsh_custom
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

source $HOME/PersonalConfigs/spaceship-config.zsh

# bindkey -e # comes after spacehip config

for f in $HOME/.personal-scripts/*; do source $f; done
for f in $HOME/PersonalConfigs/tmuxinator/*; do ln -s -f $f ~/.tmuxinator; done

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Codi
# Usage: codi [filetype] [filename]
codi() {
        local syntax="${1:-javascript}"
        shift
        vim -c \
                "let g:startify_disable_at_vimenter = 1 |\
                set bt=nofile ls=0 noru nonu nornu |\
                hi ColorColumn ctermbg=NONE |\
                hi VertSplit ctermbg=NONE |\
                hi NonText ctermfg=0 |\
                Codi $syntax" "javascript"
}
codij() {
        # vim -c "set bt=nofile | Codi javascript" dmp.javascript
        vim -c "Codi javascript | let ale_enabled = 0" dmp.javascript
}

#------------------------------------------
#--- Sky Stuff
#-----------------------------------------
[ -f ~/.sky_private ] && source ~/.sky_private
export SKY_SERVICE_FOLDER='/Users/adh23/Service'
export SKY_SERVICE_DEV_TOOLS=$SKY_SERVICE_FOLDER/skymobile-service/dev-tools
[ -r $SKY_SERVICE_DEV_TOOLS/.sky.zsh ] && source $SKY_SERVICE_DEV_TOOLS/.sky.zsh
