# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

set guifont=Liberation\ Mono\ for\ Powerline\ 10 
# ZSH_THEME="robbyrussell"
ZSH_THEME="michelebologna"
# ZSH_THEME="random"

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

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
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

function tab-title() {
  echo -e "\033];$1\007"
}

function startppOnPortp() {
    stuff="GRAPHQL_ENDPOINT=https://local.bskyb.com:"

    if [ $1 ]
    then
       "$stuff$1 $PPP_AUTHENTIFICATION"
    else
        echo 'require port number'
    fi
}
alias lkj="startppOnPortp" 
# Alias
# Github
alias ga="git add"
alias gaa="git add ."
alias gaaa="git add -A"
alias gb="git branch"
alias gbd="git branch -d"
alias gc="git commit"
alias gf="git fetch"
alias gcm="git commit -m"
alias gco="git checkout"
alias gcob="git checkout -b"
alias gcom="git checkout master"
alias gd="git diff"
alias gda="git diff HEAD"
alias gi="git init"
alias gl="git log"
alias glg="git log --graph --oneline --decorate --all"
alias gld="git log --pretty=format:"%h %ad %s" --date=short --all"
alias gm="git merge --no-ff"
alias gp="git pull"
alias gs="git status"
alias gss="git status -s"
alias gst="git stash"
alias gstl="git stash list"
alias gstp="git stash pop"
alias gstd="git stash drop"
alias grh="git reset --hard"
alias grhm="git reset --hard origin/master"

# new
alias gln="git log -n" #add a number for how many commits you want
# alias glo="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative"
# alias gpo="git pull origin"
# alias ga="git add"
# alias gs="git status"
# alias gsl="git stash list --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cd)' --abbrev-commit --date=local"
# alias git stash list="git stash list --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cd)' --abbrev-commit --date=local"
alias npmrebuild="rm -rf ./node_modules; npm cache clear; npm i"
alias spages="pages start dev"
alias stest="mv .env .notEnv || true  && npm run test:unit || true && mv .notEnv .env"
alias skyport="NODE_ENV=integration npm start"
alias ppp="echo 'you need pppd or ppps for dev or stage'"
alias ppps="$PPP_STAGE $PPP_AUTHENTIFICATION"
alias pppd="$PPP_DEV $PPP_AUTHENTIFICATION"
alias pppc="GRAPHQL_ENDPOINT=https://skyport-graphql-piggy-bank-balance.cf.dev-paas.bskyb.com $PPP_AUTHENTIFICATION"
# Useful
alias tt='tab-title'
alias zshe="vim ~/.zshrc"
alias zshr="source ~/.zshrc"
alias portsinuse="lsof -i -P | grep -i 'listen'"
alias lookbusy="cd ~/PersonalConfigs; sh lookbusy.sh;"

alias brewup='brew update; brew upgrade; brew prune; brew cleanup; brew doctor'

alias cdp="cd /Users/adh23/Service/sky-pages"
alias cdg="cd /Users/adh23/Service/skyport-graphql"

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

