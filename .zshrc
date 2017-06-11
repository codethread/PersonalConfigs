# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH
alias vim='/usr/local/bin/vim'
# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

source $ZSH/oh-my-zsh.sh
set guifont=Liberation\ Mono\ for\ Powerline\ 10 
# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)


# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

HIST_IGNORE_SPACE=true

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

export PATH="$PATH:./node_modules/.bin/"
export PATH="$PATH:$HOME/elixir/bin"
export PATH="/usr/local/opt/node@6/bin:$PATH"
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
alias skyport="NODE_ENV=integration npm start"
# Useful
alias tt='tab-title'
alias zshe="vim ~/.zshrc"
alias zshr="source ~/.zshrc"
alias portsinuse="lsof -i -P | grep -i 'listen'"
