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
#
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

alias lt="tmux ls"
function kill_tmux_session() {
  tmux kill-session -t $1;
  tmux ls
}
alias kmux="kill_tmux_session"
# alias mux="tmuxinator start"

function start_tmuxinator_with_params() {
  #---------------------------------------------#
  # VARIABLES
  # -------------------------------------------#

  VERSION='version 1.0'
  APP='spages'
  ENV=''
  LOCALLY=false
  HELP=false
  GET_VERSION=false
  DEBUG=false

  ENVIRONMENT=''
  APPLICATION=''

  HELP_MESSAGE='
    Usage:
      mux [flags with options] [flags with no options]
      e.g: mux -a spages -e E05 -l (will run spages app with skyport and E05


    Flags          Options                          Description

    -a, --app      [ spages ]                       Chose app to start
    -e, --env      [ F02 || d, E05 || s ]           Chose env, F02 or E05
    -b, --binding  [ overview || o, attatch || a ]  Chose tmux binding options


    Flags with no options 

    -l, --local    If flag is present will 
                   start skyport locally and 
                   point sPages and PPP at it
    -h, --help     output usage information
    -V, --version  output the version number


    Binding Options:

    overview       opens all windows in a seperate tmux window 
                   with name of app used and binds an overview to current view
    attatch        binds new windows to current view 
  '

  #---------------------------------------------#
  # GET ARGUMENTS
  # -------------------------------------------#

  # gets the flags and args from the bash command
  while getopts a:e:lhvx option
  do
    case "${option}" in
      a) APP=${OPTARG};;
      e) ENV=${OPTARG};;
      l) LOCALLY=true;;
      x) DEBUG=true;;
      h) HELP=true;;
      v) GET_VERSION=true;;
    esac
  done

  #---------------------------------------------#
  # IF HELP, VIEW HELP, ELSE RUN FUNCTION
  # -------------------------------------------#

  if $HELP; then
    echo $HELP_MESSAGE
  else
    #---------------------------------------------#
    # SET UP APPLICATION
    # -------------------------------------------#

    # get chosen app ready to pass to tmuxinator CURRENTLY ONLY SUPPORTS SPAGES
    case $APP in
      *) APPLICATION='spages' && APP='';; #APP='' resets env for the check further down
    esac

    # display app info to user
    if [[ "$APP" == '' ]]; then
      echo 'no app specified, or app arg not valid'
      echo 'running sky-pages with skyport and ppp'
      echo ''
    else
    fi

    #---------------------------------------------#
    # IS THIS POINTING AT ANYTHING LOCAL?
    # -------------------------------------------#

    # this so far only exists for spages and will run it locally adn point spages and ppp at local skyport
    if $LOCALLY; then
      LOCAL_FLAG='-l'
      echo 'local flag set, pointing spages at local skyport'
      echo ''
    fi

    #---------------------------------------------#
    # SET UP ENVIRONMENT
    # -------------------------------------------#

    # get chosen environement ready to pass to tmuxinator
    case $ENV in
      'F02') ENVIRONMENT='-d';;
      'd') ENVIRONMENT='-d';;
      'E05') ENVIRONMENT='-s';;
      's') ENVIRONMENT='-s';;
      *) ENVIRONMENT='-d' && ENV='';; #ENV='' resets env for the check further down
    esac

    # display env info to user
    if [[ "$ENV" == '' ]]; then
      echo 'no env specified, or env arg not valid'
      echo 'pointing' $APPLICATION ' at development F02'
      echo ''
    else
      echo 'env specified:' $ENV '=> pointing' $APPLICATION 'at' $ENVIRONMENT 'environment'
      echo ''
    fi

    #---------------------------------------------#
    # RUN TMUXINATOR WITH ARGS
    # -------------------------------------------#

    if $DEBUG; then
      echo 'tmuxinator start' $APPLICATION $ENVIRONMENT $LOCAL_FLAG
    else
      tmuxinator start shared
      tmuxinator start $APPLICATION $ENVIRONMENT $LOCAL_FLAG
    fi
  fi

  #---------------------------------------------#
  # EXTRA STUFF
  # -------------------------------------------#

  if $GET_VERSION; then
    echo $VERSION
  fi
}

alias mux="start_tmuxinator_with_params"

alias allthethings="mux shared; mux pd d; mux sky"

alias portkeys="colordiff -u  <(awk -F'\"' '/\"/ { print \$2 }' vault-keys.json | sort -u) <(cut -d'=' -f1 .env.integration | sort -u) "

alias skyport="echo 'you need to specify and env: skyport-development or skyport-stage'"
alias skyport-development="cp .env.f02 .env.integration || true && NODE_ENV=integration npm start"
alias skyport-d="skyport-development"
alias skyport-stage="cp .env.e05 .env.integration || true && NODE_ENV=integration npm start"
alias skyport-s="skyport-stage"

alias ppp="run_ppp"

function run_ppp() {

  #---------------------------------------------#
  # IMPORTANT: STORE PPP TOKENS IN LOCAL FILE
  # - YOU MAY ALREADY HAVE THIS SO CHANGE NAME
  # -------------------------------------------#
 
  if [ -r ~/.ppp_private ]
  then
    source ~/.ppp_private
  fi

  #--------------------------------------------#
  
  ENV=''
  LOCALLY=false
  DEBUG=false
  HELP=false

  ENVIORNMENT=''
  SKYPORT=''

  #---------------------------------------------#
  # GET ARGUMENTS
  # -------------------------------------------#

  # gets the flags and args from the bash command
  while getopts dslxh option
  do
    case "${option}" in
      d) ENV='dev';;
      s) ENV='stage';;
      l) LOCALLY=true;;
      x) DEBUG=true;;
      h) HELP=true;;
    esac
  done

  
  #---------------------------------------------#
  # IF HELP, VIEW HELP, ELSE RUN FUNCTION
  # -------------------------------------------#
  if $HELP; then
    echo '
    usage: run_ppp [flags]

    -d  points at dev f02 env
    -s  points at stage E05 environment
    -l  points at local graphql on port 2000

    -x  debugging mode: echos command without running
    -h  display usage
    '
  else
    #---------------------------------------------#
    # SET UP ENVIRONMENT
    # -------------------------------------------#

    case $ENV in
      'dev') ENVIRONMENT=$PPP_DEV;;
      'stage') ENVIRONMENT=$PPP_STAGE;;
      *) ENVIRONMENT=$PPP_DEV && ENV='';; #ENV='' resets env for the check further down
    esac

    # display env info to user
    if [[ "$ENV" == '' ]]; then
      ENV='dev'
      echo 'no env specified'
      echo 'pointing at dev'
      echo ''
    else
      echo 'env specified: pointing at' $ENV
      echo ''
    fi

    #---------------------------------------------#
    # POINT AT SKYPORT LOCALLY?
    # -------------------------------------------#

    if $LOCALLY; then
      SKYPORT=$PPP_SKYPORT_LOCAL
      echo 'local flag set, pointing spages at local skyport'
      echo ''
    else
      case $ENV in
        'dev') SKYPORT=$PPP_SKYPORT_DEV;;
        'stage') SKYPORT=$PPP_SKYPORT_STAGE;;
        *) SKYPORT=$PPP_SKYPORT_DEV;;
      esac
    fi

    #---------------------------------------------#
    # RUN THAT SHIT
    # -------------------------------------------#
    if $DEBUG; then
      echo "${SKYPORT} ${ENVIRONMENT} ${PPP_AUTHENTIFICATION}"
    else
      eval "${SKYPORT} ${ENVIRONMENT} ${PPP_AUTHENTIFICATION}"
    fi
  fi
}

# Useful
alias zshe="vim ~/.zshrc"
alias zshr="source ~/.zshrc"
alias portsinuse="lsof -i -P | grep -i 'listen'"

alias brewup='brew update; brew upgrade; brew prune; brew cleanup; brew doctor'

alias cdp="cd /Users/adh23/Service/sky-pages"
alias cds="cd /Users/adh23/Service/skyport-graphql"

alias ef="exercism fetch"
alias es="exercism submit"

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

