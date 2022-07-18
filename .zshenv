# loaded first by by all shells including emacs during command execution
# enable this and zshrc `zprof` for profiling
# zmodload zsh/zprof

HISTSIZE=999999999

PROFILE_STARTUP=false
if [[ "$PROFILE_STARTUP" == true ]]; then
    # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
    PS4=$'%D{%M%S%.} %N:%i> '
    exec 3>&2 2>$HOME/tmp/startlog.$$
    setopt xtrace prompt_subst
fi

# some ideas on this at: https://bitbucket.org/flowblok/shell-startup/src/default/.shell/env_functions
# and https://blog.flowblok.id.au/2013-02/shell-startup-scripts.html

# Usage: indirect_expand PATH -> $PATH
indirect_expand () {
    env |sed -n "s/^$1=//p"
}

# Usage: pathremove /path/to/bin [PATH]
# Eg, to remove ~/bin from $PATH
#     pathremove ~/bin PATH
pathremove () {
    local IFS=':'
    local newpath
    local dir
    local var=${2:-PATH}
    # Bash has ${!var}, but this is not portable.
    for dir in `indirect_expand "$var"`; do
        IFS=''
        if [ "$dir" != "$1" ]; then
            newpath=$newpath:$dir
        fi
    done
    export $var=${newpath#:}
}

# Usage: pathprepend /path/to/bin [PATH]
# Eg, to prepend ~/bin to $PATH
#     pathprepend ~/bin PATH
pathprepend () {
    # if the path is already in the variable,
    # remove it so we can move it to the front
    pathremove "$1" "$2"
    #[ -d "${1}" ] || return
    local var="${2:-PATH}"
    local value=`indirect_expand "$var"`
    export ${var}="${1}${value:+:${value}}"
}

# Usage: pathappend /path/to/bin [PATH]
# Eg, to append ~/bin to $PATH
#     pathappend ~/bin PATH
pathappend () {
    pathremove "${1}" "${2}"
    #[ -d "${1}" ] || return
    local var=${2:-PATH}
    local value=`indirect_expand "$var"`
    export $var="${value:+${value}:}${1}"
}

# Usage: ssource filename
ssource () {
    if [ -r "$1" ]; then
        . "$1"
    fi
}

####### stolen to make oh-my-zsh git plugin work

# We wrap in a local function instead of exporting the variable directly in
# order to avoid interfering with manually-run git commands by the user.
function __git_prompt_git() {
  GIT_OPTIONAL_LOCKS=0 command git "$@"
}

# Outputs the name of the current branch
# Usage example: git pull origin $(git_current_branch)
# Using '--quiet' with 'symbolic-ref' will not cause a fatal error (128) if
# it's not a symbolic ref, but in a Git repo.
function git_current_branch() {
  local ref
  ref=$(__git_prompt_git symbolic-ref --quiet HEAD 2> /dev/null)
  local ret=$?
  if [[ $ret != 0 ]]; then
    [[ $ret == 128 ]] && return  # no git repo.
    ref=$(__git_prompt_git rev-parse --short HEAD 2> /dev/null) || return
  fi
  echo ${ref#refs/heads/}
}

######## end of steal

# Ensure that a non-login, non-interactive shell has a defined environment.
# https://github.com/sorin-ionescu/prezto/blob/master/runcoms/zshenv
if [[ ( "$SHLVL" -eq 1 && ! -o LOGIN ) && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

# https://blog.patshead.com/2011/04/improve-your-oh-my-zsh-startup-time-maybe.html
skip_global_compinit=1
