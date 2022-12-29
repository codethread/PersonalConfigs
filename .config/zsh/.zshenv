# vim:fileencoding=utf-8:foldmethod=marker:foldlevel=0

# loaded first by by all shells including emacs during command execution

#: General {{{

export DOTFILES="${HOME}/PersonalConfigs"
export EDITOR='nvim'
export MANPAGER="/bin/sh -c \"col -b | nvim -c 'set ft=man ts=8 nomod nolist nonu' -\""
export MANWIDTH=80
export WAKATIME_HOME="$HOME/.config/wakatime"

#: }}}
#: Profile {{{

# enable this and zshrc `zprof` for profiling
# zmodload zsh/zprof

PROFILE_STARTUP=false
if [[ "$PROFILE_STARTUP" == true ]]; then
    # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
    PS4=$'%D{%M%S%.} %N:%i> '
    exec 3>&2 2>$HOME/tmp/startlog.$$
    setopt xtrace prompt_subst
fi

#: }}}
#: Helpers {{{

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

# https://blog.patshead.com/2011/04/improve-your-oh-my-zsh-startup-time-maybe.html
skip_global_compinit=1

#: }}}
#: VARS {{{

OS="$(uname)"
if [[ "${OS}" == "Linux" ]]; then
    export CT_IS_LINUX=1
elif [[ "${OS}" == "Darwin" ]]; then
    export CT_IS_MAC=1

    if [[ $(/usr/bin/uname -m) == "arm64" ]]; then 
        export CT_IS_ARM=1
    else 
        export CT_IS_ARM=0
    fi
else
    abort "why you no OS?"
fi

CT="$(whoami)"
if [[ "${CT}" == "adam" ]]; then
    export CT_IS_LAPTOP=1
    export CT_IS_WORK=0
elif [[ "${CT}" == "codethread" ]]; then
    export CT_IS_MINI=1
    export CT_IS_WORK=0
else
    export CT_IS_WORK=1
fi

#: }}}
#: Homebrew {{{

# hard code HOMEBREW path as fallback on new machine
if [[ -n "${CT_IS_MAC}" ]]; then
    [[ -n "${CT_IS_ARM}" ]] && export HOMEBREW_PREFIX="/opt/homebrew" || export HOMEBREW_PREFIX="/usr/local";
else
    export HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew";
fi
ssource "${ZDOTDIR}/shellenv.zsh"

if [[ -n "${CT_IS_MINI}" ]]; then
    export HOMEBREW_BUNDLE_FILE="${DOTFILES}/.config/cold-brew/Brewfile.mini.conf"
elif [[ -n "${CT_IS_LAPTOP}" ]]; then
    export HOMEBREW_BUNDLE_FILE="${DOTFILES}/.config/cold-brew/Brewfile.macbook.conf"
else
    export HOMEBREW_BUNDLE_FILE="${DOTFILES}/.config/cold-brew/Brewfile.work.conf"
fi

#: }}}
#: Language specific {{{

#: node {{{

export VOLTA_HOME="$HOME/.volta"
export HUSKY=0 # I don't need my hand holding, thanks

#: }}}

#: Golang {{{

export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export GO111MODULE=on

#: }}}

#: haskell {{{

# source ~/.ghcup/env

#: }}}

#: }}}
