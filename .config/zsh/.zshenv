# vim:fileencoding=utf-8:foldmethod=marker:foldlevel=0

# loaded first by by all shells including emacs during command execution

#: General {{{

export EDITOR='nvim'
export MANPAGER="/bin/sh -c \"col -b | nvim -c 'set ft=man ts=8 nomod nolist nonu' -\""
export MANWIDTH=80
export WAKATIME_HOME="$HOME/.config/wakatime"
export LESSHISTFILE="-" # no .lesshst

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
    export CT_IS_LINUX=0

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
    export CT_IS_LAPTOP=0
    export CT_IS_MINI=1
    export CT_IS_WORK=0
else
    export CT_IS_WORK=1
fi


function ct() {
    echo "CT_IS_MAC: ${CT_IS_MAC}"
    echo "CT_IS_ARM: ${CT_IS_ARM}"
    echo "CT_IS_LINUX: ${CT_IS_LINUX}"
    echo "CT_IS_LAPTOP: ${CT_IS_LAPTOP}"
    echo "CT_IS_MINI: ${CT_IS_MINI}"
    echo "CT_IS_WORK: ${CT_IS_WORK}"
}

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
elif [[ -n "${CT_IS_WORK}" ]]; then
    export HOMEBREW_BUNDLE_FILE="${DOTFILES}/.config/cold-brew/Brewfile.work.conf"
else
    export HOMEBREW_BUNDLE_FILE="${DOTFILES}/.config/cold-brew/Brewfile.basic.conf"
fi

export DOTFILES="${HOME}/PersonalConfigs"

if [ "${CT_IS_WORK}" -eq 1 ]; then
  export DOTTY="${HOME}/PersonalConfigs:${HOME}:${HOME}/workfiles:${HOME}"
else
  export DOTTY="${HOME}/PersonalConfigs:${HOME}"
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

#: rust {{{

if [ "${CT_IS_WORK}" -eq 1 ]; then
  export RUSTFLAGS="-C link-arg=-fuse-ld=/opt/homebrew/opt/llvm/bin/ld64.lld"
fi

#: }}}

#: }}}
#: Helpers {{{

# some ideas on this at: https://bitbucket.org/flowblok/shell-startup/src/default/.shell/env_functions
# and https://blog.flowblok.id.au/2013-02/shell-startup-scripts.html

# Usage: indirect_expand PATH -> $PATH
indirect_expand () {
    env | sed -n "s/^$1=//p"
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
    if [ -d "${1}" ]; then
    # if the path is already in the variable,
    # remove it so we can move it to the front
    pathremove "$1" "$2"
    #[ -d "${1}" ] || return
    local var="${2:-PATH}"
    local value=`indirect_expand "$var"`
    export ${var}="${1}${value:+:${value}}"
    fi
}

# Usage: pathappend /path/to/bin [PATH]
# Eg, to append ~/bin to $PATH
#     pathappend ~/bin PATH
pathappend () {
    if [ -d "${1}" ]; then
        pathremove "${1}" "${2}"
        #[ -d "${1}" ] || return
        local var=${2:-PATH}
        local value=`indirect_expand "$var"`
        export $var="${value:+${value}:}${1}"
    fi
}

#: }}}
#: PATH {{{

# Brew 
pathprepend "$HOMEBREW_PREFIX/bin" PATH
pathprepend "$HOMEBREW_PREFIX/sbin" PATH

# emacs
pathprepend "$HOME/.emacs.d/bin" PATH

# node.js
pathprepend "$VOLTA_HOME/bin" PATH

# go
pathprepend "$GOBIN" PATH
pathprepend "/usr/local/go/bin" PATH

# rust
pathprepend "$HOME/.cargo/bin" PATH

# lua
pathprepend "$HOME/.luarocks/bin" PATH

# k8
pathprepend "$HOME/istio-1.5.1/bin" PATH

# ruby
pathprepend "$HOME/.rbenv/shims" PATH

# haskell
pathprepend "$HOME/.ghcup/bin" PATH

# java
pathprepend "$HOME/.jenv/bin" PATH
pathprepend "$HOME/.jenv/shims" PATH

# unity
pathprepend "$HOME/.dotnet/tools" PATH
pathprepend "/usr/local/share/dotnet" PATH

# ruby for gem install on m1 mac
pathprepend "/opt/homebrew/opt/ruby/bin" PATH
pathprepend "/opt/homebrew/lib/ruby/gems/3.1.0/bin" PATH

# personal
pathprepend "$HOME/.local/bin" PATH

pathprepend "$HOME/nu" PATH


if [[ -n "${CT_IS_MAC}" ]]; then
    pathprepend "/Library/Frameworks/Mono.framework/Versions/Current/Commands" PATH
fi

#: }}}
