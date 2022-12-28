# vim:fileencoding=utf-8:foldmethod=marker:foldlevel=0

# only read by login shells (i.e ones opened in a terminal)
# this information is then loaded into that shell, meaning non-login scripts can still use these envs without rerunning the file

#: zsh autoload {{{

# good to put this here as it's only run on login
(
    # <https://github.com/zimfw/zimfw/blob/master/login_init.zsh>
    autoload -U zrecompile

    # Compile zcompdump
    zcompdump="${HOME}/.zcompdump"
    if [[ -s "$zcompdump" && (! -s "${zcompdump}.zwc" || "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
        zrecompile -pq "$zcompdump"
    fi
    # zcompile .zshrc
    # zrecompile -pq ${HOME}/.zshrc
) &!

#: }}}
#: Helpers {{{

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
#: }}}
#: Homebrew {{{

pathprepend "$HOMEBREW_PREFIX/bin" PATH
pathprepend "$HOMEBREW_PREFIX/sbin" PATH

  # use gnu coreutils instead of mac, e.g sed
  # this actually messed with a lot of packages that expected the defaults
  # pathprepend "$BREW_PATH/opt/coreutils/libexec/gnubin" PATH
  # pathprepend "$BREW_PATH/opt/gnu-sed/libexec/gnubin" PATH
  # pathprepend "$BREW_PATH/opt/gnu-tar/libexec/gnubin" PATH

#: }}}

pathprepend "$HOME/.local/bin" PATH

[ -d "$HOME/.emacs.d/bin" ] && pathprepend "$HOME/.emacs.d/bin" PATH

[ -d "$VOLTA_HOME" ] && pathprepend "$VOLTA_HOME/bin" PATH

[ -d "$GOBIN" ] && pathprepend "$GOBIN" PATH
pathprepend "/usr/local/go/bin" PATH

pathprepend "$HOME/.cargo/bin" PATH

pathprepend "$HOME/.luarocks/bin" PATH

[ -d "$HOME/istio-1.5.1/bin" ] && pathprepend "$HOME/istio-1.5.1/bin" PATH

[ -d "$HOME/.rbenv/shims" ] && pathprepend "$HOME/.rbenv/shims" PATH

[ -d "$HOME/.jenv/bin" ] && pathprepend "$HOME/.jenv/bin" PATH
[ -d "$HOME/.jenv/shims" ] && pathprepend "$HOME/.jenv/shims" PATH

#: unity {{{

[ -d "$HOME/.dotnet/tools" ] && pathprepend "$HOME/.dotnet/tools" PATH
[ -d "/usr/local/share/dotnet" ] && pathprepend "/usr/local/share/dotnet" PATH

if [[ -n "${CT_IS_MAC}" ]]; then
  [ -d "/Library/Frameworks/Mono.framework/Versions/Current/Commands" ] && pathprepend "/Library/Frameworks/Mono.framework/Versions/Current/Commands" PATH
fi

#: }}}
