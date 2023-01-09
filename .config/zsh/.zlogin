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

# personal
pathprepend "$HOME/.local/bin" PATH

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

if [[ -n "${CT_IS_MAC}" ]]; then
    pathprepend "/Library/Frameworks/Mono.framework/Versions/Current/Commands" PATH
fi

#: }}}
