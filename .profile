# ~/.profile: executed by the command interpreter for login shells.

# maconly
if [ "$(uname 2> /dev/null)" != "Linux" ]; then
  [ -d "/usr/local/bin" ] && \
    pathprepend "/usr/local/bin" PATH
  [ -d "/usr/local/opt/gnu-sed/libexec/gnubin" ] && \
    pathprepend "/usr/local/opt/gnu-sed/libexec/gnubin" PATH
  [ -d "/Library/Frameworks/Mono.framework/Versions/Current/Commands" ] && \
    pathprepend "/Library/Frameworks/Mono.framework/Versions/Current/Commands" PATH
fi

# linux
if [ "$(uname 2> /dev/null)" = "Linux" ]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

  # remap capslock to ctrl
  gsettings set org.gnome.desktop.input-sources xkb-options "['ctrl:nocaps']"
fi

export EDITOR='vim'
export SOURCED_PROFILE=true

# Golang
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export GO111MODULE=on

[ -d "$GOBIN" ] && pathprepend "$GOBIN" PATH
[ -d "$HOME/.bin" ] && pathprepend "$HOME/.bin" PATH
[ -d "$HOME/.local/bin" ] && pathprepend "$HOME/.local/bin" PATH
[ -d "$HOME.rbenv/shims" ] && pathprepend "$HOME.rbenv/shims" PATH
[ -d "$HOME/.nodenv/shims" ] && pathprepend "$HOME/.nodenv/shims" PATH
[ -d "$HOME/.jenv/bin" ] && pathprepend "$HOME/.jenv/bin" PATH
[ -d "$HOME/.jenv/shims" ] && pathprepend "$HOME/.jenv/shims" PATH
[ -d "$HOME/.cargo/bin" ] && pathprepend "$HOME/.cargo/bin" PATH
[ -d "$HOME/.emacs.d/bin" ] && pathprepend "$HOME/.emacs.d/bin" PATH
[ -d "$HOME/.dotnet/tools" ] && pathprepend "$HOME/.dotnet/tools" PATH
[ -d "/usr/local/share/dotnet" ] && pathprepend "/usr/local/share/dotnet" PATH
[ -d "/opt/X12/bin" ] && pathprepend "/opt/X12/bin" PATH
[ -d "/opt/local/bin" ] && pathprepend "/opt/local/bin" PATH
[ -d "$HOME/istio-1.5.1/bin" ] && pathprepend "$HOME/istio-1.5.1/bin" PATH
[ -d "$HOME/.jenv/bin:$PATH" ] && pathappend "$HOME/.jenv/bin:$PATH" PATH

# The next line updates PATH for the Google Cloud SDK.
ssource "$HOME/google-cloud-sdk/path.zsh.inc"


# java after jenv
# export JENV_SHELL=zsh
# export JENV_LOADED=1
# unset JAVA_HOME

# jenv() {
#   typeset command
#   command="$1"
#   if [ "$#" -gt 0 ]; then
#     shift
#   fi

#   case "$command" in
#   enable-plugin|rehash|shell|shell-options)
#     eval `jenv "sh-$command" "$@"`;;
#   *)
#     command jenv "$command" "$@";;
#   esac
# }


###############################
# WORK
###############################
if [[ $(whoami) =~ 'adh23' ]]; then
    export TOOLKIT_PATH="$HOME/sky/toolkit"
    export SKYPORT_GRAPHQL_DIR="$HOME/skyport/graphql"
    export SKY_SERVICE_FOLDER="$HOME/sky"
    export SKY_SERVICE_DEV_TOOLS="$SKY_SERVICE_FOLDER/skymobile-service/dev-tools"
    export PATH="$PATH:$SKY_SERVICE_DEV_TOOLS/bin"

    # give sbt enough memory
    export SBT_OPTS="-Xmx4G -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -Xss2M -Duser.timezone=GMT -Xmx6144m"
    ssource ~/.sky_private
fi
