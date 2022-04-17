# ~/.profile: executed by the command interpreter for login shells.

#------------------------------------------
#--- mac only
#-----------------------------------------
if [ "$(uname 2> /dev/null)" != "Linux" ]; then

  # one of these will be installed depending on intel/arm
  if [ -d "/usr/local/bin" ]; then
    pathprepend "/usr/local/bin" PATH
    BREW_PATH="/usr/local"
  fi

  if [ -d "/opt/homebrew/bin" ]; then
    pathprepend "/opt/homebrew/bin" PATH
    BREW_PATH="/opt/homebrew"
  fi

  # use gnu coreutils instead of mac, e.g sed
  pathprepend "$BREW_PATH/opt/coreutils/libexec/gnubin" PATH
  pathprepend "$BREW_PATH/opt/gnu-sed/libexec/gnubin" PATH
  pathprepend "$BREW_PATH/opt/gnu-tar/libexec/gnubin" PATH

  [ -d "/usr/local/opt/python/libexec/bin" ] && pathprepend "/usr/local/opt/python/libexec/bin" PATH
fi

#------------------------------------------
#--- linux only
#-----------------------------------------
if [ "$(uname 2> /dev/null)" = "Linux" ]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

  # remap capslock to ctrl
  gsettings set org.gnome.desktop.input-sources xkb-options "['ctrl:nocaps']"

  [ -d "/opt/X12/bin" ] && pathprepend "/opt/X12/bin" PATH
  [ -d "/opt/local/bin" ] && pathprepend "/opt/local/bin" PATH
fi

export EDITOR='nvim'
export MANPAGER="/bin/sh -c \"col -b | nvim -c 'set ft=man ts=8 nomod nolist nonu' -\""

# Emacs
#------------------------------------------
# https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
export LSP_USE_PLISTS=true

# Personal shared
#------------------------------------------
[ -d "$HOME/.bin" ] && pathprepend "$HOME/.bin" PATH
[ -d "$HOME/bin" ] && pathprepend "$HOME/bin" PATH
[ -d "$HOME/.local/bin" ] && pathprepend "$HOME/.local/bin" PATH
[ -d "$HOME/.emacs.d/bin" ] && pathprepend "$HOME/.emacs.d/bin" PATH

# Golang
#------------------------------------------
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export GO111MODULE=on
[ -d "$GOBIN" ] && pathprepend "$GOBIN" PATH

# node
#------------------------------------------
export VOLTA_HOME="$HOME/.volta"
[ -d "$VOLTA_HOME/bin" ] && pathprepend "$VOLTA_HOME/bin" PATH

# ruby
#------------------------------------------
[ -d "$HOME/.rbenv/shims" ] && pathprepend "$HOME/.rbenv/shims" PATH

# jvm
#------------------------------------------
[ -d "$HOME/.jenv/bin" ] && pathprepend "$HOME/.jenv/bin" PATH
[ -d "$HOME/.jenv/shims" ] && pathprepend "$HOME/.jenv/shims" PATH

# rust
#------------------------------------------
[ -d "$HOME/.cargo/bin" ] && pathprepend "$HOME/.cargo/bin" PATH

# unity
#------------------------------------------
[ -d "$HOME/.dotnet/tools" ] && pathprepend "$HOME/.dotnet/tools" PATH
[ -d "/usr/local/share/dotnet" ] && pathprepend "/usr/local/share/dotnet" PATH

if [ "$(uname 2> /dev/null)" != "Linux" ]; then
  [ -d "/Library/Frameworks/Mono.framework/Versions/Current/Commands" ] && pathprepend "/Library/Frameworks/Mono.framework/Versions/Current/Commands" PATH
fi

# misc
#------------------------------------------
[ -d "$HOME/istio-1.5.1/bin" ] && pathprepend "$HOME/istio-1.5.1/bin" PATH

ssource ~/.private

###############################
# WORK
###############################
if [[ $(whoami) =~ 'adh23' ]]; then
    [ -d "$HOME/confluent/bin" ] && pathappend "$HOME/confluent/bin" PATH

    # give sbt enough memory
    export SBT_OPTS="-Xmx4G -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -Xss2M -Duser.timezone=GMT -Xmx6144m"
    ssource ~/.sky_private
fi
