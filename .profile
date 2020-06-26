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

export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export EDITOR='vim'
export SOURCED_PROFILE=true
export GO111MODULE=on

[ -d "$GOBIN" ] && pathprepend "$GOBIN" PATH
[ -d "$HOME/.bin" ] && pathprepend "$HOME/.bin" PATH
[ -d "$HOME/.local/bin" ] && pathprepend "$HOME/.local/bin" PATH
[ -d "$HOME.rbenv/shims" ] && pathprepend "$HOME.rbenv/shims" PATH
[ -d "$HOME/.nodenv/shims" ] && pathprepend "$HOME/.nodenv/shims" PATH
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

