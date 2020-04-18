# ~/.profile: executed by the command interpreter for login shells.

# maconly
if [ "$(uname 2> /dev/null)" != "Linux" ]; then
  [ -d "/usr/local/bin" ] && PATH="/usr/local/bin:$PATH"
  [ -d "/usr/local/opt/gnu-sed/libexec/gnubin" ] && PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
  [ -d "/Library/Frameworks/Mono.framework/Versions/Current/Commands" ] && PATH="/Library/Frameworks/Mono.framework/Versions/Current/Commands:$PATH"
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

[ -d "$GOBIN" ] && PATH="$GOBIN:$PATH"
[ -d "$HOME/.bin" ] && PATH="$HOME/.bin:$PATH"
[ -d "$HOME/.local/bin" ] && PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME.rbenv/shims" ] && PATH="$HOME.rbenv/shims:$PATH"
[ -d "$HOME/.nodenv/shims" ] && PATH="$HOME/.nodenv/shims:$PATH"
[ -d "$HOME/.cargo/bin" ] && PATH="$HOME/.cargo/bin:$PATH"
[ -d "$HOME/.emacs.d/bin" ] && PATH="$HOME/.emacs.d/bin:$PATH"
[ -d "$HOME/.dotnet/tools" ] && PATH="$HOME/.dotnet/tools:$PATH"
[ -d "/usr/local/share/dotnet" ] && PATH="/usr/local/share/dotnet:$PATH"
[ -d "/opt/X12/bin" ] && PATH="/opt/X12/bin:$PATH"
[ -d "/opt/local/bin" ] && PATH="/opt/local/bin:$PATH"

