# `.zprofile' is meant as an alternative to `.zlogin' for ksh fans; the two are not intended to be used together, although this could certainly be done if desired

# PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$HOME.rbenv/shims:$HOME/.nodenv/shims:/usr/local/share/dotnet:/opt/X12/bin:$HOME/.dotnet/tools:/Library/Frameworks/Mono.framework/Versions/Current/Commands:/opt/local/bin:$HOME/.bin:$HOME/.local/.bin:$HOME/.cargo/bin:$HOME/.emacs.d/bin:$PATH"


pathappend $HOME.rbenv/shims PATH
pathappend $HOME/.nodenv/shims PATH

pathappend $HOME/.bin
pathappend $HOME/.local/.bin PATH
pathappend $HOME/.cargo/bin PATH
pathappend $HOME/.emacs.d/bin PATH

pathappend $HOME/.dotnet/tools PATH
pathappend /usr/local/share/dotnet PATH
pathappend /opt/X12/bin PATH
pathappend /opt/local/bin PATH

# maconly
if [ "$(uname 2> /dev/null)" != "Linux" ]; then
    pathprepend /usr/local/opt/gnu-sed/libexec/gnubin PATH
    pathappend /Library/Frameworks/Mono.framework/Versions/Current/Commands PATH
fi

# linux
if [ "$(uname 2> /dev/null)" = "Linux" ]; then
    eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)
fi

#------------------------------------------
#--- Sky Stuff
#-----------------------------------------
if [[ $(whoami) =~ 'adh23' ]]; then
    pathappend $HOME/confluent/bin PATH
fi

# The current directory should never be in $PATH
pathremove . PATH
pathremove "" PATH

export EDITOR='vim'
# export TERM=xterm-256color-italic
