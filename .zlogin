# only read by login shells (i.e ones opened in a terminal)
# this information is then loaded into that shell, meaning non-login scripts can still use these envs without rerunning the file
# PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$HOME.rbenv/shims:$HOME/.nodenv/shims:/usr/local/share/dotnet:/opt/X12/bin:$HOME/.dotnet/tools:/Library/Frameworks/Mono.framework/Versions/Current/Commands:/opt/local/bin:$HOME/.bin:$HOME/.local/.bin:$HOME/.cargo/bin:$HOME/.emacs.d/bin:$PATH"

pathprepend /usr/local/opt/gnu-sed/libexec/gnubin PATH

pathappend $HOME.rbenv/shims PATH
pathappend $HOME/.nodenv/shims PATH

pathappend $HOME/.bin
pathappend $HOME/.local/.bin PATH
pathappend $HOME/.cargo/bin PATH
pathappend $HOME/.emacs.d/bin PATH

pathappend $HOME/.dotnet/tools PATH
pathappend /usr/local/share/dotnet PATH
pathappend /opt/X12/bin PATH
pathappend /Library/Frameworks/Mono.framework/Versions/Current/Commands PATH
pathappend /opt/local/bin PATH

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
