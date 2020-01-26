# only read by login shells (i.e ones opened in a terminal)
# this information is then loaded into that shell, meaning non-login scripts can still use these envs without rerunning the file

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
