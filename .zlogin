(
    # <https://github.com/zimfw/zimfw/blob/master/login_init.zsh>
    autoload -U zrecompile

    # Compile zcompdump, if modified, to increase startup speed.
    zcompdump="${HOME}/.zcompdump"
    if [[ -s "$zcompdump" && (! -s "${zcompdump}.zwc" || "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
        zrecompile -pq "$zcompdump"
    fi
    # zcompile .zshrc
    # zrecompile -pq ${HOME}/.zshrc
) &!
