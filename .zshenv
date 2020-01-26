# loaded first by by all shells including emacs during command execution

# some ideas on this at: https://bitbucket.org/flowblok/shell-startup/src/default/.shell/env_functions
# and https://blog.flowblok.id.au/2013-02/shell-startup-scripts.html

# Usage: ssource filename
ssource () {
    if [ -r "$1" ]; then
        . "$1"
    fi
}

# https://blog.patshead.com/2011/04/improve-your-oh-my-zsh-startup-time-maybe.html
skip_global_compinit=1

# leaving this here as emacs needs it https://github.com/purcell/exec-path-from-shell
export PATH=/Users/adam/.rbenv/shims:/Users/adam/.nodenv/shims:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/share/dotnet:/opt/X11/bin:~/.dotnet/tools:/Library/Frameworks/Mono.framework/Versions/Current/Commands:/opt/local/bin:/Users/adam/.bin:/Users/adam/.local/.bin:/Users/adam/.cargo/bin:/Users/adam/.emacs.d/bin
