#!/bin/bash
kill_tmux_session() {
    if [[ "$1" == '' ]]; then
        echo 'no params: killing spages'
        tmux kill-session -t spages
    else
        for tmux_session in "$@"
        do
            echo "killing: $tmux_session"
            tmux kill-session -t "$tmux_session"
        done
    fi
    tmux ls
}
