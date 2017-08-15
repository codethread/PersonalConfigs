function kill_tmux_session() {
  if [[ "$1" == '' ]]; then
    echo 'no params: killing spages and spages-overview'
    tmux kill-session -t spages
    tmux kill-session -t spages-overview
  else
    tmux kill-session -t $1
  fi

  tmux ls
}
