function kill_tmux_session() {
  if [[ "$1" == '' ]]; then
    echo 'no params: killing spages'
    tmux kill-session -t spages
  else
    tmux kill-session -t $1
  fi

  tmux ls
}
