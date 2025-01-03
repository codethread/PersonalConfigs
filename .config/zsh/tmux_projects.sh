#!/usr/bin/env bash
# reload tmux.conf <C-a R> to update projects

export TMUX_SESSION_PROJ_0="${HOME}/dev/projects/qmk_firmware/keyboards/preonic/keymaps/codethread"
export TMUX_SESSION_PROJ_1="${DOTFILES}"
export TMUX_SESSION_PROJ_2="${HOME}/"
export TMUX_SESSION_PROJ_3="${HOME}/"
export TMUX_SESSION_PROJ_4="${HOME}/"
export TMUX_SESSION_PROJ_5="${HOME}/"
export TMUX_SESSION_PROJ_6="${HOME}/"
export TMUX_SESSION_PROJ_7="${HOME}/"
export TMUX_SESSION_PROJ_8="${HOME}/"
export TMUX_SESSION_PROJ_9="${HOME}/"

if [ $CT_IS_WORK -eq 1 ]; then
  ssource "${HOME}/.config/tmux/work_tmux_projects"
else
  export TMUX_SESSION_PROJ_2="${HOME}/dev/projects/qmk.nvim"
  export TMUX_SESSION_PROJ_3="${HOME}/dev/projects/tstl-prelude"
  export TMUX_SESSION_PROJ_4="${HOME}/dev/learn/rust-playground"
  export TMUX_SESSION_PROJ_5="${HOME}/dev/projects/cold-brew"
fi

tmux-echo() {
  echo 0 ${TMUX_SESSION_PROJ_0}
  echo 1 ${TMUX_SESSION_PROJ_1}
  echo 2 ${TMUX_SESSION_PROJ_2}
  echo 3 ${TMUX_SESSION_PROJ_3}
  echo 4 ${TMUX_SESSION_PROJ_4}
  echo 5 ${TMUX_SESSION_PROJ_5}
  echo 6 ${TMUX_SESSION_PROJ_6}
  echo 7 ${TMUX_SESSION_PROJ_7}
  echo 8 ${TMUX_SESSION_PROJ_8}
  echo 9 ${TMUX_SESSION_PROJ_9}
}

alias cd0="cd \${TMUX_SESSION_PROJ_0}"
alias cd1="cd \${TMUX_SESSION_PROJ_1}"
alias cd2="cd \${TMUX_SESSION_PROJ_2}"
alias cd3="cd \${TMUX_SESSION_PROJ_3}"
alias cd4="cd \${TMUX_SESSION_PROJ_4}"
alias cd5="cd \${TMUX_SESSION_PROJ_5}"
alias cd6="cd \${TMUX_SESSION_PROJ_6}"
alias cd7="cd \${TMUX_SESSION_PROJ_7}"
alias cd8="cd \${TMUX_SESSION_PROJ_8}"
alias cd9="cd \${TMUX_SESSION_PROJ_9}"
