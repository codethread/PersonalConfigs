const workp = ("~/workfiles/nu/tmux-work-projects.nu" | path expand)

export-env {
  $env.TMUX_SESSION_PROJ_0 = ("~/dev/projects/qmk_firmware/keyboards/preonic/keymaps/codethread" | path expand)
  $env.TMUX_SESSION_PROJ_1 = $env.DOTFILES
  $env.TMUX_SESSION_PROJ_2 = ("~" | path expand)
  $env.TMUX_SESSION_PROJ_3 = ("~" | path expand)
  $env.TMUX_SESSION_PROJ_4 = ("~" | path expand)
  $env.TMUX_SESSION_PROJ_5 = ("~" | path expand)
  $env.TMUX_SESSION_PROJ_6 = ("~" | path expand)
  $env.TMUX_SESSION_PROJ_7 = ("~" | path expand)
  $env.TMUX_SESSION_PROJ_8 = ("~" | path expand)
  $env.TMUX_SESSION_PROJ_9 = ("~" | path expand)

  if ($env.CT_IS_WORK) {
    source (if ($workp | path exists) { $workp } else { "empty.nu" })
  } else {
    $env.TMUX_SESSION_PROJ_2 = ("~/dev/projects/qmk.nvim" | path expand)
    $env.TMUX_SESSION_PROJ_3 = ("~/dev/projects/tstl-prelude" | path expand)
    $env.TMUX_SESSION_PROJ_4 = ("~/dev/learn/rust-playground" | path expand)
    $env.TMUX_SESSION_PROJ_5 = ("~/dev/projects/cold-brew" | path expand)
  }
}
