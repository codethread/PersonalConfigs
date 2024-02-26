use ct/tmux/utils.nu get-panes

export def openLinkInVim [...path] {
  let home = $env.HOME
  let chosen = $path | str trim | str replace "│" "" # don't grab the tmux border char please

  let panes = get-panes

  let is_active = $panes | where { $in.cmd == 'nvim' and $in.active == true } | is-empty | $in == false

  let filePWD = match ($is_active) {
    true =>  { $panes | where lastActive == true | first | get currentPath },
    false => { $panes | where active == true | first | get currentPath }
  }

  let editorPWD = $panes | where cmd == 'nvim' | first | get currentPath

  let diff = $editorPWD | path relative-to $filePWD

  match ($chosen | split row ":") {
    [$file, $line, ..] => {
      let path = [$editorPWD $diff $file] | path join
      tmux send-keys -t 1 :e Space +$line Space $path Enter
    },
    [$file] => {
      let path = [$editorPWD $diff $file] | path join
      tmux send-keys -t 1 :e Space $path Enter
    },
    [] => { tmux display-message "no path selected"},
  };

  tmux select-pane -t 1
}

