export def to-session [] {
  str replace '.' '_'
}

export def get-panes [] {
  tmux list-panes -F "#{pane_current_command} #{pane_active} #{pane_last} #{pane_current_path}"
  | str trim
  | split row "\n"
  | each { |p| 
    match ($p | split row " ") {
      [$cmd, $active, $lastActive, $currentPath] => {
         { cmd: $cmd, active: ($active == "1"), lastActive: ($lastActive == "1"), currentPath: $currentPath }
      },
    }
  }
}
