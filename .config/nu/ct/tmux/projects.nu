export def get-projects [] {
  let projects = if ($in | is-empty) { load-config } else { $in }

  echo [[key,name]; [P1, ($env.DOTFILES)]] 
  | append $projects.base 
  | match (is_work) {
    true => { $in ++ $projects.work },
    _    => { $in ++ $projects.personal },
  }
  | clog "project list:"
}

export def get-project [proj: string] {
  get-projects
  | where key == $"P($proj)"
  | get 0?.name
}

export def load-config [] {
  echo "~/.local/data/tmux.nuon" 
  | path expand
  | clog "opening config file:"
  | nuopen $in
  | clog "config:" --expand
}
