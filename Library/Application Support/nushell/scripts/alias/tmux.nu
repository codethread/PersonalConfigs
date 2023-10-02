def get-project-num [proj: string] {
  get-projects 
  | where key == $"P($proj)"
  | if ($in | is-empty) { null } else ($in | first | $in.name)
}

def get-projects [] {
  let projects = (open ~/.local/data/tmux.nuon)
  let base = [[key,name]; [P1, ($env.DOTFILES)]] | append $projects.base 

  $base ++ (if ($env.CT_USER == work) { $projects.work } else { $projects.personal })
}

export alias tmux-delete-resurrect = rm -f ~/.local/share/tmux/resurrect

export def tmux-echo [] { 
  get-projects
}

export alias cd0 = cd (get-project-num 0)
export alias cd1 = cd (get-project-num 1)
export alias cd2 = cd (get-project-num 2)
export alias cd3 = cd (get-project-num 3)
export alias cd4 = cd (get-project-num 4)
export alias cd5 = cd (get-project-num 5)
export alias cd6 = cd (get-project-num 6)
export alias cd7 = cd (get-project-num 7)
export alias cd8 = cd (get-project-num 8)
export alias cd9 = cd (get-project-num 9)
