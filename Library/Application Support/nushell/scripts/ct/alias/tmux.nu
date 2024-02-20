use ct/tmux/projects.nu [get-projects get-project]

export alias tmux-delete-resurrect = rm -rf ~/.local/share/tmux/resurrect

export alias tmux-echo = get-projects

export alias cd0 = cd (get-project 0)
export alias cd1 = cd (get-project 1)
export alias cd2 = cd (get-project 2)
export alias cd3 = cd (get-project 3)
export alias cd4 = cd (get-project 4)
export alias cd5 = cd (get-project 5)
export alias cd6 = cd (get-project 6)
export alias cd7 = cd (get-project 7)
export alias cd8 = cd (get-project 8)
export alias cd9 = cd (get-project 9)
