use ct/tmux/projects.nu [get-projects get-project]

export alias tmux-delete-resurrect = rm -rf ~/.local/share/tmux/resurrect

export alias tmux-echo = get-projects
