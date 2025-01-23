export def get-projects [] {
	let projects = if ($in | is-empty) { load-config } else { $in }

	echo [[key,name]; [P1, ($env.DOTFILES)]]
	| append $projects.base
	| (match (is_work) {
		true => { $in ++ $projects.work },
		_    => { $in ++ $projects.personal },
	})
	| clog "project list:"
}

export def get-project [proj: string] {
	get-projects
	| where key == $"P($proj)"
	| get 0?.name
}

export def load-config [] {
	# nvim --clean --headless +'echo "hi"' +qall e+o>| tmux display-message $in

	echo "~/.local/data/tmux.nuon"
	| path expand
	| clog "opening config file:"
	| open
	| clog "config:" --expand
	| upsert dirs {|c|
		$c.dirs ++ ($c.dirs_special | each {|s| run-external $s.cmd ...$s.args e+o>| })
	}
}
