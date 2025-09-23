# :module: Project configuration loader with work/personal context switching

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
	echo "~/.local/data/tmux.nuon"
	| path expand
	| clog "opening config file:"
	| open
	| clog "config:" --expand
	| upsert dirs {|c|
		# TODO: move work stuff somewhere better
		let work_conf = $env.XDG_DATA_HOME | path join "pb" "gitlab.json"
		if ($work_conf | path exists) {
			get_work_projects $work_conf
		} else { [] }
		| append ($c.dirs | expand_globs)
		| append ($c.dirs_special | par-each {|s| run-external $s.cmd ...$s.args e+o>| })
	}
}

# reads all files that exist from the gitlab index
def get_work_projects [file: path] {
	# an alternative (but slower approach) is as follows, where depth of 5 is arbitrary
	# fd -E src --type=dir --prune --hidden --case-sensitive --maxdepth 5 -F '.git' work
	$file | open
	| par-each {|| ([$env.HOME work $in.fullPath] | path join) }
	| where {|| $in | path exists }
	| where $it !~ '/work/app/'
	| path dirname | uniq
}

def expand_globs []: list<string> -> list<string> {
	par-each {|p|
		if ($p | str ends-with '*') {
			glob --no-file --depth=1 --exclude [**/.git/** **/.gitconfig/**] $p
		} else { $p }
	} | flatten
}
