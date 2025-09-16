export def nup [arg] { prev $arg }

export def plugs [] {
	help commands | where command_type == "plugin"
}

export def cmds [] {
	help commands | where command_type == "custom" | reject params
}

# checks if `cmd` is an alias or command and returns appropriate info
export def what [...cmd: string] {
	help --find ($cmd | str join " ")
}

export def nud [] {
	let val = $in;
	print $val;
	$val
}

export def pathis [] {
	$env.PATH
}

export def is-not-empty [] {
	is-empty | not $in
}

# logger: print arguments
export def clog [
	title = "log"
	--expand # expand piped input (assumes table input)
	...args
] {
	let val = $in;

	if ($env.CT_LOG? | default '0' | into bool) {
		print $"---- ($title) -----"

		$args | each { print $in }

		if $expand {
			print ($val | table --expand --flatten)
		} else {
			print $val
		}
	}

	$val
}

export def is_work [] {
	($env.CT_USER) == 'work'
}

export def is_home [] {
	($env.CT_USER) == 'home'
}

# print a string, removing all space from the begining of each line
export def dedent [str: string] {
	echo $str
	| lines --skip-empty
	| str trim
	| str join "\n"
}

# format a list as a markdown list
export def md-list []: list<string> -> string {
	str join "\n- " | "- " ++ $in
}

# run a closure and hide nearly all environment variables
export def hide-all [closure: closure] {
	let allow = [TMUX
		TERM
		SHELL
		PWD
		USER
		XPC
		PATH
		HOME
		DOTFILES
		EDITOR

		CT_USER
		CT_NOTES

		RUSTUP_HOME
		CARGO_HOME
		CARGO_BIN

		WAKATIME_HOME

		FZF

		GOBIN
		GOPATH

		VOLTA
		HUSKY
	]

	let hidden = ($env
		| transpose name value
		| where {|e|
			$allow | any {|s| $e.name starts-with $s } | $in == false
		}
		| get name)

	hide-env ...$hidden
	do $closure
}

# export most common envs to a zsh file for compatibility
export def dump-env [] {
	hide-all {|| zsh -c 'export' | rg "(.*)" --replace "export $0" | save -f ~/.config/zsh/.envs }
}

export def lv [...arg: string] {
	with-env {NVIM_APPNAME: Lazyvim} {
		nvim ...$arg
	}
}
export def nvim-sync [] {
	nvim --headless "+Lazy! clean" +qa
	nvim --headless "+Lazy! install" +qa
}

export alias cd0 = cd (ksm key -p P0)
export alias cd1 = cd (ksm key -p P1)
export alias cd2 = cd (ksm key -p P2)
export alias cd3 = cd (ksm key -p P3)
export alias cd4 = cd (ksm key -p P4)
export alias cd5 = cd (ksm key -p P5)
export alias cd6 = cd (ksm key -p P6)
export alias cd7 = cd (ksm key -p P7)
export alias cd8 = cd (ksm key -p P8)
export alias cd9 = cd (ksm key -p P9)
