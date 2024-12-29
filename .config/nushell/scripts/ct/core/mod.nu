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

	if ($env.CT_LOG? | default false) {
		print $"---- ($title) -----"

		$args | each { print $in }

		if $expand {
			print ($val | table --expand)
		} else {
			print $val
		}
	}

	$val
}

export def is_work [] {
	(whoami) == 'adam.hall'
}

export def is_home [] {
	(whoami) == 'codethread'
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
