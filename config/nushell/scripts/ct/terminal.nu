# Terminal and kitty utilities

# Kitty themes
export alias ktt = kitty +kitten themes;
export alias ktt-dark = kitty +kitten themes --reload-in=all Tokyo Night Storm
export alias ktt-light = kitty +kitten themes --reload-in=all Tokyo Night Day

# Wezterm utilities
export def wez_debug [--open] {
	let f = ls ~/.local/share/wezterm/ | sort-by modified | last
	if $open {
		nvim $f.name
	} else {
		tail -f $f.name
	}
}

export def wez_emit [blob] {
	printf $"\\033]1337;SetUserVar=event=($blob | to json | base64)\\007"
}