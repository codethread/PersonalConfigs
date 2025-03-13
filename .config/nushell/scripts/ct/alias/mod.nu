use git.nu *
use node.nu *
use tmux.nu *

# print out a table where each cell is clearly marked, helpful when understanding empty lists
alias table-debug = table --theme thin

#---------------------------------------------#
# EDITOR-ish
# -------------------------------------------#

alias v = nvim
# see https://github.com/wbthomason/packer.nvim/issues/180 for MACOSX_DEPLOYMENT_TARGET=10.15

alias nvim-boot = MACOSX_DEPLOYMENT_TARGET=10.15 nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'

def nvim-profile [] {
	nvim -c "lua require('lazy').profile()"
}

# rg --json hi | lines | each {|| from json } | where type == 'match' | get data

alias vo = ls

#---------------------------------------------#
# WEZTERM
# -------------------------------------------#

def wez_debug [--open] {
	let f = ls ~/.local/share/wezterm/ | sort-by modified | last
	if $open {
		nvim $f.name
	} else {
		tail -f $f.name
	}
}

def wez_emit [blob] {
	printf $"\\033]1337;SetUserVar=event=($blob | to json | base64)\\007"
}

#---------------------------------------------#
# SEARCH
# -------------------------------------------#

def pj [...deps: string] {
	let search = ($deps | str join "|" | $"\"\(($in)\)\"")
	print $search
	rg --glob "**/package.json" $search
}

#---------------------------------------------#
# OS
#---------------------------------------------#

alias mac-dark-toggle = osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to not dark mode'
alias l = ls -a
alias finder = ^open -a 'Finder' .
alias ports = lsof -i tcp:3000

def alert [msg = "Task Finished"] {
	osascript -e $'display notification "($msg)" with title "CMD"'
	afplay /System/Library/Sounds/Glass.aiff
}

# I always forget
def symlink [original: path, symbolic: path] {
	ln -s $original $symbolic
}

#---------------------------------------------#
# HOMEBREW
#---------------------------------------------#

# update brewfile
alias bbd = brew bundle dump
# ensure all installed
alias bbc = brew bundle check
# removed unlisted
alias bbx = brew bundle cleanup
alias bbi = brew bundle install

def brewclean [] {
	brew cleanup
	brew autoremove
}

alias brewdeps = brew deps --graph --installed

#---------------------------------------------#
# KITTY
# -------------------------------------------#

alias ktt = kitty +kitten themes;
alias ktt-dark = kitty +kitten themes --reload-in=all Tokyo Night Storm
alias ktt-light = kitty +kitten themes --reload-in=all Tokyo Night Day

#---------------------------------------------#
# KITTY
# -------------------------------------------#
alias loggy = cd `~/Library/Mobile Documents/iCloud~com~logseq~logseq/`

#---------------------------------------------#
# 1password
# -------------------------------------------#
alias op-goog-token-info = op item get "cli google token"
alias op-goog-token = op read op://perkbox/s46wd4f6paab7ao5cghok3pyy4/credential

alias op-p-auth-info = op item get "perkbox auth header"
alias op-p-auth-get = op read op://perkbox/jtipu4uwq4psxptikwmd7xxt3u/credential
alias op-p-auth-set = op item edit jtipu4uwq4psxptikwmd7xxt3u $'credential=(pbpaste)'

alias op-goog-auth-get = op item get "Perkbox Gmail"
alias op-goog-auth = op read op://perkbox/4ajg7mmj6j23yvkq6kfai52pru/password

# get slack credentials
export def slacky [] {
	let target = "~/.local/share/slacky" | path expand
	mkdir $target
	let p = op-goog-auth
	hide-all {
		(deno run
			--allow-env
			--allow-read
			--allow-write=/var/folders
			--allow-net=127.0.0.1
			--allow-run="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
			~/PersonalConfigs/_scripts/getSlackCreds.ts
			--email adam.hall@perkbox.com
			--password $p
			--domain https://perkbox.slack.com)
	} | from json
	| save ([$target slack.json] | path join)
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

		WAKATIME_HOME

		FZF

		GOBIN
		GOPATH

		VOLTA
		HUSKY
	]

	let hidden = ($env
		| transpose name value
		| filter {|e|
			$allow | any {|s| $e.name starts-with $s } | $in == false
		}
		| get name)

	hide-env ...$hidden
	do $closure
}
