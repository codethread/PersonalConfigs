export use sessionizer.nu
export use switch.nu
export use open-link.nu
export use notes.nu
use utils.nu

# Find the first pane running an interesting process and call tmux-fingers on
# it
export def finger-other-pane [] {
	# list of 'watch' process, and if these are present, I likely want to grab
	# a url from one of these
	let pss = [
		volta-shim
		cargo-watch
	]

	let panes = utils get-panes

	if ($panes| where zoomed == true | is-not-empty) {
		tmux display-message 'Not available in zoomed pane'
		return
	}

	match ([
		($panes | where cmd in $pss | get -i 0?.index)
		($panes | where lastActive == true | get -i 0?.index)
		($panes | where cmd != nvim | get -i 0?.index)
	] | compact) {
		[] => { tmux display-message "no-match" }
		[$pane, ..] => {
			tmux-fingers start $pane
			tmux select-pane -t 1
		}
	}
}
