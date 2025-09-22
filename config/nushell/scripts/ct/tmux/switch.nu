# alternative to fzf scripts which seem to fallover a lot
export def main [] {
	let attatched = (tmux list-session
		| lines
		| find -i "(attached)"
		| parse "{name}:{rest}"
		| get name
		| first)

	let sessions = (tmux list-session
		| lines
		| parse "{name}:{rest}"
		| get name
		| where $it !~ $attatched)

	let target = (echo $sessions
		| str join "\n"
		| fzf-tmux -p -w 80% -h 70% --preview $"($env.HOME)/.config/tmux/plugins/tmux-fzf/scripts/.preview {}" --preview-window "right,70%,follow,border-left"
		| complete)

	# see fzf: exit status
	match [$target.exit_code, $target.stdout] {
		[130, _] => {} # exit C-c / Esc
		[1, _] => {} # no selection
		[0, $chosen] => { tmux switch-client -t ($target.stdout | str trim) }
		_ => {
			print $"(ansi red)something went wrong(ansi reset), probably in fzf-tmux"
			$target
		}
	}
}
