
const USER_CONF: path = (echo `~/Library/Application Support/Cursor/User` | path expand)

export alias _cursor_open_config = nvim $USER_CONF

# script to walk through linking cursor config to vscode.
export def _cursor_link [] {
	let base = [Library `Application Support`]
	let p = {|parts: list<string> | $parts | path join | path expand }
	let cursor_files = [
		[cursor, code];
		[(do $p [~ ...base Cursor/User/settings.json]) (do $p [$env.DOTFILES ...base Code/User/settings.json]) ]
		[(do $p [~ ...base Cursor/User/keybindings.json]) (do $p [$env.DOTFILES ...base Code/User/keybindings.json]) ]
	]

	print $cursor_files
	exit 0

	# check-assumptions $cursor_files

	prompt-to-close
	echo "Removing existing settings to avoid code settings getting altered"

	($cursor_files | filter { $in | path exists } | each {|f|
		print $"(ansi green)Removing(ansi reset) ($f)"
		rm $f
	})

	prompt-to-import
	input "Confirm you have imported settings <enter>:"
	input "Sure?"

	prompt-to-close

	echo "Linking settings and keybindings"

	($cursor_files | each {|f|
		print $"(ansi magenta)Deleting(ansi reset) ($f)"
		rm $f
		let $vscode_original = $f | str replace "Cursor" "Code"
		print $"(ansi green)Linking(ansi reset) ($f) ($vscode_original)"
		ln -s $vscode_original $f
	})

	ignore
}

def prompt-to-close [] {
	let get_cursor = {|| ps | where name == Cursor | is-not-empty }
	mut cursor_running = do $get_cursor
	while $cursor_running {
		input "Cursor running. Close it then <enter>"
		$cursor_running = do $get_cursor
	}
}

def prompt-to-import [] {
	let get_cursor = {|| ps | where name == Cursor | is-empty  }
	if (do $get_cursor) {
		print "Cursor closed, opening..."
		cursor
	}

	mut cursor_running = (do $get_cursor)
	while $cursor_running {
		print "Cursor not running. waiting..."
		sleep 3sec
		$cursor_running = do $get_cursor
	}

	print $"Cursor open, go (ansi magenta)import your settings(ansi reset)"
}

def check-assumptions [files: list<string>] {
	let target = $USER_CONF

	if not ($target | path exists) { return }

	let existing = ls $target

	if (($files | length) != ($existing | length)) {
		print $"(ansi yellow)WARNING: additional files that may need porting"
		print "Existing" $existing
		print "Expected" $files
	}
}
