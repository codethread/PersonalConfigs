# Check that files won't be overriten during the linking stage, exit with an error if not
export def assert-no-conflicts [
	--force # delete existing files without confirmation
]: table -> table {
	let proj = $in

	let files = $proj
	| get files
	| each {|| get symlink }
	| reduce {|it,acc| $it ++ $acc }

	$files | uniq --repeated | match ($in | is-empty) {
		false => {
			let msg = $"multiple files attempted to write to ($in | str join ","). Process cancelled"
			error make -u { msg: $msg }
		}
		_ => { $files }
	}

	let existing_files = $files | par-each {|file| { file: $file, exists: (file-exists $file) } } | where exists == true | get file

	if (($existing_files | is-not-empty) and $force) {
		$existing_files | each {|f| rm $f }
	} else if ($existing_files | is-not-empty) {
		print "A number of files already exist:"
		$existing_files | to text | print
		let choice = input $"would you like to remove these? [(ansi default_bold)Y(ansi reset)/n]: "

		if ($choice != 'n') {
			$existing_files | each {|f| rm $f }
		} else {
			error make -u { msg: "you'll have to remove them manually or change some file to proceed, alternativly you can pass the --force flag to overwrite them" }
		}
	}

	$proj
}

def file-exists [file: path] {
	($file | path exists) and (is-symlink $file | not $in)
}

def is-symlink [file:path] {
	($file | path exists) and (do { ^test -L $file } | complete | get exit_code | into bool | not $in )
}
