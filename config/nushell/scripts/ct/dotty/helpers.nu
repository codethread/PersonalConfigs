# Check that files won't be overriten during the linking stage, exit with an error if not
export def assert-no-conflicts [
	--force # delete existing files without confirmation
]: table -> table {
	let proj = $in

	let files = $proj
	| get files
	| each {|| get target }
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
	($file | path exists) and (do { ^test -L $file } | complete | get exit_code) == 0
}

# Detect overlapping paths between configurations (e.g., foo vs foo/bar)
# This prevents conflicts between directory and file configurations
export def detect-path-overlaps [configs] {
	mut conflicts = []
	let config_list = ($configs | enumerate)

	for config_entry in $config_list {
		let config = $config_entry.item
		let config_idx = $config_entry.index

		for other_entry in ($config_list | where index > $config_idx) {
			let other = $other_entry.item
			# Use str replace to expand ~ without following symlinks
			let config_path = ($config.target | str replace "~" $env.HOME | path expand -n)
			let other_path = ($other.target | str replace "~" $env.HOME | path expand -n)

			# Only check for conflicts if at least one project has link_directory = true
			let config_is_dir = ($config.link_directory? | default false)
			let other_is_dir = ($other.link_directory? | default false)

			# Skip if both are individual file links (not directory links)
			if (not $config_is_dir) and (not $other_is_dir) {
				continue
			}

			# Check for exact path matches (conflict when at least one is a directory link)
			if ($config_path == $other_path) {
				let conflict_entry = {
					config1: $config.name,
					path1: $config_path,
					link_directory1: $config_is_dir,
					config2: $other.name,
					path2: $other_path,
					link_directory2: $other_is_dir
				}
				$conflicts = ($conflicts | append $conflict_entry)
			} else {
				# Check for path overlaps (parent/child relationships)
				if ($config_path | str starts-with $"($other_path)/") or ($other_path | str starts-with $"($config_path)/") {
					let conflict_entry = {
						config1: $config.name,
						path1: $config_path,
						link_directory1: $config_is_dir,
						config2: $other.name,
						path2: $other_path,
						link_directory2: $other_is_dir
					}
					$conflicts = ($conflicts | append $conflict_entry)
				}
			}
		}
	}

	if ($conflicts | is-not-empty) {
		let msg = ($conflicts
			| each {|c| $"Path overlap detected: '($c.config1)' \(($c.path1), link_directory=($c.link_directory1)) conflicts with '($c.config2)' \(($c.path2), link_directory=($c.link_directory2))" }
			| str join "\n"
		)
		error make -u { msg: $"Configuration conflicts found:\n($msg)" }
	}
}
