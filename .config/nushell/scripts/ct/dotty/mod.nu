# The dotty module exposes tools to sync directories to a target directory, i.e
# dotfiles from ~/some_dir into ~/
#
# `link` is the main command of interest
#
# Editor tooling can also be setup to run changes as needed, see `.config/nvim/lua/codethread/dotty.lua` for my reference
# - `dotty is-cwd` can help identify if dotty should be run for the given project
# - `dotty test ...files` can check if saving a file should trigger a re-sync
# - `dotty format` can display the output of `dotty link` in a nicer view for editors

use ct/core [dedent is-not-empty md-list]
export use cache.nu
export use config.nu
use helpers.nu [assert-no-conflicts]
use list-files.nu

export def link [
	--no-cache(-c)
	--force(-f)
]: nothing -> table<name: string, created: table, deleted: table> {
	config load
	| par-each { |proj| get-project-files-to-link $proj $no_cache  }
	| assert-no-conflicts --force=$force
	| par-each {|proj|
		# delete
		$proj.delete | each {|f| rm -f $f }

		# create
		$proj.files | get to | list-dirs-to-make | par-each {|dir| mkdir $dir }

		$proj.files
		| par-each {|f|
			# `try` because sometimes cache isn't up-to-date, so a link might
			# be recreated. This is trusting assert-no-conflicts to do it's job
			ln -s $f.from $f.to | complete | ignore
		}

		# update cache
		$proj.existing ++ ($proj.files | get file)
		| sort
		| cache store $proj.name

		$proj
		| rename --column { files: created, delete: deleted }
		| select name root created deleted
	}
	| filter {|r| ($r.created | is-not-empty) or ($r.deleted | is-not-empty) }
}

# Format the output from `dotty link` into something easier to read, e.g in an
# editor cli
export def format []: table -> record<changes: bool, diff: string> {
	let links = $in
	let formatted = ($links
		| each {|proj|
			let has_created = ($proj.created | is-not-empty)
			let has_deleted = ($proj.deleted | is-not-empty)

			let created = $"Created:\n($proj.created | get file | md-list)\n"
			let deleted = $"Deleted:\n($proj.deleted | md-list)\n"

			$"## Project: ($proj.name)\n\n(if $has_created { $created } else "")(if $has_deleted { $deleted } else "")"
		}
		| str join ''
	)

	{
		changes: ($links | is-not-empty)
		diff: $formatted
	}
}

# Check if the given `dir` (defaults to PWD) is part of any dotty projects
# dir is only checked against the root of a dotty project, not a nested one
export def is-cwd [
	dir?: path # directory path to check
	--exit # returns an exit code rather than true/false
] {
	let target = if ($dir | is-not-empty) { $dir } else $env.PWD
	let proj = (config load | where from == $target)
	match ([$exit, ($proj | is-empty)]) {
		[true, true] => { exit 1 },
		[true, false] => { exit 0 }
		[_, $is_cwd] => { not $is_cwd }
	}
}

# Remove symlinks that don't point to anything
export def prune [target: glob = ~/.config/**/*] {
	ls -all --long ...(glob $target)
	| where type == symlink
	| where ($it.target | path exists | $in == false)
	| each { |f|
		print $"removing ($f.name)";
		rm $f.name
	}
}

# Test if a list of files are part of a dotty project at any depth.
# Files are compared against the dotty project of the PWD.
# Expected to be called by other tools, so throws errors
export def test [...files] {
	let proj = config load | where from == $env.PWD
	if ($proj | is-empty) {
		error make -u { msg: "not a project" }
	}

	let proj = $proj | first

	let files = $files | each {|f| [$env.PWD $f] | path join } | path expand

	let non_files = $files | filter { $in | path exists | $in == false }

	if ($non_files | is-not-empty) {
		error make -u { msg: (err_format "not real files" $non_files) }
	}

	let files = $files | path relative-to $env.PWD

	let all_files = list-files $proj.from --excludes $proj.excludes

	let invalid = $files | where { $in not-in $all_files }

	if ($invalid | is-not-empty) {
		error make -u { msg: (err_format "non project files" $invalid) }
	}
}

export def teardown [] {
	config load
	| par-each {|proj|
		cd $proj.to
		let files = cache load $proj.name

		$files | par-each {|f| rm -f $f }
		$files | delete-empty-dirs

		cache delete $proj.name
	}
}

export def chmod [] {
	ls ([$env.DOTFILES .local/bin] | path join)
	| where type == file
	| par-each {|f| run-external chmod +x $f.name }
}

def err_format [ title, files ] {
	$"($title):\n($files | to text)"
}

def delete-empty-dirs []: list<string> -> list<string> {
	list-dirs-to-make
	| par-each {|dir| ls $dir | is-empty | if $in { $dir } else null }
	| compact
	| each {|dir| rm $dir }
}

# takes a list of files and returns a list of directories that will need
# to be created for them
def list-dirs-to-make []: list<string> -> list<string>  {
	path parse
	| get parent
	| uniq
	| sort
	| reduce --fold [""] {|it, acc|
		if ($it | str starts-with ($acc | last)) {
			let final_pos = ($acc | length) - 1
			$acc | upsert $final_pos $it
		} else {
			$acc | append $it
		}
	}
}

def get-project-files-to-link [proj, no_cache] {
	# cd in order to get all the gitignores correct
	cd $proj.from
	let cache = cache load $proj.name

	let files = list-files $proj.from --excludes $proj.excludes

	let $new_files = $files | match ($no_cache) {
		true => { $in },
		false => { where { $in not-in $cache } },
	}

	# TODO having a no_cache option messes with deleting old files
	let to_delete = $cache | where { $in not-in $files }
	let existing = (if $no_cache { [] } else { $cache | where { $in not-in $to_delete } })

	{
		name: $proj.name,
		root: $proj.from,
		files:
		($new_files | each {|file|
			{
				file: $file,
				from:  ($proj.from | path join $file),
				to: ($proj.to | path join $file)
			}
		})
		delete: ($to_delete | each {|f| $proj.to | path join $f | path relative-to $env.HOME })
		existing: $existing,
	}
}
