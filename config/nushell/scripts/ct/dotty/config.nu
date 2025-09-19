use ct/core clog

# load in dotty config, currently in code but could be a nuon file
export def load []: nothing -> table<name: string, real: path, symlink: path, excludes: list<path>, link_directory: bool> {
	let excludes = [
		"**/_?*/**" # files/folder starting with an underscore (but not just a single underscore)
		"**/.gitignore"
		"**/README.md"
	]

	let config = [
		[name, real, symlink, excludes, link_directory];

		[home, (dir ~/PersonalConfigs/home), (dir ~), [ "**/.stylua.toml" "**/.gitattributes" ], false]
		[config, (dir ~/PersonalConfigs/config), (dir ~/.config), [ "**/.stylua.toml" "**/.gitattributes" ], false]
		[claude, (dir ~/PersonalConfigs/claude/agents), (dir ~/.claude/agents), [], true]

		[work, (dir ~/workfiles), (dir ~), [], false]

		# as a bit of a hack, can reference git dirs directly
		[deals, (dir ~/workfiles/work/app/deals-light-ui/_git), (dir ~/work/app/deals-light-ui/.git), [], false]
	]

	$config
	| upsert excludes { |project| $project.excludes ++ $excludes }
	| where {|proj| $proj.real | path exists }
	| clog --expand
}

def dir [str: path] {
	$str | path expand
}
