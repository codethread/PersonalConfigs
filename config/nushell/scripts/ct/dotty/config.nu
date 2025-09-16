use ct/core clog

# load in dotty config, currently in code but could be a nuon file
export def load []: nothing -> table<name: string, real: path, symlink: path, excludes: list<path>> {
	let excludes = [
		"**/_?*/**" # files/folder starting with an underscore (but not just a single underscore)
		"**/.gitignore"
		"**/README.md"
	]

	let config = [
		[name, real, symlink, excludes];

		[home, (dir ~/PersonalConfigs/home), (dir ~), [ "**/.stylua.toml" "**/.gitattributes" ] ]
		[config, (dir ~/PersonalConfigs/config), (dir ~/.config), [ "**/.stylua.toml" "**/.gitattributes" ] ]
		[claude, (dir ~/PersonalConfigs/claude), (dir ~/.claude), [ "**/.stylua.toml" "**/.gitattributes" ] ]

		[work, (dir ~/workfiles), (dir ~), []]

		# as a bit of a hack, can reference git dirs directly
		[deals, (dir ~/workfiles/work/app/deals-light-ui/_git), (dir ~/work/app/deals-light-ui/.git), []]
	]

	$config
	| upsert excludes { |project| $project.excludes ++ $excludes }
	| where {|proj| $proj.real | path exists }
	| clog --expand
}

def dir [str: path] {
	$str | path expand
}
