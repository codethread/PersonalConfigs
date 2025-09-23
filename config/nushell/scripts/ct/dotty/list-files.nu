# :module: File listing utility with git ignore awareness and custom excludes

# glob all files at a given `path`, allowing for additional excludes. Results
# are returned as relative paths
export def main [path, --excludes = []]: nothing -> list<string> {
	glob **/* --no-dir --exclude ([**/target/** **/.git/**] ++ $excludes)
	| path relative-to $env.PWD
	| list-not-ignored
}

def list-not-ignored []: list<string> -> list<string> {
	let files = $in
	let ignored = $files | to text | git check-ignore --stdin | lines

	$files | where { $in not-in $ignored } | sort
}
