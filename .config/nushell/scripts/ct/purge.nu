# Search through projects, find the largest and present them ina fuzzy finder
# to be purged. At this point a git clean is run
#
# TODO:
# - would be charming to get the output
# - add some flags for actually deleting
# - show size in fuzzy if possible
export def main [] {
	let purged = dirs
	| path expand
	| where { path exists }
	| each {|p| ls $p
		| where type == "dir"
		| get name
	}
	| flatten
	| par-each {|d| $d | get-dir-size }
	| flatten
	| sort
	| last 30
	| get name
	| to text
	| fzf --multi

	print "To purge"
	print $purged
	let choice = (input $"(ansi red)PURGE?(ansi reset) [Y/n]: ")

	if (($choice | str trim) != "n") {
		$purged
		| lines
		| each {|dir| print $"cleaning $($dir)"; cd $dir | git clean -dfX }
	}

}

def get-dir-size [] {
	^du -sh $in
	| parse "{size}\t{name}"
	| str trim
	| upsert size {|r| $r.size | into filesize }
}

def dirs [] {
	[
		~/dev/vendor
		~/dev/learn
		~/dev/projects
		~/work/services
		~/work/lambdas
		~/work/utilities
		~/work/infrastructure
		~/work/libraries
		~/work/docs
	]
}
