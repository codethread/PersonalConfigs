use ct/core [clog]

# check if the currently running terminal program has full disk access
# TODO: could be extended to take an application if required
export def macos_has_full_disk_access [
	--check # don't fail the given pipeline if false
] {
	# https://apple.stackexchange.com/questions/362865/macos-list-apps-authorized-for-full-disk-access

	let missing_access = (sqlite3 `/Library/Application Support/com.apple.TCC/TCC.db` 'select * from access'
		| complete
		| clog "macos_has_full_disk_access"
		| get stderr
		# Error: unable to open database "/Library/Application Support/com.apple.TCC/TCC.db": authorization denied
		| str contains 'authorization')

	(match [$missing_access, $check] {
		[true, false] => { error make -u { msg: "Terminal requires full disk access" } }
		[_, true] => { not $missing_access }
	})
}

export def macos_toggle_reduce_motion [] {
	(match (defaults read com.apple.universalaccess reduceMotion) {
		"0" => { defaults write com.apple.universalaccess reduceMotion -bool true },
		"1" => { defaults write com.apple.universalaccess reduceMotion -bool false }
	})
	killall Dock
}
