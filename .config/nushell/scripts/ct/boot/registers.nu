# register all various plugins, should only be needed once per update
export def main [] {
	# remove old stuff
	rm `~/Library/Application Support/nushell/plugin.nu`

	handle nu_plugin_gstat
	handle nu_plugin_query
	# don't realy need this
	# handle nu_plugin_highlight

	# version also lists plugins
	nu -l -c "version"
}

def handle [$name] {
	cargo install $name
	nu -l -c $"plugin add ($name)"
}
