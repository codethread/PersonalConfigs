use log.nu

export def main [] {
	log step "Homebrew"

	let is_macos = ((sys host).name == "Darwin")
	let brew_path = if $is_macos { "/opt/homebrew" } else { "/home/linuxbrew/.linuxbrew" }

	if not ($brew_path | path exists) {
		log sub-step "Installing" "homebrew not present on system"
		let cmd = r#'curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh'#
		log bash $cmd
		bash -c $cmd
	} else {
		log skip "Homebrew" "cached"
	}

	# On Linux, ensure linuxbrew is in PATH for the rest of this session
	if not $is_macos {
		$env.PATH = ($env.PATH | prepend $"($brew_path)/bin")
	}

	log sub-step "Brew" "sync"
	# custom nushell code to use a Brewfile
	brew sync
}
