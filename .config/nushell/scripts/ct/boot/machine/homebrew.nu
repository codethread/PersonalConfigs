use log.nu

export def main [] {
	log step "Homebrew"

	if not ("/opt/homebrew" | path exists) {
		log sub-step "Installing" "homebrew not present on system"
		let cmd = r#'curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh'#
		log bash $cmd
		bash -c $cmd
	} else {
		log skip "Homebrew" "cached"
	}

	log sub-step "Brew" "sync"
	# custom nushell code to use a Brewfile
	brew sync
}
