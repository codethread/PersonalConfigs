use log.nu

export def main [] {
	log step "Rustup"

	if ($env |get --optional  RUSTUP_HOME|is-empty) {
		log warn RUSTUP_HOME not defined, skipping rustup installation
		return
	}

	if not ($env.RUSTUP_HOME | path exists) {
		log sub-step "Installing" "rustup not present on system"
		# let cmd = r#'curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | bash -s -- --help'#
		let cmd = r#'curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | bash -s -- --no-modify-path -y'#
		log bash $cmd
		bash -c $cmd
	} else {
		log skip "Rustup"
	}

}
