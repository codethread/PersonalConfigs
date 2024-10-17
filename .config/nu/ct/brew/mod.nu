const core = '
# bundle itself, might be built in
tap "homebrew/bundle"
# Mac App Store command-line interface (needed for mas installs)
brew "mas"

brew "deno"

tap "oven-sh/bun"
brew "oven-sh/bun/bun"

# autocompletion engine for nushell
tap "rsteube/homebrew-tap"
brew "rsteube/tap/carapace"

brew "fd"
brew "fzf"
brew "neovim"
brew "ripgrep"
brew "tmux"
brew "starship"
brew "stylua"
# Intuitive find & replace CLI
brew "sd"
brew "volta"
brew "wakatime-cli"

# PAM module for reattaching to the users GUI (Aqua) session
brew "pam-reattach"

# Send macOS User Notifications from the command-line
brew "terminal-notifier"

cask "1password"
cask "alfred"
cask "google-chrome"
cask "kitty"
# Menu bar manager
cask "jordanbaird-ice"

cask "font-fira-code-nerd-font"
cask "font-symbols-only-nerd-font"
cask "visual-studio-code"

mas "DaisyDisk", id: 411643860
mas "Key Codes", id: 414568915

mas "Vimari", id: 1480933944
';

const home = '
cask "discord"
cask "whatsapp"

# Fast, disk space efficient package manager
brew "pnpm"

# Run arbitrary commands when files change
brew "entr"
brew "luarocks"

mas "Paprika Recipe Manager 3", id: 1303222628
';

# Remove all brew related packages not listed on the current menu
export def sync [
	--log
	--dry-run # output brewfile contents but don't save or run anything
	--clean
] {
	let conf = echo ($core ++ $home) 

	if ($dry_run) {
		print $conf
	} else if ($clean) {
		echo $conf 
		| ^brew bundle --file=- --no-upgrade --no-lock --cleanup --zap --verbose
	} else {
		echo $conf 
		| ^brew bundle --file=- --no-upgrade --no-lock
	}
}

export def tap [...args] {
	err
}

export def install [...args] {
	err
}

def err [] {
	error make -u {msg: $"Do this through (ansi cyan)([$env.DOTFILES .config/nu/ct/brew/mod.nu] | path join)(ansi reset)"}
}
