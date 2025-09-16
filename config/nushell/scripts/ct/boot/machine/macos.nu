use log.nu
use ct/macos

export def main [] {
	# if not ("/etc/pam.d/sudo_local" | path exists) {
	# 	print $"(ansi green)Setting up touchid(ansi reset)"
	#
	# 	# be very very careful here, you can corrupt the sudo_local file and then have a pain resetting it
	# 	(dedent "
	# 		# auth       optional       /opt/homebrew/Cellar/pam-reattach/1.3/lib/pam/pam_reattach.so
	# 		auth       sufficient     pam_tid.so
	# 		" | save --force ~/.tmp)
	#
	# 	zsh -c $"sudo mv ~/.tmp /etc/pam.d/sudo_local"
	# } else {
	# 	print $"(ansi cyan)[cached] Touchid(ansi reset)"
	# }

	print $"(ansi green)Setting up MacOS defaults(ansi reset)"
	macos_set_defaults
	print $"(ansi cyan)Done(ansi reset) some settings may require log in and out"

	setup_background_items

	# setup paths and envs for gui programs like wezterm
	# i'm not sure this really works
	# envy
	# macos env-store
}

# Setup macos launchd processes as plist files
#
# In short these are plist files that are managed by launchd and they are
# written into the appropriate folder then started. Keep them simple at let the
# content of the script do most of the heavy lifting, just use the plist to
# manage scheduling
#
# handy stuff from:
# - https://www.youtube.com/watch?v=guBV0jftT40&ab_channel=AUC_ANZ
# - https://www.launchd.info/
def setup_background_items [
	--force # if set, will rerun launch agent if already started
] {
	let bg_ps = (launchctl list | detect columns)
	print $"(ansi green)Background:(ansi reset) setting up launchagents"
	let files = ls ~/.config/nushell/scripts/ct/boot/_LaunchAgents

	$files
	| each {|f|
		let target = $f.name | split row "_" | get 1
		let domain = $f.name | path parse | get stem
		let target_file = [~/Library $target] | path join | path expand
		let log_dir = ([~/ .local/state $domain] | path join | path expand)

		# $HOME doesn't seem to expand
		let content = open $f.name
		| str replace --all "{{HOME}}" $env.HOME
		| str replace --all "{{LOGFILE}}" ([$log_dir std.log] | path join)
		| str replace --all "{{PATH}}" ($env.PATH | str join ":")

		if (not $force and ($bg_ps | where label starts-with $domain | is-not-empty)) {
			log skip $domain process running
			return
		}
		mkdir $log_dir
		# print dir $log_dir
		# may not have been setup so will do these in try
		print $"(ansi cyan)Creating(ansi reset) ($target_file)"
		# print $content

		try { launchctl unload $target_file }
		try { rm $target_file }
		echo $content | save $target_file --force
		launchctl load $target_file
	}
}

def macos_set_defaults [] {
	# more things in
	# - https://gist.github.com/scottstanfield/688909eb2cc2b3dfcea2d9e50027d212
	# - https://macos-defaults.com/dock/autohide-delay.html
	# - https://github.com/LnL7/nix-darwin/blob/698a62c628c2ec423aa770d8ec0e1d0bcf4fca1a/modules/system/defaults-write.nix#L34

	# prevent opening animations
	defaults write -g NSAutomaticWindowAnimationsEnabled -bool false

	# faster changes
	defaults write com.apple.universalaccess reduceMotion -bool true

	defaults write NSGlobalDomain AppleShowScrollBars -string "WhenScrolling"
	defaults write NSGlobalDomain InitialKeyRepeat -int 12
	defaults write NSGlobalDomain KeyRepeat -int 2
	defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
	defaults write NSGlobalDomain AppleShowAllExtensions -bool true
	defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

	defaults write com.apple.dock autohide -bool true
	defaults write com.apple.dock mru-spaces -bool false
	defaults write com.apple.dock static-only -bool true
	defaults write com.apple.dock orientation -string "left"
	defaults write com.apple.dock tilesize -int 50
	# seems to prevent it showing
	defaults write com.apple.dock expose-animation-duration -float 0

	# tidy
	defaults write com.apple.screencapture location -string ([$env.HOME Pictures] | path join)

	# finder
	defaults write com.apple.finder AppleShowAllFiles -bool true
	defaults write com.apple.finder FXDefaultSearchScope -string "SCcf" # this dir
	defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false
	defaults write com.apple.finder FXPreferredViewStyle -string "Flwv" # this dir
	defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
	defaults write com.apple.finder ShowHardDrivesOnDesktop -bool false
	defaults write com.apple.finder ShowRecentTags -bool false;
	defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true
	defaults write com.apple.finder ShowStatusBar -bool true
	defaults write com.apple.finder _FXShowPosixPathInTitle -bool false
	defaults write com.apple.finder _FXSortFoldersFirst -bool true


	# make mission control ok
	# https://nikitabobko.github.io/AeroSpace/guide#a-note-on-mission-control
	defaults write com.apple.dock expose-group-apps -bool true

	if (which aerospace | is-not-empty) {
		print $"(ansi green)MacOS(ansi reset) setup things for aerospace"

		# disable multiple spaces (don't full screen workspace)
		# https://nikitabobko.github.io/AeroSpace/guide#a-note-on-displays-have-separate-spaces
		defaults write com.apple.spaces spans-displays -bool true;
	}

	killall SystemUIServer
	killall Finder
	killall Dock
}
