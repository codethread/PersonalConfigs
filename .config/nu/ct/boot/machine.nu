use ct/dotty
use ct/brew
use ct/core [dedent is-not-empty]
use ct/git [git_is_dirty]

export def main [
	--skip-brew # skip installation and syncing of brew packages
	--clean # install fresh dirs
	--force # clean even with uncommitted changes
] {
	print $"(ansi cyan)Linking homefiles(ansi reset)"
	dotty link;

	# setup some folder structures how I like them
	print $"(ansi green)Creating dirs(ansi reset)"
	mkdir -v ~/dev/vendor/ ~/dev/learn/ ~/dev/projects/

	clone_tools --clean=$clean --force=$force

	print $"(ansi green)Setting up touchid(ansi reset)"
	if not ("/etc/pam.d/sudo_local" | path exists) {
		(dedent "
			auth       optional       /opt/homebrew/Cellar/pam-reattach/1.3/lib/pam/pam_reattach.so
			auth       sufficient     pam_tid.so
			" | save --force ~/.tmp)

		zsh -c $"sudo mv ~/.tmp /etc/pam.d/sudo_local"
	}

	if not $skip_brew { # brew installer
		if not ("/opt/homebrew" | path exists) {
			zsh -c 'bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"'
		} else {
			print $"(ansi cyan)Homebrew aldready installed(ansi reset)"
		}

		# custom nushell code to use a Brewfile
		brew sync
	}

	do_macos_things

	print 'Files linked'
}

# Install various projects I use either for boot or general use (not available
# through brew)
def clone_tools [
	--clean # install fresh dirs
	--force # clean even with uncommitted changes
] {
	print $"(ansi green)Cloning tools(ansi reset)"

	let tools = [
		[git,                                   dir,          install];
		[git@github.com:nushell/nu_scripts.git, ~/dev/vendor, {||}]
	]

	$tools | par-each { |t|
		cd $t.dir
		let project = ($t.git | parse --regex '.*?:(?<owner>.+?)/(?<repo>.+?).git' | first)
		let is_cloned = ($project.repo | path exists)

		print $"(ansi green)Cloning(ansi reset) ($t.git) into ($t.dir)"

		if $clean and $is_cloned { 
			print $"(ansi cyan)Cleaning(ansi reset) ($project.repo)"
			cd $project.repo

			if (git_is_dirty) and not $force {
				print $"(ansi yellow)WARN(ansi reset) ($project.repo) has unstaged changes"
				print "Uninstall manually first or run with --force"
				print ""
				return
			}

			cd -;
			rm -rf $project.repo 
		}

		if not $is_cloned {
			git clone -q $t.git 
		}

		print $"(ansi green)Running(ansi reset) install script for ($project.repo)"
		do $t.install 
	}
}

def do_macos_things [] {
	# more things in https://gist.github.com/scottstanfield/688909eb2cc2b3dfcea2d9e50027d212

	# prevent opening animations
	defaults write -g NSAutomaticWindowAnimationsEnabled -bool false

	defaults write NSGlobalDomain AppleShowScrollBars -string "WhenScrolling"
	defaults write NSGlobalDomain InitialKeyRepeat -int 12
	defaults write NSGlobalDomain KeyRepeat -int 2
	defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
	defaults write NSGlobalDomain AppleShowAllExtensions -bool true

	defaults write com.apple.dock autohide -bool true
	defaults write com.apple.dock mru-spaces -bool false
	defaults write com.apple.dock static-only -bool true
	defaults write com.apple.dock orientation -string "left"
	defaults write com.apple.dock tilesize -int 50
	defaults write com.apple.dock expose-animation-duration -float 0.1

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
	defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
	defaults write com.apple.finder _FXSortFoldersFirst -bool true


	if (which aerospace | is-not-empty) {
		print $"(ansi green)MacOS(ansi reset) setup things for aerospace"

		# make mission control ok
		# https://nikitabobko.github.io/AeroSpace/guide#a-note-on-mission-control
		defaults write com.apple.dock expose-group-apps -bool true


		# disable multiple spaces (don't full screen workspace)
		# https://nikitabobko.github.io/AeroSpace/guide#a-note-on-displays-have-separate-spaces
		defaults write com.apple.spaces spans-displays -bool true;
	}

	killall SystemUIServer
	killall Finder
	killall Dock

	print $"(ansi cyan)Done(ansi reset) some settings may require log in and out"
}
