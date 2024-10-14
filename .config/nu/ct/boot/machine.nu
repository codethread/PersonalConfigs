use ct/dotty
use ct/brew
use ct/core [dedent]
use ct/git [git_is_dirty]

export def main [
	--skip-brew # skip installation and syncing of brew packages
	--clean # install fresh dirs
	--force # clean even with uncommitted changes
] {
	print $"(ansi cyan)Linking homefiles(ansi reset)"
	dotty link;

	{ # setup some folder structures how I like them
		print $"(ansi green)Creating dirs(ansi reset)"

		mkdir -v ~/dev/vendor/ ~/dev/learn/ ~/dev/projects/
	}

	clone_tools --clean=$clean --force=$force

	if not $skip_brew { # brew installer
		if not ("/opt/homebrew" | path exists) {
			zsh -c 'bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"'
		} else {
			print $"(ansi cyan)Homebrew aldready installed(ansi reset)"
		}

		# custom nushell code to use a Brewfile
		brew sync
	}


	{ # setup touch id on mac
		if not ("/etc/pam.d/sudo_local") {
			(dedent "
				auth       optional       /opt/homebrew/Cellar/pam-reattach/1.3/lib/pam/pam_reattach.so
				auth       sufficient     pam_tid.so
				" | save --force ~/.tmp)

			zsh -c $"sudo mv ~/.tmp /etc/pam.d/sudo_local"
		}
	}

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
