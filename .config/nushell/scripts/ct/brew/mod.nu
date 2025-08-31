# This brew module wraps macos `brew`, this then prevents accidental installs (which will be stopped)
# inspired by nix - all installs are managed through `brew sync` via a brewfile

use ct/core [dedent]

# Remove all brew related packages not listed on the current menu
# see https://github.com/Homebrew/homebrew-bundle?tab=readme-ov-file#usage for docs
export def sync [
	--log
	--dry-run # output brewfile contents but don't save or run anything
	--clean
	--with-temp
	--mas
] {
	print $"(ansi green)Running Brew Sync(ansi reset)"
	let conf = (get_bundle_for_machine --with-temp=$with_temp)

	if ($dry_run) {
		$conf
	} else if ($clean) {
		echo $conf
		| ^brew bundle --cleanup --zap --verbose --file=-
	} else {
		with-env {
			HOMEBREW_NO_AUTO_UPDATE: 1
			HOMEBREW_BUNDLE_NO_UPGRADE: 1
			HOMEBREW_BUNDLE_MAS_SKIP: (not $mas)
		} {
			echo $conf | ^brew bundle --no-upgrade --file=-
		}
	}
}

export def tap [...args] {
	err
}

export def install [...args] {
	err
}

export def diff [] {
	let conf = get_bundle_for_machine | lines | strip-comments | sort
	let actual = with-env {
		HOMEBREW_NO_AUTO_UPDATE: 1
		HOMEBREW_BUNDLE_NO_UPGRADE: 1
		HOMEBREW_BUNDLE_MAS_SKIP: 1
	} {
		^brew bundle dump --vscode --file=- | lines | strip-comments | sort
	}

	$actual | where $it not-in $conf
}

def get_bundle_for_machine [
	--with-temp
] {
	let brews = (open ([$env.DOTFILES ".local/data/brews.nuon"] | path join))

	dedent ($brews.core
		++ (match $env.CT_USER {
			work => $brews.work,
			home => $brews.home
			_ => {
				print $"(ansi yellow)WARN: ($env.CT_USER)(ansi reset) didn't match known profile, only installing core packages"
				return ""
			},
		})
		++ (if $with_temp { $brews.temp } else "")
	);
}

######################
# HELPERS
######################

def strip-comments []: list<string> -> list<string> {
	$in
	| where (
		$it starts-with "brew"
		or $it starts-with "tap"
		or $it starts-with "cask"
		or $it starts-with "mas"
		or $it starts-with "vscode"
	)
}


def err [] {
	error make -u {msg: $"Do this through (ansi cyan)([$env.DOTFILES .config/nu/ct/brew/mod.nu] | path join)(ansi reset)"}
}
