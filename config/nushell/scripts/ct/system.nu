# macOS system utilities and general OS functions

export alias mac-dark-toggle = osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to not dark mode'
export alias l = ls -a
export alias finder = ^open -a 'Finder' .
export alias ports = lsof -i tcp:3000
export alias loggy = cd `~/Library/Mobile Documents/iCloud~com~logseq~logseq/`

export def alert [msg = "Task Finished"] {
	osascript -e $'display notification "($msg)" with title "CMD"'
	afplay /System/Library/Sounds/Glass.aiff
}

# Rebuild and switch NixOS configuration
export def nrs [profile: string = "vm"] {
	sudo nixos-rebuild switch --flake $"path:($env.HOME)/PersonalConfigs/nix#($profile)"
}

# I always forget
export def symlink [original: path, symbolic: path] {
	ln -s $original $symbolic
}

# export most common envs to a zsh file for compatibility
export def dump-env [] {
	hide-all {|| zsh -c 'export' | rg "(.*)" --replace "export $0" | save -f ~/.config/zsh/.envs }
}