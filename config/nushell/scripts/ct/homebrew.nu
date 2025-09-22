# Homebrew package management utilities

# update brewfile
export alias bbd = brew bundle dump
# ensure all installed
export alias bbc = brew bundle check
# removed unlisted
export alias bbx = brew bundle cleanup
export alias bbi = brew bundle install

export def brewclean [] {
	brew cleanup
	brew autoremove
}

export alias brewdeps = brew deps --graph --installed