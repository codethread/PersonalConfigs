brew tap rsteube/homebrew-tap
brew install rsteube/tap/carapace

mkdir ~/.cache/carapace
carapace _carapace nushell | save --force ~/.cache/carapace/init.nu
