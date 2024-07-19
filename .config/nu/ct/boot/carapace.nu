# install carapace from brew for autocompletions
# https://carapace.sh/
export def main [] {
  match (which carapace | is-empty) {
    false => { update },
    true => { install },
  }
}

def install [] {
  brew tap rsteube/homebrew-tap
  brew install rsteube/tap/carapace

  mkdir ~/.cache/carapace

  gen
}

def update [] {
  # brew upgrade carapace

  gen
}

def gen [] {
  carapace _carapace nushell 
  | save --force ~/.cache/carapace/init.nu
}
