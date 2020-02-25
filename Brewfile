tap "homebrew/bundle"
tap "homebrew/cask"
tap "homebrew/core"

brew "cask"
brew "gcc"

tap "getantibody/tap"
brew "getantibody/tap/antibody"

brew "colordiff"
brew "exercism"
brew "fd"
brew "htop"
brew "jq"
brew "kafka"
brew "reattach-to-user-namespace"
brew "ripgrep"
brew "sbt"
brew "scala"
brew "shellcheck"
brew "sqlite"
brew "tmux"
brew "tree"
brew "vim"
brew "watch"
brew "watchman"

tap "nodenv/nodenv"
tap "ouchxp/nodenv"
brew "node-build"
brew "nodenv"

tap "elastic/tap"
brew "elastic/tap/filebeat-full"

tap "rockymadden/rockymadden"
brew "rockymadden/rockymadden/slack-cli"

# ctag replacement for vim
tap "universal-ctags/universal-ctags"
brew "universal-ctags/universal-ctags/universal-ctags", args: ["HEAD"]

# gnu standards
brew "coreutils"
brew "findutils"
brew "gawk"
brew "git"
brew "git-flow"
brew "gnu-getopt"
brew "gnu-indent"
brew "gnu-sed"
brew "gnu-tar"
brew "gnutls"
brew "grep"

if OS.mac?
  # mac only
  tap "d12frosted/emacs-plus"
  brew "d12frosted/emacs-plus/emacs-plus", args: ["HEAD", "with-jansson", "without-spacemacs-icon"], link: false

  brew "aspell"
  brew "docker-machine", link: false
  cask "alacritty"
  cask "amethyst"

  tap "homebrew/cask-fonts"
  cask "font-firacode-nerd-font"
  cask "font-hack-nerd-font"

  cask "iterm2"
end
