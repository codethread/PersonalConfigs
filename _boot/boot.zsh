#!/usr/bin/env zsh

# non-interactive script so we need to grab anything from here
source "${ZDOTDIR}/.zlogin"

# install homebrew
if command -v brew > /dev/null 2>&1; then
    echo "( ◕ ◡ ◕ ) brew installed, skipping install"
else
    echo "( ◕ ◡ ◕ ) installing brew"
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

echo "( ◕ ◡ ◕ ) running brew bundle, go grab a drink"
brew bundle --verbose

echo "( ◕ ◡ ◕ ) running dotty"
dotty setup

echo "( ◕ ◡ ◕ ) setting up path for everyone"
envy

# load correct envs to get the ZDOTDIR
source "${DOTFILES}/.zshenv"

echo "( ◕ ◡ ◕ ) setting brewshell env"
brew shellenv | sed 's/export PATH.*//' > "${ZDOTDIR}/shellenv.zsh"

echo "( ◕ ◡ ◕ ) running antiup to install zsh plugins"
antiup
