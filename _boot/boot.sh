#!/usr/bin/env bash

export DOTFILES="${HOME}/PersonalConfigs"

cd || exit 1

echo "( ◕ ◡ ◕ ) Setting up system"
echo "If this script fails at any point it can be rerun"
echo ""

# check for git
if ! command -v git 2>&1 >/dev/null; then
  echo "missing git, likely needs developer tools"
  exit 1
fi

# check for ssh before trying to clone
if [ ! -d "${HOME}/.ssh" ]; then
  echo "ssh not setup, you probably need https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent"
  open https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent
  exit 1
fi

# clone dotfiles
if [ ! -d "${DOTFILES}" ]; then
  echo "( ◕ ◡ ◕ ) Cloning dotfiles"
  git clone git@github.com:codethread/PersonalConfigs.git "${DOTFILES}"
else
  echo "( ◕ ◡ ◕ ) Dotfiles present, skipping clone"
fi

export XDG_CONFIG_HOME="${DOTFILES}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_CACHE_HOME="${HOME}/.local/cache"
export PATH="${HOME}/.local/bin:/opt/homebrew/bin:${PATH}"

mkdir -p "$XDG_DATA_HOME"
mkdir -p "$XDG_CONFIG_HOME"
mkdir -p "$XDG_STATE_HOME"
mkdir -p "$XDG_CACHE_HOME"

echo "( ◕ ◡ ◕ ) Installing nushell"
"${DOTFILES}/.local/bin/install-nushell"

echo "( ◕ ◡ ◕ ) Booting machine"
echo "available again with 'boot machine --help'"

~/.local/bin/nu \
  --env-config "${DOTFILES}/.config/nushell/env.nu" \
  --config "${DOTFILES}/.config/nushell/config.nu" \
  --commands "boot machine"

echo "( ◕ ◡ ◕ ) complete, open new shell"
