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
  SSH_DOCS="https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent"
  echo "ssh not setup, you probably need $SSH_DOCS"
  if [ "$(uname)" = "Darwin" ]; then
    open "$SSH_DOCS"
  elif command -v xdg-open &>/dev/null; then
    xdg-open "$SSH_DOCS"
  fi
  exit 1
fi

# clone dotfiles
if [ ! -d "${DOTFILES}" ]; then
  echo "( ◕ ◡ ◕ ) Cloning dotfiles"
  git clone git@github.com:codethread/PersonalConfigs.git "${DOTFILES}"
else
  echo "( ◕ ◡ ◕ ) Dotfiles present, skipping clone"
fi

export XDG_CONFIG_HOME="${DOTFILES}/config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_CACHE_HOME="${HOME}/.local/cache"

# Platform-specific PATH additions
if [ "$(uname)" = "Darwin" ]; then
  export PATH="${HOME}/.local/bin:/opt/homebrew/bin:${PATH}"
elif [ -f "/etc/NIXOS" ]; then
  # NixOS: nushell and all tools installed by Nix, skip install-nushell
  export PATH="${HOME}/.local/bin:${PATH}"
else
  export PATH="${HOME}/.local/bin:/home/linuxbrew/.linuxbrew/bin:${PATH}"
fi

mkdir -p "$XDG_DATA_HOME"
mkdir -p "$XDG_CONFIG_HOME"
mkdir -p "$XDG_STATE_HOME"
mkdir -p "$XDG_CACHE_HOME"

if [ -f "/etc/NIXOS" ]; then
  echo "( ◕ ◡ ◕ ) NixOS detected — nushell managed by Nix, skipping install"
else
  echo "( ◕ ◡ ◕ ) Installing nushell"
  "${DOTFILES}/home/.local/bin/install-nushell"
  "${DOTFILES}/home/.local/bin/install-nushell" --upgrade
fi

echo "( ◕ ◡ ◕ ) Booting machine"
echo "available again with 'boot machine --help'"

# On NixOS, nushell is in PATH (system-managed); otherwise use local install
if [ -f "/etc/NIXOS" ]; then
  nu_bin="nu"
else
  nu_bin="${HOME}/.local/bin/nu"
fi

$nu_bin \
  --env-config "${DOTFILES}/config/nushell/env.nu" \
  --config "${DOTFILES}/config/nushell/config.nu" \
  --commands "boot machine"

echo "( ◕ ◡ ◕ ) complete, open new shell"
