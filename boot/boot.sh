#!/usr/bin/env bash

export DOTFILES="${HOME}/PersonalConfigs"

cd || exit 1

echo "( ◕ ◡ ◕ ) Setting up system"
echo "If this script fails at any point it can be rerun"
echo ""

# NixOS profile to build (vm | nixos | ...) — passed as first argument, defaults to vm
NIXOS_PROFILE="${1:-vm}"

# clone dotfiles — on NixOS git may not exist yet, use nix-shell to bootstrap
if [ ! -d "${DOTFILES}" ]; then
  echo "( ◕ ◡ ◕ ) Cloning dotfiles"
  _clone_url=""
  if [ -d "${HOME}/.ssh" ]; then
    _clone_url="git@github.com:codethread/PersonalConfigs.git"
  else
    # no SSH yet (e.g. fresh NixOS); clone via HTTPS, switch remote to SSH after key setup
    echo "  (no ~/.ssh found, cloning via HTTPS)"
    _clone_url="https://github.com/codethread/PersonalConfigs.git"
  fi

  if command -v git 2>&1 >/dev/null; then
    git clone --branch linux "$_clone_url" "${DOTFILES}"
  elif [ -f "/etc/NIXOS" ]; then
    echo "  (no git in PATH, using nix-shell)"
    nix-shell -p git --run "git clone --branch linux ${_clone_url} ${DOTFILES}"
  else
    echo "missing git, likely needs developer tools"
    exit 1
  fi
else
  echo "( ◕ ◡ ◕ ) Dotfiles present, skipping clone"
fi

# copy hardware configuration if the repo still has the empty stub
if [ -f "/etc/NIXOS" ]; then
  _hw_src="/etc/nixos/hardware-configuration.nix"
  _hw_dest="${DOTFILES}/nix/hosts/nixos/hardware-configuration.nix"
  if [ -f "${_hw_src}" ] && grep -q '{ \.\.\. }: { }' "${_hw_dest}" 2>/dev/null; then
    echo "( ◕ ◡ ◕ ) Copying hardware configuration from installer"
    cp "${_hw_src}" "${_hw_dest}"
  fi
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

# run nixos-rebuild if nushell is not yet on the system (i.e. first boot)
if [ -f "/etc/NIXOS" ] && ! command -v nu 2>&1 >/dev/null; then
  echo "( ◕ ◡ ◕ ) NixOS: running nixos-rebuild (profile: ${NIXOS_PROFILE})"
  sudo nixos-rebuild switch --flake "${DOTFILES}/nix#${NIXOS_PROFILE}"
fi

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
