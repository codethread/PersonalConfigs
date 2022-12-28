#!/usr/bin/env bash

export DOTFILES="${HOME}/PersonalConfigs";

cd || exit 1

# clone dotfiles
if [ ! -d "${DOTFILES}" ]; then
    git clone git@github.com:codethread/PersonalConfigs.git "${DOTFILES}"
else
    echo "( ◕ ◡ ◕ ) dotfiles present, skipping clone"
fi

# temporary location set for nested zsh shell
export ZDOTDIR="${DOTFILES}/.config/zsh"

# start shell with path and envs from config
zsh "${DOTFILES}/_boot/boot.zsh"

echo "( ◕ ◡ ◕ ) complete, open new shell"
