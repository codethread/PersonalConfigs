#!/usr/bin/env bash

export DOTFILES="${HOME}/PersonalConfigs"

cd || exit 1

# clone dotfiles
if [ ! -d "${DOTFILES}" ]; then
	echo "( ◕ ◡ ◕ ) Cloning dotfiles"
	git clone git@github.com:codethread/PersonalConfigs.git "${DOTFILES}"
else
	echo "( ◕ ◡ ◕ ) Dotfiles present, skipping clone"
fi

echo "( ◕ ◡ ◕ ) Installing nushell"
"${DOTFILES}/.local/bin/install-nushell"

~/.local/bin/nu \
	--env-config "${DOTFILES}/Library/Application Support/nushell/env.nu" \
	--config "${DOTFILES}/Library/Application Support/nushell/config.nu" \
	--commands "boot machine"

echo "( ◕ ◡ ◕ ) complete, open new shell"
