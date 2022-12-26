Life and soul

# xcode tools

This is required by most command line tools, so we'll need this first

```sh
xcode-select --install
```

# Github

## generate key

[instructions here](https://help.github.com/en/enterprise/2.19/user/github/authenticating-to-github/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent)
**commands as of Jul 2021**

```sh
ssh-keygen -t ed25519 "adamhalldesigns@gmail.com"

eval "$(ssh-agent -s)"
touch ~/.ssh/config
```

update `~/.ssh/config`

```sh
Host *
AddKeysToAgent yes
UseKeychain yes
IdentityFile ~/.ssh/id_ed25519

ssh-add -K ~/.ssh/id_ed25519
```

## upload key

[instructions here](https://help.github.com/en/enterprise/2.19/user/github/authenticating-to-github/adding-a-new-ssh-key-to-your-github-account)

```sh
pbcopy < ~/.ssh/id_rsa.pub
```

upload to github

# brew

Download [homebrew](https://brew.sh/)
**command as of Dec 2022**

```sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

This is a package manager for mac, and stores executables in `brew --prefix`

```sh
# check where brew is installed to and replace `/opt/homebrew` accordingly
# add various envs without clobbering PATH
mkdir -p ~/config/cold-brew
/opt/homebrew/bin/brew shellenv | sed 's/export PATH.*//' > ~/.config/cold-brew/shellenv

# update PATH
~/PersonalConfigs/.local/bin/envy

# this takes a long time, so grab a ☕️
brew bundle

# link directories
dotty setup

# update PATH
envy

# Reboot
# if Alfred already ran `clear Application cache` in advanced
```

# Anitbody (zsh plugin manager)

_Important to have run `brew` above (or whatever package manager) to
ensure `dotty` is installed_

```sh
# run antiup to set up plugins
antiup
```

# Logseq notes

Once installed and setup, symlink the settings from dropbox
Unless on work machine and not using plugins

```sh
rm -rf ~/.logseq
ln -s ~/Dropbox/logseq-settings .logseq
```
