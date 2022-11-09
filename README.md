Life and soul

# xcode tools

This is required by most command line tools, so we'll need this first

    xcode-select --install

# Github

## generate key

[instructions here](https://help.github.com/en/enterprise/2.19/user/github/authenticating-to-github/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent)
**commands as of Jul 2021**

    ssh-keygen -t ed25519 "adamhalldesigns@gmail.com"

    eval "$(ssh-agent -s)"
    touch ~/.ssh/config

update `~/.ssh/config`

    Host *
    AddKeysToAgent yes
    UseKeychain yes
    IdentityFile ~/.ssh/id_ed25519

    ssh-add -K ~/.ssh/id_ed25519

## upload key

[instructions here](https://help.github.com/en/enterprise/2.19/user/github/authenticating-to-github/adding-a-new-ssh-key-to-your-github-account)

    pbcopy < ~/.ssh/id_rsa.pub

upload to github

# brew

Download [homebrew](https://brew.sh/)
**command as of Feb 2020**

    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

This is a package manager for mac, and stores executables in `brew --prefix`

    # this takes a long time, so grab a ☕️
    cd ~/PersonalConfigs && brew bundle

    # link directories
    dotty setup

    # ensure MacOS apps can see path
    path-gui
    # Reboot
    # if Alfred already ran `clear Application cache` in advanced

# Anitbody (zsh plugin manager)

_Important to have run `brew` above (or whatever package manager) to
ensure `dotty` is installed_

    # run antiup to set up plugins
    antiup

# Terminal Fonts

follow instructions in

    cd ~/PersonalConfigs/_colors+fonts
    cat xterm-256color-italic.terminfo

# Logseq notes

Once installed and setup, symlink the settings from dropbox

```sh
rm -rf ~/.logseq
ln -s ~/Dropbox/logseq-settings .logseq
```
