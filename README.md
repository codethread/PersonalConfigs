[![Coding time tracker](https://wakatime.com/badge/github/AHDesigns/PersonalConfigs.svg)](https://wakatime.com/badge/github/AHDesigns/PersonalConfigs)

# Configs

Life and soul

## mac

https://www.maketecheasier.com/install-macos-virtualbox/
experimenting with virtual box
https://blog.petehouston.com/download-and-convert-macos-mojave-installer-into-iso-file/

### xcode tools
This is required by most command line tools, so we'll need this first
```sh
xcode-select --install
```

### brew install
Download [homebrew](https://brew.sh/)
*command as of Feb 2020*

```sh
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

This is a package manager for mac, and stores executables in `/usr/local`

#### packages

```sh
cd ~/PersonalConfigs && brew bundle
```

### zsh

*not needed on mac anymore* [installing zsh](https://gist.github.com/derhuerst/12a1558a4b408b3b2b6e)

#### anitbody (zsh plugin manager)

run setup command to copy files

```sh
# run antiup to set up plugins
~/PersonalConfigs/.bin/antiup

# use custom zsh scripts
mkdir ~/old_zsh
mv ~/.zlogin ~/old_zsh/
mv ~/.zsh* ~/old_zsh/

# link directories
~/PersonalConfigs/_setup
```

### vim

```
vim --noplugin
```

then run `:PlugInstall`

### git
go and change `.gitconfig` and other git files

### nodenv

note the version of the latest LTS from [node](https://nodejs.org/en/)

```sh
# install a LTS version
nodenv install <latest version>
```

```sh
# set this up globally
nodenv global <latest version>
```

```sh
# install global npm modules
~/PersonalConfigs/.bin/npmg
```

```sh
# rehash to pick up the executables now known to nodenv
nodenv rehash
```

### iterm

follow instructions in 

```sh
cd ~/PersonalConfigs/_colors+fonts
cat xterm-256color-italic.terminfo
```
