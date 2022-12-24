#---------------------------------------------#
# EDITOR-ish
# -------------------------------------------#
alias v="nvim"
alias zz="nvim ~/.zshrc"
alias zp="nvim ~/.zprofile"
alias vv="cd ~/PersonalConfigs/.config/nvim; nvim ./init.lua" # open vim config
alias aa="nvim ~/.aliases.zsh" # open aliases
# see https://github.com/wbthomason/packer.nvim/issues/180 for MACOSX_DEPLOYMENT_TARGET=10.15
alias nvim-boot="MACOSX_DEPLOYMENT_TARGET=10.15 nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'"

alias zr="source ~/.zshrc"
alias vo="nvim \$(fzf)"
alias als="eval \$(cat ~/.aliases.zsh | grep \"^alias.\+=\\\"\" | fzf -i --header='[run:]' | sed -E \"s:alias.*\\\"(.*)\\\":\\1:\" )"

#---------------------------------------------#
# EMACS
# -------------------------------------------#
alias emacs-doom="rm ~/.emacs.d; ln -s ~/doom-emacs ~/.emacs.d"
alias emacs-adam="rm ~/.emacs.d; ln -s ~/emacs ~/.emacs.d"
alias emacs-min="rm ~/.emacs.d; ln -s ~/emacs-minimal ~/.emacs.d"

alias ec="emacsclient -a \"\" -c -t"
alias ed="emacs --daemon"
alias eg="/Applications/Emacs.app/Contents/MacOS/Emacs"

#---------------------------------------------#
# KITTY
# -------------------------------------------#
alias ktt="kitty +kitten themes"
alias ktt-dark="kitty +kitten themes --reload-in=all Tokyo Night Storm"
alias ktt-light="kitty +kitten themes --reload-in=all Tokyo Night Day"

#---------------------------------------------#
# HELPERS
# -------------------------------------------#
alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'

alias ls='ls -pGF --color=auto'
# alias l='ls -lah'
alias l='exa -lah'

alias pathis='echo $PATH | tr -s ":" "\n"'
alias portsinuse="lsof -i -p | grep -i 'listen'"
alias ports="echo lsof -i tcp:3000"
alias finder='open -a 'Finder' .'
alias screen='export TERM=screen-256color && screen'
alias treee='tree src -I "*~"'

#---------------------------------------------#
# RUNNERS
# -------------------------------------------#
alias alert="osascript -e 'display notification \"Task Finished\" with title \"CMD\"'; afplay /System/Library/Sounds/Glass.aiff"
alias ser='python -m SimpleHTTPServer $1' # start server
alias nn='netlify'

#---------------------------------------------#
# HOMEBREW
# -------------------------------------------#
alias bi="brew install"
alias brewup='brew update; brew doctor; brew upgrade; brewclean'
alias brewclean='brew cleanup; brew autoremove'
alias brewdeps="brew deps --graph --installed"

alias bbd="brew bundle dump" # update brewfile
alias bbc="brew bundle check" # ensure all installed
alias bbx="brew bundle cleanup" # removed unlisted
alias bbi="brew bundle install"

#---------------------------------------------#
# RARE USE
# -------------------------------------------#
alias mkssh="openssl rsa -in ~/.ssh/id_rsa | openssl base64 | tr -d '\n' | pbcopy"
alias pipes="pipes.sh -f 60 -s 8"

#---------------------------------------------#
# QMK
# -------------------------------------------#
alias qmk-print="qmk c2json keymap.c -km codethread -kb preonic/rev3_drop --no-cpp"

#---------------------------------------------#
# NODE
# -------------------------------------------#
alias npc="rm -rf ./node_modules; npm cache clear; npm i"
alias npl="npm -g ls --depth=0"
alias yy="eval \$(cat package.json | jq -S '.scripts' | sed '1d;$d' | fzf -i --header='[run:]' | sed -E \"s/\\\"(.*)\\\":.*/yarn run \\1/\" )"
alias rn-nuke="rm -rf node_modules && yarn && cd ios && pod install && cd .. && yarn start --reset-cache"

#---------------------------------------------#
# DOCKER
# -------------------------------------------#
alias dock_ri='docker rmi $(docker images -a --filter=dangling=true -q)' #docker_clean_images
alias dock_rc='docker rm $(docker ps --filter=status=exited --filter=status=created -q)' #docker_clean_ps
alias dock_prune='docker system prune -a'
alias dcup='docker-compose up'
alias dcdn='docker-compose down'
alias dick="docker ps | grep '[a-z0-9]' | awk '{ print $1 }' | xargs docker kill"
alias dc="docker-compose config --services | fzf --multi | tr '\n' ' ' | xargs docker-compose up"

#---------------------------------------------#
# GOLANG
# -------------------------------------------#
alias gob=go1.18beta1

#---------------------------------------------#
# GIT
# -------------------------------------------#
alias gBranch='git rev-parse --abbrev-ref HEAD'
alias gclean="git clean -dfX"
alias gcp="git cherry-pick"
alias gst="git status -s"
alias gfuck='git fetch origin; git reset --hard origin ${gBranch}'
alias gignore="git rm -r --cached .; git add .; git commit -m '.gitignore is now working'"
alias gkill="git branch | grep -v \"master\" | xargs git branch -D"
alias gl="git log --oneline main..HEAD"
alias gld="git log --oneline develop..HEAD"
alias grr="git rebase -i HEAD~\"$(gld | wc -l | xargs)\""
alias gmm="git checkout develop && git pull && git checkout - && git rebase develop"
alias gnah="git reset --hard; git clean -df"
alias gnuke="git clean -dfX"
alias gr='git rebase -i HEAD~$1'
alias gundo="git reset --soft HEAD~1 && git restore --staged ."
alias gwip="git add . && git commit -nm 'wip'"
alias ghub="gh repo view --web"
alias bdiff="git diff --name-only --relative --diff-filter=d | xargs bat --diff"

#---------------------------------------------#
# OS
#---------------------------------------------#
alias mac-dark-toggle="osascript -e 'tell app \"System Events\" to tell appearance preferences to set dark mode to not dark mode'"

#---------------------------------------------#
# TMUXINATOR
# -------------------------------------------#
alias mux="tmuxinator"
alias muxa="mux spages; mux exc;"
alias kmux="kill_tmux_session"

##############################################
# work
##############################################
alias nvm="volta pin node@14 && volta pin yarn@1.22.4"
