#---------------------------------------------#
# EMACS
# -------------------------------------------#
# INVALID multiline
# export alias emacs-doom = rm ~/.emacs.d; ln -s ~/doom-emacs ~/.emacs.d
# INVALID multiline
# export alias emacs-adam = rm ~/.emacs.d; ln -s ~/emacs ~/.emacs.d
# INVALID multiline
# export alias emacs-min = rm ~/.emacs.d; ln -s ~/emacs-minimal ~/.emacs.d

export alias ec = emacsclient -a \"\" -c -t
export alias ed = emacs --daemon
export alias eg = /Applications/Emacs.app/Contents/MacOS/Emacs

#---------------------------------------------#
# KITTY
# -------------------------------------------#
export alias ktt = kitty +kitten themes
export alias ktt-dark = kitty +kitten themes --reload-in=all Tokyo Night Storm
export alias ktt-light = kitty +kitten themes --reload-in=all Tokyo Night Day

#---------------------------------------------#
# HELPERS
# -------------------------------------------#
export alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'

export alias ls='ls -pGF --color=auto'
# export alias l='ls -lah'
export alias l='exa -lah --icons'
export alias lt='exa -1ah --tree'

export alias pathis='echo $PATH | tr -s ":" "\n"'
export alias portsinuse = lsof -i -p | grep -i 'listen'
export alias ports = echo lsof -i tcp:3000
export alias finder='open -a 'Finder' .'
# INVALID multicomand
# export alias screen='export TERM=screen-256color && screen'
export alias treee='tree src -I "*~"'

#---------------------------------------------#
# RUNNERS
# -------------------------------------------#
# INVALID multiline
# export alias alert = osascript -e 'display notification \"Task Finished\" with title \"CMD\"'; afplay /System/Library/Sounds/Glass.aiff
export alias ser='python -m SimpleHTTPServer $1' # start server
export alias nn='netlify'

#---------------------------------------------#
# HOMEBREW
# -------------------------------------------#
export alias bi = brew install
# INVALID multiline
# export alias brewup='brew update; brew doctor; brew upgrade; brewclean'
# INVALID multiline
# export alias brewclean='brew cleanup; brew autoremove'
export alias brewdeps = brew deps --graph --installed

export alias bbd = brew bundle dump # update brewfile
export alias bbc = brew bundle check # ensure all installed
export alias bbx = brew bundle cleanup # removed unlisted
export alias bbi = brew bundle install

#---------------------------------------------#
# RARE USE
# -------------------------------------------#
export alias mkssh = openssl rsa -in ~/.ssh/id_rsa | openssl base64 | tr -d '\n' | pbcopy
export alias pipes = pipes.sh -f 60 -s 8

#---------------------------------------------#
# QMK
# -------------------------------------------#
export alias qmk-print = qmk c2json keymap.c -km codethread -kb preonic/rev3_drop --no-cpp

#---------------------------------------------#
# NODE
# -------------------------------------------#
# INVALID multiline
# export alias npc = rm -rf ./node_modules; npm cache clear; npm i
export alias npl = npm -g ls --depth=0
# INVALID multiline
# export alias yy = eval \$(cat package.json | jq -S '.scripts' | sed '1d;$d' | fzf -i --header='[run:]' | sed -E \"s/\\\"(.*)\\\":.*/yarn run \\1/\" )
# INVALID multicomand
# export alias rn-nuke = gnuke && yarn && cd ios && pod install && cd .. && yarn run-ios --reset-cache

#---------------------------------------------#
# DOCKER
# -------------------------------------------#
export alias dock_ri='docker rmi $(docker images -a --filter=dangling=true -q)' #docker_clean_images
export alias dock_rc='docker rm $(docker ps --filter=status=exited --filter=status=created -q)' #docker_clean_ps
export alias dock_prune='docker system prune -a'
export alias dcup='docker-compose up'
export alias dcdn='docker-compose down'
export alias dick = docker ps | grep '[a-z0-9]' | awk '{ print $1 }' | xargs docker kill
export alias dc = docker-compose config --services | fzf --multi | tr '\n' ' ' | xargs docker-compose up

export alias k8='kubectl'

#---------------------------------------------#
# GOLANG
# -------------------------------------------#
export alias gob=go1.18beta1

#---------------------------------------------#
# GIT
# -------------------------------------------#
export alias -g gbranch='git rev-parse --abbrev-ref HEAD'
export alias -g gtrunk = git symbolic-ref refs/remotes/origin/HEAD | sed 's@^refs/remotes/origin/@@'
export alias gclean = git clean -dfX
export alias gcp = git cherry-pick
export alias gst = git status -s
# INVALID multiline
# export alias gfuck='git fetch origin; git reset --hard origin/$(gbranch)'
# INVALID multiline
# export alias gignore = git rm -r --cached .; git add .; git commit -m '.gitignore is now working'
export alias gkill='git branch | grep -v "$( gtrunk )" | xargs git branch -D'
export alias gl='git log --oneline $( gtrunk )..HEAD'
export alias grr = gl | wc -l | rargs -p '\\s*(.*)\\s*' git rebase -i HEAD~{1}
# INVALID multicomand
# export alias gmm='git checkout $( gtrunk ) && git pull && git checkout - && git rebase $( gtrunk )'
# INVALID multicomand
# export alias gmm='git fetch origin $(gtrunk):$(gtrunk) && git rebase $( gtrunk )'
# INVALID multiline
# export alias gnah = git reset --hard; git clean -df
export alias gnuke = git clean -dfX
export alias gr='git rebase -i HEAD~$1'
# INVALID multicomand
# export alias gundo = git reset --soft HEAD~1 && git restore --staged .
# INVALID multicomand
# export alias gwip = git add . && git commit -nm 'wip'
export alias bdiff = git diff --name-only --relative --diff-filter=d | xargs bat --diff
export alias gpopular = git log --format=format: --name-only --since=12.month | egrep -v '^$' | sort | uniq -c | sort -nr | head -50
export alias gbranch-mine = git for-each-ref --format=' %(authorname) %09 %(refname)' --sort=authorname | grep 'adam.hall'
export alias gconflict = git diff --name-only --diff-filter=U --relative
export alias gcache-clear = git rev-parse --show-toplevel | rargs rm -rf {0}/.git/rr-cache # https://stackoverflow.com/questions/18500016/undo-a-git-rerere-resolution-that-was-done-in-a-rebase

# worktree
export alias gw = git worktree

#---------------------------------------------#
# OS
#---------------------------------------------#
export alias mac-dark-toggle = osascript -e 'tell app \"System Events\" to tell appearance preferences to set dark mode to not dark mode'

#---------------------------------------------#
# TMUX
# -------------------------------------------#
export alias tmux-delete-resurrect = rm -f ~/.local/share/tmux/resurrect

##############################################
# work
##############################################
# INVALID multicomand
# export alias nvm = volta pin node@14 && volta pin yarn@1.22.4
# INVALID multicomand
# export alias react-native-clean = git pull && gclean && yarn && yarn install:app && yarn run-ios --simulator \"iPhone SE (3rd generation)\"
