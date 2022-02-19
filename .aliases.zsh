#---------------------------------------------#
# EDITOR-ish
# -------------------------------------------#
alias vim="nvim"
alias vi="vim --noplugin" # vi is vim on mac anyway so this is a 'light' version
alias zz="vi ~/.zshrc" # open zsh
alias vv="vi ~/.vimrc" # open vim config
alias aa="vi ~/.aliases.zsh" # open aliases
alias codij="vim -c \"Codi javascript | let ale_enabled = 0\" dmp.javascript"

alias zr="source ~/.zshrc"
alias vo="vim \$(fzf)"
alias als="eval \$(cat ~/.aliases.zsh | grep \"^alias.\+=\\\"\" | fzf -i --header='[run:]' | sed -E \"s:alias.*\\\"(.*)\\\":\\1:\" )"

#---------------------------------------------#
# EMACS
# -------------------------------------------#
alias emacs-doom="rm ~/.emacs.d && ln -s ~/doom-emacs ~/.emacs.d"
alias emacs-adam="rm ~/.emacs.d && ln -s ~/emacs ~/.emacs.d"

alias ec="emacsclient -a \"\" -c -t"
alias ed="emacs --daemon"
alias eg="/Applications/Emacs.app/Contents/MacOS/Emacs"

#---------------------------------------------#
# HELPERS
# -------------------------------------------#
alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'

alias ls='ls -pGF'
# alias l='ls -lah'
alias l='exa -lah'

alias pathis='echo $PATH | tr -s ":" "\n"'

alias portsinuse="lsof -i -p | grep -i 'listen'"

alias ports="echo lsof -i tcp:3000"

alias finder='open -a 'Finder' .'

alias screen='export TERM=screen-256color && screen'

alias treee='tree src -I "*~"'

alias mcfuckoff='sudo /usr/local/McAfee/AntiMalware/VSControl stopoas'

#---------------------------------------------#
# RUNNERS
# -------------------------------------------#
alias bi="brew install"
alias brewup='brew update; brew doctor; brew upgrade; brew cleanup'
alias alert="osascript -e 'display notification \"Task Finished\" with title \"CMD\"'; afplay /System/Library/Sounds/Glass.aiff"
alias ser='python -m SimpleHTTPServer $1' # start server

#---------------------------------------------#
# RARE USE
# -------------------------------------------#
alias mkssh="openssl rsa -in ~/.ssh/id_rsa | openssl base64 | tr -d '\n' | pbcopy"
alias pipes="pipes.sh -f 60 -s 8"

#---------------------------------------------#
# NODE
# -------------------------------------------#
alias npc="rm -rf ./node_modules; npm cache clear; npm i"
alias npl="npm -g ls --depth=0"
alias yy="eval \$(cat package.json | jq -S '.scripts' | sed '1d;$d' | fzf -i --header='[run:]' | sed -E \"s/\\\"(.*)\\\":.*/yarn run \\1/\" )"

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
# GIT
# -------------------------------------------#
alias gBranch='git rev-parse --abbrev-ref HEAD'
alias gclean="git clean -dfX"
alias gcp="git cherry-pick"
alias gfuck='git fetch origin; git reset --hard origin ${gBranch}'
alias gignore="git rm -r --cached .; git add .; git commit -m '.gitignore is now working'"
alias gkill="git branch | grep -v \"master\" | xargs git branch -D"
alias gl="git log --oneline master..HEAD"
alias gmm="git checkout master && git pull && git checkout - && git rebase master"
alias gnah="git reset --hard; git clean -df"
alias gnuke="git clean -dfX"
alias gr='git rebase -i HEAD~$1'
alias gundo="git reset --soft HEAD~1 && git restore --staged ."
alias gwip="git add . && git commit -nm 'wip'"
alias ghub="open -a '/Applications/Google Chrome.app' \`git remote -v | grep git@github.com | grep fetch | head -1 | cut -f2 | cut -d' ' -f1 | sed -e's/:/\//' -e 's/git@/http:\/\//'\`"

#---------------------------------------------#
# TMUXINATOR
# -------------------------------------------#
alias mux="tmuxinator"
alias muxa="mux spages; mux exc;"
alias kmux="kill_tmux_session"

##############################################
# SKY
##############################################
alias uni="unicorn_build"
alias cdi="cd ~/sky/id-idris"

#---------------------------------------------#
# OTHER
# -------------------------------------------#
alias cs="slack chat send -tx '@Waldorf akamai delete cache https://static.skyassets.com/content-api/v1/mobile-service-hub/app' -ch 'C7Y53DL90'"

alias poco="watch -n0.2 slack chat send -tx 'PCOO?' -ch 'D35J9H880'"

alias jisql="rlwrap java -cp $HOME/jisql/target/jisql-jar-with-dependencies.jar:$HOME/jdbc/ojdbc8-12.2.0.1.jar com.xigole.util.sql.Jisql -driver oraclethin -c \; "
