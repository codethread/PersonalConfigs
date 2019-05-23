#---------------------------------------------#
# EDITOR-ish
# -------------------------------------------#
alias zz="vim ~/.zshrc" # open zsh
alias vv="vim ~/.vimrc" # open vim config
alias aa="vim ~/.aliases.zsh" # open aliases
alias codij="vim -c \"Codi javascript | let ale_enabled = 0\" dmp.javascript"

alias zr="source ~/.zshrc"
alias vo="vim \$(fzf)"
alias mvim="/Applications/MacVim.app/Contents/bin/mvim"
alias als="eval \$(cat ~/.aliases.zsh | grep \"^alias.\+=\\\"\" | fzf -i --header='[run:]' | sed -E \"s:alias.*\\\"(.*)\\\":\\1:\" )"

#---------------------------------------------#
# HELPERS
# -------------------------------------------#
alias ctags="`brew --prefix`/bin/ctags"
alias grep='grep  --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'

alias ls='ls -pGF'
alias l='ls -lah'
alias lc='colorls -A --sd --gs'

alias pathis="echo $PATH | tr -s ':' '\n'"

alias portsinuse="lsof -i -p | grep -i 'listen'"
alias ports="echo lsof -i tcp:3000"

alias finder='open -a 'Finder' .'
alias butter='echo "Lucy says stop coding and relax 🐼"'

#---------------------------------------------#
# RUNNERS
# -------------------------------------------#
alias bi="brew install"
alias brewup='brew update; brew doctor; brew upgrade; brew cleanup'
alias alert="osascript -e 'display notification \"Task Finished\" with title \"CMD\"'; afplay /System/Library/Sounds/Glass.aiff"
alias ser="python -m SimpleHTTPServer $1" # start server

#---------------------------------------------#
# RARE USE
# -------------------------------------------#
alias mkssh="openssl rsa -in ~/.ssh/id_rsa | openssl base64 | tr -d '\n' | pbcopy"
alias pipes="pipes.sh -f 60 -s 8"

#---------------------------------------------#
# NODE
# -------------------------------------------#
alias nps="echo \$(cat package.json | jq -S '.scripts' | fzf -i --header='[run:]')"
alias npc="rm -rf ./node_modules; npm cache clear; npm i"
alias npg="node_g_installs"
alias npl="npm -g ls --depth=0"
alias npl="npm -g ls --depth=0"
alias yy="cat package.json | jq -S '.scripts' | fzf"

#---------------------------------------------#
# DOCKER
# -------------------------------------------#
alias dci='docker rmi $(docker images -a --filter=dangling=true -q)' #docker_clean_images
alias dcp='docker rm $(docker ps --filter=status=exited --filter=status=created -q)' #docker_clean_ps
alias dcup='docker-compose up'
alias dcdn='docker-compose down'
alias dick="docker ps | grep '[a-z0-9]' | awk '{ print $1 }' | xargs docker kill"

#---------------------------------------------#
# GIT
# -------------------------------------------#
alias gBranch="git rev-parse --abbrev-ref HEAD"
alias gignore="git rm -r --cached .; git add .; git commit -m '.gitignore is now working'"
alias gmm="git checkout master && git pull && git checkout - && git rebase master"
alias gcp="git cherry-pick"
alias gkill="git branch | grep -v "master" | xargs git branch -D"
alias gfuck="git fetch origin; git reset --hard origin ${gBranch}"
alias gnah="git reset --hard; git clean -df"
alias gh="open \`git remote -v | grep git@github.com | grep fetch | head -1 | cut -f2 | cut -d' ' -f1 | sed -e's/:/\//' -e 's/git@/http:\/\//'\`"
alias gr="git rebase -i HEAD~$1"
alias gclean="git clean -dfX"
alias gwip="git add . && git commit -nm 'wip'"
alias gl="git --no-pager log -n 5"

#---------------------------------------------#
# TMUXINATOR
# -------------------------------------------#
alias mux="tmuxinator start"
alias muxa="mux spages; mux exc;"
alias kmux="kill_tmux_session"

##############################################
# SKY
##############################################
alias uni="unicorn_build"

#---------------------------------------------#
# SPAGES
# -------------------------------------------#
alias spg="cd /users/adh23/service/sky-pages"
alias sps="pages start dev --apps mobile,bill,myaccount,mobile-bill | lolcat"
alias spu="pages start dev --apps mobile-service | lolcat"
alias spt="mv .env .nenv; pages test unit -q; mv .nenv .env; alert"
alias spc="ctags -R apps/ && ctags -R -a src/"

alias spd="docker_deploy_nimbus"
alias spn="git pull; npm i; sps"

#---------------------------------------------#
# Pages-lib
# -------------------------------------------#
alias libg="cd /users/adh23/service/pages-lib"
alias libs="yarn storybook:start"
alias libw="yarn watch:pages-apps"
alias libt="yarn test --watch --coverage --changedSince=master"


alias libapollo="git cherry-pick 613033541867eb1b1f52d303b007d6193101aef2"
# build
alias liba="yarn build:apollo-content"
alias libo="yarn build:organisms"
alias libm="yarn build:molecules"

#---------------------------------------------#
# pages-apps
# -------------------------------------------#
alias pag="cd /users/adh23/service/pages-apps"
alias pas="yarn start:dev"

#---------------------------------------------#
# SKYPORT
# -------------------------------------------#
alias gqg="cd /users/adh23/service/skyport-graphql"
alias gqs="start_skyport"
# function gqs () { start_skyport "$1" | skyportJq }
alias gqt="skyport_test_modified_files"
alias gqc="ctags -R src/schema/"
alias gqv="compare_skyport_vault_keys"

#---------------------------------------------#
# OTHER
# -------------------------------------------#
alias ppp="start_ppp"
alias cs="slack chat send -tx '@Waldorf akamai delete cache https://static.skyassets.com/content-api/v1/mobile-service-hub/app' -ch 'C7Y53DL90'"

alias poco="watch -n0.2 slack chat send -tx 'PCOO?' -ch 'D35J9H880'"
# alias sam="slack chat send -tx ${SLACK_MESSAGE} -ch 'D3VFV16U8'"
alias graeme="slack chat send -tx 'built' -ch 'D3U9RFCE8'"
alias ste="watch -n0.2 slack chat send -tx 'ಠ_ಠ' -ch 'DAFGH3E79'"
alias jisql="rlwrap java -cp $HOME/jisql/target/jisql-jar-with-dependencies.jar:$HOME/jdbc/ojdbc8-12.2.0.1.jar com.xigole.util.sql.Jisql -driver oraclethin -c \; "
