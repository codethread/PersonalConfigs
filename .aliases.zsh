#---------------------------------------------#
# UTILS
# -------------------------------------------#
alias npmrebuild="rm -rf ./node_modules; npm cache clear; npm i"
alias npg="node_g_installs"

alias zshe="vim ~/.zshrc"
alias zshr="source ~/.zshrc"
alias portsinuse="lsof -i -p | grep -i 'listen'"
alias ports="echo lsof -i tcp:3000"
alias lst="ls -1 -a -F -G"

alias brewup='brew update; brew doctor; brew upgrade; brew cleanup'
alias cmds='cat ~/.aliases.zsh'

alias gcp="git cherry-pick"
alias gbr="git branch | grep -v "master" | xargs git branch -D"
alias gfuck="git fetch origin; git reset --hard origin/master"
alias gh="open \`git remote -v | grep git@github.com | grep fetch | head -1 | cut -f2 | cut -d' ' -f1 | sed -e's/:/\//' -e 's/git@/http:\/\//'\`"
alias alert="osascript -e 'display notification \"Task Finished\" with title \"CMD\"'; afplay /System/Library/Sounds/Glass.aiff"
alias mkssh="openssl rsa -in ~/.ssh/id_rsa | openssl base64 | tr -d '\n' | pbcopy"

alias docker_clean_images='docker rmi $(docker images -a --filter=dangling=true -q)'
alias docker_clean_ps='docker rm $(docker ps --filter=status=exited --filter=status=created -q)'
#---------------------------------------------#
# TMUXINATOR
# -------------------------------------------#

alias mux-a="mux spages; mux exc;"
# muxs - see below
alias kmux="kill_tmux_session"

#---------------------------------------------#
# MISC
# -------------------------------------------#
alias ef="exercism fetch"
alias es="exercism submit"

alias path="echo $PATH | tr -s ':' '\n'"

alias ctags="`brew --prefix`/bin/ctags"

alias als="cat ~/.aliases.zsh | grep \"^alias.\+=\\\"\""
alias cdh="cat ~/.aliases.zsh | grep \"^alias cd.=\""
alias serv="python -m SimpleHTTPServer 8005"
alias vo="vim \$(fzf)"
alias pipes="pipes.sh -f 60 -s 8"
alias gql="pbpaste | jq '.info.message | split(\result:\") | .[1] | fromjson' "

alias poco="watch -n20 slack chat send -tx 'PCOO?' -ch 'D35J9H880'"
alias graeme="slack chat send -tx 'built' -ch 'D3U9RFCE8'"
#---------------------------------------------#
# SKY
# -------------------------------------------#
# scripts
# -------------------------------------------#
alias ppp="start_ppp"
alias skyport="start_skyport"
# alias skyport="start_skyport | jq -R -r '. as $line | try fromjson .info.message | split(\"options:\") | .[1] | fromjson'"

# alias spages="start_spages"
alias spages="pages start dev --apps mobile,bill,employee-engagement | lolcat"
alias muxs="tmuxinator_spages"
alias st="skyport_test_modified_files"
alias dp="docker_deploy_nimbus"
alias ctagp="ctags -R apps/ && ctags -R -a src/"
alias npml="npm -g ls --depth=0"

# alias stest="mv .env .notenv || true  && npm run test:unit || true && mv .notenv .env"
alias pt="mv .env .nenv; pages test unit -q; mv .nenv .env; alert"
alias portkeys="compare_skyport_vault_keys"

alias cdp="cd /users/adh23/service/sky-pages"
alias cdg="cd /users/adh23/service/skyport-graphql"

# for f in apps/mobile/**/*.jsx; do sed -i.bak "s/SWAP_ADDITIONAL_DAMGAE/SWAP_ADDITIONAL_DAMAGE/g" "$f"; done
# for f in ./*.js; do sed -i.bak "s/RUN/READ/g" "$f"; done
# curl -sLf https://spacevim.org/install.sh | bash
# curl -sLf https://spacevim.org/install.sh | bash -s -- --uninstall

