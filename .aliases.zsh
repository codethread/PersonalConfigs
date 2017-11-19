#---------------------------------------------#
# UTILS
# -------------------------------------------#
alias npmrebuild="rm -rf ./node_modules; npm cache clear; npm i"

alias zshe="vim ~/.zshrc"
alias zshr="source ~/.zshrc"
alias portsinuse="lsof -i -p | grep -i 'listen'"
alias lst="ls -1 -a -F -G"

alias brewup='brew update; brew upgrade; brew prune; brew cleanup; brew doctor'
alias cmds='cat ~/.aliases.zsh'
#---------------------------------------------#
# TMUXINATOR
# -------------------------------------------#

alias mux-a="mux spages; mux exc;"
# muxs - see below
alias kmux="kill_tmux_session"

#---------------------------------------------#
# SKY
# -------------------------------------------#
# scripts
# -------------------------------------------#
alias ppp="start_ppp"
alias skyport="start_skyport"
alias spages="start_spages"
alias muxs="tmuxinator_spages"
alias st="skyport_test_modified_files"

# alias stest="mv .env .notenv || true  && npm run test:unit || true && mv .notenv .env"
alias stest="pages test --no-bail unit"
alias portkeys="compare_skyport_vault_keys"
alias dspages="deploy_pages"

alias cdp="cd /users/adh23/service/sky-pages"
alias cdg="cd /users/adh23/service/skyport-graphql"

#---------------------------------------------#
# MISC
# -------------------------------------------#
alias ef="exercism fetch"
alias es="exercism submit"

alias ctags="`brew --prefix`/bin/ctags"

alias als="cat ~/.aliases.zsh | grep \"^alias.\+=\\\"\""
alias cdh="cat ~/.aliases.zsh | grep \"^alias cd.=\""

