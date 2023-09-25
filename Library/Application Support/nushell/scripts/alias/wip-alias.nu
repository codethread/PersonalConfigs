# export alias portsinuse = lsof -i -p | grep -i 'listen'
# export alias treee='tree src -I "*~"'

#---------------------------------------------#
# GIT
# -------------------------------------------#
# export alias -g gbranch='git rev-parse --abbrev-ref HEAD'
# export alias -g gtrunk = git symbolic-ref refs/remotes/origin/HEAD | sed 's@^refs/remotes/origin/@@'
# export alias gclean = git clean -dfX
# export alias gcp = git cherry-pick
# export alias gst = git status -s
# INVALID multiline
# export alias gfuck='git fetch origin; git reset --hard origin/$(gbranch)'
# INVALID multiline
# export alias gignore = git rm -r --cached .; git add .; git commit -m '.gitignore is now working'
# export alias gkill='git branch | grep -v "$( gtrunk )" | xargs git branch -D'
# export alias gl='git log --oneline $( gtrunk )..HEAD'
# export alias grr = gl | wc -l | rargs -p '\\s*(.*)\\s*' git rebase -i HEAD~{1}
# INVALID multicomand
# export alias gmm='git checkout $( gtrunk ) && git pull && git checkout - && git rebase $( gtrunk )'
# INVALID multicomand
# export alias gmm='git fetch origin $(gtrunk):$(gtrunk) && git rebase $( gtrunk )'
# INVALID multiline
# export alias gnah = git reset --hard; git clean -df
# export alias gnuke = git clean -dfX
# export alias gr='git rebase -i HEAD~$1'
# INVALID multicomand
# export alias gundo = git reset --soft HEAD~1 && git restore --staged .
# INVALID multicomand
# export alias gwip = git add . && git commit -nm 'wip'
# export alias bdiff = git diff --name-only --relative --diff-filter=d | xargs bat --diff
# export alias gpopular = git log --format=format: --name-only --since=12.month | egrep -v '^$' | sort | uniq -c | sort -nr | head -50
# export alias gbranch-mine = git for-each-ref --format=' %(authorname) %09 %(refname)' --sort=authorname | grep 'adam.hall'
# export alias gconflict = git diff --name-only --diff-filter=U --relative
# export alias gcache-clear = git rev-parse --show-toplevel | rargs rm -rf {0}/.git/rr-cache # https://stackoverflow.com/questions/18500016/undo-a-git-rerere-resolution-that-was-done-in-a-rebase

# worktree
# export alias gw = git worktree

