bindkey -s '^o' "code \$(rg --hidden --no-line-number --no-heading . | fzf --reverse | awk -F: '{print \$1}')\n"
